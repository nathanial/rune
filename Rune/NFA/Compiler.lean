/-
  Rune - Thompson NFA construction from regex AST
-/

import Rune.AST.Types
import Rune.NFA.Types

namespace Rune.NFA

/-- Compiler state during NFA construction -/
structure CompilerState where
  nextStateId : StateId := 0
  states : Array State := #[]
  hasLazyQuantifier : Bool := false
  subNFAs : Array NFA := #[]  -- Sub-NFAs for lookahead assertions
  -- Flags inherited from parent pattern for sub-NFA compilation
  caseInsensitive : Bool := false
  multiline : Bool := false
  dotAll : Bool := false
  deriving Repr

/-- Compiler monad -/
abbrev Compiler := StateM CompilerState

namespace Compiler

/-- Allocate a new state -/
def newState (isAccept : Bool := false) : Compiler StateId := do
  let s ← get
  let id := s.nextStateId
  let state : State := { id, isAccept }
  set { s with nextStateId := id + 1, states := s.states.push state }
  return id

/-- Add a transition from one state to another -/
def addTransition (from_ : StateId) (t : Transition) : Compiler Unit := do
  let s ← get
  if from_ < s.states.size then
    let state := s.states[from_]!
    let state' := state.addTransition t
    set { s with states := s.states.set! from_ state' }

/-- Add an epsilon transition -/
def addEpsilon (from_ to : StateId) (captures : List CaptureOp := []) : Compiler Unit :=
  addTransition from_ { label := .epsilon, target := to, captures }

/-- Mark that we've used a lazy quantifier -/
def markLazyQuantifier : Compiler Unit :=
  modify fun s => { s with hasLazyQuantifier := true }

/-- Register a sub-NFA for lookahead and return its index -/
def registerSubNFA (subNFA : NFA) : Compiler Nat := do
  let s ← get
  let idx := s.subNFAs.size
  set { s with subNFAs := s.subNFAs.push subNFA }
  return idx

end Compiler

-- Mutually recursive compile functions
mutual
  /-- Compile an expression to an NFA fragment -/
  partial def compileExpr (e : Expr) : Compiler Fragment := do
    match e with
    | .empty =>
      let s ← Compiler.newState
      return { start := s, accept := s }

    | .literal c =>
      let start ← Compiler.newState
      let accept ← Compiler.newState
      Compiler.addTransition start { label := .char c, target := accept }
      return { start, accept }

    | .dot =>
      let start ← Compiler.newState
      let accept ← Compiler.newState
      Compiler.addTransition start { label := .dot, target := accept }
      return { start, accept }

    | .bracket br =>
      let start ← Compiler.newState
      let accept ← Compiler.newState
      -- Compile the bracket expression to a bitmap for O(1) ASCII lookup
      let compiled := CompiledCharClass.compile br
      Compiler.addTransition start { label := .charClass compiled, target := accept }
      return { start, accept }

    | .concat exprs =>
      match exprs with
      | [] => compileExpr .empty
      | [e] => compileExpr e
      | e :: es => do
        let first ← compileExpr e
        let rest ← compileExpr (.concat es)
        Compiler.addEpsilon first.accept rest.start
        return { start := first.start, accept := rest.accept }

    | .alt left right =>
      let l ← compileExpr left
      let r ← compileExpr right
      let start ← Compiler.newState
      let accept ← Compiler.newState
      Compiler.addEpsilon start l.start
      Compiler.addEpsilon start r.start
      Compiler.addEpsilon l.accept accept
      Compiler.addEpsilon r.accept accept
      return { start, accept }

    | .quantified expr q =>
      compileQuantified expr q

    | .group kind expr => do
      let frag ← compileExpr expr
      match kind with
      | .capturing idx | .named _ idx =>
        let start ← Compiler.newState
        let accept ← Compiler.newState
        Compiler.addEpsilon start frag.start [.openGroup idx]
        Compiler.addEpsilon frag.accept accept [.closeGroup idx]
        return { start, accept }
      | .nonCapturing =>
        return frag

    | .anchorStart =>
      let start ← Compiler.newState
      let accept ← Compiler.newState
      Compiler.addTransition start { label := .anchorStart, target := accept }
      return { start, accept }

    | .anchorEnd =>
      let start ← Compiler.newState
      let accept ← Compiler.newState
      Compiler.addTransition start { label := .anchorEnd, target := accept }
      return { start, accept }

    | .wordBoundary =>
      let start ← Compiler.newState
      let accept ← Compiler.newState
      Compiler.addTransition start { label := .wordBoundary, target := accept }
      return { start, accept }

    | .nonWordBoundary =>
      let start ← Compiler.newState
      let accept ← Compiler.newState
      Compiler.addTransition start { label := .nonWordBoundary, target := accept }
      return { start, accept }

    | .positiveLookahead inner => do
      -- Compile the inner expression to a separate sub-NFA, inheriting parent flags
      let parentState ← get
      let (innerFrag, innerState) := compileExpr inner |>.run {
        nextStateId := 0
        caseInsensitive := parentState.caseInsensitive
        multiline := parentState.multiline
        dotAll := parentState.dotAll
      }
      let innerStates := innerState.states.map fun s =>
        if s.id == innerFrag.accept then { s with isAccept := true } else s
      let subNFA : NFA := {
        states := innerStates
        start := innerFrag.start
        accept := innerFrag.accept
        captureCount := 0  -- Lookahead captures are discarded
        namedGroups := []
        subNFAs := innerState.subNFAs  -- Nested lookaheads
        caseInsensitive := parentState.caseInsensitive
        multiline := parentState.multiline
        dotAll := parentState.dotAll
      }
      let idx ← Compiler.registerSubNFA subNFA
      let start ← Compiler.newState
      let accept ← Compiler.newState
      Compiler.addTransition start { label := .positiveLookahead idx, target := accept }
      return { start, accept }

    | .negativeLookahead inner => do
      -- Compile the inner expression to a separate sub-NFA, inheriting parent flags
      let parentState ← get
      let (innerFrag, innerState) := compileExpr inner |>.run {
        nextStateId := 0
        caseInsensitive := parentState.caseInsensitive
        multiline := parentState.multiline
        dotAll := parentState.dotAll
      }
      let innerStates := innerState.states.map fun s =>
        if s.id == innerFrag.accept then { s with isAccept := true } else s
      let subNFA : NFA := {
        states := innerStates
        start := innerFrag.start
        accept := innerFrag.accept
        captureCount := 0  -- Lookahead captures are discarded
        namedGroups := []
        subNFAs := innerState.subNFAs  -- Nested lookaheads
        caseInsensitive := parentState.caseInsensitive
        multiline := parentState.multiline
        dotAll := parentState.dotAll
      }
      let idx ← Compiler.registerSubNFA subNFA
      let start ← Compiler.newState
      let accept ← Compiler.newState
      Compiler.addTransition start { label := .negativeLookahead idx, target := accept }
      return { start, accept }

  /-- Compile a quantified expression -/
  partial def compileQuantified (expr : Expr) (q : Quantifier) : Compiler Fragment := do
    match q.min, q.max with
    | 0, some 0 =>
      compileExpr .empty

    | 0, some 1 =>
      -- ? or ?? (zero or one)
      let frag ← compileExpr expr
      let start ← Compiler.newState
      let accept ← Compiler.newState
      -- For greedy: try to match first, then skip
      -- For lazy: try to skip first, then match
      if q.greedy then
        Compiler.addEpsilon start frag.start
        Compiler.addEpsilon start accept
      else
        Compiler.markLazyQuantifier
        Compiler.addEpsilon start accept
        Compiler.addEpsilon start frag.start
      Compiler.addEpsilon frag.accept accept
      return { start, accept }

    | 0, none =>
      -- * or *? (zero or more)
      let frag ← compileExpr expr
      let start ← Compiler.newState
      let accept ← Compiler.newState
      -- For greedy: try to match first, then skip
      -- For lazy: try to skip first, then match
      if q.greedy then
        Compiler.addEpsilon start frag.start
        Compiler.addEpsilon start accept
        Compiler.addEpsilon frag.accept frag.start
        Compiler.addEpsilon frag.accept accept
      else
        Compiler.markLazyQuantifier
        Compiler.addEpsilon start accept
        Compiler.addEpsilon start frag.start
        Compiler.addEpsilon frag.accept accept
        Compiler.addEpsilon frag.accept frag.start
      return { start, accept }

    | 1, none =>
      -- + or +? (one or more)
      let frag ← compileExpr expr
      let start ← Compiler.newState
      let accept ← Compiler.newState
      Compiler.addEpsilon start frag.start
      -- For greedy: try to repeat first, then accept
      -- For lazy: try to accept first, then repeat
      if q.greedy then
        Compiler.addEpsilon frag.accept frag.start
        Compiler.addEpsilon frag.accept accept
      else
        Compiler.markLazyQuantifier
        Compiler.addEpsilon frag.accept accept
        Compiler.addEpsilon frag.accept frag.start
      return { start, accept }

    | m, some n =>
      if m == n then
        compileExactly expr m
      else
        let required ← compileExactly expr m
        let optional ← compileOptionalUpTo expr (n - m) q.greedy
        Compiler.addEpsilon required.accept optional.start
        return { start := required.start, accept := optional.accept }

    | m, none =>
      let required ← compileExactly expr m
      -- Pass greediness to the unbounded part
      let starQ := if q.greedy then Quantifier.zeroOrMore else Quantifier.zeroOrMoreLazy
      let star ← compileQuantified expr starQ
      Compiler.addEpsilon required.accept star.start
      return { start := required.start, accept := star.accept }

  /-- Compile exactly n copies of an expression -/
  partial def compileExactly (expr : Expr) (n : Nat) : Compiler Fragment := do
    if n == 0 then
      compileExpr .empty
    else if n == 1 then
      compileExpr expr
    else
      let first ← compileExpr expr
      let rest ← compileExactly expr (n - 1)
      Compiler.addEpsilon first.accept rest.start
      return { start := first.start, accept := rest.accept }

  /-- Compile 0 to n optional copies of an expression -/
  partial def compileOptionalUpTo (expr : Expr) (n : Nat) (greedy : Bool := true) : Compiler Fragment := do
    if n == 0 then
      compileExpr .empty
    else
      if !greedy then
        Compiler.markLazyQuantifier
      let start ← Compiler.newState
      let accept ← Compiler.newState
      let mut current := start
      for _ in [:n] do
        let frag ← compileExpr expr
        -- For greedy: try to match first, then skip
        -- For lazy: try to skip first, then match
        if greedy then
          Compiler.addEpsilon current frag.start
          Compiler.addEpsilon current accept
        else
          Compiler.addEpsilon current accept
          Compiler.addEpsilon current frag.start
        Compiler.addEpsilon frag.accept accept
        current := frag.accept
      return { start, accept }
end

/-- Compile a regex AST to an NFA -/
def compile (ast : RegexAST) : NFA :=
  let initialState : CompilerState := {
    caseInsensitive := ast.flags.caseInsensitive
    multiline := ast.flags.multiline
    dotAll := ast.flags.dotAll
  }
  let (frag, finalState) := compileExpr ast.root |>.run initialState
  let states := finalState.states.map fun s =>
    if s.id == frag.accept then { s with isAccept := true } else s
  {
    states,
    start := frag.start,
    accept := frag.accept,
    captureCount := ast.captureCount,
    namedGroups := ast.namedGroups,
    prefersShortestMatch := finalState.hasLazyQuantifier,
    caseInsensitive := ast.flags.caseInsensitive,
    multiline := ast.flags.multiline,
    dotAll := ast.flags.dotAll,
    subNFAs := finalState.subNFAs
  }

end Rune.NFA
