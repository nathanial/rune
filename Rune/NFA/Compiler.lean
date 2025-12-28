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
      Compiler.addTransition start { label := .charClass br, target := accept }
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

    | .anchorStart | .anchorEnd =>
      compileExpr .empty

  /-- Compile a quantified expression -/
  partial def compileQuantified (expr : Expr) (q : Quantifier) : Compiler Fragment := do
    match q.min, q.max with
    | 0, some 0 =>
      compileExpr .empty

    | 0, some 1 =>
      -- ? (zero or one)
      let frag ← compileExpr expr
      let start ← Compiler.newState
      let accept ← Compiler.newState
      Compiler.addEpsilon start frag.start
      Compiler.addEpsilon start accept
      Compiler.addEpsilon frag.accept accept
      return { start, accept }

    | 0, none =>
      -- * (zero or more)
      let frag ← compileExpr expr
      let start ← Compiler.newState
      let accept ← Compiler.newState
      Compiler.addEpsilon start frag.start
      Compiler.addEpsilon start accept
      Compiler.addEpsilon frag.accept frag.start
      Compiler.addEpsilon frag.accept accept
      return { start, accept }

    | 1, none =>
      -- + (one or more)
      let frag ← compileExpr expr
      let start ← Compiler.newState
      let accept ← Compiler.newState
      Compiler.addEpsilon start frag.start
      Compiler.addEpsilon frag.accept frag.start
      Compiler.addEpsilon frag.accept accept
      return { start, accept }

    | m, some n =>
      if m == n then
        compileExactly expr m
      else
        let required ← compileExactly expr m
        let optional ← compileOptionalUpTo expr (n - m)
        Compiler.addEpsilon required.accept optional.start
        return { start := required.start, accept := optional.accept }

    | m, none =>
      let required ← compileExactly expr m
      let star ← compileQuantified expr Quantifier.zeroOrMore
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
  partial def compileOptionalUpTo (expr : Expr) (n : Nat) : Compiler Fragment := do
    if n == 0 then
      compileExpr .empty
    else
      let start ← Compiler.newState
      let accept ← Compiler.newState
      let mut current := start
      for _ in [:n] do
        let frag ← compileExpr expr
        Compiler.addEpsilon current frag.start
        Compiler.addEpsilon current accept
        Compiler.addEpsilon frag.accept accept
        current := frag.accept
      return { start, accept }
end

/-- Compile a regex AST to an NFA -/
def compile (ast : RegexAST) : NFA :=
  let (frag, finalState) := compileExpr ast.root |>.run {}
  let states := finalState.states.map fun s =>
    if s.id == frag.accept then { s with isAccept := true } else s
  {
    states,
    start := frag.start,
    accept := frag.accept,
    captureCount := ast.captureCount,
    namedGroups := ast.namedGroups
  }

end Rune.NFA
