/-
  Rune - NFA state machine types
-/

import Rune.Core.CharClass

namespace Rune.NFA

/-- NFA state identifier -/
abbrev StateId := Nat

/-- Transition label - what must be matched to traverse this edge -/
inductive TransLabel where
  | epsilon                        -- Epsilon transition (no input consumed)
  | char (c : Char)                -- Single character
  | charClass (compiled : CompiledCharClass) -- Compiled character class with O(1) ASCII lookup
  | dot                            -- Any character (except newline)
  | anchorStart                    -- ^ anchor (must be at start of input)
  | anchorEnd                      -- $ anchor (must be at end of input)
  | wordBoundary                   -- \b anchor (at word/non-word boundary)
  | nonWordBoundary                -- \B anchor (not at word boundary)
  | positiveLookahead (nfaIndex : Nat)  -- (?=...) references sub-NFA by index
  | negativeLookahead (nfaIndex : Nat)  -- (?!...) references sub-NFA by index
  deriving Repr, BEq, Inhabited

namespace TransLabel

/-- Case-insensitive character comparison -/
@[inline]
private def charEqIgnoreCase (a b : Char) : Bool :=
  a == b || a.toLower == b.toLower

/-- Check if a character matches this transition label -/
@[inline]
def test (label : TransLabel) (c : Char) (caseInsensitive : Bool := false) (dotAll : Bool := false) : Bool :=
  match label with
  | .epsilon => false  -- Epsilon never matches a character
  | .char x =>
    if caseInsensitive then charEqIgnoreCase c x else c == x
  | .charClass cc =>
    if caseInsensitive then
      -- For case-insensitive, test both cases
      cc.test c || cc.test c.toLower || cc.test c.toUpper
    else
      cc.test c
  | .dot =>
    if dotAll then true else c != '\n'
  | .anchorStart => false  -- Anchors never match characters
  | .anchorEnd => false
  | .wordBoundary => false
  | .nonWordBoundary => false
  | .positiveLookahead _ => false  -- Lookaheads are zero-width
  | .negativeLookahead _ => false

/-- Check if this is a zero-width transition (epsilon or anchor or lookahead) -/
def isZeroWidth : TransLabel → Bool
  | .epsilon => true
  | .anchorStart => true
  | .anchorEnd => true
  | .wordBoundary => true
  | .nonWordBoundary => true
  | .positiveLookahead _ => true
  | .negativeLookahead _ => true
  | _ => false

/-- Check if this is an epsilon transition -/
def isEpsilon : TransLabel → Bool
  | .epsilon => true
  | _ => false

end TransLabel

/-- Capture operation to perform when traversing an edge -/
inductive CaptureOp where
  | openGroup (index : Nat)        -- Start of capture group
  | closeGroup (index : Nat)       -- End of capture group
  deriving Repr, BEq, Inhabited

/-- A single NFA transition (edge) -/
structure Transition where
  label : TransLabel
  target : StateId
  captures : List CaptureOp := []  -- Capture operations on this edge
  deriving Repr, BEq, Inhabited

/-- An NFA state -/
structure State where
  id : StateId
  transitions : Array Transition := #[]
  isAccept : Bool := false
  deriving Repr, BEq, Inhabited

namespace State

/-- Add a transition to this state -/
def addTransition (s : State) (t : Transition) : State :=
  { s with transitions := s.transitions.push t }

/-- Get all epsilon transitions from this state -/
def epsilonTransitions (s : State) : Array Transition :=
  s.transitions.filter (·.label.isEpsilon)

/-- Get all non-epsilon transitions from this state -/
def charTransitions (s : State) : Array Transition :=
  s.transitions.filter (!·.label.isEpsilon)

end State

/-- Thompson NFA representation -/
structure NFA where
  states : Array State
  start : StateId
  accept : StateId                 -- Single accept state (Thompson property)
  captureCount : Nat
  namedGroups : List (String × Nat)
  prefersShortestMatch : Bool := false  -- True if pattern has lazy quantifiers
  caseInsensitive : Bool := false       -- (?i) flag
  multiline : Bool := false             -- (?m) flag
  dotAll : Bool := false                -- (?s) flag
  subNFAs : Array NFA := #[]            -- Sub-NFAs for lookahead assertions
  deriving Repr, Inhabited

namespace NFA

/-- Get a state by ID -/
def getState (nfa : NFA) (id : StateId) : Option State :=
  if h : id < nfa.states.size then
    some (nfa.states[id])
  else
    none

/-- Get a state by ID (unsafe) -/
def getState! (nfa : NFA) (id : StateId) : State :=
  nfa.states[id]!

/-- Number of states in the NFA -/
def numStates (nfa : NFA) : Nat :=
  nfa.states.size

/-- Check if a state is the accept state -/
def isAcceptState (nfa : NFA) (id : StateId) : Bool :=
  id == nfa.accept

end NFA

/-- NFA fragment during construction (intermediate representation) -/
structure Fragment where
  start : StateId
  accept : StateId
  deriving Repr, BEq, Inhabited

end Rune.NFA
