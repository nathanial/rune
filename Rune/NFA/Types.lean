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
  | charClass (bracket : BracketExpr) -- Character class from bracket expression
  | dot                            -- Any character (except newline)
  deriving Repr, BEq, Inhabited

namespace TransLabel

/-- Check if a character matches this transition label -/
def test (label : TransLabel) (c : Char) : Bool :=
  match label with
  | .epsilon => false  -- Epsilon never matches a character
  | .char x => c == x
  | .charClass br => br.test c
  | .dot => c != '\n'

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
  deriving Repr

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
