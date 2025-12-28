/-
  Rune - Match result types
-/

import Rune.NFA.Types

namespace Rune

/-- A captured substring from a match -/
structure Capture where
  start : Nat           -- Start position (inclusive)
  stop : Nat            -- End position (exclusive)
  deriving Repr, BEq, Inhabited

namespace Capture

/-- Extract the captured text from the input string -/
def text (c : Capture) (input : String) : String :=
  (input.toSubstring.extract ⟨c.start⟩ ⟨c.stop⟩).toString

/-- Length of the capture -/
def length (c : Capture) : Nat :=
  c.stop - c.start

/-- Check if the capture is empty -/
def isEmpty (c : Capture) : Bool :=
  c.start == c.stop

end Capture

/-- Result of a successful match -/
structure Match where
  fullMatch : Capture                          -- The entire matched region
  captures : Array (Option Capture)            -- Indexed capture groups
  namedGroups : List (String × Nat)            -- Named group index mapping
  input : String                               -- Original input string
  deriving Repr

namespace Match

/-- Get the full matched text -/
def text (m : Match) : String :=
  m.fullMatch.text m.input

/-- Get the start position of the match -/
def start (m : Match) : Nat :=
  m.fullMatch.start

/-- Get the end position of the match -/
def stop (m : Match) : Nat :=
  m.fullMatch.stop

/-- Get capture group by index (1-indexed, 0 returns full match) -/
def group (m : Match) (n : Nat) : Option String :=
  if n == 0 then
    some m.text
  else if h : n - 1 < m.captures.size then
    match m.captures[n - 1] with
    | some cap => some (cap.text m.input)
    | none => none
  else
    none

/-- Get capture group by name -/
def namedGroup (m : Match) (name : String) : Option String :=
  match m.namedGroups.find? (·.1 == name) with
  | some (_, idx) => m.group (idx + 1)
  | none => none

/-- Get all capture groups as strings -/
def groups (m : Match) : List (Option String) :=
  m.captures.toList.map fun cap =>
    cap.map (·.text m.input)

/-- Number of capture groups (not including full match) -/
def numCaptures (m : Match) : Nat :=
  m.captures.size

end Match

/-- Thread state during NFA simulation -/
structure Thread where
  stateId : NFA.StateId
  captureStarts : Array (Option Nat)  -- Start positions of capture groups
  captureEnds : Array (Option Nat)    -- End positions of capture groups
  priority : Nat := 0                 -- For leftmost-longest matching
  deriving Repr, BEq, Inhabited

namespace Thread

/-- Create an initial thread at the given state -/
def initial (stateId : NFA.StateId) (captureCount : Nat) : Thread :=
  {
    stateId,
    captureStarts := Array.replicate captureCount none,
    captureEnds := Array.replicate captureCount none
  }

/-- Apply a capture operation to this thread -/
def applyCaptureOp (t : Thread) (op : NFA.CaptureOp) (pos : Nat) : Thread :=
  match op with
  | .openGroup idx =>
    if idx < t.captureStarts.size then
      { t with captureStarts := t.captureStarts.set! idx (some pos) }
    else t
  | .closeGroup idx =>
    if idx < t.captureEnds.size then
      { t with captureEnds := t.captureEnds.set! idx (some pos) }
    else t

/-- Apply multiple capture operations -/
def applyCaptureOps (t : Thread) (ops : List NFA.CaptureOp) (pos : Nat) : Thread :=
  ops.foldl (fun thread op => thread.applyCaptureOp op pos) t

/-- Extract captures from a completed thread -/
def extractCaptures (t : Thread) : Array (Option Capture) :=
  let pairs := t.captureStarts.zip t.captureEnds
  pairs.map fun (startOpt, endOpt) =>
    match startOpt, endOpt with
    | some s, some e => some { start := s, stop := e }
    | _, _ => none

/-- Move thread to a new state -/
def moveTo (t : Thread) (newState : NFA.StateId) : Thread :=
  { t with stateId := newState }

end Thread

end Rune
