/-
  Rune - Thompson NFA simulation engine
-/

import Std.Data.HashSet
import Rune.NFA.Types
import Rune.Match.Types

namespace Rune.Match

open NFA

/-- Set of visited states for epsilon closure -/
abbrev StateSet := Std.HashSet StateId

/-- Check if a character at a position is a word character (using char array) -/
def isWordCharAtArray (chars : Array Char) (pos : Nat) : Bool :=
  if pos < chars.size then
    BracketExpr.isWordChar (chars[pos]!)
  else
    false

/-- Check if a character at a position is a word character -/
def isWordCharAt (input : String) (pos : Nat) : Bool :=
  isWordCharAtArray input.toList.toArray pos

/-- Check if position is at a word boundary -/
def isAtWordBoundary (input : String) (pos : Nat) : Bool :=
  let prevIsWord := pos > 0 && isWordCharAt input (pos - 1)
  let currIsWord := isWordCharAt input pos
  prevIsWord != currIsWord

/-- Check if position is at start of line (for multiline mode) -/
def isAtLineStart (input : String) (pos : Nat) : Bool :=
  pos == 0 || (pos > 0 && input.toList.getD (pos - 1) ' ' == '\n')

/-- Check if position is at end of line (for multiline mode) -/
def isAtLineEnd (input : String) (pos : Nat) : Bool :=
  pos == input.length || input.toList.getD pos ' ' == '\n'

/-- Check if an anchor condition is satisfied at the given position -/
def checkAnchor (label : TransLabel) (input : String) (pos : Nat) (multiline : Bool := false) : Bool :=
  match label with
  | .anchorStart =>
    if multiline then isAtLineStart input pos else pos == 0
  | .anchorEnd =>
    if multiline then isAtLineEnd input pos else pos == input.length
  | .wordBoundary => isAtWordBoundary input pos
  | .nonWordBoundary => !isAtWordBoundary input pos
  | .positiveLookahead _ => false  -- Handled separately in epsilonClosure
  | .negativeLookahead _ => false  -- Handled separately in epsilonClosure
  | _ => true  -- Non-anchor labels always pass

mutual
  /-- Evaluate a lookahead sub-NFA at the current position.
      Returns true if the sub-NFA can match starting at pos. -/
  partial def evaluateLookahead (nfa : NFA) (subNFAIndex : Nat) (input : String) (pos : Nat)
      : Bool := Id.run do
    -- Get the sub-NFA from the NFA's registry
    match nfa.subNFAs[subNFAIndex]? with
    | none => return false  -- Invalid index
    | some subNFA =>
      -- Create initial thread for sub-NFA (no captures needed)
      let initialThread := Thread.initial subNFA.start 0

      -- Compute epsilon closure at current position
      let mut threads := epsilonClosureForLookahead subNFA #[initialThread] input pos

      -- Check for immediate match
      if threads.any fun t => subNFA.isAcceptState t.stateId then
        return true

      -- Process remaining input
      let chars := input.toList.toArray
      let inputLen := chars.size
      let mut currentPos := pos

      while currentPos < inputLen do
        let c := chars[currentPos]!

        -- Step: advance threads by one character
        let mut nextThreads : Array Thread := #[]
        for thread in threads do
          match subNFA.getState thread.stateId with
          | none => continue
          | some state =>
            for t in state.transitions do
              if t.label.test c subNFA.caseInsensitive subNFA.dotAll then
                nextThreads := nextThreads.push { thread with stateId := t.target }

        -- Compute epsilon closure of next states
        threads := epsilonClosureForLookahead subNFA nextThreads input (currentPos + 1)
        currentPos := currentPos + 1

        -- Check for match
        if threads.any fun t => subNFA.isAcceptState t.stateId then
          return true

        if threads.size == 0 then
          break

      return false

  /-- Epsilon closure for lookahead evaluation (handles nested lookaheads) -/
  partial def epsilonClosureForLookahead (subNFA : NFA) (threads : Array Thread)
      (input : String) (pos : Nat) : Array Thread := Id.run do
    let mut visited : StateSet := {}
    let mut result : Array Thread := #[]
    let mut worklist := threads
    let mut idx := 0

    while idx < worklist.size do
      let thread := worklist[idx]!
      idx := idx + 1

      if visited.contains thread.stateId then
        continue
      visited := visited.insert thread.stateId

      match subNFA.getState thread.stateId with
      | none => continue
      | some state =>
        let mut hasCharTransition := false
        for t in state.transitions do
          if t.label.isZeroWidth then
            -- Check anchors
            if checkAnchor t.label input pos subNFA.multiline then
              worklist := worklist.push { thread with stateId := t.target }
            else
              -- Handle nested lookaheads using THIS sub-NFA's sub-NFA registry
              match t.label with
              | .positiveLookahead subIdx =>
                if evaluateLookahead subNFA subIdx input pos then
                  worklist := worklist.push { thread with stateId := t.target }
              | .negativeLookahead subIdx =>
                if !evaluateLookahead subNFA subIdx input pos then
                  worklist := worklist.push { thread with stateId := t.target }
              | _ => pure ()
          else
            hasCharTransition := true
        if hasCharTransition || state.isAccept then
          result := result.push thread

    result

  /-- Compute epsilon closure from a set of threads (using Array for performance) -/
  partial def epsilonClosure (nfa : NFA) (threads : Array Thread) (input : String) (pos : Nat)
      : Array Thread := Id.run do
    let mut visited : StateSet := {}
    let mut result : Array Thread := #[]
    let mut worklist := threads
    let mut idx := 0

    while idx < worklist.size do
      let thread := worklist[idx]!
      idx := idx + 1

      if visited.contains thread.stateId then
        continue
      visited := visited.insert thread.stateId

      match nfa.getState thread.stateId with
      | none => continue
      | some state =>
        -- Process all zero-width transitions (epsilon and anchors)
        let mut hasCharTransition := false
        for t in state.transitions do
          if t.label.isZeroWidth then
            -- Check anchor conditions before following (respecting multiline flag)
            if checkAnchor t.label input pos nfa.multiline then
              let newThread := thread.applyCaptureOps t.captures pos
              worklist := worklist.push { newThread with stateId := t.target }
            else
              -- Handle lookahead transitions
              match t.label with
              | .positiveLookahead subIdx =>
                if evaluateLookahead nfa subIdx input pos then
                  let newThread := thread.applyCaptureOps t.captures pos
                  worklist := worklist.push { newThread with stateId := t.target }
              | .negativeLookahead subIdx =>
                if !evaluateLookahead nfa subIdx input pos then
                  let newThread := thread.applyCaptureOps t.captures pos
                  worklist := worklist.push { newThread with stateId := t.target }
              | _ => pure ()
          else
            hasCharTransition := true
        -- If this state has character transitions, add to result
        if hasCharTransition || state.isAccept then
          result := result.push thread

    result
end

/-- Advance threads by one character (using Array for performance) -/
def step (nfa : NFA) (threads : Array Thread) (c : Char) (input : String) (pos : Nat)
    : Array Thread := Id.run do
  let mut nextThreads : Array Thread := #[]

  for thread in threads do
    match nfa.getState thread.stateId with
    | none => continue
    | some state =>
      for t in state.transitions do
        -- Pass caseInsensitive and dotAll flags to character matching
        if t.label.test c nfa.caseInsensitive nfa.dotAll then
          let newThread := thread.applyCaptureOps t.captures pos
          nextThreads := nextThreads.push { newThread with stateId := t.target }

  -- Compute epsilon closure of next states
  epsilonClosure nfa nextThreads input (pos + 1)

/-- Check if any thread is in an accept state -/
def findAcceptingThread (nfa : NFA) (threads : Array Thread) : Option Thread :=
  threads.find? fun t => nfa.isAcceptState t.stateId

/-- Build a Match from a successful thread -/
def buildMatch (input : String) (startPos endPos : Nat) (thread : Thread)
    (namedGroups : List (String × Nat)) : Match :=
  {
    fullMatch := { start := startPos, stop := endPos },
    captures := thread.extractCaptures,
    namedGroups,
    input
  }

/-- Find a match starting at a specific position -/
def findMatchAt (nfa : NFA) (input : String) (startPos : Nat) : Option Match := Id.run do
  let initialThread := Thread.initial nfa.start nfa.captureCount

  let mut threads := epsilonClosure nfa #[initialThread] input startPos
  let mut lastMatch : Option (Thread × Nat) := none
  let mut pos := startPos

  -- Check for immediate match (empty pattern at start)
  if let some thread := findAcceptingThread nfa threads then
    -- For lazy patterns, return the first match immediately
    if nfa.prefersShortestMatch then
      return some (buildMatch input startPos pos thread nfa.namedGroups)
    lastMatch := some (thread, pos)

  -- Process each character using array indexing (toList.toArray computed once)
  let chars := input.toList.toArray
  let inputLen := chars.size

  while pos < inputLen do
    let c := chars[pos]!
    threads := step nfa threads c input pos
    pos := pos + 1

    -- Check for match
    if let some thread := findAcceptingThread nfa threads then
      -- For lazy patterns, return the first match immediately
      if nfa.prefersShortestMatch then
        return some (buildMatch input startPos pos thread nfa.namedGroups)
      -- For greedy patterns, keep the longest match
      lastMatch := some (thread, pos)

    if threads.size == 0 then
      break

  -- Build match result from last matching thread
  match lastMatch with
  | some (thread, endPos) =>
    some (buildMatch input startPos endPos thread nfa.namedGroups)
  | none => none

/-- Find the first match anywhere in the string -/
def find (nfa : NFA) (input : String) : Option Match := Id.run do
  for startPos in List.range (input.length + 1) do
    if let some m := findMatchAt nfa input startPos then
      return m
  return none

/-- Check if pattern matches entire string -/
def matchFull (nfa : NFA) (input : String) : Option Match :=
  match findMatchAt nfa input 0 with
  | some m =>
    if m.stop == input.length then some m else none
  | none => none

/-- Find all non-overlapping matches -/
def findAll (nfa : NFA) (input : String) : List Match := Id.run do
  let mut results : List Match := []
  let mut pos := 0

  while pos <= input.length do
    match findMatchAt nfa input pos with
    | some m =>
      -- Build list in reverse (O(1) prepend instead of O(n) append)
      results := m :: results
      -- Advance past this match (but at least one character)
      pos := max (m.stop) (pos + 1)
    | none =>
      pos := pos + 1

  -- Reverse to get correct order
  results.reverse

/-- Test if pattern matches anywhere in string -/
def test (nfa : NFA) (input : String) : Bool :=
  (find nfa input).isSome

/-- Count number of matches -/
def count (nfa : NFA) (input : String) : Nat :=
  (findAll nfa input).length

end Rune.Match
