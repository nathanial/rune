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

/-- Check if an anchor condition is satisfied at the given position -/
def checkAnchor (label : TransLabel) (pos : Nat) (inputLength : Nat) : Bool :=
  match label with
  | .anchorStart => pos == 0
  | .anchorEnd => pos == inputLength
  | _ => true  -- Non-anchor labels always pass

/-- Compute epsilon closure from a set of threads -/
partial def epsilonClosure (nfa : NFA) (threads : List Thread) (pos : Nat) (inputLength : Nat)
    : List Thread := Id.run do
  let mut visited : StateSet := {}
  let mut result : List Thread := []
  let mut worklist := threads

  while !worklist.isEmpty do
    match worklist with
    | [] => break
    | thread :: rest =>
      worklist := rest
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
            -- Check anchor conditions before following
            if checkAnchor t.label pos inputLength then
              let newThread := thread.applyCaptureOps t.captures pos
              worklist := { newThread with stateId := t.target } :: worklist
          else
            hasCharTransition := true
        -- If this state has character transitions, add to result
        if hasCharTransition || state.isAccept then
          result := thread :: result

  result

/-- Advance threads by one character -/
def step (nfa : NFA) (threads : List Thread) (c : Char) (pos : Nat) (inputLength : Nat)
    : List Thread := Id.run do
  let mut nextThreads : List Thread := []

  for thread in threads do
    match nfa.getState thread.stateId with
    | none => continue
    | some state =>
      for t in state.transitions do
        if t.label.test c then
          let newThread := thread.applyCaptureOps t.captures pos
          nextThreads := { newThread with stateId := t.target } :: nextThreads

  -- Compute epsilon closure of next states
  epsilonClosure nfa nextThreads (pos + 1) inputLength

/-- Check if any thread is in an accept state -/
def findAcceptingThread (nfa : NFA) (threads : List Thread) : Option Thread :=
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
  let inputLength := input.length

  let mut threads := epsilonClosure nfa [initialThread] startPos inputLength
  let mut lastMatch : Option (Thread × Nat) := none
  let mut pos := startPos

  -- Check for immediate match (empty pattern at start)
  if let some thread := findAcceptingThread nfa threads then
    lastMatch := some (thread, pos)

  -- Process each character
  let chars := input.toList.drop startPos
  for c in chars do
    threads := step nfa threads c pos inputLength
    pos := pos + 1

    -- Check for match (keep the longest)
    if let some thread := findAcceptingThread nfa threads then
      lastMatch := some (thread, pos)

    if threads.isEmpty then
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
      results := results ++ [m]
      -- Advance past this match (but at least one character)
      pos := max (m.stop) (pos + 1)
    | none =>
      pos := pos + 1

  results

/-- Test if pattern matches anywhere in string -/
def test (nfa : NFA) (input : String) : Bool :=
  (find nfa input).isSome

/-- Count number of matches -/
def count (nfa : NFA) (input : String) : Nat :=
  (findAll nfa input).length

end Rune.Match
