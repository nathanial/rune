/-
  Rune - Public API for regular expressions
-/

import Rune.Core.Error
import Rune.AST.Types
import Rune.Parser.Parser
import Rune.NFA.Types
import Rune.NFA.Compiler
import Rune.Match.Types
import Rune.Match.Simulation

namespace Rune

/-- A compiled regular expression -/
structure Regex where
  private mk ::
  nfa : NFA.NFA
  pattern : String
  deriving Repr

namespace Regex

/-- Compile a regex pattern string -/
def compile (pattern : String) : Except ParseError Regex := do
  let ast ← Parser.parse pattern
  let nfa := NFA.compile ast
  return { nfa, pattern }

/-- Get the original pattern string -/
def getPattern (re : Regex) : String :=
  re.pattern

/-- Get the number of capture groups -/
def captureCount (re : Regex) : Nat :=
  re.nfa.captureCount

/-- Check if the entire string matches the pattern -/
def isMatch (re : Regex) (input : String) : Option Match :=
  Match.matchFull re.nfa input

/-- Find the first match anywhere in the string -/
def find (re : Regex) (input : String) : Option Match :=
  Match.find re.nfa input

/-- Find all non-overlapping matches -/
def findAll (re : Regex) (input : String) : List Match :=
  Match.findAll re.nfa input

/-- Test if pattern matches anywhere in string -/
def test (re : Regex) (input : String) : Bool :=
  Match.test re.nfa input

/-- Count number of matches -/
def count (re : Regex) (input : String) : Nat :=
  Match.count re.nfa input

end Regex

end Rune
