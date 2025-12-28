/-
  Rune - A Lean 4 Regular Expression Library

  Implements POSIX Extended Regular Expressions (ERE) with
  Thompson NFA simulation for guaranteed linear-time matching.

  ## Features

  - POSIX ERE syntax: `.` `*` `+` `?` `|` `()` `[]` `[^]` `{n,m}`
  - POSIX character classes: `[:alpha:]` `[:digit:]` etc.
  - Thompson NFA engine (no catastrophic backtracking)
  - Capture groups: `(pattern)` and named `(?<name>pattern)`
  - Find all non-overlapping matches
  - Replace/substitute with backreferences

  ## Quick Start

  ```lean
  import Rune

  def main : IO Unit := do
    -- Compile a regex
    let re ← match Rune.Regex.compile "[a-z]+" with
      | .ok r => pure r
      | .error e => throw (IO.userError s!"Parse error: {e}")

    -- Test if pattern matches
    IO.println s!"matches: {re.test \"hello\"}"  -- true

    -- Find first match
    if let some m := re.find "hello world" then
      IO.println s!"found: {m.text}"  -- "hello"

    -- Find all matches
    for m in re.findAll "hello world" do
      IO.println s!"match: {m.text}"  -- "hello", "world"

    -- Replace matches
    IO.println (re.replaceAll "hello world" "X")  -- "X X"
  ```

  ## Capture Groups

  ```lean
  let re ← Rune.Regex.compile "([a-z]+)@([a-z]+)"
  if let some m := re.find "user@domain" then
    IO.println (m.group 1)  -- some "user"
    IO.println (m.group 2)  -- some "domain"
  ```

  ## Named Groups

  ```lean
  let re ← Rune.Regex.compile "(?<user>[a-z]+)@(?<host>[a-z]+)"
  if let some m := re.find "user@domain" then
    IO.println (m.namedGroup "user")  -- some "user"
    IO.println (m.namedGroup "host")  -- some "domain"
  ```
-/

-- Core types
import Rune.Core.Error
import Rune.Core.CharClass

-- AST
import Rune.AST.Types

-- Parser
import Rune.Parser.Parser

-- NFA
import Rune.NFA.Types
import Rune.NFA.Compiler

-- Matching
import Rune.Match.Types
import Rune.Match.Simulation

-- Public API
import Rune.API
import Rune.Replace
