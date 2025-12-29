/-
  Rune.Macro - Compile-time regex validation

  The `regex%` macro validates regex patterns at compile time,
  catching syntax errors during compilation rather than at runtime.

  Usage:
    let re := regex% "[a-z]+"
    -- `re` has type `Regex` (not `Except ParseError Regex`)
-/
import Lean
import Rune.Core.Error
import Rune.Parser.Parser
import Rune.API

namespace Rune

open Lean Elab Term

/-- Compile-time regex validation and construction.

    Validates the pattern at compile time. If the pattern is invalid,
    reports a compile error. If valid, generates code that compiles
    the pattern at runtime (which is guaranteed to succeed).

    Example:
    ```lean
    let emailPattern := regex% "[a-zA-Z0-9]+@[a-zA-Z0-9]+\\.[a-z]+"
    -- emailPattern : Regex

    -- This would fail at compile time:
    -- let bad := regex% "[unclosed"
    -- Error: regex pattern error: unbalanced brackets
    ```
-/
elab "regex% " pattern:str : term => do
  let patternStr := pattern.getString
  -- Validate at compile time by running the parser
  match Parser.parse patternStr with
  | .error err =>
    -- Report compile-time error with detailed message
    throwError s!"regex pattern error: {err}"
  | .ok _ =>
    -- Pattern is valid - generate runtime compilation call
    -- We use Regex.compile! which panics on failure (safe since we validated)
    let stx ← `(Rune.Regex.compile! $(Lean.quote patternStr))
    elabTerm stx none

end Rune
