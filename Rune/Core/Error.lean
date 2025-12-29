/-
  Rune - Error types for regex parsing and matching
-/

namespace Rune

/-- Errors that can occur during regex parsing -/
inductive ParseError where
  | unexpectedChar (pos : Nat) (char : Char) (expected : String)
  | unexpectedEnd (context : String)
  | invalidEscape (pos : Nat) (char : Char)
  | invalidCharClass (pos : Nat) (name : String)
  | invalidQuantifier (pos : Nat) (msg : String)
  | unbalancedParens (pos : Nat)
  | unbalancedBrackets (pos : Nat)
  | emptyGroup (pos : Nat)
  | invalidGroupName (pos : Nat) (name : String)
  | invalidRange (pos : Nat) (lo hi : Char)
  deriving Repr, BEq, Inhabited

namespace ParseError

def position : ParseError → Option Nat
  | unexpectedChar pos _ _ => some pos
  | unexpectedEnd _ => none
  | invalidEscape pos _ => some pos
  | invalidCharClass pos _ => some pos
  | invalidQuantifier pos _ => some pos
  | unbalancedParens pos => some pos
  | unbalancedBrackets pos => some pos
  | emptyGroup pos => some pos
  | invalidGroupName pos _ => some pos
  | invalidRange pos _ _ => some pos

end ParseError

instance : ToString ParseError where
  toString e := match e with
    | .unexpectedChar pos c exp =>
        s!"position {pos}: unexpected character '{c}', expected {exp}"
    | .unexpectedEnd ctx =>
        s!"unexpected end of pattern while parsing {ctx}"
    | .invalidEscape pos c =>
        s!"position {pos}: invalid escape sequence '\\{c}'"
    | .invalidCharClass pos name =>
        s!"position {pos}: unknown character class '{name}'"
    | .invalidQuantifier pos msg =>
        s!"position {pos}: invalid quantifier: {msg}"
    | .unbalancedParens pos =>
        s!"position {pos}: unbalanced parentheses"
    | .unbalancedBrackets pos =>
        s!"position {pos}: unbalanced brackets"
    | .emptyGroup pos =>
        s!"position {pos}: empty group"
    | .invalidGroupName pos name =>
        s!"position {pos}: invalid group name '{name}'"
    | .invalidRange pos lo hi =>
        s!"position {pos}: invalid character range '{lo}'-'{hi}'"

end Rune
