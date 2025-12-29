/-
  Rune - Character class types and matching
-/

namespace Rune

/-- POSIX character class names -/
inductive POSIXClass where
  | alnum   -- Alphanumeric characters
  | alpha   -- Alphabetic characters
  | blank   -- Space and tab
  | cntrl   -- Control characters
  | digit   -- Decimal digits
  | graph   -- Visible characters (not space)
  | lower   -- Lowercase letters
  | print   -- Visible characters (including space)
  | punct   -- Punctuation characters
  | space   -- Whitespace characters
  | upper   -- Uppercase letters
  | xdigit  -- Hexadecimal digits
  deriving Repr, BEq, Inhabited, DecidableEq

namespace POSIXClass

/-- Parse a POSIX class name string -/
def fromString? (s : String) : Option POSIXClass :=
  match s with
  | "alnum"  => some .alnum
  | "alpha"  => some .alpha
  | "blank"  => some .blank
  | "cntrl"  => some .cntrl
  | "digit"  => some .digit
  | "graph"  => some .graph
  | "lower"  => some .lower
  | "print"  => some .print
  | "punct"  => some .punct
  | "space"  => some .space
  | "upper"  => some .upper
  | "xdigit" => some .xdigit
  | _        => none

/-- Check if a character matches this POSIX class -/
def test (cls : POSIXClass) (c : Char) : Bool :=
  match cls with
  | .alnum  => c.isAlphanum
  | .alpha  => c.isAlpha
  | .blank  => c == ' ' || c == '\t'
  | .cntrl  => c.toNat < 32 || c.toNat == 127
  | .digit  => c.isDigit
  | .graph  => c.toNat > 32 && c.toNat < 127
  | .lower  => c.isLower
  | .print  => c.toNat >= 32 && c.toNat < 127
  | .punct  => isPunct c
  | .space  => c == ' ' || c == '\t' || c == '\n' || c == '\r' ||
               c == '\x0C' || c == '\x0B'  -- form feed, vertical tab
  | .upper  => c.isUpper
  | .xdigit => c.isDigit || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
where
  isPunct (c : Char) : Bool :=
    let n := c.toNat
    (33 <= n && n <= 47) ||   -- ! " # $ % & ' ( ) * + , - . /
    (58 <= n && n <= 64) ||   -- : ; < = > ? @
    (91 <= n && n <= 96) ||   -- [ \ ] ^ _ `
    (123 <= n && n <= 126)    -- { | } ~

end POSIXClass

/-- A single element in a bracket expression -/
inductive CharSetElem where
  | single (c : Char)           -- Single character
  | range (lo hi : Char)        -- Character range [a-z]
  | posix (cls : POSIXClass)    -- POSIX class [:alpha:]
  deriving Repr, BEq, Inhabited

namespace CharSetElem

/-- Check if a character matches this element -/
def test (elem : CharSetElem) (c : Char) : Bool :=
  match elem with
  | .single x => c == x
  | .range lo hi => lo <= c && c <= hi
  | .posix cls => cls.test c

end CharSetElem

/-- A bracket expression with possible negation -/
structure BracketExpr where
  negated : Bool := false
  elements : List CharSetElem := []
  deriving Repr, BEq, Inhabited

namespace BracketExpr

/-- Check if a character matches this bracket expression -/
def test (br : BracketExpr) (c : Char) : Bool :=
  let matchesAny := br.elements.any (·.test c)
  if br.negated then !matchesAny else matchesAny

/-- Create a bracket expression for a single character -/
def single (c : Char) : BracketExpr :=
  { elements := [.single c] }

/-- Create a bracket expression for a character range -/
def range (lo hi : Char) : BracketExpr :=
  { elements := [.range lo hi] }

/-- Create a negated bracket expression -/
def negate (br : BracketExpr) : BracketExpr :=
  { br with negated := !br.negated }

/-- Combine two bracket expressions (union) -/
def union (a b : BracketExpr) : BracketExpr :=
  { negated := false, elements := a.elements ++ b.elements }

-- Predefined shorthand character classes

/-- \d - digit characters [0-9] -/
def digit : BracketExpr :=
  { elements := [.range '0' '9'] }

/-- \D - non-digit characters [^0-9] -/
def nonDigit : BracketExpr :=
  { negated := true, elements := [.range '0' '9'] }

/-- \w - word characters [a-zA-Z0-9_] -/
def word : BracketExpr :=
  { elements := [.range 'a' 'z', .range 'A' 'Z', .range '0' '9', .single '_'] }

/-- \W - non-word characters [^a-zA-Z0-9_] -/
def nonWord : BracketExpr :=
  { negated := true, elements := [.range 'a' 'z', .range 'A' 'Z', .range '0' '9', .single '_'] }

/-- \s - whitespace characters [ \t\n\r\f\v] -/
def whitespace : BracketExpr :=
  { elements := [.single ' ', .single '\t', .single '\n', .single '\r',
                 .single '\x0C', .single '\x0B'] }

/-- \S - non-whitespace characters [^ \t\n\r\f\v] -/
def nonWhitespace : BracketExpr :=
  { negated := true, elements := [.single ' ', .single '\t', .single '\n', .single '\r',
                                   .single '\x0C', .single '\x0B'] }

/-- Check if a character is a word character (for \b boundary checking) -/
def isWordChar (c : Char) : Bool :=
  c.isAlphanum || c == '_'

end BracketExpr

end Rune
