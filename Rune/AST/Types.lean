/-
  Rune - Abstract Syntax Tree types for regular expressions
-/

import Rune.Core.CharClass

namespace Rune

/-- Quantifier bounds for repetition operators -/
structure Quantifier where
  min : Nat              -- Minimum number of repetitions
  max : Option Nat       -- Maximum (None = unbounded)
  greedy : Bool := true  -- Greedy (default) or lazy matching
  deriving Repr, BEq, Inhabited

namespace Quantifier

/-- Zero or more: * -/
def zeroOrMore : Quantifier := { min := 0, max := none }

/-- One or more: + -/
def oneOrMore : Quantifier := { min := 1, max := none }

/-- Zero or one: ? -/
def zeroOrOne : Quantifier := { min := 0, max := some 1 }

/-- Exactly n: {n} -/
def exactly (n : Nat) : Quantifier := { min := n, max := some n }

/-- At least n: {n,} -/
def atLeast (n : Nat) : Quantifier := { min := n, max := none }

/-- Between m and n: {m,n} -/
def between (m n : Nat) : Quantifier := { min := m, max := some n }

/-- Check if this quantifier matches exactly one occurrence -/
def isExactlyOne (q : Quantifier) : Bool :=
  q.min == 1 && q.max == some 1

/-- Make a quantifier lazy (non-greedy) -/
def lazy (q : Quantifier) : Quantifier :=
  { q with greedy := false }

/-- Zero or more lazy: *? -/
def zeroOrMoreLazy : Quantifier := { min := 0, max := none, greedy := false }

/-- One or more lazy: +? -/
def oneOrMoreLazy : Quantifier := { min := 1, max := none, greedy := false }

/-- Zero or one lazy: ?? -/
def zeroOrOneLazy : Quantifier := { min := 0, max := some 1, greedy := false }

end Quantifier

/-- Kind of capture group -/
inductive GroupKind where
  | capturing (index : Nat)                -- (pattern) - numbered group
  | named (name : String) (index : Nat)    -- (?<name>pattern) - named group
  | nonCapturing                           -- (?:pattern) - non-capturing
  deriving Repr, BEq, Inhabited

namespace GroupKind

/-- Get the capture index if this is a capturing group -/
def captureIndex? : GroupKind → Option Nat
  | capturing idx => some idx
  | named _ idx => some idx
  | nonCapturing => none

/-- Check if this is a capturing group -/
def isCapturing : GroupKind → Bool
  | nonCapturing => false
  | _ => true

end GroupKind

/-- Abstract syntax tree for regular expressions -/
inductive Expr where
  | empty                                    -- Empty pattern (matches empty string)
  | literal (c : Char)                       -- Single character literal
  | dot                                      -- . (any character except newline)
  | bracket (expr : BracketExpr)             -- [...] or [^...]
  | concat (exprs : List Expr)               -- Concatenation
  | alt (left right : Expr)                  -- | alternation
  | quantified (expr : Expr) (q : Quantifier) -- Quantified expression
  | group (kind : GroupKind) (expr : Expr)   -- Grouped expression
  | anchorStart                              -- ^ anchor
  | anchorEnd                                -- $ anchor
  | wordBoundary                             -- \b anchor
  | nonWordBoundary                          -- \B anchor
  | positiveLookahead (expr : Expr)          -- (?=...) positive lookahead
  | negativeLookahead (expr : Expr)          -- (?!...) negative lookahead
  deriving Repr, BEq, Inhabited

namespace Expr

/-- Create a literal expression from a string -/
def fromString (s : String) : Expr :=
  match s.toList with
  | [] => .empty
  | [c] => .literal c
  | cs => .concat (cs.map .literal)

/-- Check if this expression can match the empty string -/
partial def canMatchEmpty : Expr → Bool
  | .empty => true
  | .literal _ => false
  | .dot => false
  | .bracket _ => false
  | .concat exprs => exprs.all canMatchEmpty
  | .alt l r => l.canMatchEmpty || r.canMatchEmpty
  | .quantified _ q => q.min == 0
  | .group _ e => e.canMatchEmpty
  | .anchorStart => true
  | .anchorEnd => true
  | .wordBoundary => true
  | .nonWordBoundary => true
  | .positiveLookahead _ => true  -- Zero-width, always matches empty
  | .negativeLookahead _ => true  -- Zero-width, always matches empty

end Expr

/-- Regex matching flags/modifiers -/
structure RegexFlags where
  caseInsensitive : Bool := false  -- (?i) - case-insensitive matching
  multiline : Bool := false        -- (?m) - ^ and $ match line boundaries
  dotAll : Bool := false           -- (?s) - . matches newlines
  deriving Repr, BEq, Inhabited

namespace RegexFlags

/-- Default flags (all disabled) -/
def default : RegexFlags := {}

/-- Merge two flag sets (new flags override) -/
def merge (base new : RegexFlags) : RegexFlags :=
  { caseInsensitive := new.caseInsensitive || base.caseInsensitive
  , multiline := new.multiline || base.multiline
  , dotAll := new.dotAll || base.dotAll
  }

end RegexFlags

/-- A parsed regex with metadata -/
structure RegexAST where
  root : Expr
  captureCount : Nat                          -- Number of capture groups
  namedGroups : List (String × Nat)           -- Named group mappings
  flags : RegexFlags := {}                    -- Global flags
  deriving Repr

namespace RegexAST

/-- Create an empty regex AST -/
def empty : RegexAST :=
  { root := .empty, captureCount := 0, namedGroups := [] }

/-- Get a named group's index -/
def getNamedGroup (ast : RegexAST) (name : String) : Option Nat :=
  ast.namedGroups.find? (·.1 == name) |>.map (·.2)

end RegexAST

end Rune
