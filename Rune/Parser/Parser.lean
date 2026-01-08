/-
  Rune - Parser for POSIX ERE using Sift combinators
-/

import Sift
import Rune.Core.Error
import Rune.Core.CharClass
import Rune.AST.Types

namespace Rune.Parser

/-! ### Parser State -/

/-- User state for tracking capture groups and flags during parsing -/
structure RegexParserState where
  captureCount : Nat := 0
  namedGroups : List (String × Nat) := []
  flags : RegexFlags := {}
  deriving Repr, Inhabited

/-- Parser type alias for regex parsing -/
abbrev RP (α : Type) := Sift.Parser RegexParserState α

/-! ### State Helper Functions -/

/-- Allocate a new capture group index -/
def newCaptureGroup : RP Nat := do
  let s ← Sift.Parser.getUserState
  let idx := s.captureCount
  Sift.Parser.modifyUserState fun st => { st with captureCount := st.captureCount + 1 }
  pure idx

/-- Register a named group -/
def registerNamedGroup (name : String) (idx : Nat) : RP Unit :=
  Sift.Parser.modifyUserState fun st => { st with namedGroups := st.namedGroups ++ [(name, idx)] }

/-- Set a flag -/
def setFlag (flag : Char) : RP Unit :=
  Sift.Parser.modifyUserState fun st =>
    match flag with
    | 'i' => { st with flags := { st.flags with caseInsensitive := true } }
    | 'm' => { st with flags := { st.flags with multiline := true } }
    | 's' => { st with flags := { st.flags with dotAll := true } }
    | _ => st

/-- Check if a character is a flag character -/
def isFlagChar (c : Char) : Bool :=
  c == 'i' || c == 'm' || c == 's'

/-- Parse a sequence of flag characters -/
partial def parseFlags : RP RegexFlags := do
  let mut flags : RegexFlags := {}
  while true do
    match ← Sift.peek with
    | some 'i' =>
      let _ ← Sift.anyChar
      flags := { flags with caseInsensitive := true }
    | some 'm' =>
      let _ ← Sift.anyChar
      flags := { flags with multiline := true }
    | some 's' =>
      let _ ← Sift.anyChar
      flags := { flags with dotAll := true }
    | _ => break
  pure flags

/-! ### Metacharacter Helpers -/

/-- Check if a character is a metacharacter -/
def isMetaChar (c : Char) : Bool :=
  c ∈ ['.', '*', '+', '?', '|', '(', ')', '[', ']', '{', '}', '^', '$', '\\']

/-! ### Escape Sequence Parsing -/

/-- Parse an escape sequence (after consuming the backslash) -/
def parseEscape : RP Char := do
  let c ← Sift.anyChar
  match c with
  | 'n' => pure '\n'
  | 't' => pure '\t'
  | 'r' => pure '\r'
  | 'f' => pure '\x0C'  -- form feed
  | 'v' => pure '\x0B'  -- vertical tab
  | '0' => pure '\x00'  -- null
  | '\\' => pure '\\'
  | '.' => pure '.'
  | '*' => pure '*'
  | '+' => pure '+'
  | '?' => pure '?'
  | '|' => pure '|'
  | '(' => pure '('
  | ')' => pure ')'
  | '[' => pure '['
  | ']' => pure ']'
  | '{' => pure '{'
  | '}' => pure '}'
  | '^' => pure '^'
  | '$' => pure '$'
  | '-' => pure '-'
  | _ => Sift.Parser.fail s!"invalid escape '\\{c}'"

/-! ### POSIX Character Class Parsing -/

/-- Parse a POSIX character class name [:name:] (after consuming [:) -/
def parsePOSIXClass : RP POSIXClass := do
  let name ← Sift.takeWhile1 Char.isAlpha <?> "POSIX class name"
  let _ ← Sift.string ":]" <?> "closing :]"
  match POSIXClass.fromString? name with
  | some cls => pure cls
  | none => Sift.Parser.fail s!"invalid POSIX class ':{name}:'"

/-! ### Bracket Expression Parsing -/

/-- Parse a bracket expression element -/
partial def parseBracketElem : RP CharSetElem := do
  let c ← Sift.peek!
  -- Check for POSIX class [:name:]
  if c == '[' then
    let _ ← Sift.anyChar
    match ← Sift.peek with
    | some ':' =>
      let _ ← Sift.anyChar
      let cls ← parsePOSIXClass
      pure (.posix cls)
    | _ =>
      -- Not a POSIX class, treat [ as literal
      pure (.single '[')
  -- Check for escape
  else if c == '\\' then
    let _ ← Sift.anyChar
    let ec ← parseEscape
    pure (.single ec)
  else
    let _ ← Sift.anyChar
    -- Check for range
    match ← Sift.peek with
    | some '-' =>
      -- Could be a range, peek further
      let saved ← Sift.Parser.get
      let _ ← Sift.anyChar
      match ← Sift.peek with
      | some ']' =>
        -- End of bracket, - is literal, backtrack
        Sift.Parser.set saved
        pure (.single c)
      | some hi =>
        let _ ← Sift.anyChar
        if hi < c then
          Sift.Parser.fail s!"invalid range '{c}'-'{hi}'"
        pure (.range c hi)
      | none =>
        Sift.Parser.fail "unexpected end in bracket expression"
    | _ =>
      pure (.single c)

/-- Parse a bracket expression [...] -/
partial def parseBracket : RP BracketExpr := do
  let _ ← Sift.char '['
  -- Check for negation
  let negated ← match ← Sift.peek with
    | some '^' => let _ ← Sift.anyChar; pure true
    | _ => pure false
  -- Special case: ] as first character is literal
  let mut elements : List CharSetElem := []
  if (← Sift.peek) == some ']' then
    let _ ← Sift.anyChar
    elements := [.single ']']
  -- Parse elements until ]
  while true do
    match ← Sift.peek with
    | some ']' =>
      let _ ← Sift.anyChar
      break
    | some _ =>
      let elem ← parseBracketElem
      elements := elements ++ [elem]
    | none =>
      Sift.Parser.fail "unclosed bracket expression"
  pure { negated, elements }

/-! ### Quantifier Parsing -/

/-- Parse a bounded quantifier {n}, {n,}, or {n,m} -/
def parseBoundedQuantifier : RP Quantifier := do
  let _ ← Sift.char '{'
  -- Parse first number
  let nStr ← Sift.takeWhile1 Char.isDigit <?> "number in quantifier"
  let n := nStr.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  -- Check for comma
  match ← Sift.peek with
  | some ',' =>
    let _ ← Sift.anyChar
    -- Check for upper bound
    match ← Sift.peek with
    | some '}' =>
      let _ ← Sift.anyChar
      pure (Quantifier.atLeast n)
    | some c =>
      if c.isDigit then
        let mStr ← Sift.takeWhile1 Char.isDigit
        let m := mStr.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
        let _ ← Sift.char '}' <?> "closing }"
        if m < n then
          Sift.Parser.fail s!"invalid quantifier: max {m} < min {n}"
        pure (Quantifier.between n m)
      else
        Sift.Parser.fail "expected number or '}' in quantifier"
    | none =>
      Sift.Parser.fail "unexpected end in quantifier"
  | some '}' =>
    let _ ← Sift.anyChar
    pure (Quantifier.exactly n)
  | _ =>
    Sift.Parser.fail "expected ',' or '}' in quantifier"

/-- Parse a quantifier *, +, ?, or {n,m}, with optional ? suffix for lazy matching -/
def parseQuantifier? : RP (Option Quantifier) := do
  let base? ← match ← Sift.peek with
    | some '*' => let _ ← Sift.anyChar; pure (some Quantifier.zeroOrMore)
    | some '+' => let _ ← Sift.anyChar; pure (some Quantifier.oneOrMore)
    | some '?' => let _ ← Sift.anyChar; pure (some Quantifier.zeroOrOne)
    | some '{' => some <$> parseBoundedQuantifier
    | _ => pure none
  -- Check for lazy modifier (trailing ?)
  match base? with
  | some q =>
    match ← Sift.peek with
    | some '?' =>
      let _ ← Sift.anyChar
      pure (some q.lazy)
    | _ => pure (some q)
  | none => pure none

/-! ### Mutual Recursion for Expression Parsing -/

mutual
  /-- Parse a regex (alternation of branches) -/
  partial def parseRegex : RP Expr := do
    let first ← parseBranch
    let mut result := first
    while true do
      match ← Sift.peek with
      | some '|' =>
        let _ ← Sift.anyChar
        let nxt ← parseBranch
        result := .alt result nxt
      | _ => break
    pure result

  /-- Parse a branch (concatenation of pieces) -/
  partial def parseBranch : RP Expr := do
    let mut pieces : List Expr := []
    while true do
      match ← Sift.peek with
      | none => break
      | some ')' => break
      | some '|' => break
      | _ =>
        let piece ← parsePiece
        pieces := pieces ++ [piece]
    match pieces with
    | [] => pure .empty
    | [e] => pure e
    | es => pure (.concat es)

  /-- Parse a piece (atom with optional quantifier) -/
  partial def parsePiece : RP Expr := do
    let atom ← parseAtom
    match ← parseQuantifier? with
    | some q => pure (.quantified atom q)
    | none => pure atom

  /-- Parse an atom (single matching unit) -/
  partial def parseAtom : RP Expr := do
    let c ← Sift.peek!
    match c with
    | '.' =>
      let _ ← Sift.anyChar
      pure .dot
    | '^' =>
      let _ ← Sift.anyChar
      pure .anchorStart
    | '$' =>
      let _ ← Sift.anyChar
      pure .anchorEnd
    | '[' =>
      let br ← parseBracket
      pure (.bracket br)
    | '(' =>
      parseGroup
    | '\\' =>
      let _ ← Sift.anyChar
      -- Check for shorthand classes and anchors before regular escapes
      let esc ← Sift.peek!
      match esc with
      | 'd' => let _ ← Sift.anyChar; pure (.bracket BracketExpr.digit)
      | 'D' => let _ ← Sift.anyChar; pure (.bracket BracketExpr.nonDigit)
      | 'w' => let _ ← Sift.anyChar; pure (.bracket BracketExpr.word)
      | 'W' => let _ ← Sift.anyChar; pure (.bracket BracketExpr.nonWord)
      | 's' => let _ ← Sift.anyChar; pure (.bracket BracketExpr.whitespace)
      | 'S' => let _ ← Sift.anyChar; pure (.bracket BracketExpr.nonWhitespace)
      | 'b' => let _ ← Sift.anyChar; pure .wordBoundary
      | 'B' => let _ ← Sift.anyChar; pure .nonWordBoundary
      | _ =>
        let ec ← parseEscape
        pure (.literal ec)
    | _ =>
      if isMetaChar c then
        Sift.Parser.fail s!"unexpected metacharacter '{c}'"
      else
        let _ ← Sift.anyChar
        pure (.literal c)

  /-- Parse a group (...) -/
  partial def parseGroup : RP Expr := do
    let _ ← Sift.char '('
    -- Check for special group syntax
    match ← Sift.peek with
    | some '?' =>
      let _ ← Sift.anyChar
      parseSpecialGroup
    | _ =>
      -- Capturing group (...)
      let idx ← newCaptureGroup
      let inner ← parseRegex
      let _ ← Sift.char ')' <?> "closing )"
      pure (.group (GroupKind.capturing idx) inner)

  /-- Parse special group syntax after (? -/
  partial def parseSpecialGroup : RP Expr := do
    match ← Sift.peek with
    | some ':' =>
      -- Non-capturing group (?:...)
      let _ ← Sift.anyChar
      let inner ← parseRegex
      let _ ← Sift.char ')' <?> "closing )"
      pure (.group GroupKind.nonCapturing inner)
    | some '<' =>
      -- Named group (?<name>...)
      let _ ← Sift.anyChar
      let name ← Sift.takeWhile1 (fun c => c.isAlphanum || c == '_') <?> "group name"
      let _ ← Sift.char '>' <?> "closing >"
      if name.isEmpty then
        Sift.Parser.fail "empty group name"
      let idx ← newCaptureGroup
      registerNamedGroup name idx
      let inner ← parseRegex
      let _ ← Sift.char ')' <?> "closing )"
      pure (.group (GroupKind.named name idx) inner)
    | some '=' =>
      -- Positive lookahead (?=...)
      let _ ← Sift.anyChar
      let inner ← parseRegex
      let _ ← Sift.char ')' <?> "closing )"
      pure (.positiveLookahead inner)
    | some '!' =>
      -- Negative lookahead (?!...)
      let _ ← Sift.anyChar
      let inner ← parseRegex
      let _ ← Sift.char ')' <?> "closing )"
      pure (.negativeLookahead inner)
    | some c =>
      if isFlagChar c then
        -- Flag group: (?i), (?im), (?i:...), etc.
        let flags ← parseFlags
        match ← Sift.peek with
        | some ')' =>
          -- (?i) - set global flags
          let _ ← Sift.anyChar
          if flags.caseInsensitive then setFlag 'i'
          if flags.multiline then setFlag 'm'
          if flags.dotAll then setFlag 's'
          pure .empty
        | some ':' =>
          -- (?i:...) - scoped flags (non-capturing group)
          let _ ← Sift.anyChar
          if flags.caseInsensitive then setFlag 'i'
          if flags.multiline then setFlag 'm'
          if flags.dotAll then setFlag 's'
          let inner ← parseRegex
          let _ ← Sift.char ')' <?> "closing )"
          pure (.group GroupKind.nonCapturing inner)
        | _ =>
          Sift.Parser.fail "expected ')' or ':' after flags"
      else
        Sift.Parser.fail s!"unexpected character '{c}' after '(?'"
    | none =>
      Sift.Parser.fail "unexpected end after '(?'"
end

/-! ### Public API -/

/-- Check if needle is a substring of haystack -/
private def containsSubstr (haystack needle : String) : Bool :=
  needle.length == 0 || Id.run do
    for i in [:haystack.length - needle.length + 1] do
      if haystack.drop i |>.startsWith needle then
        return true
    return false

/-- Convert Sift error to Rune ParseError based on message pattern -/
private def convertError (e : Sift.ParseError) : ParseError :=
  let msg := e.message
  let pos := e.pos.offset
  -- Check for invalid escape
  if msg.startsWith "invalid escape '\\" then
    -- Extract the character after the backslash
    let charStart := "invalid escape '\\".length
    if charStart < msg.utf8ByteSize then
      let c := String.Pos.Raw.get msg ⟨charStart⟩
      .invalidEscape pos c
    else
      .unexpectedChar pos '?' msg
  -- Check for invalid quantifier
  else if msg.startsWith "invalid quantifier:" then
    .invalidQuantifier pos (msg.drop "invalid quantifier: ".length)
  -- Check for invalid range
  else if msg.startsWith "invalid range '" then
    -- Extract lo and hi chars from "invalid range 'x'-'y'"
    let rest := msg.drop "invalid range '".length
    if rest.utf8ByteSize >= 5 then  -- Need at least "x'-'y"
      let lo := String.Pos.Raw.get rest ⟨0⟩
      -- Skip "'-'" to get to hi
      let hi := String.Pos.Raw.get rest ⟨4⟩
      .invalidRange pos lo hi
    else
      .unexpectedChar pos '?' msg
  -- Check for unclosed bracket
  else if containsSubstr msg "unclosed bracket" then
    .unbalancedBrackets pos
  -- Check for unbalanced parens
  else if containsSubstr msg "closing )" then
    .unbalancedParens pos
  -- Default
  else
    .unexpectedChar pos '?' msg

/-- Parse a regex pattern string -/
def parse (pattern : String) : Except ParseError RegexAST := do
  let initState : RegexParserState := {}
  match Sift.Parser.runWithStateFull parseRegex pattern initState with
  | .ok (expr, finalState) =>
    -- Check we consumed all input
    if finalState.pos < pattern.utf8ByteSize then
      let c := String.Pos.Raw.get pattern ⟨finalState.pos⟩
      .error (.unexpectedChar finalState.pos c "end of pattern")
    else
      .ok {
        root := expr
        captureCount := finalState.userState.captureCount
        namedGroups := finalState.userState.namedGroups
        flags := finalState.userState.flags
      }
  | .error e =>
    .error (convertError e)

end Rune.Parser
