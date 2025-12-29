/-
  Rune - Recursive descent parser for POSIX ERE
-/

import Rune.Core.Error
import Rune.Core.CharClass
import Rune.AST.Types

namespace Rune.Parser

/-- Parser state -/
structure ParserState where
  input : String
  pos : Nat := 0
  captureCount : Nat := 0
  namedGroups : List (String × Nat) := []
  flags : RegexFlags := {}
  deriving Repr

/-- Parser monad -/
abbrev Parser := ExceptT ParseError (StateM ParserState)

namespace Parser

/-- Get current position -/
def getPos : Parser Nat := do
  return (← get).pos

/-- Check if at end of input -/
def atEnd : Parser Bool := do
  let s ← get
  return s.pos >= s.input.length

/-- Peek at current character without consuming -/
def peek? : Parser (Option Char) := do
  let s ← get
  if s.pos >= s.input.length then
    return none
  else
    return some (s.input.toList[s.pos]!)

/-- Peek at current character, fail if at end -/
def peek : Parser Char := do
  match ← peek? with
  | some c => return c
  | none => throw (.unexpectedEnd "pattern")

/-- Consume and return current character -/
def next : Parser Char := do
  let c ← peek
  modify fun s => { s with pos := s.pos + 1 }
  return c

/-- Consume character if it matches expected -/
def expect (expected : Char) : Parser Unit := do
  let c ← peek
  if c == expected then
    discard next
  else
    let pos ← getPos
    throw (.unexpectedChar pos c s!"'{expected}'")

/-- Try to consume a specific character -/
def tryChar (c : Char) : Parser Bool := do
  match ← peek? with
  | some x =>
    if x == c then
      discard next
      return true
    else
      return false
  | none => return false

/-- Allocate a new capture group index -/
def newCaptureGroup : Parser Nat := do
  let s ← get
  let idx := s.captureCount
  set { s with captureCount := s.captureCount + 1 }
  return idx

/-- Register a named group -/
def registerNamedGroup (name : String) (idx : Nat) : Parser Unit := do
  modify fun s => { s with namedGroups := s.namedGroups ++ [(name, idx)] }

/-- Set a flag -/
def setFlag (flag : Char) : Parser Unit := do
  modify fun s =>
    let f := s.flags
    match flag with
    | 'i' => { s with flags := { f with caseInsensitive := true } }
    | 'm' => { s with flags := { f with multiline := true } }
    | 's' => { s with flags := { f with dotAll := true } }
    | _ => s

/-- Check if a character is a flag character -/
def isFlagChar (c : Char) : Bool :=
  c == 'i' || c == 'm' || c == 's'

/-- Parse a sequence of flag characters, returns the flags parsed -/
def parseFlags : Parser RegexFlags := do
  let mut flags : RegexFlags := {}
  while true do
    match ← peek? with
    | some 'i' =>
      discard next
      flags := { flags with caseInsensitive := true }
    | some 'm' =>
      discard next
      flags := { flags with multiline := true }
    | some 's' =>
      discard next
      flags := { flags with dotAll := true }
    | _ => break
  return flags

/-- Check if a character is a metacharacter -/
def isMetaChar (c : Char) : Bool :=
  c ∈ ['.', '*', '+', '?', '|', '(', ')', '[', ']', '{', '}', '^', '$', '\\']

/-- Parse an escape sequence -/
def parseEscape : Parser Char := do
  let pos ← getPos
  let c ← next
  match c with
  | 'n' => return '\n'
  | 't' => return '\t'
  | 'r' => return '\r'
  | 'f' => return '\x0C'  -- form feed
  | 'v' => return '\x0B'  -- vertical tab
  | '0' => return '\x00'  -- null
  | '\\' => return '\\'
  | '.' => return '.'
  | '*' => return '*'
  | '+' => return '+'
  | '?' => return '?'
  | '|' => return '|'
  | '(' => return '('
  | ')' => return ')'
  | '[' => return '['
  | ']' => return ']'
  | '{' => return '{'
  | '}' => return '}'
  | '^' => return '^'
  | '$' => return '$'
  | '-' => return '-'
  | _ => throw (.invalidEscape pos c)

/-- Parse a POSIX character class name [:name:] -/
def parsePOSIXClass : Parser POSIXClass := do
  expect ':'
  let mut name := ""
  while true do
    let c ← peek
    if c == ':' then break
    if !c.isAlpha then
      let pos ← getPos
      throw (.invalidCharClass pos name)
    name := name.push c
    discard next
  expect ':'
  expect ']'
  match POSIXClass.fromString? name with
  | some cls => return cls
  | none =>
    let pos ← getPos
    throw (.invalidCharClass pos name)

/-- Parse a bracket expression element -/
def parseBracketElem : Parser CharSetElem := do
  let c ← peek
  -- Check for POSIX class [:name:]
  if c == '[' then
    discard next
    match ← peek? with
    | some ':' =>
      let cls ← parsePOSIXClass
      return .posix cls
    | _ =>
      -- Not a POSIX class, treat [ as literal
      return .single '['
  -- Check for escape
  else if c == '\\' then
    discard next
    let ec ← parseEscape
    return .single ec
  else
    discard next
    -- Check for range
    match ← peek? with
    | some '-' =>
      -- Could be a range
      let savedPos := (← get).pos
      discard next
      match ← peek? with
      | some ']' =>
        -- End of bracket, - is literal, backtrack
        modify fun s => { s with pos := savedPos }
        return .single c
      | some hi =>
        discard next
        if hi < c then
          let pos ← getPos
          throw (.invalidRange pos c hi)
        return .range c hi
      | none =>
        throw (.unexpectedEnd "bracket expression")
    | _ =>
      return .single c

/-- Parse a bracket expression [...] -/
def parseBracket : Parser BracketExpr := do
  let pos ← getPos
  expect '['
  -- Check for negation
  let negated ← tryChar '^'
  -- Special case: ] as first character is literal
  let mut elements : List CharSetElem := []
  if (← peek?) == some ']' then
    discard next
    elements := [.single ']']
  -- Parse elements until ]
  while true do
    match ← peek? with
    | some ']' =>
      discard next
      break
    | some _ =>
      let elem ← parseBracketElem
      elements := elements ++ [elem]
    | none =>
      throw (.unbalancedBrackets pos)
  return { negated, elements }

/-- Parse a bounded quantifier {n}, {n,}, or {n,m} -/
def parseBoundedQuantifier : Parser Quantifier := do
  let pos ← getPos
  expect '{'
  -- Parse first number
  let mut n := 0
  let mut hasDigit := false
  while true do
    match ← peek? with
    | some c =>
      if c.isDigit then
        n := n * 10 + (c.toNat - '0'.toNat)
        hasDigit := true
        discard next
      else
        break
    | none =>
      throw (.unexpectedEnd "quantifier")
  if !hasDigit then
    throw (.invalidQuantifier pos "expected number")
  -- Check for comma
  if ← tryChar ',' then
    -- Check for upper bound
    match ← peek? with
    | some '}' =>
      discard next
      return Quantifier.atLeast n
    | some c =>
      if c.isDigit then
        let mut m := 0
        while true do
          match ← peek? with
          | some c =>
            if c.isDigit then
              m := m * 10 + (c.toNat - '0'.toNat)
              discard next
            else
              break
          | none =>
            throw (.unexpectedEnd "quantifier")
        expect '}'
        if m < n then
          throw (.invalidQuantifier pos s!"max {m} < min {n}")
        return Quantifier.between n m
      else
        throw (.invalidQuantifier pos s!"expected number or '}}'")
    | none =>
      throw (.unexpectedEnd "quantifier")
  else
    expect '}'
    return Quantifier.exactly n

/-- Parse a quantifier *, +, ?, or {n,m}, with optional ? suffix for lazy matching -/
def parseQuantifier? : Parser (Option Quantifier) := do
  let base? ← match ← peek? with
  | some '*' => discard next; pure (some Quantifier.zeroOrMore)
  | some '+' => discard next; pure (some Quantifier.oneOrMore)
  | some '?' => discard next; pure (some Quantifier.zeroOrOne)
  | some '{' => pure (some (← parseBoundedQuantifier))
  | _ => pure none
  -- Check for lazy modifier (trailing ?)
  match base? with
  | some q =>
    if ← tryChar '?' then
      return some q.lazy
    else
      return some q
  | none => return none

end Parser

-- Mutually recursive parsing functions
mutual
  /-- Parse a regex (alternation of branches) -/
  partial def parseRegex : Parser Expr := do
    let first ← parseBranch
    let mut result := first
    while (← Parser.tryChar '|') do
      let nxt ← parseBranch
      result := .alt result nxt
    return result

  /-- Parse a branch (concatenation of pieces) -/
  partial def parseBranch : Parser Expr := do
    let mut pieces : List Expr := []
    while true do
      match ← Parser.peek? with
      | none => break
      | some ')' => break
      | some '|' => break
      | _ =>
        let piece ← parsePiece
        pieces := pieces ++ [piece]
    match pieces with
    | [] => return .empty
    | [e] => return e
    | es => return .concat es

  /-- Parse a piece (atom with optional quantifier) -/
  partial def parsePiece : Parser Expr := do
    let atom ← parseAtom
    match ← Parser.parseQuantifier? with
    | some q => return .quantified atom q
    | none => return atom

  /-- Parse an atom (single matching unit) -/
  partial def parseAtom : Parser Expr := do
    let c ← Parser.peek
    match c with
    | '.' =>
      discard Parser.next
      return .dot
    | '^' =>
      discard Parser.next
      return .anchorStart
    | '$' =>
      discard Parser.next
      return .anchorEnd
    | '[' =>
      let br ← Parser.parseBracket
      return .bracket br
    | '(' =>
      parseGroup
    | '\\' =>
      discard Parser.next
      -- Check for shorthand classes and anchors before regular escapes
      let esc ← Parser.peek
      match esc with
      | 'd' => discard Parser.next; return .bracket BracketExpr.digit
      | 'D' => discard Parser.next; return .bracket BracketExpr.nonDigit
      | 'w' => discard Parser.next; return .bracket BracketExpr.word
      | 'W' => discard Parser.next; return .bracket BracketExpr.nonWord
      | 's' => discard Parser.next; return .bracket BracketExpr.whitespace
      | 'S' => discard Parser.next; return .bracket BracketExpr.nonWhitespace
      | 'b' => discard Parser.next; return .wordBoundary
      | 'B' => discard Parser.next; return .nonWordBoundary
      | _ =>
        let ec ← Parser.parseEscape
        return .literal ec
    | _ =>
      if Parser.isMetaChar c then
        let pos ← Parser.getPos
        throw (.unexpectedChar pos c "literal or metacharacter")
      else
        discard Parser.next
        return .literal c

  /-- Parse a group (...) -/
  partial def parseGroup : Parser Expr := do
    let pos ← Parser.getPos
    Parser.expect '('
    -- Check for special group syntax
    if ← Parser.tryChar '?' then
      match ← Parser.peek? with
      | some ':' =>
        -- Non-capturing group (?:...)
        discard Parser.next
        let inner ← parseRegex
        match ← Parser.peek? with
        | some ')' =>
          discard Parser.next
          return .group GroupKind.nonCapturing inner
        | _ =>
          throw (.unbalancedParens pos)
      | some '<' =>
        -- Named group (?<name>...)
        discard Parser.next
        let mut name := ""
        while true do
          let c ← Parser.peek
          if c == '>' then
            discard Parser.next
            break
          if !c.isAlphanum && c != '_' then
            throw (.invalidGroupName pos name)
          name := name.push c
          discard Parser.next
        if name.isEmpty then
          throw (.invalidGroupName pos name)
        let idx ← Parser.newCaptureGroup
        Parser.registerNamedGroup name idx
        let inner ← parseRegex
        match ← Parser.peek? with
        | some ')' =>
          discard Parser.next
          return .group (GroupKind.named name idx) inner
        | _ =>
          throw (.unbalancedParens pos)
      | some '=' =>
        -- Positive lookahead (?=...)
        discard Parser.next
        let inner ← parseRegex
        match ← Parser.peek? with
        | some ')' =>
          discard Parser.next
          return .positiveLookahead inner
        | _ =>
          throw (.unbalancedParens pos)
      | some '!' =>
        -- Negative lookahead (?!...)
        discard Parser.next
        let inner ← parseRegex
        match ← Parser.peek? with
        | some ')' =>
          discard Parser.next
          return .negativeLookahead inner
        | _ =>
          throw (.unbalancedParens pos)
      | some c =>
        if Parser.isFlagChar c then
          -- Flag group: (?i), (?im), (?i:...), etc.
          let flags ← Parser.parseFlags
          match ← Parser.peek? with
          | some ')' =>
            -- (?i) - set global flags
            discard Parser.next
            for flag in ['i', 'm', 's'] do
              if flag == 'i' && flags.caseInsensitive then
                Parser.setFlag 'i'
              else if flag == 'm' && flags.multiline then
                Parser.setFlag 'm'
              else if flag == 's' && flags.dotAll then
                Parser.setFlag 's'
            return .empty
          | some ':' =>
            -- (?i:...) - scoped flags (non-capturing group)
            -- For simplicity, we set global flags (not truly scoped)
            discard Parser.next
            for flag in ['i', 'm', 's'] do
              if flag == 'i' && flags.caseInsensitive then
                Parser.setFlag 'i'
              else if flag == 'm' && flags.multiline then
                Parser.setFlag 'm'
              else if flag == 's' && flags.dotAll then
                Parser.setFlag 's'
            let inner ← parseRegex
            match ← Parser.peek? with
            | some ')' =>
              discard Parser.next
              return .group GroupKind.nonCapturing inner
            | _ =>
              throw (.unbalancedParens pos)
          | _ =>
            throw (.unexpectedEnd "flag group")
        else
          throw (.unexpectedEnd "group specifier")
      | none =>
        throw (.unexpectedEnd "group specifier")
    else
      -- Capturing group (...)
      let idx ← Parser.newCaptureGroup
      let inner ← parseRegex
      match ← Parser.peek? with
      | some ')' =>
        discard Parser.next
        return .group (GroupKind.capturing idx) inner
      | _ =>
        throw (.unbalancedParens pos)
end

/-- Parse a regex pattern string -/
def parse (pattern : String) : Except ParseError RegexAST := do
  let initialState : ParserState := { input := pattern }
  let (result, finalState) := parseRegex.run initialState
  match result with
  | .ok expr =>
    -- Check we consumed all input
    if finalState.pos < pattern.length then
      let c := pattern.toList[finalState.pos]!
      .error (.unexpectedChar finalState.pos c "end of pattern")
    else
      .ok {
        root := expr
        captureCount := finalState.captureCount
        namedGroups := finalState.namedGroups
        flags := finalState.flags
      }
  | .error e => .error e

end Rune.Parser
