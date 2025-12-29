# Rune

A regular expression library for Lean 4 implementing POSIX Extended Regular Expressions (ERE) with Thompson NFA simulation for guaranteed linear-time matching.

## Features

- **POSIX ERE syntax**: `.` `*` `+` `?` `|` `()` `[]` `[^]` `{n,m}`
- **Thompson NFA engine**: No catastrophic backtracking, guaranteed O(n*m) time
- **Capture groups**: `(pattern)` for numbered captures
- **Named groups**: `(?<name>pattern)` for named captures
- **Find all matches**: Non-overlapping match enumeration
- **Replace/substitute**: With backreference support (`\1`, `\g<name>`)

## Installation

Add to your `lakefile.lean`:

```lean
require rune from git "https://github.com/nathanial/rune" @ "v0.0.1"
```

## Quick Start

```lean
import Rune

-- Compile a pattern
let re ← match Rune.Regex.compile "[a-z]+" with
  | .ok r => pure r
  | .error e => throw (IO.userError s!"compile error: {e}")

-- Test if pattern matches anywhere in string
re.test "hello123"  -- true

-- Find first match
re.find "123 hello world"  -- some (match with text="hello")

-- Find all matches
re.findAll "hello world foo"  -- [match "hello", match "world", match "foo"]

-- Replace
re.replaceFirst "hello world" "X"  -- "X world"
re.replaceAll "hello world" "X"    -- "X X"

-- Replace with backreferences
let re2 ← Regex.compile "([a-z]+)"
re2.replaceAll "hello world" "[\\1]"  -- "[hello] [world]"

-- Split
let re3 ← Regex.compile ","
re3.split "a,b,c"  -- ["a", "b", "c"]
```

## API Reference

### Compilation

```lean
Regex.compile (pattern : String) : Except ParseError Regex
```

### Matching

```lean
-- Test if pattern matches anywhere in string
Regex.test (re : Regex) (input : String) : Bool

-- Find first match
Regex.find (re : Regex) (input : String) : Option Match

-- Find all non-overlapping matches
Regex.findAll (re : Regex) (input : String) : List Match

-- Check if pattern matches entire string
Regex.isMatch (re : Regex) (input : String) : Option Match

-- Count matches
Regex.count (re : Regex) (input : String) : Nat
```

### Match Results

```lean
-- Full matched text
Match.text : String

-- Match position
Match.start : Nat
Match.stop : Nat

-- Capture groups (0 = full match, 1+ = groups)
Match.group (n : Nat) : Option String

-- Named capture groups
Match.namedGroup (name : String) : Option String
```

### Replacement

```lean
-- Replace first match
Regex.replaceFirst (re : Regex) (input replacement : String) : String

-- Replace all matches
Regex.replaceAll (re : Regex) (input replacement : String) : String

-- Replace with function
Regex.replaceFirstWith (re : Regex) (input : String) (f : Match → String) : String
Regex.replaceAllWith (re : Regex) (input : String) (f : Match → String) : String
```

Backreference syntax in replacement strings:
- `\0` or `\&` - full match
- `\1`, `\2`, ... - numbered groups
- `\g<name>` - named groups
- `\\` - literal backslash

### Splitting

```lean
-- Split by pattern
Regex.split (re : Regex) (input : String) : List String

-- Split with limit
Regex.splitN (re : Regex) (input : String) (n : Nat) : List String
```

## Supported Syntax

| Pattern | Description |
|---------|-------------|
| `.` | Any character except newline |
| `*` | Zero or more (greedy) |
| `+` | One or more (greedy) |
| `?` | Zero or one (greedy) |
| `{n}` | Exactly n times |
| `{n,}` | At least n times |
| `{n,m}` | Between n and m times |
| `\|` | Alternation |
| `(...)` | Capture group |
| `(?:...)` | Non-capturing group |
| `(?<name>...)` | Named capture group |
| `[abc]` | Character class |
| `[^abc]` | Negated character class |
| `[a-z]` | Character range |
| `^` | Start anchor |
| `$` | End anchor |
| `\n` `\t` `\r` | Escape sequences |
| `\\` `\.` etc. | Escaped metacharacters |

## Building

```bash
lake build        # Build library
lake test         # Run tests
```

## Architecture

Rune uses a classic three-stage regex implementation:

1. **Parser** (`Rune/Parser/`): Recursive descent parser producing an AST
2. **Compiler** (`Rune/NFA/`): Thompson NFA construction from AST
3. **Simulator** (`Rune/Match/`): NFA simulation with capture tracking

The Thompson NFA approach guarantees linear-time matching in the input length, avoiding the exponential backtracking behavior possible with recursive backtracking engines.

## License

MIT License - see [LICENSE](LICENSE) for details.
