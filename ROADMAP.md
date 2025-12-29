# Rune Roadmap

This document tracks potential improvements, new features, and code cleanup opportunities for the Rune regular expression library.

---

## Feature Proposals

### [COMPLETED] Shorthand Character Classes

**Status:** Implemented and tested (2024-12-28)

**Description:** Added support for `\d`, `\w`, `\s` and their negations (`\D`, `\W`, `\S`).

**Changes Made:**
- Added predefined `BracketExpr` patterns in `Core/CharClass.lean`: `digit`, `nonDigit`, `word`, `nonWord`, `whitespace`, `nonWhitespace`
- Updated `parseAtom` in `Parser/Parser.lean` to recognize shorthand escapes and return bracket expressions
- Added `isWordChar` helper for word boundary checking

**Test Coverage:** 7 new tests in `MatchTests.lean`:
- `digit class \d`, `non-digit class \D`
- `word class \w`, `non-word class \W`
- `whitespace class \s`, `non-whitespace class \S`
- `combined shorthand classes`

---

### [COMPLETED] Word Boundary Anchors

**Status:** Implemented and tested (2024-12-28)

**Description:** Added support for `\b` (word boundary) and `\B` (non-word boundary) anchors.

**Changes Made:**
- Added `wordBoundary` and `nonWordBoundary` variants to `Expr` in `AST/Types.lean`
- Added corresponding `TransLabel` variants in `NFA/Types.lean`
- Updated parser to recognize `\b` and `\B` escapes
- Updated compiler to emit word boundary transitions
- Added `isWordCharAt` and `isAtWordBoundary` helpers in `Simulation.lean`
- Updated `checkAnchor` to validate word boundary conditions

**Test Coverage:** 6 new tests in `MatchTests.lean`:
- `word boundary \b basic`, `word boundary at start`, `word boundary at end`
- `non-word boundary \B`, `word boundary with digits`, `word boundary findAll`

---

### [COMPLETED] Anchor Enforcement in NFA

**Status:** Implemented and tested (2024-12-28)

**Description:** The `^` and `$` anchors now correctly enforce start/end positioning.

**Changes Made:**
- Added `anchorStart` and `anchorEnd` variants to `TransLabel` in `NFA/Types.lean`
- Added `isZeroWidth` function to identify zero-width transitions (epsilon + anchors)
- Compiler now emits proper anchor transitions instead of empty expressions
- `epsilonClosure` checks anchor conditions: `^` passes only at position 0, `$` passes only at input length
- Added 9 comprehensive anchor tests covering basic usage, alternations, quantifiers, empty strings, and findAll

**Test Coverage:** 9 new tests in `MatchTests.lean`:
- `start anchor basic`, `end anchor basic`, `both anchors require full match`
- `anchors with quantifiers`, `start anchor with alternation`, `end anchor with alternation`
- `anchors with empty string`, `anchored findAll`, `end anchored pattern mid-string fails`

---

### [COMPLETED] Lazy (Non-Greedy) Quantifiers

**Status:** Implemented and tested (2024-12-28)

**Description:** Added support for lazy quantifiers `*?`, `+?`, `??`, `{n,m}?` that match as few characters as possible.

**Changes Made:**
- Added `greedy : Bool := true` field to `Quantifier` in `AST/Types.lean`
- Added lazy quantifier constructors: `Quantifier.lazy`, `zeroOrMoreLazy`, `oneOrMoreLazy`, `zeroOrOneLazy`
- Updated parser to recognize `?` suffix after quantifiers for lazy matching
- Updated NFA compiler to adjust epsilon transition ordering for lazy mode (skip before take)
- Added `hasLazyQuantifier` tracking in compiler state
- Added `prefersShortestMatch` flag to NFA structure
- Updated simulation to return first match for lazy patterns instead of longest match

**Test Coverage:** 24 new tests in `MatchTests.lean`:
- Basic lazy quantifiers: `*?`, `+?`, `??`
- HTML tag matching example
- Lazy with captures, anchors, word boundaries
- Bounded lazy quantifiers: `{n,m}?`, `{n,}?`, `{0,n}?`
- Greedy vs lazy comparisons
- Edge cases: empty input, nested patterns, alternation

---

### [COMPLETED] Lookahead Assertions

**Status:** Implemented and tested (2024-12-28)

**Description:** Added support for positive lookahead `(?=...)` and negative lookahead `(?!...)`.

**Changes Made:**
- Added `positiveLookahead` and `negativeLookahead` variants to `Expr` in `AST/Types.lean`
- Added corresponding `TransLabel` variants in `NFA/Types.lean` with sub-NFA index references
- Added `subNFAs : Array NFA` field to NFA structure for storing lookahead patterns
- Updated parser to recognize `(?=...)` and `(?!...)` syntax in groups
- Compiler creates separate sub-NFAs for lookahead patterns with flag inheritance
- Added `evaluateLookahead` function in simulation to run sub-NFA matching at current position
- Updated `epsilonClosure` to handle lookahead transitions as zero-width assertions
- Supports nested lookaheads (sub-NFAs can reference their own sub-NFAs)

**Test Coverage:** 20 new tests in `MatchTests.lean`:
- Basic positive/negative lookahead
- Lookahead does not consume input
- Lookahead with findAll
- Nested lookaheads (positive in positive, negative in positive)
- Lookahead with quantifiers, alternation, character classes
- Multiple consecutive lookaheads (password validation pattern)
- Lookahead with anchors, word boundaries, captures
- Case-insensitive lookahead (flag inheritance)

---

### [COMPLETED] Regex Flags/Modifiers

**Status:** Implemented and tested (2024-12-28)

**Description:** Added support for flags `(?i)` for case-insensitive matching, `(?m)` for multiline mode (where `^`/`$` match line boundaries), and `(?s)` for dotall mode (where `.` matches newlines).

**Changes Made:**
- Added `RegexFlags` structure to `AST/Types.lean` with `caseInsensitive`, `multiline`, `dotAll` fields
- Updated `RegexAST` and `NFA` structures to store flags
- Updated parser to recognize inline flags `(?i)`, `(?m)`, `(?s)` and combinations like `(?im)`
- Added support for scoped flags `(?i:...)` that create non-capturing groups
- Updated `TransLabel.test` to accept and respect `caseInsensitive` and `dotAll` flags
- Added `charEqIgnoreCase` helper for case-insensitive character comparison
- Added `isAtLineStart` and `isAtLineEnd` helpers for multiline mode
- Updated `checkAnchor` to respect multiline flag for `^` and `$` anchors
- NFA compiler passes flags from AST through to NFA structure

**Test Coverage:** 19 new tests in `MatchTests.lean`:
- Case insensitive: basic, character classes, ranges, alternation, scoped
- Multiline: caret, dollar, findAll across lines
- Dotall: basic, with star quantifier
- Combined flags: `(?im)`, `(?ims)`
- Flags with captures, quantifiers, email patterns

---

### [Priority: Medium] Unicode Support

**Description:** Add proper Unicode support including Unicode property escapes (`\p{Letter}`, `\p{Emoji}`, etc.) and correct handling of Unicode codepoints.

**Rationale:** The current implementation uses `String.length` which counts Unicode scalars, but character class matching uses byte-level comparisons. This could lead to incorrect behavior with non-ASCII text.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Core/CharClass.lean` - add Unicode categories
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Parser/Parser.lean` - parse `\p{...}` syntax
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Match/Simulation.lean` - verify Unicode-correct iteration

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: Medium] Capture Group Iteration API

**Description:** Add an iterator or stream-based API for processing matches without collecting all into a list.

**Rationale:** `findAll` allocates a full list which is inefficient for large inputs with many matches. An iterator would allow lazy, memory-efficient processing.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/API.lean` - add `findIter` returning an iterator type
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Match/Simulation.lean` - implement stateful match iterator

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Low] Possessive Quantifiers

**Description:** Add support for possessive quantifiers `*+`, `++`, `?+`, `{n,m}+` that never backtrack.

**Rationale:** While the Thompson NFA already avoids catastrophic backtracking, possessive quantifiers can still provide semantic value and compatibility with patterns from other engines.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/AST/Types.lean` - add possessive flag
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Parser/Parser.lean` - parse `+` suffix
- `/Users/Shared/Projects/lean-workspace/rune/Rune/NFA/Compiler.lean` - atomic grouping compilation

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Low] Conditional Patterns

**Description:** Add support for conditional patterns `(?(condition)yes|no)` based on whether a capture group matched.

**Rationale:** Useful for advanced patterns like matching balanced quotes or conditionally structured data.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/AST/Types.lean` - add conditional expression
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Parser/Parser.lean` - parse conditional syntax
- `/Users/Shared/Projects/lean-workspace/rune/Rune/NFA/Compiler.lean` - compile conditionals

**Estimated Effort:** Large

**Dependencies:** None

---

## Code Improvements

### [COMPLETED] Optimize String Iteration in Simulation

**Status:** Implemented and tested (2024-12-28)

**Description:** Optimized string iteration to avoid repeated allocations.

**Changes Made:**
- `findMatchAt`: Convert string to char array once (`input.toList.toArray`) instead of using `toList.drop` for each starting position
- `findAll`: Build result list in reverse with O(1) prepend, then reverse once at end (O(n) total instead of O(n²))
- Added `isWordCharAtArray` helper for efficient array-based word boundary checking

**Benefits:** Significant performance improvement for long strings, especially with many match attempts.

---

### [COMPLETED] Use Array Instead of List for Thread Management

**Status:** Implemented (2024-12-28)

**Description:** Converted thread management from `List Thread` to `Array Thread` for better performance.

**Changes Made:**
- `epsilonClosure`: Now takes and returns `Array Thread`, uses index-based iteration instead of pattern matching
- `step`: Now takes and returns `Array Thread`, uses `Array.push` instead of list cons
- `findAcceptingThread`: Updated signature to use `Array Thread`
- `findMatchAt`: Uses `#[initialThread]` array literal, checks `threads.size == 0`

**Benefits:** O(1) amortized push, better cache locality, reduced allocations during NFA simulation.

---

### [Priority: Medium] Optimize findAll Result Construction

**Current State:** `findAll` in `Match/Simulation.lean` line 133 uses `results ++ [m]` which is O(n) for each append.

**Proposed Change:** Either build the list in reverse and reverse at the end, or use an Array and convert to List at the end.

**Benefits:** O(n) total instead of O(n^2) for many matches.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Match/Simulation.lean`

**Estimated Effort:** Small

---

### [COMPLETED] Pre-compiled Character Class Predicates

**Status:** Implemented (2024-12-28)

**Description:** Character classes now use a 128-bit ASCII bitmap for O(1) lookup instead of iterating through elements.

**Changes Made:**
- Added `CompiledCharClass` structure with two `UInt64` bitmaps covering ASCII 0-127
- `compile` function builds bitmap at NFA construction time, pre-computing all character matches
- For POSIX classes, all 128 ASCII characters are tested and bits set accordingly
- Non-ASCII characters fall back to original `BracketExpr.test` implementation
- `TransLabel.charClass` now stores `CompiledCharClass` instead of `BracketExpr`
- NFA compiler calls `CompiledCharClass.compile` when creating character class transitions

**Performance:** O(1) bit test for ASCII characters (was O(n) list iteration)

**Test Coverage:** Added 23 new character class edge case tests validating:
- Range boundaries, overlapping ranges, single-char ranges
- Literal hyphens and brackets, caret as literal
- Negated classes, mixed elements
- All ASCII chars against `\w` and `\s`

---

### [COMPLETED] Deduplicate compile! Helper in Tests

**Status:** Implemented (2024-12-28)

**Description:** Moved shared `compile!` helper to `TestUtils.lean`.

**Changes Made:**
- Created `RuneTests/TestUtils.lean` with shared `compile!` function
- Updated `MatchTests.lean` and `ReplaceTests.lean` to import `TestUtils`
- Removed duplicate `compile!` definitions from both test files

---

### [Priority: Low] Use Substring Instead of String for Match Input

**Current State:** `Match` stores the full `input : String` and extracts substrings for each group access.

**Proposed Change:** Consider storing `Substring` references or a single shared input with positions.

**Benefits:** Reduced memory usage when many matches are retained.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Match/Types.lean`

**Estimated Effort:** Small

---

### [Priority: Low] Tail-Recursive Concat Compilation

**Current State:** `compileExpr` for `.concat` is recursive on the list tail, but not tail-recursive.

**Proposed Change:** Rewrite to be tail-recursive or iterative.

**Benefits:** Avoid potential stack overflow on patterns with very long concatenations.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/NFA/Compiler.lean`

**Estimated Effort:** Small

---

## Code Cleanup

### [COMPLETED] Remove Unused emptyPattern Error Case

**Status:** Implemented (2024-12-28)

**Description:** Removed unused `ParseError.emptyPattern` error variant.

**Changes Made:**
- Removed `emptyPattern` from `ParseError` inductive type
- Removed corresponding case from `position` function
- Removed corresponding case from `ToString` instance

Empty patterns are valid and parse as `Expr.empty`, so this error was never thrown.

---

### [Priority: Medium] Add Documentation Comments to Core Types

**Issue:** Several core types lack documentation comments that would appear in generated docs:
- `Thread` in Match/Types.lean
- `CompilerState` and `Compiler` in NFA/Compiler.lean
- `StateSet` in Match/Simulation.lean

**Location:** Various files

**Action Required:** Add `/-- ... -/` doc comments to all public types and functions.

**Estimated Effort:** Small

---

### [Priority: Medium] Consistent Error Message Formatting

**Issue:** Error messages use inconsistent quoting styles. Some use single quotes `'x'`, others use backticks or no quotes.

**Location:** `/Users/Shared/Projects/lean-workspace/rune/Rune/Core/Error.lean` lines 40-62

**Action Required:** Standardize on a single quoting style for characters and strings in error messages.

**Estimated Effort:** Small

---

### [Priority: Low] Clean Up Partial Function Annotations

**Issue:** Functions like `canMatchEmpty` in AST/Types.lean are marked `partial` due to mutual recursion, but termination could potentially be proven.

**Location:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/AST/Types.lean` line 87
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Parser/Parser.lean` lines 268-371
- `/Users/Shared/Projects/lean-workspace/rune/Rune/NFA/Compiler.lean` lines 46-188
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Match/Simulation.lean` line 17

**Action Required:** Investigate whether termination proofs can be added to remove `partial` annotations.

**Estimated Effort:** Large (depends on complexity of termination proofs)

---

### [COMPLETED] Add POSIX Class Tests

**Status:** Implemented (2024-12-28)

**Description:** Added comprehensive tests for all 12 POSIX character classes.

**Test Coverage:** 16 new tests in `MatchTests.lean`:
- Individual class tests: `[:alpha:]`, `[:digit:]`, `[:alnum:]`, `[:lower:]`, `[:upper:]`
- Whitespace classes: `[:space:]`, `[:blank:]`
- Special classes: `[:punct:]`, `[:xdigit:]`, `[:graph:]`, `[:print:]`, `[:cntrl:]`
- Negated class: `[^[:digit:]]`
- Combined classes: `[[:alpha:][:digit:]]`
- Mixed with other elements: `[[:alpha:]_-]`
- Complex patterns: `[[:alpha:]][[:alnum:]]*`

---

## Test Coverage Gaps

### [COMPLETED] Add Anchor Behavior Tests

**Status:** Implemented (2024-12-28)

**Description:** 9 comprehensive anchor tests added to `MatchTests.lean` validating:
- `^abc` matches "abc" but not "xabc"
- `abc$` matches "abc" but not "abcx"
- `^abc$` matches only "abc"
- Anchors with quantifiers, alternations, and empty strings
- `findAll` respects anchor constraints

---

### [Priority: Medium] Add Edge Case Tests

**Issue:** Missing tests for edge cases:
- Empty capture groups `()`
- Nested groups `((a)(b))`
- Quantified groups `(ab)+`
- Alternation with empty branch `a|`
- Very long patterns
- Very long input strings
- Unicode characters in patterns and input

**Location:** `/Users/Shared/Projects/lean-workspace/rune/RuneTests/`

**Action Required:** Add comprehensive edge case test suite.

**Estimated Effort:** Medium

---

### [COMPLETED] Add Negative Tests for Invalid Patterns

**Status:** Implemented (2024-12-28)

**Description:** Added tests verifying parser returns correct errors for invalid patterns.

**Test Coverage:** 4 new tests in `ParserTests.lean`:
- `reject invalid escape sequence` - `\q` throws `invalidEscape`
- `reject invalid quantifier bounds` - `a{5,2}` throws `invalidQuantifier`
- `reject invalid character range` - `[z-a]` throws `invalidRange`
- `reject empty group name` - `(?<>abc)` throws error

---

### [Priority: Low] Add Performance Regression Tests

**Issue:** No benchmarks or performance tests exist to catch regressions.

**Location:** Create new: `/Users/Shared/Projects/lean-workspace/rune/RuneTests/BenchmarkTests.lean`

**Action Required:** Add tests that verify matching performance on known patterns doesn't regress significantly.

**Estimated Effort:** Medium

---

## API Enhancements

### [COMPLETED] Add Regex.escape Function

**Status:** Implemented and tested (2024-12-28)

**Description:** Added `Regex.escape` to escape metacharacters for literal matching.

**Changes Made:**
- Added `escape` function to `Rune/API.lean`
- Escapes all regex metacharacters: `\[](){}*+?.|^$`

**Test Coverage:** 3 new tests in `MatchTests.lean`:
- `Regex.escape escapes metacharacters` - verifies each metacharacter is escaped
- `Regex.escape produces valid literal pattern` - verifies escaped pattern matches literally
- `Regex.escape with all metacharacters` - comprehensive test with all special chars

**Usage:**
```lean
Regex.escape "hello.world" == "hello\\.world"
Regex.escape "[test]" == "\\[test\\]"
```

---

### [Priority: Medium] Add Match Position Accessors

**Description:** Add `Match.range` returning `(Nat, Nat)` tuple, and consider adding `Match.before` / `Match.after` helpers.

**Rationale:** Common operations are accessing positions and surrounding context.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Match/Types.lean`

**Estimated Effort:** Small

---

### [COMPLETED] Compile-Time Regex Validation

**Status:** Implemented and tested (2024-12-28)

**Description:** Added `regex%` macro that validates patterns at compile time.

**Changes Made:**
- Created `Rune/Macro.lean` with `regex%` term elaborator
- Added `Regex.compile!` unsafe helper in `API.lean` (panics on invalid pattern)
- Macro parses pattern at compile time using existing parser
- Invalid patterns report compile-time errors with detailed messages
- Valid patterns generate `compile!` calls (guaranteed to succeed since pre-validated)
- Added `Inhabited` instances to `NFA` and `Regex` for `panic!` support

**Key Design Decision:** Rather than implementing `Quote` instances for all NFA types
(~200 lines of boilerplate), the macro validates at compile time but defers NFA
construction to runtime. This achieves the same user benefit with minimal code.

**Test Coverage:** 25 new tests in `MacroTests.lean`:
- Basic patterns, escape sequences, character classes
- Anchors, word boundaries, groups (numbered and named)
- Quantifiers (greedy and lazy), lookahead assertions
- Flags (case-insensitive, multiline, dotall)
- Pattern preservation, findAll, count

**Usage:**
```lean
-- Validated at compile time - returns Regex (not Except)
let re := regex% "[a-z]+"

-- Invalid patterns fail to compile:
-- let bad := regex% "[unclosed"
-- Error: regex pattern error: position 0: unbalanced brackets
```

---

### [Priority: Low] Add Regex Combinators

**Description:** Add combinators like `Regex.and`, `Regex.or`, `Regex.not`, `Regex.seq` for building patterns programmatically.

**Rationale:** Sometimes patterns are easier to express compositionally rather than as strings.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/API.lean` or new combinator module

**Estimated Effort:** Medium

---

## Documentation

### [Priority: Medium] Add API Documentation

**Issue:** README provides examples but no comprehensive API documentation.

**Action Required:** Either expand README or create separate API.md with:
- All public types and their fields
- All functions with signatures and descriptions
- Error handling patterns
- Performance characteristics

**Estimated Effort:** Medium

---

### [Priority: Low] Add Architecture Documentation

**Issue:** README briefly mentions the three-stage architecture but doesn't explain the design decisions.

**Action Required:** Add ARCHITECTURE.md explaining:
- Why Thompson NFA was chosen
- Data flow from parse to match
- Extension points for new features

**Estimated Effort:** Small

---

## Summary

| Category | High | Medium | Low | Completed | Total |
|----------|------|--------|-----|-----------|-------|
| Features | 0 | 2 | 2 | 6 | 10 |
| Improvements | 0 | 1 | 2 | 4 | 7 |
| Cleanup | 0 | 2 | 2 | 2 | 6 |
| Tests | 0 | 1 | 0 | 3 | 4 |
| API | 0 | 1 | 1 | 2 | 4 |
| Documentation | 0 | 1 | 1 | 0 | 2 |
| **Total** | **0** | **8** | **8** | **17** | **33** |

### Completed Items

1. **Anchor enforcement** (2024-12-28) - `^` and `$` anchors now correctly enforce position constraints
2. **Anchor behavior tests** (2024-12-28) - 9 comprehensive tests added
3. **Shorthand character classes** (2024-12-28) - `\d`, `\w`, `\s` and negations implemented with 7 tests
4. **Word boundary anchors** (2024-12-28) - `\b` and `\B` implemented with 6 tests
5. **String iteration optimization** (2024-12-28) - Char array computed once, O(n) findAll
6. **POSIX class tests** (2024-12-28) - 16 comprehensive tests for all 12 POSIX classes
7. **Array thread management** (2024-12-28) - Converted from List to Array for O(1) push and better cache locality
8. **Pre-compiled character classes** (2024-12-28) - 128-bit ASCII bitmap for O(1) lookup, 23 new edge case tests
9. **Lazy quantifiers** (2024-12-28) - `*?`, `+?`, `??`, `{n,m}?` for non-greedy matching, 24 new tests
10. **Regex flags** (2024-12-28) - `(?i)`, `(?m)`, `(?s)` for case-insensitive, multiline, and dotall modes, 19 new tests
11. **Lookahead assertions** (2024-12-28) - `(?=...)` and `(?!...)` for zero-width pattern matching, 20 new tests
12. **Compile-time regex validation** (2024-12-28) - `regex%` macro for compile-time pattern validation, 25 new tests
13. **Remove emptyPattern error** (2024-12-28) - Removed unused error variant from Error.lean
14. **Regex.escape function** (2024-12-28) - Escape metacharacters for literal matching, 3 new tests
15. **Negative parser tests** (2024-12-28) - Tests for invalid escapes, quantifiers, ranges, 4 new tests
16. **Deduplicate compile! helper** (2024-12-28) - Moved to shared TestUtils.lean

**Test count: 208 tests (4 suites)**

### Recommended Next Steps

1. **Edge case tests** - Empty groups, nested groups, quantified groups
2. **Unicode support** - Unicode property escapes `\p{Letter}`, proper codepoint handling
3. **Capture group iteration API** - Lazy `findIter` for memory-efficient processing
4. **Match position accessors** - `Match.range`, `Match.before`, `Match.after`
