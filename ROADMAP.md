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

### [Priority: Medium] Lazy (Non-Greedy) Quantifiers

**Description:** Add support for lazy quantifiers `*?`, `+?`, `??`, `{n,m}?` that match as few characters as possible.

**Rationale:** Greedy-only quantifiers limit the library's usefulness. For example, matching the first HTML tag in `<b>bold</b>` with `<.*>` captures the entire string greedily; users need `<.*?>` to capture just `<b>`.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/AST/Types.lean` - add `greedy : Bool` field to Quantifier
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Parser/Parser.lean` - parse `?` suffix after quantifiers
- `/Users/Shared/Projects/lean-workspace/rune/Rune/NFA/Compiler.lean` - adjust epsilon ordering for lazy mode
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Match/Simulation.lean` - respect quantifier greediness

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Lookahead Assertions

**Description:** Add support for positive lookahead `(?=...)` and negative lookahead `(?!...)`.

**Rationale:** Lookaheads are commonly needed for password validation, parsing complex formats, and other advanced patterns. They're standard in most modern regex engines.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/AST/Types.lean` - add lookahead expression variants
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Parser/Parser.lean` - parse lookahead syntax
- `/Users/Shared/Projects/lean-workspace/rune/Rune/NFA/Compiler.lean` - compile lookahead (may need sub-NFA)
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Match/Simulation.lean` - implement lookahead evaluation

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: Medium] Regex Flags/Modifiers

**Description:** Add support for flags like `(?i)` for case-insensitive matching, `(?m)` for multiline mode (where `^`/`$` match line boundaries), and `(?s)` for dotall mode (where `.` matches newlines).

**Rationale:** Many patterns require case-insensitive matching. The current API has no way to enable this without the user manually constructing case-insensitive character classes.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/API.lean` - add `CompileOptions` structure
- `/Users/Shared/Projects/lean-workspace/rune/Rune/NFA/Types.lean` - store flags in NFA
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Parser/Parser.lean` - parse inline flags
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Match/Simulation.lean` - respect flags during matching

**Estimated Effort:** Medium

**Dependencies:** None (anchor enforcement is now complete)

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

### [Priority: High] Optimize String Iteration in Simulation

**Current State:** `Match/Simulation.lean` line 93 uses `input.toList.drop startPos` which allocates a new list for each starting position during the find loop.

**Proposed Change:** Use `String.Pos` and `String.Iterator` for zero-allocation character iteration, or pre-compute the character array once.

**Benefits:** Significant performance improvement for long strings with no matches or late matches.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Match/Simulation.lean`

**Estimated Effort:** Small

---

### [Priority: High] Use Array Instead of List for Thread Management

**Current State:** Thread lists in `epsilonClosure` and `step` use `List` type which has O(n) append and poor cache locality.

**Proposed Change:** Use `Array Thread` with in-place mutation for better performance.

**Benefits:** Reduced allocations and better cache performance during NFA simulation.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Match/Simulation.lean`

**Estimated Effort:** Small

---

### [Priority: Medium] Optimize findAll Result Construction

**Current State:** `findAll` in `Match/Simulation.lean` line 133 uses `results ++ [m]` which is O(n) for each append.

**Proposed Change:** Either build the list in reverse and reverse at the end, or use an Array and convert to List at the end.

**Benefits:** O(n) total instead of O(n^2) for many matches.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Match/Simulation.lean`

**Estimated Effort:** Small

---

### [Priority: Medium] Pre-compiled Character Class Predicates

**Current State:** Character class matching iterates through all elements for each character test (`BracketExpr.test` calls `List.any`).

**Proposed Change:** For character ranges, precompute a bitmap or sorted interval list for O(log n) or O(1) lookup.

**Benefits:** Faster matching for complex character classes like `[a-zA-Z0-9_]`.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Core/CharClass.lean`
- `/Users/Shared/Projects/lean-workspace/rune/Rune/NFA/Types.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Deduplicate compile! Helper in Tests

**Current State:** `compile!` helper is defined identically in both `MatchTests.lean` and `ReplaceTests.lean`.

**Proposed Change:** Move to a shared test utilities module.

**Benefits:** DRY principle, easier maintenance.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/RuneTests/MatchTests.lean`
- `/Users/Shared/Projects/lean-workspace/rune/RuneTests/ReplaceTests.lean`
- Create: `/Users/Shared/Projects/lean-workspace/rune/RuneTests/TestUtils.lean`

**Estimated Effort:** Small

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

### [Priority: High] Remove or Implement emptyPattern Error Case

**Issue:** `ParseError.emptyPattern` exists in Error.lean but is never thrown by the parser. Empty patterns parse successfully as `Expr.empty`.

**Location:** `/Users/Shared/Projects/lean-workspace/rune/Rune/Core/Error.lean` line 18

**Action Required:** Either remove this unused error variant or implement validation that throws it.

**Estimated Effort:** Small

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

### [Priority: Low] Add POSIX Class Tests

**Issue:** POSIX character classes (`[:alpha:]`, `[:digit:]`, etc.) are implemented in `CharClass.lean` but there are no tests exercising them.

**Location:** `/Users/Shared/Projects/lean-workspace/rune/RuneTests/ParserTests.lean`

**Action Required:** Add tests for patterns like `[[:alpha:]]`, `[[:digit:][:space:]]`, etc.

**Estimated Effort:** Small

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

### [Priority: Medium] Add Negative Tests for Invalid Patterns

**Issue:** Only two negative tests exist (unbalanced parens/brackets). Missing tests for:
- Invalid escape sequences
- Invalid quantifier bounds `{5,2}`
- Invalid character ranges `[z-a]`
- Invalid group names `(?<123>...)`

**Location:** `/Users/Shared/Projects/lean-workspace/rune/RuneTests/ParserTests.lean`

**Action Required:** Add tests verifying specific error types are returned.

**Estimated Effort:** Small

---

### [Priority: Low] Add Performance Regression Tests

**Issue:** No benchmarks or performance tests exist to catch regressions.

**Location:** Create new: `/Users/Shared/Projects/lean-workspace/rune/RuneTests/BenchmarkTests.lean`

**Action Required:** Add tests that verify matching performance on known patterns doesn't regress significantly.

**Estimated Effort:** Medium

---

## API Enhancements

### [Priority: Medium] Add Regex.escape Function

**Description:** Add a function to escape special characters in a string for use as a literal pattern.

**Rationale:** Users often need to match literal strings that may contain metacharacters. Currently they must manually escape each character.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/API.lean`

**Estimated Effort:** Small

---

### [Priority: Medium] Add Match Position Accessors

**Description:** Add `Match.range` returning `(Nat, Nat)` tuple, and consider adding `Match.before` / `Match.after` helpers.

**Rationale:** Common operations are accessing positions and surrounding context.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/rune/Rune/Match/Types.lean`

**Estimated Effort:** Small

---

### [Priority: Low] Add Compile-Time Regex Validation

**Description:** Create an `regex%` macro that validates patterns at compile time.

**Rationale:** Catch pattern errors during compilation rather than at runtime.

**Affected Files:**
- Create: `/Users/Shared/Projects/lean-workspace/rune/Rune/Macro.lean`
- `/Users/Shared/Projects/lean-workspace/rune/Rune.lean`

**Estimated Effort:** Medium

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
| Features | 0 | 5 | 2 | 3 | 10 |
| Improvements | 2 | 3 | 2 | 0 | 7 |
| Cleanup | 1 | 2 | 3 | 0 | 6 |
| Tests | 0 | 2 | 1 | 1 | 4 |
| API | 0 | 2 | 2 | 0 | 4 |
| Documentation | 0 | 1 | 1 | 0 | 2 |
| **Total** | **3** | **15** | **11** | **4** | **33** |

### Completed Items

1. **Anchor enforcement** (2024-12-28) - `^` and `$` anchors now correctly enforce position constraints
2. **Anchor behavior tests** (2024-12-28) - 9 comprehensive tests added
3. **Shorthand character classes** (2024-12-28) - `\d`, `\w`, `\s` and negations implemented with 7 tests
4. **Word boundary anchors** (2024-12-28) - `\b` and `\B` implemented with 6 tests

**Test count: 74 tests (was 52)**

### Recommended Next Steps

1. **Optimize string iteration** - Quick performance win (`input.toList.drop` allocates per position)
2. **Add POSIX class tests** - Validate existing but untested functionality
3. **Lazy quantifiers** (`*?`, `+?`) - Commonly needed feature
4. **Regex flags** (`(?i)`, `(?m)`) - Case-insensitive and multiline modes
