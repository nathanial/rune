import Rune
import Crucible

namespace RuneTests.MacroTests

open Crucible
open Rune

testSuite "Compile-Time Regex Macro"

test "regex% compiles valid pattern" := do
  let re := regex% "[a-z]+"
  shouldSatisfy (re.test "hello") "should match hello"
  shouldSatisfy (!re.test "123") "should not match digits"

test "regex% with escape sequences" := do
  let re := regex% "\\d+"
  shouldSatisfy (re.test "123") "should match digits"
  shouldSatisfy (!re.test "abc") "should not match letters"

test "regex% with word class" := do
  let re := regex% "\\w+"
  shouldSatisfy (re.test "hello123") "should match word chars"
  shouldSatisfy (re.test "_underscore") "should match underscore"

test "regex% with whitespace class" := do
  let re := regex% "\\s+"
  shouldSatisfy (re.test " ") "should match space"
  shouldSatisfy (re.test "\t\n") "should match tab and newline"

test "regex% with anchors" := do
  let re := regex% "^hello$"
  shouldSatisfy (re.test "hello") "should match exact"
  shouldSatisfy (!re.test "hello world") "should not match with trailing"
  shouldSatisfy (!re.test "say hello") "should not match with leading"

test "regex% with word boundary" := do
  let re := regex% "\\bword\\b"
  shouldSatisfy (re.test "a word here") "should match word with boundaries"
  shouldSatisfy (!re.test "swordfish") "should not match word in middle"

test "regex% with groups" := do
  let re := regex% "(\\w+)@(\\w+)"
  if let some m := re.find "user@domain" then
    (m.group 1) ≡ some "user"
    (m.group 2) ≡ some "domain"
  else
    ensure false "should match"

test "regex% with named groups" := do
  let re := regex% "(?<name>\\w+)"
  if let some m := re.find "hello" then
    (m.namedGroup "name") ≡ some "hello"
  else
    ensure false "should match"

test "regex% with non-capturing group" := do
  let re := regex% "(?:abc)+"
  shouldSatisfy (re.test "abcabc") "should match repeated group"
  if let some m := re.find "abcabc" then
    m.text ≡ "abcabc"
    -- Non-capturing groups don't create captures
    (m.group 1) ≡ none
  else
    ensure false "should match"

test "regex% with exact quantifier" := do
  let re := regex% "a{3}"
  shouldSatisfy (re.test "aaa") "should match exactly 3"
  shouldSatisfy (!re.test "aa") "should not match 2"

test "regex% with range quantifier" := do
  let re := regex% "a{2,4}"
  shouldSatisfy (re.test "aa") "should match 2"
  shouldSatisfy (re.test "aaa") "should match 3"
  shouldSatisfy (re.test "aaaa") "should match 4"
  shouldSatisfy (!re.test "a") "should not match 1"

test "regex% with lazy quantifier" := do
  let re := regex% "<.*?>"
  if let some m := re.find "<a><b>" then
    m.text ≡ "<a>"
  else
    ensure false "should match shortest"

test "regex% with lazy plus" := do
  let re := regex% "a+?"
  if let some m := re.find "aaaa" then
    m.text ≡ "a"
  else
    ensure false "should match minimal"

test "regex% with positive lookahead" := do
  let re := regex% "foo(?=bar)"
  shouldSatisfy (re.test "foobar") "should match foo before bar"
  shouldSatisfy (!re.test "foobaz") "should not match foo before baz"
  if let some m := re.find "foobar" then
    m.text ≡ "foo"
  else
    ensure false "should match"

test "regex% with negative lookahead" := do
  let re := regex% "foo(?!bar)"
  shouldSatisfy (!re.test "foobar") "should not match foo before bar"
  shouldSatisfy (re.test "foobaz") "should match foo before baz"

test "regex% with case insensitive flag" := do
  let re := regex% "(?i)hello"
  shouldSatisfy (re.test "HELLO") "should match uppercase"
  shouldSatisfy (re.test "Hello") "should match mixed case"
  shouldSatisfy (re.test "hello") "should match lowercase"

test "regex% with multiline flag" := do
  let re := regex% "(?m)^line"
  shouldSatisfy (re.test "line one") "should match at start"
  shouldSatisfy (re.test "first\nline two") "should match after newline"

test "regex% with dotall flag" := do
  let re := regex% "(?s)a.b"
  shouldSatisfy (re.test "a\nb") "dot should match newline in dotall mode"

test "regex% with complex email pattern" := do
  let re := regex% "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
  shouldSatisfy (re.test "user@example.com") "should match simple email"
  shouldSatisfy (re.test "first.last@subdomain.example.org") "should match complex email"
  shouldSatisfy (!re.test "invalid") "should not match non-email"

test "regex% with alternation" := do
  let re := regex% "cat|dog|bird"
  shouldSatisfy (re.test "cat") "should match cat"
  shouldSatisfy (re.test "dog") "should match dog"
  shouldSatisfy (re.test "bird") "should match bird"
  shouldSatisfy (!re.test "fish") "should not match fish"

test "regex% with dot" := do
  let re := regex% "a.c"
  shouldSatisfy (re.test "abc") "should match abc"
  shouldSatisfy (re.test "axc") "should match axc"
  shouldSatisfy (!re.test "ac") "should not match ac"

test "regex% with POSIX class" := do
  let re := regex% "[[:alpha:]]+"
  shouldSatisfy (re.test "hello") "should match letters"
  shouldSatisfy (!re.test "123") "should not match digits"

test "regex% preserves pattern in Regex" := do
  let re := regex% "[a-z]+"
  re.getPattern ≡ "[a-z]+"

test "regex% with findAll" := do
  let re := regex% "\\d+"
  let results := re.findAll "a1b22c333"
  (results.length) ≡ 3
  (results.head?.map (·.text)) ≡ some "1"

test "regex% with count" := do
  let re := regex% "[aeiou]"
  (re.count "hello world") ≡ 3

-- Note: Invalid patterns cannot be tested at runtime since they cause
-- compile-time errors. Manual verification required:
-- These should fail to compile:
--   let bad1 := regex% "[unclosed"       -- unbalanced brackets
--   let bad2 := regex% "(unclosed"       -- unbalanced parens
--   let bad3 := regex% "\\q"             -- invalid escape
--   let bad4 := regex% "a{5,2}"          -- invalid quantifier bounds
--   let bad5 := regex% "[z-a]"           -- invalid character range

#generate_tests

end RuneTests.MacroTests
