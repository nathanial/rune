import Rune
import Crucible

namespace RuneTests.MatchTests

open Crucible
open Rune

/-- Helper to compile a regex or fail -/
def compile! (pattern : String) : IO Regex := do
  match Regex.compile pattern with
  | .ok re => pure re
  | .error e => throw (IO.userError s!"compile error: {e}")

testSuite "Matching"

test "match literal" := do
  let re ← compile! "abc"
  shouldSatisfy (re.test "abc") "should match 'abc'"
  shouldSatisfy (re.test "xabcy") "should match 'abc' in 'xabcy'"
  shouldSatisfy (!re.test "ab") "should not match 'ab'"

test "match dot" := do
  let re ← compile! "a.c"
  shouldSatisfy (re.test "abc") "should match 'abc'"
  shouldSatisfy (re.test "axc") "should match 'axc'"
  shouldSatisfy (!re.test "ac") "should not match 'ac'"

test "dot does not match newline" := do
  let re ← compile! "a.c"
  shouldSatisfy (!re.test "a\nc") "dot should not match newline"

test "match alternation" := do
  let re ← compile! "cat|dog"
  shouldSatisfy (re.test "cat") "should match 'cat'"
  shouldSatisfy (re.test "dog") "should match 'dog'"
  shouldSatisfy (!re.test "bird") "should not match 'bird'"

test "match star quantifier" := do
  let re ← compile! "ab*c"
  shouldSatisfy (re.test "ac") "should match 'ac'"
  shouldSatisfy (re.test "abc") "should match 'abc'"
  shouldSatisfy (re.test "abbc") "should match 'abbc'"
  shouldSatisfy (re.test "abbbc") "should match 'abbbc'"

test "match plus quantifier" := do
  let re ← compile! "ab+c"
  shouldSatisfy (!re.test "ac") "should not match 'ac'"
  shouldSatisfy (re.test "abc") "should match 'abc'"
  shouldSatisfy (re.test "abbc") "should match 'abbc'"

test "match question quantifier" := do
  let re ← compile! "ab?c"
  shouldSatisfy (re.test "ac") "should match 'ac'"
  shouldSatisfy (re.test "abc") "should match 'abc'"
  shouldSatisfy (!re.test "abbc") "should not match 'abbc'"

test "match bracket expression" := do
  let re ← compile! "[abc]"
  shouldSatisfy (re.test "a") "should match 'a'"
  shouldSatisfy (re.test "b") "should match 'b'"
  shouldSatisfy (re.test "c") "should match 'c'"
  shouldSatisfy (!re.test "d") "should not match 'd'"

test "match negated bracket" := do
  let re ← compile! "[^abc]"
  shouldSatisfy (!re.test "a") "should not match 'a'"
  shouldSatisfy (re.test "d") "should match 'd'"
  shouldSatisfy (re.test "x") "should match 'x'"

test "match character range" := do
  let re ← compile! "[a-z]"
  shouldSatisfy (re.test "a") "should match 'a'"
  shouldSatisfy (re.test "m") "should match 'm'"
  shouldSatisfy (re.test "z") "should match 'z'"
  shouldSatisfy (!re.test "A") "should not match 'A'"
  shouldSatisfy (!re.test "0") "should not match '0'"

test "match groups" := do
  let re ← compile! "(abc)"
  if let some m := re.find "xabcy" then
    m.text ≡ "abc"
    (m.group 1) ≡ some "abc"
  else
    ensure false "should have matched"

test "match multiple groups" := do
  let re ← compile! "([a-z]+)@([a-z]+)"
  if let some m := re.find "user@domain" then
    m.text ≡ "user@domain"
    (m.group 1) ≡ some "user"
    (m.group 2) ≡ some "domain"
  else
    ensure false "should have matched"

test "match named groups" := do
  let re ← compile! "(?<user>[a-z]+)@(?<host>[a-z]+)"
  if let some m := re.find "user@domain" then
    (m.namedGroup "user") ≡ some "user"
    (m.namedGroup "host") ≡ some "domain"
  else
    ensure false "should have matched"

test "find returns first match" := do
  let re ← compile! "[a-z]+"
  if let some m := re.find "123 hello world" then
    m.text ≡ "hello"
  else
    ensure false "should have matched"

test "findAll returns all matches" := do
  let re ← compile! "[a-z]+"
  let matchList := re.findAll "hello world foo"
  matchList.length ≡ 3
  (matchList[0]?.map (·.text)) ≡ some "hello"
  (matchList[1]?.map (·.text)) ≡ some "world"
  (matchList[2]?.map (·.text)) ≡ some "foo"

test "isMatch requires full match" := do
  let re ← compile! "[a-z]+"
  shouldSatisfy (re.isMatch "hello").isSome "should match 'hello'"
  shouldSatisfy (re.isMatch "hello123").isNone "should not match 'hello123' (not full)"

test "empty pattern matches empty string" := do
  let re ← compile! ""
  shouldSatisfy (re.test "") "should match empty string"

test "bounded quantifier exactly" := do
  let re ← compile! "a{3}"
  shouldSatisfy (!re.test "aa") "should not match 'aa'"
  shouldSatisfy (re.test "aaa") "should match 'aaa'"
  shouldSatisfy (re.test "aaaa") "should match in 'aaaa'"

test "bounded quantifier range" := do
  let re ← compile! "a{2,4}"
  shouldSatisfy (!re.test "a") "should not match 'a'"
  shouldSatisfy (re.test "aa") "should match 'aa'"
  shouldSatisfy (re.test "aaa") "should match 'aaa'"
  shouldSatisfy (re.test "aaaa") "should match 'aaaa'"

test "complex email pattern" := do
  let re ← compile! "[a-z]+@[a-z]+\\.[a-z]+"
  shouldSatisfy (re.test "user@example.com") "should match email"
  shouldSatisfy (!re.test "invalid") "should not match 'invalid'"

-- Anchor tests

test "start anchor basic" := do
  let re ← compile! "^abc"
  shouldSatisfy (re.test "abc") "should match 'abc' at start"
  shouldSatisfy (re.test "abcdef") "should match 'abc' at start of 'abcdef'"
  shouldSatisfy (!re.test "xabc") "should NOT match 'abc' not at start"
  shouldSatisfy (!re.test " abc") "should NOT match with leading space"

test "end anchor basic" := do
  let re ← compile! "abc$"
  shouldSatisfy (re.test "abc") "should match 'abc' at end"
  shouldSatisfy (re.test "xyzabc") "should match 'abc' at end of 'xyzabc'"
  shouldSatisfy (!re.test "abcx") "should NOT match 'abc' not at end"
  shouldSatisfy (!re.test "abc ") "should NOT match with trailing space"

test "both anchors require full match" := do
  let re ← compile! "^abc$"
  shouldSatisfy (re.test "abc") "should match exact 'abc'"
  shouldSatisfy (!re.test "abcd") "should NOT match 'abcd'"
  shouldSatisfy (!re.test "xabc") "should NOT match 'xabc'"
  shouldSatisfy (!re.test " abc ") "should NOT match with spaces"

test "anchors with quantifiers" := do
  let re ← compile! "^a+$"
  shouldSatisfy (re.test "a") "should match 'a'"
  shouldSatisfy (re.test "aaa") "should match 'aaa'"
  shouldSatisfy (!re.test "ba") "should NOT match 'ba'"
  shouldSatisfy (!re.test "ab") "should NOT match 'ab'"

test "start anchor with alternation" := do
  let re ← compile! "^(cat|dog)"
  shouldSatisfy (re.test "cat") "should match 'cat' at start"
  shouldSatisfy (re.test "dog") "should match 'dog' at start"
  shouldSatisfy (!re.test "the cat") "should NOT match 'cat' not at start"

test "end anchor with alternation" := do
  let re ← compile! "(cat|dog)$"
  shouldSatisfy (re.test "cat") "should match 'cat' at end"
  shouldSatisfy (re.test "my dog") "should match 'dog' at end"
  shouldSatisfy (!re.test "cats") "should NOT match 'cat' not at end"

test "anchors with empty string" := do
  let reStart ← compile! "^"
  let reEnd ← compile! "$"
  let reBoth ← compile! "^$"
  shouldSatisfy (reStart.test "") "^ should match empty string"
  shouldSatisfy (reEnd.test "") "$ should match empty string"
  shouldSatisfy (reBoth.test "") "^$ should match empty string"
  shouldSatisfy (reStart.test "x") "^ should match any string at start"
  shouldSatisfy (reEnd.test "x") "$ should match any string at end"
  shouldSatisfy (!reBoth.test "x") "^$ should NOT match non-empty string"

test "anchored findAll" := do
  let re ← compile! "^hello"
  let results := re.findAll "hello world hello"
  -- Only the first "hello" should match because it's at position 0
  results.length ≡ 1
  (results[0]?.map (·.text)) ≡ some "hello"

test "end anchored pattern mid-string fails" := do
  let re ← compile! "world$"
  let results := re.findAll "world hello world"
  -- Only the last "world" should match because it's at the end
  results.length ≡ 1
  if let some m := results[0]? then
    m.start ≡ 12  -- Position of the last "world"

#generate_tests

end RuneTests.MatchTests
