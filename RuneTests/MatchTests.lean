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

-- Shorthand character class tests

test "digit class \\d" := do
  let re ← compile! "\\d+"
  shouldSatisfy (re.test "123") "should match digits"
  shouldSatisfy (re.test "abc123def") "should match digits in string"
  shouldSatisfy (!re.test "abc") "should not match non-digits"
  if let some m := re.find "abc123def" then
    m.text ≡ "123"

test "non-digit class \\D" := do
  let re ← compile! "\\D+"
  shouldSatisfy (re.test "abc") "should match non-digits"
  shouldSatisfy (!re.test "123") "should not match only digits"
  if let some m := re.find "123abc456" then
    m.text ≡ "abc"

test "word class \\w" := do
  let re ← compile! "\\w+"
  shouldSatisfy (re.test "hello") "should match letters"
  shouldSatisfy (re.test "hello123") "should match alphanumeric"
  shouldSatisfy (re.test "hello_world") "should match with underscore"
  shouldSatisfy (!re.test "!@#$%") "should not match symbols"
  if let some m := re.find "---abc_123---" then
    m.text ≡ "abc_123"

test "non-word class \\W" := do
  let re ← compile! "\\W+"
  shouldSatisfy (re.test "!@#") "should match non-word chars"
  shouldSatisfy (!re.test "abc123") "should not match only word chars"
  if let some m := re.find "hello world" then
    m.text ≡ " "  -- Space between words

test "whitespace class \\s" := do
  let re ← compile! "\\s+"
  shouldSatisfy (re.test " ") "should match space"
  shouldSatisfy (re.test "\t\n\r") "should match various whitespace"
  shouldSatisfy (!re.test "abc") "should not match non-whitespace"
  if let some m := re.find "hello world" then
    m.text ≡ " "

test "non-whitespace class \\S" := do
  let re ← compile! "\\S+"
  shouldSatisfy (re.test "abc") "should match non-whitespace"
  shouldSatisfy (!re.test "   ") "should not match only whitespace"
  if let some m := re.find "  hello  " then
    m.text ≡ "hello"

test "combined shorthand classes" := do
  -- Match identifier: starts with letter/underscore, followed by word chars
  let re ← compile! "[a-zA-Z_]\\w*"
  shouldSatisfy (re.test "myVar") "should match identifier"
  shouldSatisfy (re.test "_private") "should match underscore start"
  shouldSatisfy (re.test "var123") "should match with numbers"
  if let some m := re.find "123 myVar = 5" then
    m.text ≡ "myVar"

-- Word boundary tests

test "word boundary \\b basic" := do
  let re ← compile! "\\bword\\b"
  shouldSatisfy (re.test "word") "should match standalone 'word'"
  shouldSatisfy (re.test "a word here") "should match 'word' with spaces"
  shouldSatisfy (!re.test "password") "should NOT match 'word' in 'password'"
  shouldSatisfy (!re.test "wording") "should NOT match 'word' in 'wording'"
  shouldSatisfy (!re.test "sword") "should NOT match 'word' in 'sword'"

test "word boundary at start" := do
  let re ← compile! "\\btest"
  shouldSatisfy (re.test "test") "should match at string start"
  shouldSatisfy (re.test "test case") "should match at word start"
  shouldSatisfy (re.test " test") "should match after space"
  shouldSatisfy (!re.test "attest") "should NOT match mid-word"

test "word boundary at end" := do
  let re ← compile! "test\\b"
  shouldSatisfy (re.test "test") "should match at string end"
  shouldSatisfy (re.test "a test") "should match at word end"
  shouldSatisfy (re.test "test ") "should match before space"
  shouldSatisfy (!re.test "testing") "should NOT match mid-word"

test "non-word boundary \\B" := do
  let re ← compile! "\\Bword\\B"
  -- "password" ends with 'word' at boundary, so doesn't match \Bword\B
  shouldSatisfy (!re.test "password") "should NOT match 'word' at end of 'password'"
  -- "swordfish" has 'word' embedded with word chars on both sides
  shouldSatisfy (re.test "swordfish") "should match 'word' inside 'swordfish'"
  shouldSatisfy (!re.test "word") "should NOT match standalone 'word'"
  shouldSatisfy (!re.test "a word") "should NOT match 'word' at boundary"

test "word boundary with digits" := do
  let re ← compile! "\\b\\d+\\b"
  shouldSatisfy (re.test "123") "should match standalone number"
  shouldSatisfy (re.test "abc 123 def") "should match number with spaces"
  shouldSatisfy (!re.test "abc123def") "should NOT match embedded number"

test "word boundary findAll" := do
  let re ← compile! "\\bthe\\b"
  let results := re.findAll "the other theater"
  -- Only "the" at position 0 and "the" in "other" should NOT match
  -- Actually: "the" at 0, and "the" at 10 in "theater" shouldn't match because it's not at word boundary
  results.length ≡ 1  -- Only the first "the"

-- POSIX character class tests

test "POSIX [:alpha:] class" := do
  let re ← compile! "[[:alpha:]]+"
  shouldSatisfy (re.test "hello") "should match letters"
  shouldSatisfy (re.test "WORLD") "should match uppercase"
  shouldSatisfy (!re.test "12345") "should not match digits only"
  if let some m := re.find "123abc456" then
    m.text ≡ "abc"

test "POSIX [:digit:] class" := do
  let re ← compile! "[[:digit:]]+"
  shouldSatisfy (re.test "123") "should match digits"
  shouldSatisfy (!re.test "abc") "should not match letters only"
  if let some m := re.find "abc123def" then
    m.text ≡ "123"

test "POSIX [:alnum:] class" := do
  let re ← compile! "[[:alnum:]]+"
  shouldSatisfy (re.test "abc123") "should match alphanumeric"
  shouldSatisfy (re.test "ABC") "should match letters"
  shouldSatisfy (re.test "789") "should match digits"
  shouldSatisfy (!re.test "!@#") "should not match punctuation only"
  if let some m := re.find "---test123---" then
    m.text ≡ "test123"

test "POSIX [:lower:] class" := do
  let re ← compile! "[[:lower:]]+"
  shouldSatisfy (re.test "hello") "should match lowercase"
  shouldSatisfy (!re.test "HELLO") "should not match uppercase"
  shouldSatisfy (!re.test "123") "should not match digits"
  if let some m := re.find "ABCdefGHI" then
    m.text ≡ "def"

test "POSIX [:upper:] class" := do
  let re ← compile! "[[:upper:]]+"
  shouldSatisfy (re.test "HELLO") "should match uppercase"
  shouldSatisfy (!re.test "hello") "should not match lowercase"
  if let some m := re.find "abcDEFghi" then
    m.text ≡ "DEF"

test "POSIX [:space:] class" := do
  let re ← compile! "[[:space:]]+"
  shouldSatisfy (re.test " ") "should match space"
  shouldSatisfy (re.test "\t\n\r") "should match various whitespace"
  shouldSatisfy (!re.test "abc") "should not match letters"
  if let some m := re.find "hello world" then
    m.text ≡ " "

test "POSIX [:blank:] class" := do
  let re ← compile! "[[:blank:]]+"
  shouldSatisfy (re.test " ") "should match space"
  shouldSatisfy (re.test "\t") "should match tab"
  shouldSatisfy (!re.test "\n") "should not match newline"
  shouldSatisfy (!re.test "abc") "should not match letters"

test "POSIX [:punct:] class" := do
  let re ← compile! "[[:punct:]]+"
  shouldSatisfy (re.test "!@#") "should match punctuation"
  shouldSatisfy (re.test ".,;:") "should match more punctuation"
  shouldSatisfy (!re.test "abc") "should not match letters"
  shouldSatisfy (!re.test "123") "should not match digits"
  if let some m := re.find "hello, world!" then
    m.text ≡ ","

test "POSIX [:xdigit:] class" := do
  let re ← compile! "[[:xdigit:]]+"
  shouldSatisfy (re.test "0123456789") "should match decimal digits"
  shouldSatisfy (re.test "abcdef") "should match lowercase hex"
  shouldSatisfy (re.test "ABCDEF") "should match uppercase hex"
  shouldSatisfy (re.test "DeadBeef") "should match mixed case hex"
  shouldSatisfy (!re.test "ghij") "should not match non-hex letters"
  -- Use non-hex prefix (only g-z, G-Z are not hex digits)
  if let some m := re.find "just: #ff00aa" then
    m.text ≡ "ff00aa"

test "POSIX [:graph:] class" := do
  let re ← compile! "[[:graph:]]+"
  shouldSatisfy (re.test "abc123!@#") "should match visible chars"
  shouldSatisfy (!re.test "   ") "should not match spaces only"
  if let some m := re.find "  visible  " then
    m.text ≡ "visible"

test "POSIX [:print:] class" := do
  let re ← compile! "[[:print:]]+"
  shouldSatisfy (re.test "hello world") "should match printable including space"
  shouldSatisfy (re.test "abc 123 !@#") "should match mixed printable"

test "POSIX [:cntrl:] class" := do
  let re ← compile! "[[:cntrl:]]+"
  shouldSatisfy (re.test "\x00\x01\x02") "should match control chars"
  shouldSatisfy (re.test "\x1F") "should match unit separator"
  shouldSatisfy (!re.test "abc") "should not match letters"
  shouldSatisfy (!re.test " ") "should not match space"

test "POSIX negated class [^[:digit:]]" := do
  let re ← compile! "[^[:digit:]]+"
  shouldSatisfy (re.test "abc") "should match non-digits"
  shouldSatisfy (!re.test "123") "should not match digits only"
  if let some m := re.find "123abc456" then
    m.text ≡ "abc"

test "POSIX combined classes" := do
  -- Match alpha or digit (like alnum but explicit)
  let re ← compile! "[[:alpha:][:digit:]]+"
  shouldSatisfy (re.test "abc123") "should match letters and digits"
  shouldSatisfy (re.test "ABC") "should match letters"
  shouldSatisfy (re.test "789") "should match digits"
  if let some m := re.find "---test123---" then
    m.text ≡ "test123"

test "POSIX class with other elements" := do
  -- Match alpha, underscore, or hyphen (like identifier with hyphens)
  let re ← compile! "[[:alpha:]_-]+"
  shouldSatisfy (re.test "hello-world") "should match with hyphen"
  shouldSatisfy (re.test "my_var") "should match with underscore"
  shouldSatisfy (re.test "kebab-case") "should match kebab-case"
  if let some m := re.find "123 my-var 456" then
    m.text ≡ "my-var"

test "POSIX in complex pattern" := do
  -- Match a simple identifier: starts with alpha, followed by alnum
  let re ← compile! "[[:alpha:]][[:alnum:]]*"
  shouldSatisfy (re.test "myVar123") "should match identifier"
  -- Note: test/find finds matches anywhere, so "123abc" matches "abc"
  -- Use isMatch for full string matching
  shouldSatisfy (re.isMatch "123abc").isNone "should not full-match starting with digit"
  shouldSatisfy (re.isMatch "myVar").isSome "should full-match valid identifier"
  if let some m := re.find "  hello123  " then
    m.text ≡ "hello123"

-- Character class edge case tests (for optimization validation)

test "bracket single char" := do
  let re ← compile! "[x]"
  shouldSatisfy (re.test "x") "should match 'x'"
  shouldSatisfy (!re.test "y") "should not match 'y'"
  shouldSatisfy (!re.test "X") "should not match 'X' (case sensitive)"

test "bracket multiple singles" := do
  let re ← compile! "[aeiou]"
  shouldSatisfy (re.test "a") "should match 'a'"
  shouldSatisfy (re.test "e") "should match 'e'"
  shouldSatisfy (re.test "i") "should match 'i'"
  shouldSatisfy (re.test "o") "should match 'o'"
  shouldSatisfy (re.test "u") "should match 'u'"
  shouldSatisfy (!re.test "b") "should not match 'b'"
  shouldSatisfy (!re.test "z") "should not match 'z'"

test "range exact boundaries" := do
  let re ← compile! "[a-f]"
  shouldSatisfy (re.test "a") "should match 'a' (lower bound)"
  shouldSatisfy (re.test "f") "should match 'f' (upper bound)"
  shouldSatisfy (re.test "c") "should match 'c' (middle)"
  shouldSatisfy (!re.test "`") "should not match '`' (char before 'a')"
  shouldSatisfy (!re.test "g") "should not match 'g' (char after 'f')"

test "numeric range boundaries" := do
  let re ← compile! "[3-7]"
  shouldSatisfy (re.test "3") "should match '3' (lower bound)"
  shouldSatisfy (re.test "7") "should match '7' (upper bound)"
  shouldSatisfy (re.test "5") "should match '5' (middle)"
  shouldSatisfy (!re.test "2") "should not match '2' (before range)"
  shouldSatisfy (!re.test "8") "should not match '8' (after range)"

test "single char range" := do
  -- [a-a] should match only 'a'
  let re ← compile! "[a-a]"
  shouldSatisfy (re.test "a") "should match 'a'"
  shouldSatisfy (!re.test "b") "should not match 'b'"

test "multiple ranges" := do
  let re ← compile! "[a-cA-C0-2]"
  -- lowercase a-c
  shouldSatisfy (re.test "a") "should match 'a'"
  shouldSatisfy (re.test "b") "should match 'b'"
  shouldSatisfy (re.test "c") "should match 'c'"
  shouldSatisfy (!re.test "d") "should not match 'd'"
  -- uppercase A-C
  shouldSatisfy (re.test "A") "should match 'A'"
  shouldSatisfy (re.test "B") "should match 'B'"
  shouldSatisfy (re.test "C") "should match 'C'"
  shouldSatisfy (!re.test "D") "should not match 'D'"
  -- digits 0-2
  shouldSatisfy (re.test "0") "should match '0'"
  shouldSatisfy (re.test "1") "should match '1'"
  shouldSatisfy (re.test "2") "should match '2'"
  shouldSatisfy (!re.test "3") "should not match '3'"

test "overlapping ranges" := do
  -- [a-za-m] has overlapping ranges, should work correctly
  let re ← compile! "[a-za-m]"
  shouldSatisfy (re.test "a") "should match 'a'"
  shouldSatisfy (re.test "m") "should match 'm'"
  shouldSatisfy (re.test "z") "should match 'z'"
  shouldSatisfy (!re.test "A") "should not match 'A'"

test "bracket literal hyphen at start" := do
  -- [-a] means literal hyphen and 'a'
  let re ← compile! "[-a]"
  shouldSatisfy (re.test "-") "should match '-'"
  shouldSatisfy (re.test "a") "should match 'a'"
  shouldSatisfy (!re.test "b") "should not match 'b'"

test "bracket literal hyphen at end" := do
  -- [a-] means 'a' and literal hyphen
  let re ← compile! "[a-]"
  shouldSatisfy (re.test "a") "should match 'a'"
  shouldSatisfy (re.test "-") "should match '-'"
  shouldSatisfy (!re.test "b") "should not match 'b'"

test "bracket literal right bracket at start" := do
  -- []a] means literal ']' and 'a'
  let re ← compile! "[]a]"
  shouldSatisfy (re.test "]") "should match ']'"
  shouldSatisfy (re.test "a") "should match 'a'"
  shouldSatisfy (!re.test "[") "should not match '['"

test "negated bracket basics" := do
  let re ← compile! "[^abc]"
  shouldSatisfy (!re.test "a") "should not match 'a'"
  shouldSatisfy (!re.test "b") "should not match 'b'"
  shouldSatisfy (!re.test "c") "should not match 'c'"
  shouldSatisfy (re.test "d") "should match 'd'"
  shouldSatisfy (re.test "x") "should match 'x'"
  shouldSatisfy (re.test "0") "should match '0'"

test "negated bracket with range" := do
  let re ← compile! "[^a-z]"
  shouldSatisfy (!re.test "a") "should not match 'a'"
  shouldSatisfy (!re.test "m") "should not match 'm'"
  shouldSatisfy (!re.test "z") "should not match 'z'"
  shouldSatisfy (re.test "A") "should match 'A'"
  shouldSatisfy (re.test "0") "should match '0'"
  shouldSatisfy (re.test "!") "should match '!'"

test "caret not at start is literal" := do
  -- [a^b] means 'a', '^', or 'b'
  let re ← compile! "[a^b]"
  shouldSatisfy (re.test "a") "should match 'a'"
  shouldSatisfy (re.test "^") "should match '^'"
  shouldSatisfy (re.test "b") "should match 'b'"
  shouldSatisfy (!re.test "c") "should not match 'c'"

test "bracket with special chars" := do
  let re ← compile! "[.+*?]"
  shouldSatisfy (re.test ".") "should match '.'"
  shouldSatisfy (re.test "+") "should match '+'"
  shouldSatisfy (re.test "*") "should match '*'"
  shouldSatisfy (re.test "?") "should match '?'"
  shouldSatisfy (!re.test "a") "should not match 'a'"

test "bracket ASCII boundaries" := do
  -- Test ASCII code boundaries
  let re ← compile! "[ -~]"  -- Space (32) to tilde (126) - all printable ASCII
  shouldSatisfy (re.test " ") "should match space (32)"
  shouldSatisfy (re.test "~") "should match tilde (126)"
  shouldSatisfy (re.test "A") "should match 'A'"
  shouldSatisfy (re.test "0") "should match '0'"

test "bracket mixed elements" := do
  -- Mix of single chars, ranges, and the result of matching
  let re ← compile! "[aeiou0-9X-Z]"
  -- vowels
  shouldSatisfy (re.test "a") "should match 'a'"
  shouldSatisfy (re.test "u") "should match 'u'"
  -- digits
  shouldSatisfy (re.test "0") "should match '0'"
  shouldSatisfy (re.test "9") "should match '9'"
  -- X-Z range
  shouldSatisfy (re.test "X") "should match 'X'"
  shouldSatisfy (re.test "Z") "should match 'Z'"
  -- should not match
  shouldSatisfy (!re.test "b") "should not match 'b'"
  shouldSatisfy (!re.test "W") "should not match 'W'"

test "shorthand \\d boundary chars" := do
  let re ← compile! "\\d"
  shouldSatisfy (re.test "0") "should match '0'"
  shouldSatisfy (re.test "9") "should match '9'"
  shouldSatisfy (!re.test "/") "should not match '/' (char before '0')"
  shouldSatisfy (!re.test ":") "should not match ':' (char after '9')"

test "shorthand \\w boundary chars" := do
  let re ← compile! "\\w"
  -- lowercase
  shouldSatisfy (re.test "a") "should match 'a'"
  shouldSatisfy (re.test "z") "should match 'z'"
  shouldSatisfy (!re.test "`") "should not match '`' (before 'a')"
  shouldSatisfy (!re.test "{") "should not match '{' (after 'z')"
  -- uppercase
  shouldSatisfy (re.test "A") "should match 'A'"
  shouldSatisfy (re.test "Z") "should match 'Z'"
  shouldSatisfy (!re.test "@") "should not match '@' (before 'A')"
  shouldSatisfy (!re.test "[") "should not match '[' (after 'Z')"
  -- digits
  shouldSatisfy (re.test "0") "should match '0'"
  shouldSatisfy (re.test "9") "should match '9'"
  -- underscore
  shouldSatisfy (re.test "_") "should match '_'"

test "all ASCII chars against \\w" := do
  -- Comprehensive test: check every printable ASCII char
  let re ← compile! "^\\w$"
  -- These should match (word chars)
  for c in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_".toList do
    shouldSatisfy (re.isMatch (String.singleton c)).isSome s!"should match '{c}'"
  -- These should not match (non-word chars)
  for c in " !\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~".toList do
    shouldSatisfy (re.isMatch (String.singleton c)).isNone s!"should not match '{c}'"

test "all ASCII chars against \\s" := do
  let re ← compile! "^\\s$"
  -- Whitespace chars
  shouldSatisfy (re.isMatch " ").isSome "should match space"
  shouldSatisfy (re.isMatch "\t").isSome "should match tab"
  shouldSatisfy (re.isMatch "\n").isSome "should match newline"
  shouldSatisfy (re.isMatch "\r").isSome "should match carriage return"
  -- Non-whitespace
  shouldSatisfy (re.isMatch "a").isNone "should not match 'a'"
  shouldSatisfy (re.isMatch "0").isNone "should not match '0'"

test "complex negated pattern" := do
  -- Match anything that's not alphanumeric
  let re ← compile! "[^a-zA-Z0-9]+"
  if let some m := re.find "hello...world" then
    m.text ≡ "..."
  if let some m := re.find "test@example.com" then
    m.text ≡ "@"

test "bracket with escaped chars" := do
  let re ← compile! "[\\n\\t]"
  shouldSatisfy (re.test "\n") "should match newline"
  shouldSatisfy (re.test "\t") "should match tab"
  shouldSatisfy (!re.test " ") "should not match space"
  shouldSatisfy (!re.test "n") "should not match 'n'"

test "repeated bracket pattern performance" := do
  -- This pattern would be slow without optimization
  let re ← compile! "[a-zA-Z0-9_]+@[a-zA-Z0-9_]+\\.[a-zA-Z]+"
  shouldSatisfy (re.test "user@example.com") "should match email"
  shouldSatisfy (re.test "test_123@domain.org") "should match with underscore"
  shouldSatisfy (!re.test "@invalid") "should not match invalid email"

#generate_tests

end RuneTests.MatchTests
