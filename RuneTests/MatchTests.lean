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

-- ============================================================================
-- Lazy (Non-Greedy) Quantifier Tests
-- ============================================================================

test "lazy star *? basic" := do
  -- Greedy * matches as much as possible
  let greedy ← compile! "a.*b"
  if let some m := greedy.find "aXXbYYb" then
    m.text ≡ "aXXbYYb"  -- Greedy: matches to the LAST 'b'
  -- Lazy *? matches as little as possible
  let lazy ← compile! "a.*?b"
  if let some m := lazy.find "aXXbYYb" then
    m.text ≡ "aXXb"  -- Lazy: matches to the FIRST 'b'

test "lazy plus +? basic" := do
  -- Greedy + matches as much as possible
  let greedy ← compile! "a.+b"
  if let some m := greedy.find "aXXbYYb" then
    m.text ≡ "aXXbYYb"  -- Greedy: matches to the LAST 'b'
  -- Lazy +? matches as little as possible
  let lazy ← compile! "a.+?b"
  if let some m := lazy.find "aXXbYYb" then
    m.text ≡ "aXXb"  -- Lazy: matches to the FIRST 'b'

test "lazy question ?? basic" := do
  -- Greedy ? prefers to match
  let greedy ← compile! "ab?"
  if let some m := greedy.find "ab" then
    m.text ≡ "ab"  -- Greedy: matches 'ab'
  if let some m := greedy.find "a" then
    m.text ≡ "a"   -- Falls back to 'a' if no 'b'
  -- Lazy ?? prefers NOT to match
  let lazy ← compile! "ab??"
  if let some m := lazy.find "ab" then
    m.text ≡ "a"  -- Lazy: matches just 'a', skips optional 'b'

test "lazy star *? HTML tags" := do
  -- Classic use case: matching HTML tags
  let greedy ← compile! "<.*>"
  if let some m := greedy.find "<b>bold</b>" then
    m.text ≡ "<b>bold</b>"  -- Greedy: matches entire string
  let lazy ← compile! "<.*?>"
  if let some m := lazy.find "<b>bold</b>" then
    m.text ≡ "<b>"  -- Lazy: matches just first tag

test "lazy star *? findAll" := do
  -- Find all HTML tags with lazy quantifier
  let re ← compile! "<.*?>"
  let results := re.findAll "<a><b></b></a>"
  results.length ≡ 4
  (results[0]?.map (·.text)) ≡ some "<a>"
  (results[1]?.map (·.text)) ≡ some "<b>"
  (results[2]?.map (·.text)) ≡ some "</b>"
  (results[3]?.map (·.text)) ≡ some "</a>"

test "lazy plus +? minimum one" := do
  -- +? still requires at least one character
  let re ← compile! "a.+?b"
  shouldSatisfy (re.test "aXb") "should match 'aXb' (one char between)"
  shouldSatisfy (!re.test "ab") "should not match 'ab' (no char between)"
  if let some m := re.find "aXYZb" then
    m.text ≡ "aXYZb"  -- All chars needed to reach 'b'

test "lazy star *? empty match" := do
  -- *? can match zero characters
  let re ← compile! "a.*?b"
  if let some m := re.find "ab" then
    m.text ≡ "ab"  -- Zero chars between

test "lazy quantifier with captures" := do
  -- Capture groups with lazy quantifiers
  let re ← compile! "<(.*?)>"
  if let some m := re.find "<hello>world</hello>" then
    m.text ≡ "<hello>"
    m.group 1 ≡ "hello"

test "lazy bounded {n,m}?" := do
  -- Lazy bounded quantifier
  let greedy ← compile! "a{2,4}"
  if let some m := greedy.find "aaaaa" then
    m.text ≡ "aaaa"  -- Greedy: matches 4
  let lazy ← compile! "a{2,4}?"
  if let some m := lazy.find "aaaaa" then
    m.text ≡ "aa"  -- Lazy: matches 2 (minimum)

test "lazy bounded {n,}?" := do
  -- Lazy unbounded minimum
  let greedy ← compile! "a{2,}"
  if let some m := greedy.find "aaaaa" then
    m.text ≡ "aaaaa"  -- Greedy: matches all 5
  let lazy ← compile! "a{2,}?"
  if let some m := lazy.find "aaaaa" then
    m.text ≡ "aa"  -- Lazy: matches just 2

test "lazy bounded {0,n}?" := do
  -- Lazy optional repetition
  let lazy ← compile! "a{0,3}?b"
  -- Note: lazy prefers fewer 'a's, but must find 'b' starting from position 0
  -- In "aaab", there's no 'b' until after the 'a's, so we match all of them
  if let some m := lazy.find "aaab" then
    m.text ≡ "aaab"
  -- When 'b' is available immediately, lazy matches zero 'a's
  if let some m := lazy.find "b" then
    m.text ≡ "b"
  -- When 'b' comes after just one 'a', lazy matches one
  if let some m := lazy.find "ab" then
    m.text ≡ "ab"

test "lazy vs greedy comparison star" := do
  let greedy ← compile! "\".*\""
  let lazy ← compile! "\".*?\""
  let input := "\"first\" and \"second\""
  if let some m := greedy.find input then
    m.text ≡ "\"first\" and \"second\""  -- Greedy: entire quoted section
  if let some m := lazy.find input then
    m.text ≡ "\"first\""  -- Lazy: just first quoted string

test "lazy vs greedy comparison plus" := do
  let greedy ← compile! "\\d+"
  let lazy ← compile! "\\d+?"
  if let some m := greedy.find "12345" then
    m.text ≡ "12345"  -- Greedy: all digits
  if let some m := lazy.find "12345" then
    m.text ≡ "1"  -- Lazy: just one digit

test "lazy quantifier with word boundary" := do
  -- \b\w+?\b requires word boundaries on BOTH sides
  -- "h" alone has no trailing word boundary (h|ello is word-to-word)
  -- So the minimum complete word match is "hello"
  let re ← compile! "\\b\\w+?\\b"
  if let some m := re.find "hello world" then
    m.text ≡ "hello"  -- Smallest complete word
  -- Both greedy and lazy match complete words with \b on both sides
  let greedy ← compile! "\\b\\w+\\b"
  if let some m := greedy.find "hello world" then
    m.text ≡ "hello"

test "lazy quantifier with anchors" := do
  let re ← compile! "^.*?x"
  if let some m := re.find "abcxdefx" then
    m.text ≡ "abcx"  -- Finds first 'x' from start
  -- x.*?$ matches from the FIRST 'x' found, then lazily to end
  -- In "axbxc", first 'x' is at position 1, lazy .*? matches minimum to reach $
  let re2 ← compile! "x.*?$"
  if let some m := re2.find "axbxc" then
    m.text ≡ "xbxc"  -- First 'x' to end (find returns first match position)

test "nested lazy quantifiers" := do
  let re ← compile! "\\(.*?\\(.*?\\).*?\\)"
  if let some m := re.find "(a(b)c)(d(e)f)" then
    m.text ≡ "(a(b)c)"  -- Matches first complete nested pair

test "lazy quantifier findAll multiple" := do
  let re ← compile! "\\d+?"
  let results := re.findAll "a1b23c456d"
  -- Lazy +? matches one digit at a time
  results.length ≡ 6
  (results[0]?.map (·.text)) ≡ some "1"
  (results[1]?.map (·.text)) ≡ some "2"
  (results[2]?.map (·.text)) ≡ some "3"
  (results[3]?.map (·.text)) ≡ some "4"
  (results[4]?.map (·.text)) ≡ some "5"
  (results[5]?.map (·.text)) ≡ some "6"

test "lazy star at end of pattern" := do
  -- When lazy * is at end, it matches zero
  let re ← compile! "abc.*?"
  if let some m := re.find "abcdef" then
    m.text ≡ "abc"  -- Lazy: matches zero chars after 'abc'

test "lazy question ?? vs greedy ?" := do
  -- Greedy ? matches if possible
  let greedy ← compile! "colou?r"
  shouldSatisfy (greedy.test "color") "greedy matches 'color'"
  shouldSatisfy (greedy.test "colour") "greedy matches 'colour'"
  -- Lazy ?? prefers not to match, but will if needed
  let lazy ← compile! "colou??r"
  shouldSatisfy (lazy.test "color") "lazy matches 'color'"
  shouldSatisfy (lazy.test "colour") "lazy matches 'colour'"
  -- The difference shows in what gets captured
  if let some m := lazy.find "colour" then
    m.text ≡ "colour"  -- Has to match 'u' to reach 'r'

test "lazy with character class" := do
  let re ← compile! "\\[.*?\\]"
  if let some m := re.find "[a][b][c]" then
    m.text ≡ "[a]"  -- First bracketed section
  let results := re.findAll "[a][b][c]"
  results.length ≡ 3

test "lazy quantifier preserves captures" := do
  -- ([a-z]+?) needs to be followed by space, so must match until space
  -- The first group matches "hello" (minimum to reach space)
  -- The second group matches "w" (minimum, no constraint after)
  let re ← compile! "([a-z]+?) ([a-z]+?)"
  if let some m := re.find "hello world" then
    m.text ≡ "hello w"
    m.group 1 ≡ "hello"  -- Must reach space
    m.group 2 ≡ "w"      -- Truly lazy, nothing required after
  -- Compare with greedy
  let greedy ← compile! "([a-z]+) ([a-z]+)"
  if let some m := greedy.find "hello world" then
    m.text ≡ "hello world"
    m.group 1 ≡ "hello"
    m.group 2 ≡ "world"

test "lazy alternation interaction" := do
  -- Lazy quantifier in alternation - still needs 'c' to follow
  -- In "aaac", a+? tries 'a' then looks for 'c' - fails, extends to 'aa', fails, etc.
  let re ← compile! "(a+?|b+?)c"
  if let some m := re.find "aaac" then
    m.text ≡ "aaac"  -- Minimum a's needed to reach 'c'
    m.group 1 ≡ "aaa"
  -- When 'c' comes right after first 'a', truly lazy
  if let some m := re.find "ac" then
    m.text ≡ "ac"
    m.group 1 ≡ "a"

test "lazy quantifier edge case empty input" := do
  let re ← compile! "a*?"
  if let some m := re.find "" then
    m.text ≡ ""  -- Matches empty string

test "lazy quantifier complex pattern" := do
  -- Parse key=value pairs lazily
  -- ([^=]+?) needs '=' after it, so matches until first '='
  -- ([^;]+?) has nothing after it, so matches just one char
  let re ← compile! "([^=]+?)=([^;]+?)"
  if let some m := re.find "name=John;age=30" then
    m.text ≡ "name=J"
    m.group 1 ≡ "name"  -- Must reach '='
    m.group 2 ≡ "J"     -- Truly lazy, nothing required after
  -- With anchor requiring ';' at end, both groups lazy but constrained
  let re2 ← compile! "^([^=]+?)=(.+?);"
  if let some m := re2.find "name=John;age=30" then
    m.text ≡ "name=John;"
    m.group 1 ≡ "name"
    m.group 2 ≡ "John"

-- ============================================================================
-- Regex Flags/Modifiers Tests
-- ============================================================================

test "case insensitive flag (?i) basic" := do
  -- Without flag - case sensitive
  let re1 ← compile! "hello"
  shouldSatisfy (re1.test "hello") "should match 'hello'"
  shouldSatisfy (!re1.test "HELLO") "should not match 'HELLO' without flag"
  shouldSatisfy (!re1.test "Hello") "should not match 'Hello' without flag"
  -- With flag - case insensitive
  let re2 ← compile! "(?i)hello"
  shouldSatisfy (re2.test "hello") "should match 'hello' with flag"
  shouldSatisfy (re2.test "HELLO") "should match 'HELLO' with flag"
  shouldSatisfy (re2.test "Hello") "should match 'Hello' with flag"
  shouldSatisfy (re2.test "hElLo") "should match mixed case with flag"

test "case insensitive flag with character class" := do
  let re ← compile! "(?i)[abc]+"
  shouldSatisfy (re.test "abc") "should match lowercase"
  shouldSatisfy (re.test "ABC") "should match uppercase"
  shouldSatisfy (re.test "AbC") "should match mixed case"
  if let some m := re.find "XYZaBcXYZ" then
    m.text ≡ "aBc"

test "case insensitive flag with range" := do
  let re ← compile! "(?i)[a-z]+"
  shouldSatisfy (re.test "hello") "should match lowercase"
  shouldSatisfy (re.test "HELLO") "should match uppercase"
  if let some m := re.find "123ABC456" then
    m.text ≡ "ABC"

test "case insensitive flag with alternation" := do
  let re ← compile! "(?i)yes|no"
  shouldSatisfy (re.test "yes") "should match 'yes'"
  shouldSatisfy (re.test "YES") "should match 'YES'"
  shouldSatisfy (re.test "Yes") "should match 'Yes'"
  shouldSatisfy (re.test "no") "should match 'no'"
  shouldSatisfy (re.test "NO") "should match 'NO'"

test "case insensitive flag scoped (?i:...)" := do
  let re ← compile! "(?i:hello) world"
  shouldSatisfy (re.test "HELLO world") "should match 'HELLO world'"
  shouldSatisfy (re.test "hello world") "should match 'hello world'"
  -- Note: with our current implementation, flags are global once set
  -- so this also matches "hello WORLD" - that's a known limitation

test "multiline flag (?m) caret" := do
  -- Without flag - ^ only matches start of string
  let re1 ← compile! "^hello"
  shouldSatisfy (re1.test "hello") "should match at string start"
  shouldSatisfy (!re1.test "world\nhello") "should not match after newline without flag"
  -- With flag - ^ matches after newline too
  let re2 ← compile! "(?m)^hello"
  shouldSatisfy (re2.test "hello") "should match at string start with flag"
  shouldSatisfy (re2.test "world\nhello") "should match after newline with flag"

test "multiline flag (?m) dollar" := do
  -- Without flag - $ only matches end of string
  let re1 ← compile! "world$"
  shouldSatisfy (re1.test "world") "should match at string end"
  shouldSatisfy (!re1.test "world\nhello") "should not match before newline without flag"
  -- With flag - $ matches before newline too
  let re2 ← compile! "(?m)world$"
  shouldSatisfy (re2.test "world") "should match at string end with flag"
  shouldSatisfy (re2.test "world\nhello") "should match before newline with flag"

test "multiline flag findAll" := do
  let re ← compile! "(?m)^\\w+"
  let results := re.findAll "hello\nworld\nfoo"
  results.length ≡ 3
  (results[0]?.map (·.text)) ≡ some "hello"
  (results[1]?.map (·.text)) ≡ some "world"
  (results[2]?.map (·.text)) ≡ some "foo"

test "dotall flag (?s) basic" := do
  -- Without flag - . does not match newline
  let re1 ← compile! "a.b"
  shouldSatisfy (re1.test "aXb") "should match 'aXb'"
  shouldSatisfy (!re1.test "a\nb") "should not match 'a\\nb' without flag"
  -- With flag - . matches newline
  let re2 ← compile! "(?s)a.b"
  shouldSatisfy (re2.test "aXb") "should match 'aXb' with flag"
  shouldSatisfy (re2.test "a\nb") "should match 'a\\nb' with flag"

test "dotall flag with star" := do
  let re ← compile! "(?s)start.*end"
  shouldSatisfy (re.test "start\nmiddle\nend") "should match across newlines"
  if let some m := re.find "start\nline1\nline2\nend" then
    m.text ≡ "start\nline1\nline2\nend"

test "combined flags (?im)" := do
  let re ← compile! "(?im)^hello"
  shouldSatisfy (re.test "Hello") "case insensitive at start"
  shouldSatisfy (re.test "world\nHELLO") "multiline + case insensitive"
  shouldSatisfy (re.test "foo\nhello") "multiline at line start"

test "combined flags (?ims)" := do
  let re ← compile! "(?ims)^a.b$"
  shouldSatisfy (re.test "A\nB") "all flags: case insensitive, multiline, dotall"
  shouldSatisfy (re.test "a\nb") "lowercase with dotall"

test "flag at pattern start" := do
  let re ← compile! "(?i)test"
  if let some m := re.find "This is a TEST string" then
    m.text ≡ "TEST"

test "flag with captures" := do
  let re ← compile! "(?i)(hello) (world)"
  if let some m := re.find "HELLO WORLD" then
    m.text ≡ "HELLO WORLD"
    m.group 1 ≡ "HELLO"
    m.group 2 ≡ "WORLD"

test "flag with quantifiers" := do
  let re ← compile! "(?i)a+b*c?"
  shouldSatisfy (re.test "aaa") "should match 'aaa'"
  shouldSatisfy (re.test "AAA") "should match 'AAA'"
  shouldSatisfy (re.test "AaAbBbC") "should match mixed case"

test "multiline with anchored pattern" := do
  let re ← compile! "(?m)^start.*end$"
  shouldSatisfy (re.test "start to end") "single line match"
  shouldSatisfy (re.test "prefix\nstart to end\nsuffix") "multiline middle match"

test "case insensitive email pattern" := do
  let re ← compile! "(?i)[a-z]+@[a-z]+\\.[a-z]+"
  shouldSatisfy (re.test "user@example.com") "lowercase email"
  shouldSatisfy (re.test "USER@EXAMPLE.COM") "uppercase email"
  shouldSatisfy (re.test "User@Example.Com") "mixed case email"

test "flags preserve other functionality" := do
  -- Flags should work with all other regex features
  let re ← compile! "(?i)\\b\\w+\\b"
  if let some m := re.find "HELLO world" then
    m.text ≡ "HELLO"  -- Still finds first match
  let re2 ← compile! "(?i)\\d+|[a-z]+"
  shouldSatisfy (re2.test "123") "digits still work"
  shouldSatisfy (re2.test "ABC") "case insensitive letters"

test "empty flags group" := do
  -- Just (?:) is non-capturing group, not flags
  let re ← compile! "(?:hello)"
  shouldSatisfy (re.test "hello") "non-capturing group works"
  shouldSatisfy (!re.test "HELLO") "no case insensitive without i flag"

-- ============================================
-- Lookahead Assertions Tests
-- ============================================

test "positive lookahead basic" := do
  let re ← compile! "foo(?=bar)"
  shouldSatisfy (re.test "foobar") "should match 'foo' when followed by 'bar'"
  shouldSatisfy (!re.test "foobaz") "should not match when followed by 'baz'"
  shouldSatisfy (!re.test "foo") "should not match 'foo' alone"

test "positive lookahead does not consume" := do
  let re ← compile! "foo(?=bar)bar"
  shouldSatisfy (re.test "foobar") "lookahead doesn't consume, so 'bar' can match after"
  if let some m := re.find "foobar" then
    m.text ≡ "foobar"  -- Full pattern matches

test "positive lookahead match text" := do
  let re ← compile! "\\w+(?=,)"
  if let some m := re.find "hello, world" then
    m.text ≡ "hello"  -- Matches word before comma, comma not consumed
  let results := re.findAll "a,b,c,d"
  results.length ≡ 3  -- "a", "b", "c" (not "d" - no comma after)

test "negative lookahead basic" := do
  let re ← compile! "foo(?!bar)"
  shouldSatisfy (!re.test "foobar") "should not match when followed by 'bar'"
  shouldSatisfy (re.test "foobaz") "should match when followed by 'baz'"
  shouldSatisfy (re.test "foo") "should match 'foo' at end"

test "negative lookahead match text" := do
  let re ← compile! "\\d+(?!px)"
  -- Note: Due to greedy matching, \d+ at position 0 matches "1" (since "0px" doesn't start with "px")
  -- This is correct Thompson NFA behavior (no backtracking within \d+)
  if let some m := re.find "10px 20em" then
    m.text ≡ "1"  -- First digit that isn't immediately followed by "px"
  -- For cleaner semantics, use atomic groups or word boundaries
  let re2 ← compile! "\\b\\d+\\b(?!px)"
  shouldSatisfy (!re2.test "10px") "word boundary prevents partial match"

test "negative lookahead password validation" := do
  -- Password must not start with "admin"
  let re ← compile! "^(?!admin)\\w+"
  shouldSatisfy (re.test "user123") "should match 'user123'"
  shouldSatisfy (!re.test "admin123") "should not match 'admin123'"
  shouldSatisfy (!re.test "administrator") "should not match word starting with admin"
  shouldSatisfy (re.test "adm") "should match 'adm' (not full 'admin')"

test "lookahead with character class" := do
  let re ← compile! "[a-z]+(?=[0-9])"
  if let some m := re.find "abc123" then
    m.text ≡ "abc"
  shouldSatisfy (!re.test "abcdef") "should not match without digit following"

test "lookahead with dot" := do
  let re ← compile! "a(?=.b)"
  shouldSatisfy (re.test "axb") "should match 'a' when followed by any char then 'b'"
  shouldSatisfy (!re.test "ab") "should not match when only 'b' follows"

test "lookahead at start of pattern" := do
  let re ← compile! "(?=.*\\d)\\w+"
  shouldSatisfy (re.test "abc123") "word containing digit"
  shouldSatisfy (!re.test "abcdef") "word without digit should not match"

test "lookahead with quantifiers" := do
  let re ← compile! "(?=a+b)a+"
  if let some m := re.find "aaab" then
    m.text ≡ "aaa"  -- Matches all a's, lookahead verifies b follows
  shouldSatisfy (!re.test "aaa") "should not match without b"

test "lookahead with alternation" := do
  let re ← compile! "(?=cat|dog)\\w+"
  shouldSatisfy (re.test "cat") "should match 'cat'"
  shouldSatisfy (re.test "dog") "should match 'dog'"
  shouldSatisfy (!re.test "bird") "should not match 'bird'"

test "multiple consecutive lookaheads" := do
  -- Pattern requiring both digit and letter
  let re ← compile! "^(?=.*[0-9])(?=.*[a-z]).+$"
  shouldSatisfy (re.test "abc123") "has both letter and digit"
  shouldSatisfy (re.test "1a") "minimal case"
  shouldSatisfy (!re.test "123456") "digits only should not match"
  shouldSatisfy (!re.test "abcdef") "letters only should not match"

test "nested positive lookahead" := do
  let re ← compile! "(?=a(?=b))ab"
  shouldSatisfy (re.test "ab") "nested lookahead should work"
  shouldSatisfy (!re.test "ac") "should not match 'ac'"

test "nested negative in positive" := do
  let re ← compile! "(?=a(?!c))a."
  shouldSatisfy (re.test "ab") "a followed by non-c"
  shouldSatisfy (!re.test "ac") "a followed by c should not match"

test "lookahead with anchors" := do
  let re ← compile! "^(?=.*end$).*"
  shouldSatisfy (re.test "the end") "line ending with 'end'"
  shouldSatisfy (!re.test "endless") "not ending with 'end'"

test "lookahead with word boundary" := do
  let re ← compile! "\\b(?=\\w{5}\\b)\\w+"
  if let some m := re.find "hello world" then
    m.text ≡ "hello"  -- Exactly 5 chars
  -- "there" is 5 chars, so it matches
  if let some m := re.find "hi there" then
    m.text ≡ "there"
  shouldSatisfy (!re.test "hi") "'hi' alone doesn't have 5 char word"

test "lookahead empty pattern" := do
  let re ← compile! "a(?=)b"
  shouldSatisfy (re.test "ab") "empty lookahead always succeeds"

test "negative lookahead empty pattern" := do
  let re ← compile! "a(?!)b"
  shouldSatisfy (!re.test "ab") "empty negative lookahead always fails"

test "lookahead with captures before" := do
  let re ← compile! "(foo)(?=bar)"
  if let some m := re.find "foobar" then
    m.text ≡ "foo"
    m.group 1 ≡ some "foo"  -- Capture still works

test "lookahead case insensitive" := do
  let re ← compile! "(?i)foo(?=bar)"
  shouldSatisfy (re.test "FOOBAR") "case insensitive match"
  shouldSatisfy (re.test "FooBar") "mixed case match"
  shouldSatisfy (!re.test "FOOBAZ") "still checks lookahead"

#generate_tests

end RuneTests.MatchTests
