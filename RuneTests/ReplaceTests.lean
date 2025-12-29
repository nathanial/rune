import Rune
import Crucible
import RuneTests.TestUtils

namespace RuneTests.ReplaceTests

open Crucible
open Rune
open RuneTests

testSuite "Replace"

test "replaceFirst simple" := do
  let re ← compile! "[a-z]+"
  let result := re.replaceFirst "hello world" "X"
  result ≡ "X world"

test "replaceAll simple" := do
  let re ← compile! "[a-z]+"
  let result := re.replaceAll "hello world foo" "X"
  result ≡ "X X X"

test "replaceFirst with backref" := do
  let re ← compile! "([a-z]+)"
  let result := re.replaceFirst "hello world" "[\\1]"
  result ≡ "[hello] world"

test "replaceAll with backref" := do
  let re ← compile! "([a-z]+)"
  let result := re.replaceAll "hello world" "[\\1]"
  result ≡ "[hello] [world]"

test "replace with full match backref" := do
  let re ← compile! "[a-z]+"
  let result := re.replaceAll "hello world" "[\\0]"
  result ≡ "[hello] [world]"

test "replace with named group backref" := do
  let re ← compile! "(?<word>[a-z]+)"
  let result := re.replaceFirst "hello world" "[\\g<word>]"
  result ≡ "[hello] world"

test "replace no match" := do
  let re ← compile! "[0-9]+"
  let result := re.replaceFirst "hello world" "X"
  result ≡ "hello world"

test "split simple" := do
  let re ← compile! ","
  let parts := re.split "a,b,c"
  parts.length ≡ 3
  (parts[0]?) ≡ some "a"
  (parts[1]?) ≡ some "b"
  (parts[2]?) ≡ some "c"

test "split with spaces" := do
  let re ← compile! " +"
  let parts := re.split "hello  world   foo"
  parts.length ≡ 3
  (parts[0]?) ≡ some "hello"
  (parts[1]?) ≡ some "world"
  (parts[2]?) ≡ some "foo"

test "split no match" := do
  let re ← compile! ","
  let parts := re.split "hello"
  parts.length ≡ 1
  (parts[0]?) ≡ some "hello"

test "splitN limits splits" := do
  let re ← compile! ","
  let parts := re.splitN "a,b,c,d,e" 2
  parts.length ≡ 3
  (parts[0]?) ≡ some "a"
  (parts[1]?) ≡ some "b"
  (parts[2]?) ≡ some "c,d,e"

test "replaceAllWith function" := do
  let re ← compile! "[a-z]+"
  let result := re.replaceAllWith "hello world" fun m => m.text.toUpper
  result ≡ "HELLO WORLD"

#generate_tests

end RuneTests.ReplaceTests
