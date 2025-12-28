import Rune
import Crucible

namespace RuneTests.ParserTests

open Crucible
open Rune

testSuite "Parser"

test "parse empty pattern" := do
  match Rune.Parser.parse "" with
  | .ok ast => ast.captureCount ≡ 0
  | .error _ => ensure false "unexpected parse error"

test "parse literal" := do
  match Rune.Parser.parse "abc" with
  | .ok ast => ast.captureCount ≡ 0
  | .error _ => ensure false "unexpected parse error"

test "parse dot" := do
  match Rune.Parser.parse "a.b" with
  | .ok _ => pure ()
  | .error _ => ensure false "unexpected parse error"

test "parse alternation" := do
  match Rune.Parser.parse "a|b" with
  | .ok _ => pure ()
  | .error _ => ensure false "unexpected parse error"

test "parse star quantifier" := do
  match Rune.Parser.parse "a*" with
  | .ok _ => pure ()
  | .error _ => ensure false "unexpected parse error"

test "parse plus quantifier" := do
  match Rune.Parser.parse "a+" with
  | .ok _ => pure ()
  | .error _ => ensure false "unexpected parse error"

test "parse question quantifier" := do
  match Rune.Parser.parse "a?" with
  | .ok _ => pure ()
  | .error _ => ensure false "unexpected parse error"

test "parse capture group" := do
  match Rune.Parser.parse "(abc)" with
  | .ok ast => ast.captureCount ≡ 1
  | .error _ => ensure false "unexpected parse error"

test "parse multiple groups" := do
  match Rune.Parser.parse "(a)(b)(c)" with
  | .ok ast => ast.captureCount ≡ 3
  | .error _ => ensure false "unexpected parse error"

test "parse named group" := do
  match Rune.Parser.parse "(?<name>abc)" with
  | .ok ast =>
    ast.captureCount ≡ 1
    ast.namedGroups.length ≡ 1
  | .error _ => ensure false "unexpected parse error"

test "parse bracket expression" := do
  match Rune.Parser.parse "[abc]" with
  | .ok _ => pure ()
  | .error _ => ensure false "unexpected parse error"

test "parse negated bracket" := do
  match Rune.Parser.parse "[^abc]" with
  | .ok _ => pure ()
  | .error _ => ensure false "unexpected parse error"

test "parse character range" := do
  match Rune.Parser.parse "[a-z]" with
  | .ok _ => pure ()
  | .error _ => ensure false "unexpected parse error"

test "parse bounded quantifier exactly" := do
  match Rune.Parser.parse "a{3}" with
  | .ok _ => pure ()
  | .error _ => ensure false "unexpected parse error"

test "parse bounded quantifier range" := do
  match Rune.Parser.parse "a{2,5}" with
  | .ok _ => pure ()
  | .error _ => ensure false "unexpected parse error"

test "parse bounded quantifier at least" := do
  match Rune.Parser.parse "a{2,}" with
  | .ok _ => pure ()
  | .error _ => ensure false "unexpected parse error"

test "parse escape sequence" := do
  match Rune.Parser.parse "\\n\\t" with
  | .ok _ => pure ()
  | .error _ => ensure false "unexpected parse error"

test "parse complex pattern" := do
  match Rune.Parser.parse "([a-z]+)@([a-z]+)\\.com" with
  | .ok ast => ast.captureCount ≡ 2
  | .error _ => ensure false "unexpected parse error"

test "reject unbalanced parens" := do
  match Rune.Parser.parse "(abc" with
  | .ok _ => ensure false "should have failed"
  | .error _ => pure ()

test "reject unbalanced brackets" := do
  match Rune.Parser.parse "[abc" with
  | .ok _ => ensure false "should have failed"
  | .error _ => pure ()

#generate_tests

end RuneTests.ParserTests
