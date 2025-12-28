import Rune
import Crucible

import RuneTests.ParserTests
import RuneTests.MatchTests
import RuneTests.ReplaceTests

open Crucible

def main : IO UInt32 := do
  IO.println "╔══════════════════════════════════════╗"
  IO.println "║     Rune Regex Library Tests         ║"
  IO.println "╚══════════════════════════════════════╝"
  IO.println ""

  let exitCode ← runAllSuites

  IO.println ""
  if exitCode == 0 then
    IO.println "✓ All test suites passed!"
  else
    IO.println s!"✗ {exitCode} test suite(s) had failures"

  return exitCode
