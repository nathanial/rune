import Rune

namespace RuneTests

/-- Helper to compile a regex or fail the test -/
def compile! (pattern : String) : IO Rune.Regex := do
  match Rune.Regex.compile pattern with
  | .ok re => pure re
  | .error e => throw (IO.userError s!"compile error: {e}")

end RuneTests
