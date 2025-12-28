import Lake
open Lake DSL

package rune where
  version := v!"0.1.0"

require crucible from ".." / "crucible"

@[default_target]
lean_lib Rune where
  roots := #[`Rune]

lean_lib RuneTests where
  globs := #[.submodules `RuneTests]

@[test_driver]
lean_exe rune_tests where
  root := `RuneTests.Main
