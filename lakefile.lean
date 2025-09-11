import Lake
open Lake DSL

package "MiniCcgParser" where
  version := v!"0.1.0"

lean_lib «MiniCcgParser» where
  -- add library configuration options here

@[default_target]
lean_exe "miniccgparser" where
  root := `Main
