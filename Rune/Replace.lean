/-
  Rune - String replacement with backreferences
-/

import Rune.API

namespace Rune

/-- Expand backreferences in a replacement string
    Supports:
    - \0 or \& for full match
    - \1, \2, ... for numbered groups
    - \g<name> for named groups
    - \\ for literal backslash
-/
def expandBackrefs (m : Match) (replacement : String) : String := Id.run do
  let mut result := ""
  let mut i := 0
  let chars := replacement.toList

  while i < chars.length do
    let c := chars[i]!
    if c == '\\' && i + 1 < chars.length then
      let next := chars[i + 1]!
      if next == '\\' then
        result := result.push '\\'
        i := i + 2
      else if next == '&' || next == '0' then
        result := result ++ m.text
        i := i + 2
      else if next.isDigit then
        -- Parse group number (could be multi-digit)
        let mut numStr := ""
        let mut j := i + 1
        while j < chars.length && chars[j]!.isDigit do
          numStr := numStr.push chars[j]!
          j := j + 1
        match numStr.toNat? with
        | some n =>
          result := result ++ (m.group n).getD ""
          i := j
        | none =>
          result := result.push c
          i := i + 1
      else if next == 'g' && i + 2 < chars.length && chars[i + 2]! == '<' then
        -- Parse named group \g<name>
        let mut name := ""
        let mut j := i + 3
        while j < chars.length && chars[j]! != '>' do
          name := name.push chars[j]!
          j := j + 1
        if j < chars.length then
          result := result ++ (m.namedGroup name).getD ""
          i := j + 1
        else
          result := result.push c
          i := i + 1
      else if next == 'n' then
        result := result.push '\n'
        i := i + 2
      else if next == 't' then
        result := result.push '\t'
        i := i + 2
      else if next == 'r' then
        result := result.push '\r'
        i := i + 2
      else
        -- Unknown escape, keep literal
        result := result.push c
        result := result.push next
        i := i + 2
    else
      result := result.push c
      i := i + 1

  result

namespace Regex

/-- Replace the first match with a replacement string -/
def replaceFirst (re : Regex) (input : String) (replacement : String) : String :=
  match re.find input with
  | none => input
  | some m =>
    let before := (input.toSubstring.extract ⟨0⟩ ⟨m.start⟩).toString
    let after := (input.toSubstring.extract ⟨m.stop⟩ ⟨input.length⟩).toString
    let replaced := expandBackrefs m replacement
    before ++ replaced ++ after

/-- Replace all matches with a replacement string -/
def replaceAll (re : Regex) (input : String) (replacement : String) : String := Id.run do
  let matchList := re.findAll input
  if matchList.isEmpty then return input

  let mut result := ""
  let mut lastEnd : Nat := 0

  for m in matchList do
    -- Add text between last match and this one
    result := result ++ (input.toSubstring.extract ⟨lastEnd⟩ ⟨m.start⟩).toString
    -- Add replacement with backrefs expanded
    result := result ++ expandBackrefs m replacement
    lastEnd := m.stop

  -- Add remaining text after last match
  result ++ (input.toSubstring.extract ⟨lastEnd⟩ ⟨input.length⟩).toString

/-- Replace first match using a function -/
def replaceFirstWith (re : Regex) (input : String) (f : Match → String) : String :=
  match re.find input with
  | none => input
  | some m =>
    let before := (input.toSubstring.extract ⟨0⟩ ⟨m.start⟩).toString
    let after := (input.toSubstring.extract ⟨m.stop⟩ ⟨input.length⟩).toString
    before ++ f m ++ after

/-- Replace all matches using a function -/
def replaceAllWith (re : Regex) (input : String) (f : Match → String) : String := Id.run do
  let matchList := re.findAll input
  if matchList.isEmpty then return input

  let mut result := ""
  let mut lastEnd : Nat := 0

  for m in matchList do
    result := result ++ (input.toSubstring.extract ⟨lastEnd⟩ ⟨m.start⟩).toString
    result := result ++ f m
    lastEnd := m.stop

  result ++ (input.toSubstring.extract ⟨lastEnd⟩ ⟨input.length⟩).toString

/-- Split string by pattern -/
def split (re : Regex) (input : String) : List String := Id.run do
  let matchList := re.findAll input
  if matchList.isEmpty then return [input]

  let mut result : List String := []
  let mut lastEnd : Nat := 0

  for m in matchList do
    let segment := (input.toSubstring.extract ⟨lastEnd⟩ ⟨m.start⟩).toString
    result := result ++ [segment]
    lastEnd := m.stop

  -- Add final segment
  let final := (input.toSubstring.extract ⟨lastEnd⟩ ⟨input.length⟩).toString
  result ++ [final]

/-- Split string by pattern, limiting number of splits -/
def splitN (re : Regex) (input : String) (n : Nat) : List String := Id.run do
  if n == 0 then return [input]

  let matchList := re.findAll input
  if matchList.isEmpty then return [input]

  let mut result : List String := []
  let mut lastEnd : Nat := 0
  let mut count := 0

  for m in matchList do
    if count >= n then break
    let segment := (input.toSubstring.extract ⟨lastEnd⟩ ⟨m.start⟩).toString
    result := result ++ [segment]
    lastEnd := m.stop
    count := count + 1

  -- Add remaining text as final segment
  let final := (input.toSubstring.extract ⟨lastEnd⟩ ⟨input.length⟩).toString
  result ++ [final]

end Regex

end Rune
