import unittest, pegs, logging

import grammarian/common, grammarian/patterns

enableLogging()

suite "Alternatives split tests":

  test "alts  1: <123>":
    let isMatch = "123" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "123"
      check matches[1] == ""
    else:
      echo("Failed match!")

  test "alts  2: < 123>":
    let isMatch = " 123" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "123"
      check matches[1] == ""
    else:
      echo("Failed match!")

  test "alts  3: <123 >":
    let isMatch = "123 " =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "123"
      check matches[1] == ""
    else:
      echo("Failed match!")

  test "alts  4: <a/d/1>":
    let isMatch = "a/d/1" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "a"
      check matches[1] == "d/1"
    else:
      echo("Failed match!")

  test "alts  5: <a / d / 1>":
    let isMatch = "a / d / 1" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "a"
      check matches[1] == "d / 1"
    else:
      echo("Failed match!")

  test "alts  6: < a / d / 1>":
    let isMatch = " a / d / 1" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "a"
      check matches[1] == "d / 1"
    else:
      echo("Failed match!")

  test "alts  7: < (a) /b/c>":
    let isMatch = " (a) /b/c" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "(a)"
      check matches[1] == "b/c"
    else:
      echo("Failed match!")

  test "alts  8: < (a) b(c) /d>":
    let isMatch = " (a) b(c) /d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "(a) b(c)"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 8a: < (a b(c)) /d>":
    let isMatch = " (a b(c)) /d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "(a b(c))"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts  9: < (a b(c)e) /d>":
    let isMatch = " (a b(c)e) /d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "(a b(c)e)"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 10: < (a b(c)e) >":
    let isMatch = " (a b(c)e) " =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "(a b(c)e)"
      check matches[1] == ""
    else:
      echo("Failed match!")


  test "alts 11: < abc # A comment>":
    let isMatch = " abc # A comment" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "abc"
      check matches[1] == ""
    else:
      echo("Failed match!")


  test "alts 12: < abc # A comment>":
    let isMatch = " abc / d # A comment" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "abc"
      check matches[1] == "d # A comment"
    else:
      echo("Failed match!")


  test "alts 13: < 'abc' # A comment>":
    let isMatch = " 'abc' / d # A comment" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "'abc'"
      check matches[1] == "d # A comment"
    else:
      echo("Failed match!")


  test "alts 14: < 'ab/c' # A comment>":
    let isMatch = " 'ab/c' / d # A comment" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "'ab/c'"
      check matches[1] == "d # A comment"
    else:
      echo("Failed match!")


  test "alts 15: `List<a / b> / d`":
    let isMatch = "List<a / b> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List<a / b>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 16: `List<a,b> / d`":
    let isMatch = "List<a,b> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List<a,b>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 17: `List<a,b/c> / d`":
    let isMatch = "List<a,b/c> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List<a,b/c>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 18: `List<a/b,c> / d`":
    let isMatch = "List<a/b,c> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List<a/b,c>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 19: `List<a/b, c> / d`":
    let isMatch = "List<a/b, c> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List<a/b, c>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 20: `List<a / b, c> / d`":
    let isMatch = "List<a / b, c> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List<a / b, c>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 20: `List<(a b) c> / d`":
    let isMatch = "List<(a b) c> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List<(a b) c>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 21: `List<(a / b) c> / d`":
    let isMatch = "List<(a / b) c> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List<(a / b) c>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 22: `List<a, c> / d`":
    let isMatch = "List<a, c> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List<a, c>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 23: `List<(a / b) c, 'e'> / d`":
    let isMatch = "List<(a / b) c, 'e'> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List<(a / b) c, 'e'>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 24: `List<(a / b) c, 'e<f'> / d`":
    let isMatch = "List<(a / b) c, 'e<f'> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List<(a / b) c, 'e<f'>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 25: `List<(a / b) c, 'e>f'> / d`":
    let isMatch = "List<(a / b) c, 'e>f'> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List<(a / b) c, 'e>f'>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 26: `List<a, b<c>> / d`":
    let isMatch = "List<a, b<c>> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List<a, b<c>>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 27: `List< a,b> / d`":
    let isMatch = "List< a,b> / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List< a,b>"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 28: `List< a,b > / d`":
    let isMatch = "List< a,b > / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "List< a,b >"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 29: `(e List< a,b > ) / d`":
    let isMatch = "(e List< a,b > ) / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "(e List< a,b > )"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 30: `(e List< a, b / c > ) / d`":
    let isMatch = "(e List< a, b / c > ) / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "(e List< a, b / c > )"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 31: `(e List< a, (b / c) e > ) / d`":
    let isMatch = "(e List< a, (b / c) e > ) / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "(e List< a, (b / c) e > )"
      check matches[1] == "d"
    else:
      echo("Failed match!")

  test "alts 32: `(e List< a, (b< x > / c) e > ) / d`":
    let isMatch = "(e List< a, (b< x > / c) e > ) / d" =~ firstAltAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == "(e List< a, (b< x > / c) e > )"
      check matches[1] == "d"
    else:
      echo("Failed match!")


