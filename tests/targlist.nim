import unittest, pegs, logging

import grammarian/common, grammarian/patterns

enableLogging()


proc testArgList(value: string, firstArg: string, rest: string) =
    let isMatch = value =~ firstRuleRefArgAndRestPeg
    check isMatch
    if isMatch:
      check matches[0] == firstArg
      check matches[1] == rest
      check matches[2] == ""
    else:
      echo("Failed match!")


suite "Splitting first argument off ruleref argument list":

  test "1 <one> ---> <one>, <>":
    testArgList("one", "one", "")

  test "2 <one / two> ---> <one / two>, <>":
    testArgList("one / two", "one / two", "")

  test "3 <one / two, three> ---> <one / two>, <three>":
    testArgList("one / two, three", "one / two", "three")

  test "4 <one / two, three, four> ---> <one / two>, <three, four>":
    testArgList("one / two, three, four", "one / two", "three, four")

  test "5 <one / 'three, four'> ---> <one / 'three, four'>, <>":
    testArgList("one / 'three, four'", "one / 'three, four'", "")

  test "6 <[a-z]+ \\d / (', ' abc)?, def> ---> <[a-z]+ \\d / (', ' abc)?>, <def>":
    testArgList("[a-z]+ \\d / (', ' abc)?, def", "[a-z]+ \\d / (', ' abc)?", "def")

  test "7 [spaced<','>, test] ---> [spaced<','>], [test]":
    testArgList("spaced<','>, test", "spaced<','>", "test")

  test "8 [spaced<'a < b'>, test] ---> [spaced<'a < b'>], [test]":
    testArgList("spaced<'a < b'>, test", "spaced<'a < b'>", "test")

