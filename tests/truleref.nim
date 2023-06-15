import unittest, pegs, logging, strutils

import grammarian/common, grammarian/patterns

# enableLogging()

proc testRuleRef(value: string, rulename: string, params: string) =
    let isMatch = value =~ ruleRefPeg
    check isMatch
    if isMatch:
      check matches[0] == rulename
      check matches[1] == params
      check matches[2] == ""
    else:
      echo("Failed match!")


suite "Rule ref pattern":

  test "ruleref  1: only rulename":
    testRuleRef("test", "test", "")

  test "ruleref  2: only rulename, spaced":
    testRuleRef(" test ", "test", "")

  test "ruleref  3: rulename with param":
    testRuleRef("test<param>", "test", "param")

  test "ruleref  4: rulename with param":
    testRuleRef("test <param>", "test", "param")

  test "ruleref  5: rulename with param":
    testRuleRef("test< param >", "test", "param")

  test "ruleref  6: rulename with param and subparam":
    testRuleRef("test< param<sub> >", "test", "param<sub>")

  test "ruleref  7: rulename with params and subparam":
    testRuleRef("list<[a-zA-Z], spaced<','>>", "list", "[a-zA-Z], spaced<','>")
