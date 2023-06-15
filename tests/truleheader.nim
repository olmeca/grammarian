import unittest, pegs, logging, strutils

import grammarian/common, grammarian/patterns

# enableLogging()

proc testRuleHeader(value: string, rulename: string, params: string, flavor: string) =
    let isMatch = value =~ ruleHeaderPeg
    check isMatch
    if isMatch:
      check matches[0] == rulename
      check matches[1] == flavor
      check matches[2] == params
      check matches[3] == ""
    else:
      echo("Failed match!")


suite "Rule header pattern, no flavor":

  test "ruleheader  1: only rulename":
    testRuleHeader("test", "test", "", "")

  test "ruleheader  2: only rulename, spaced":
    testRuleHeader(" test ", "test", "", "")

  test "ruleheader  3: rulename with param":
    testRuleHeader("test<param>", "test", "param", "")

  test "ruleheader  4: rulename with param":
    testRuleHeader("test <param>", "test", "param", "")

  test "ruleheader  5: rulename with param":
    testRuleHeader("test< param >", "test", "param", "")

  test "ruleheader  6: rulename with param and subparam":
    testRuleHeader("test< param<sub> >", "test", "param<sub>", "")

  test "ruleheader  7: rulename with params and subparam":
    testRuleHeader("list<[a-zA-Z], spaced<','>>", "list", "[a-zA-Z], spaced<','>", "")


suite "Rule header pattern, with flavor":

  test "ruleheader, flavor  1: only rulename and flavor":
    testRuleHeader("test:flavor", "test", "", "flavor")

  test "ruleheader, flavor  2: only rulename and flavor, spaced":
    testRuleHeader(" test:flavor ", "test", "", "flavor")

  test "ruleheader, flavor  3: rulename and flavor with param":
    testRuleHeader("test:flavor<param>", "test", "param", "flavor")

  test "ruleheader, flavor  4: rulename and flavor with param":
    testRuleHeader("test:flavor <param>", "test", "param", "flavor")

  test "ruleheader, flavor  5: rulename and flavor with param":
    testRuleHeader("test:flavor< param >", "test", "param", "flavor")

  test "ruleheader, flavor  6: rulename and flavor with param and subparam":
    testRuleHeader("test:flavor< param<sub> >", "test", "param<sub>", "flavor")

  test "ruleheader, flavor  7: rulename and flavor with params and subparam":
    testRuleHeader("list:flavor<[a-zA-Z], spaced<','>>", "list", "[a-zA-Z], spaced<','>", "flavor")
