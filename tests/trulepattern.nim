import unittest, pegs
import grammarian, grammarian/patterns


suite "rulename":
  test "name only":
    check "name <- pattern".match(firstRuleAndRestPeg)

  test "found parts":
    if " name <- pattern " =~ firstRuleAndRestPeg:
      check matches[0] == "name"
      check matches[1] == "pattern "

  test "full name part matches":
    check " name:variant<a, b> ".match(ruleHeaderPeg)

  test "name and variant matches":
    check " name:variant ".match(ruleHeaderPeg)

  test "name and params matches":
    check " name <a, b> ".match(ruleHeaderPeg)

  test "full name part contents match":
    if " name:variant<a, b> " =~ ruleHeaderPeg:
      check matches[0] == "name"
      check matches[1] == "variant"
      check matches[2] == "a, b"

  test "name and variant contents match":
    if " name:variant " =~ ruleHeaderPeg:
      check matches[0] == "name"
      check matches[1] == "variant"
      check matches[2] == ""

  test "name and params contents match":
    if " name <a, b> " =~ ruleHeaderPeg:
      check matches[0] == "name"
      check matches[1] == ""
      check matches[2] == "a, b"

  test "non-terminal param match":
    check "Test".match(firstRuleRefArgAndRestPeg)

  test "non-terminal param list match":
    check "Test, Param".match(firstRuleRefArgAndRestPeg)

  test "non-terminal param list with spaces match":
    check " Test, Param ".match(firstRuleRefArgAndRestPeg)

  test "param list literals match":
    check "'test', 'Param'".match(firstRuleRefArgAndRestPeg)

  test " param list literals cardinal match":
    check "'test'+, 'Param'?".match(firstRuleRefArgAndRestPeg)

  test " param list literals successor match":
    check "!'test', &'Param'".match(firstRuleRefArgAndRestPeg)

  test " param list literals successor and cardinal match":
    check "!'test'*, &'Param'?".match(firstRuleRefArgAndRestPeg)

  test "param list with charset match":
    check " [a-z0-9]+, Param ".match(firstRuleRefArgAndRestPeg)

  test "param list any char match":
    check " .+, Param ".match(firstRuleRefArgAndRestPeg)

  test "param list any char match":
    check " .?, .* ".match(firstRuleRefArgAndRestPeg)

  test "param list escaped char match":
    check " \\13 \\10, Param ".match(firstRuleRefArgAndRestPeg)

  test "param list alternatives match":
    check " 'a' / 'b', [a-z]+ / [0-9]* ".match(firstRuleRefArgAndRestPeg)

  test "param list sequence match":
    check " 'a'  'b', [a-z]+  [0-9]* ".match(firstRuleRefArgAndRestPeg)

  test "param list sequence match":
    check " ('a' / 'b')+, ([a-z]+ / [0-9]*) 'test' ".match(firstRuleRefArgAndRestPeg)

