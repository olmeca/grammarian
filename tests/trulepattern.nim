import unittest, pegs
import grammarian, grammarian/patterns


suite "rulename":
  test "name only":
    check "name <- pattern".match(named_rule_peg)

  test "found parts":
    if " name <- pattern " =~ named_rule_peg:
      check matches[0] == " name "
      check matches[1] == " pattern "

  test "full name part matches":
    check " name:variant<a, b> ".match(name_section_peg)

  test "name and variant matches":
    check " name:variant ".match(name_section_peg)

  test "name and params matches":
    check " name <a, b> ".match(name_section_peg)

  test "full name part contents match":
    if " name:variant<a, b> " =~ name_section_peg:
      check matches[0] == "name"
      check matches[1] == "variant"
      check matches[2] == "a, b"

  test "name and variant contents match":
    if " name:variant " =~ name_section_peg:
      check matches[0] == "name"
      check matches[1] == "variant"
      check matches[2] == ""

  test "name and params contents match":
    if " name <a, b> " =~ name_section_peg:
      check matches[0] == "name"
      check matches[1] == ""
      check matches[2] == "a, b"

  test "non-terminal param match":
    check "Test".match(rule_params_peg)

  test "non-terminal param list match":
    check "Test, Param".match(rule_params_peg)

  test "non-terminal param list with spaces match":
    check " Test, Param ".match(rule_params_peg)

  test "param list literals match":
    check "'test', 'Param'".match(rule_params_peg)

  test " param list literals cardinal match":
    check "'test'+, 'Param'?".match(rule_params_peg)

  test " param list literals successor match":
    check "!'test', &'Param'".match(rule_params_peg)

  test " param list literals successor and cardinal match":
    check "!'test'*, &'Param'?".match(rule_params_peg)

  test "param list with charset match":
    check " [a-z0-9]+, Param ".match(rule_params_peg)

  test "param list any char match":
    check " .+, Param ".match(rule_params_peg)

  test "param list any char match":
    check " .?, .* ".match(rule_params_peg)

  test "param list escaped char match":
    check " \\13 \\10, Param ".match(rule_params_peg)

  test "param list alternatives match":
    check " 'a' / 'b', [a-z]+ / [0-9]* ".match(rule_params_peg)

  test "param list sequence match":
    check " 'a'  'b', [a-z]+  [0-9]* ".match(rule_params_peg)

  test "param list sequence match":
    check " ('a' / 'b')+, ([a-z]+ / [0-9]*) 'test' ".match(rule_params_peg)


suite "lineparsing":

  test "name variant":
    let rule: Rule = read_peg_line("name:var <- value ")
    check rule.name == "name"
    check rule.variant == "var"

  test "name variant params":
    let rule = read_peg_line("name:var<a,b> <- value ")
    check rule.name == "name"
    check rule.parameters.sequal(["a", "b"])
    check rule.variant == "var"

  test "name variant spaced params":
    let rule = read_peg_line("name:var< a, b > <- value ")
    check rule.name == "name"
    check rule.parameters.sequal(["a", "b"])
    check rule.variant == "var"

  test "name spaced params":
    let rule = read_peg_line("name< a, b > <- value ")
    check rule.name == "name"
    check rule.parameters.sequal(["a", "b"])

  test "name one params":
    let rule = read_peg_line("name< a > <- value ")
    check rule.name == "name"
    check rule.parameters.sequal(["a"])
