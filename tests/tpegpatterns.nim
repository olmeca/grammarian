import unittest, pegs
import grammarian/patterns

proc testPattern(pattern: Peg, source: string) =
  check source =~ pattern

suite "patternmatch":
  test "pattern match alternatives 1":
    testPattern(rule_peg_alternatives, "Alternative ('/' Sp  Alternative)* / Nothing / (! ([a-z] / [A-Z]) .)+")

  test "pattern match sequences 1":
    testPattern(rule_peg_sequence, "One (! ([a-z] / [A-Z]) .)+ Three")

  test "pattern match sequences 2":
    testPattern(rule_peg_sequence, "One (! ([a-z] / [A-Z]) .)+ ! Three &(!'3' .)* ")

  test "pattern match composite":
    testPattern(rule_peg_composite, "(! ([a-z] / [A-Z]) .)")

  test "pattern match item non-terminal simple":
    testPattern(rule_peg_item, "One")

  test "pattern match item non-terminal +":
    testPattern(rule_peg_item, "One+")

  test "pattern match item !non-terminal":
    testPattern(rule_peg_item, "!One")

  test "pattern match item !non-terminal +":
    testPattern(rule_peg_item, "&One+")

  test "pattern match item composite":
    testPattern(rule_peg_item, "(One / Two)+")

  test "pattern match item charset +":
    testPattern(rule_peg_item, "[a-z]+")

  test "pattern match item & charset +":
    testPattern(rule_peg_item, "&[a-z] +")

  test "pattern match item & literal +":
    testPattern(rule_peg_item, "& 'test' +")

  test "pattern match parameterized item":
    testPattern(rule_peg_item, "List<Word, Item>")

  test "pattern match parameterized item with space":
    testPattern(rule_peg_item, "List<Word, Item> ")

  test "pattern match with anchors":
    testPattern(rule_peg_alternatives, " ^ SelectClause FromClause WhereClause OrderClause? !.")

  test "pattern with underscores":
    let pattern = peg"""
    Pattern <-  'Value: ' {Value}
    Value <- List_Ident_Dot / List_Number_Comma
    List_Ident_Dot <- Ident Dot List_Ident_Dot / Ident
    Ident <-  [a-z]+
    Dot <-  '.'
    List_Number_Comma <- Number Comma List_Number_Comma / Number
    Number <-  [0-9]+
    Comma <-  ','

    """
    testPattern(pattern, "Value: 12345")

