import unittest, pegs, strutils
import grammarian/common, grammarian/patterns

proc splitFirstRule(value: string): (string, string, string) =
  if value =~ firstRuleAndRestPeg:
    (matches[0], matches[1], matches[2])
  else:
    raise newException(NoMatchError, ("value: '$#'" % value))


suite "Pegs":
  test "pegs  1: choice of literals":
    check splitFirstRule("Test <- 'a' / 'b' 'd' / 'c'") == ("Test", "'a' / 'b' 'd' / 'c'", "")

  test "pegs  2: choice of literals with comment":
    check splitFirstRule("Test <- 'a' / 'b' 'd' / 'c' # Comment") == ("Test", "'a' / 'b' 'd' / 'c' # Comment", "")

  test "pegs  3: choice of literal case insensitive":
    check splitFirstRule("Test <- i 'a'") == ("Test", "i 'a'", "")

  test "pegs  4: choice of literal case insensitive":
    check splitFirstRule("Test <- '[' @ ']'") == ("Test", "'[' @ ']'", "")

  test "pegs  5: choice of literal case insensitive":
    check splitFirstRule("Test <- i 'a' b <- 'b'") == ("Test", "i 'a'", " b <- 'b'")

  test "pegs  6: choice of literal with escaped quote":
    check splitFirstRule("Test <- i 'a \\' c' b <- 'b'") == ("Test", "i 'a \\' c'", " b <- 'b'")

  test "pegs  7: choice of literal case insensitive":
    check splitFirstRule("Test <- i 'a' / 'c' b <- 'b'") == ("Test", "i 'a' / 'c'", " b <- 'b'")

  test "pegs  8: two rules, first with subchoice":
    check splitFirstRule("Test <- i ('a' / [bcd]) / 'c' b <- 'b'") == ("Test", "i ('a' / [bcd]) / 'c'", " b <- 'b'")

  test "pegs  9: two rules, first with subsequence":
    check splitFirstRule("Test <- i ('a'  [bcd]) / 'c' b <- 'b'") == ("Test", "i ('a'  [bcd]) / 'c'", " b <- 'b'")

  test "pegs 10: comment at start":
    check splitFirstRule("# my comment\nTest <- i ('a'  [bcd]) / 'c' b <- 'b'") == ("Test", "i ('a'  [bcd]) / 'c'", " b <- 'b'")

  test "pegs 11: one rule with one param":
    check splitFirstRule("Test<one> <- i 'a' one") == ("Test<one>", "i 'a' one", "")

  test "pegs 12: one rule with one param containing garbage":
    check splitFirstRule("Test<one? !sb> <- i 'a' one") == ("Test<one? !sb>", "i 'a' one", "")

  test "pegs 13: one rule with one param":
    check splitFirstRule("Test:variant<one> <- i 'a' one") == ("Test:variant<one>", "i 'a' one", "")

  test "pegs 14: one rule with one param":
    check splitFirstRule("Test:variant <- i 'a' one") == ("Test:variant", "i 'a' one", "")

  test "pegs 15: one rule with one param and trailing comment":
    check splitFirstRule("Test:variant <- i 'a' one #c") == ("Test:variant", "i 'a' one #c", "")

  test "pegs 16: one rule with one param and trailing space":
    check splitFirstRule("Test:variant <- i 'a' one ") == ("Test:variant", "i 'a' one ", "")

  test "pegs 17: one rule with one param and leading space":
    check splitFirstRule(" Test:variant <- i 'a' one") == ("Test:variant", "i 'a' one", "")

  test "pegs 18: one rule with one param and leading comment":
    check splitFirstRule(" #c\n Test:variant <- i 'a' one") == ("Test:variant", "i 'a' one", "")
