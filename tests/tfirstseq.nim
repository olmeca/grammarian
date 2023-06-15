import unittest, pegs, logging, strutils

import grammarian/common, grammarian/patterns

# enableLogging()

proc split(pattern: Peg, value: string): (string, string) =
  if value =~ pattern:
    (matches[0], matches[1])
  else:
    raise newException(NoMatchError, ("value: '$#'" % value))

suite "Sequence split tests":

  test "seqs  1: ruleref: 'a123 bcd'":
    check firstSeqItemAndRestPeg.split("a123 bcd") == ("a123", "bcd")

  test "seqs  2: ruleref with params 'a123<blah,iI12> c3'":
    check firstSeqItemAndRestPeg.split("a123<blah,iI12> c3") == ("a123<blah,iI12>", "c3")

  test "seqs  3: ruleref with params +: '! a123<blah,iI12> + c3'":
    check firstSeqItemAndRestPeg.split("! a123<blah,iI12> + c3") == ("! a123<blah,iI12> +", "c3")

  test "seqs  4: literal: <'a123 bcd' efg>":
    check firstSeqItemAndRestPeg.split("'a123 bcd' efg") == ("'a123 bcd'", "efg")

  test "seqs  5: literal escaped quote: <'a123 \\' bcd' efg>":
    check firstSeqItemAndRestPeg.split("'a123 \\' bcd' efg") == ("'a123 \\' bcd'", "efg")

  test "seqs  6: sub: '(one)+ two' -> '(one)+', 'two'":
    check firstSeqItemAndRestPeg.split("(one)+ two") == ("(one)+", "two")

  test "seqs  7: sub: ' (one) + two' -> '(one) +', 'two'":
    check firstSeqItemAndRestPeg.split("(one)+ two") == ("(one)+", "two")

  test "seqs  8: sub: '(one two)+ three' -> '(one two)+', 'three'":
    check firstSeqItemAndRestPeg.split("(one two)+ three") == ("(one two)+", "three")

  test "seqs  9: sub: <(one 'two')+ three> -> <(one 'two')+>, <three>":
    check firstSeqItemAndRestPeg.split("(one 'two')+ three") == ("(one 'two')+", "three")

  test "seqs 10: sub: <(one 'two(')+ three> -> <(one 'two(')+>, <three>":
    check firstSeqItemAndRestPeg.split("(one 'two(')+ three") == ("(one 'two(')+", "three")

  test "seqs 11: sub: <(one (x) 'two')+ three> -> <(one (x) 'two')+>, <three>":
    check firstSeqItemAndRestPeg.split("(one (x) 'two')+ three") == ("(one (x) 'two')+", "three")

  test "seqs 12: rule params: '!list<w, s> + two' -> '!list<w, s>+', 'two'":
    check firstSeqItemAndRestPeg.split("!list<w, s> + two") == ("!list<w, s> +", "two")

  test "seqs 13: sub: '(one / two)+ three' -> '(one / two)+', 'three'":
    check firstSeqItemAndRestPeg.split("(one / two)+ three") == ("(one / two)+", "three")

