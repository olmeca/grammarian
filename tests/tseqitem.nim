import unittest, pegs, logging, strutils

import grammarian/common, grammarian/patterns

# enableLogging()

proc testCompositeSeqItem(value: string, prefix: string, content: string, postfix: string) =
    let isMatch = value =~ compositeSeqItemPeg
    check isMatch
    if isMatch:
      check matches[0] == prefix
      check matches[1] == content
      check matches[2] == postfix
      check matches[3] == ""
    else:
      echo("Failed match!")

proc testSeqItem(value: string, prefix: string, content: string, postfix: string) =
    let isMatch = value =~ seqItemPeg
    check isMatch
    if isMatch:
      check matches[0] == prefix
      check matches[1] == content
      check matches[2] == postfix
      check matches[3] == ""
    else:
      echo("Failed match!")

suite "Seq Item: composite":

  test "composite  1: <(abc)>":
    testCompositeSeqItem("(abc)", "", "abc", "")

  test "composite  2: prefix <!(abc)>":
    testCompositeSeqItem("!(abc)", "!", "abc", "")

  test "composite  3: postfix <(abc)+>":
    testCompositeSeqItem("(abc)+", "", "abc", "+")

  test "composite  4: prefix and postfix <!(abc)+>":
    testCompositeSeqItem("!(abc)+", "!", "abc", "+")

  test "composite  5: spaces between <! ( abc ) +>":
    testCompositeSeqItem("! (abc) +", "!", "abc", "+")

  test "composite  6: trailing space <! ( abc ) + >":
    testCompositeSeqItem("! (abc) + ", "!", "abc", "+")

  test "composite  7: subcomposite <! ( abc ) +>":
    testCompositeSeqItem("! (ab (def) c) +", "!", "ab (def) c", "+")

  test "composite  8: subcomposite, string literal <! (ab (def) 'x' c) +>":
    testCompositeSeqItem("! (ab (def) 'x' c) +", "!", "ab (def) 'x' c", "+")

  test "composite  9: string lit with paren <! ( ab'('c ) + >":
    testCompositeSeqItem("! (ab'('c) + ", "!", "ab'('c", "+")

  test "composite 10: string lit with paren <! ( ab')'c ) + >":
    testCompositeSeqItem("! (ab')'c) + ", "!", "ab')'c", "+")

suite "Seq Item: singular":

  test "singular  1: <abc>":
    testSeqItem("abc", "", "abc", "")

  test "singular  2: prefix <!abc>":
    testSeqItem("!abc", "!", "abc", "")

  test "singular  3: postfix <abc+>":
    testSeqItem("abc+", "", "abc", "+")

  test "singular  4: prefix and postfix <!abc+>":
    testSeqItem("!abc+", "!", "abc", "+")

  test "singular  5: spaces between <! abc +>":
    testSeqItem("! abc +", "!", "abc", "+")

  test "singular  6: trailing space <! abc + >":
    testSeqItem("! abc + ", "!", "abc", "+")

  test "singular  7: string lit < 'test' >":
    testSeqItem(" 'test' ", "", "'test'", "")

  test "singular  8: charset <[a-z0-9_]+>":
    testSeqItem(" [a-z0-9_]+ ", "", "[a-z0-9_]", "+")

  test "singular  9: charset <! [a-z0-9_]+>":
    testSeqItem("! [a-z0-9_] + ", "!", "[a-z0-9_]", "+")

  test "singular 10: builtin < \\ident >":
    testSeqItem(" \\ident ", "", "\\ident", "")

  test "singular 11: builtin <! \\ident ?>":
    testSeqItem("! \\ident ?", "!", "\\ident", "?")


suite "Seq Item: ruleref params":

  test "singular  1: 'abc<def>'":
    testSeqItem("abc<def>", "", "abc<def>", "")

  test "singular  1: ' abc<def> '":
    testSeqItem(" abc<def> ", "", "abc<def>", "")

  test "singular  1: ' abc<d<gh>ef> '":
    testSeqItem(" abc<d<gh>ef> ", "", "abc<d<gh>ef>", "")

  test "singular  1: ' abc<d<'gh'>ef> '":
    testSeqItem(" abc<d<'gh'>ef> ", "", "abc<d<'gh'>ef>", "")

  test "singular  1: list<[a-z], spaced<','>>":
    testSeqItem("list<[a-z], spaced<','>>", "", "list<[a-z], spaced<','>>", "")
