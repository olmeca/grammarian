import unittest, pegs, streams, sequtils, strutils, sugar
import grammarian, grammarian/patterns


proc mark4capture(pattern: string, targets: seq[string]): string =
  let rule = newRule("Dummy", pattern)
  let ruleRes = applier(rule, @[])
  var ruleRefs: seq[RuleRef] = @[]
  var buf = newStringStream()
  let targetRefs = targets.map(t => newRuleRef(t))
  resolvePatternSpec(ruleRes, targetRefs, ruleRefs, buf)
  setPosition(buf, 0)
  result = readAll(buf).strip


suite "capture":
  test "mark4capture 1":
    check mark4capture("Word Sep Number", @["Number"]) == "Word Sep {Number}"

  test "mark4capture 1":
    check mark4capture("Word (Sep / Number)", @["Number"]) == "Word (Sep / {Number} )"


