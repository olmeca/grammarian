import unittest, pegs, streams, sequtils, strutils, sugar, logging
import grammarian, grammarian/common, grammarian/patterns


proc mark4capture(pattern: string, targets: seq[string]): string =
  let rule = newRule("Dummy", pattern)
  let grammar = newGrammar()
  addRule(grammar, rule)
  let ruleRes = applier(rule, @[])
  var ruleRefs: seq[RuleRef] = @[]
  var buf = newStringStream()
  let targetRefs = targets.map(t => newRuleRef(t))
  let spec = SubGrammarSpec(grammar: grammar, variant: "", captures: targetRefs)
  resolveRuleRes(spec, ruleRes, ruleRefs, buf)
  setPosition(buf, 0)
  result = readAll(buf).strip

enableLogging()

suite "capture":
  test "mark4capture 1":
    check mark4capture("Word Sep Number", @["Number"]) == "Word Sep {Number}"

  test "mark4capture 2":
    check mark4capture("Number Sep Numbers / Number", @["Number"]) == "{Number} Sep Numbers / {Number}"


