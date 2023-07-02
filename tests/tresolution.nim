import unittest, pegs, streams, strutils, sequtils, logging
import grammarian, grammarian/patterns, grammarian/common


proc subSpec(grammar: Grammar, captures: seq[RuleRef]): SubGrammarSpec =
  SubGrammarSpec(grammar: grammar, variant: "", captures: captures)


suite "patternresolution":

  setup:
    var buf = newStringStream()
    var subs: seq[RuleRef]

  test "resolve non-literal part with one param":
    let ruleRef = newRuleRef("List", @["Word"])
    let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let grammar = newGrammar()
    addRule(grammar, rule)
    let resolver = applier(rule, ruleRef)
    let spec = subSpec( grammar, @[])
    resolveRuleRes(spec, resolver, subs, buf)
    setPosition(buf, 0)
    check buf.readAll().strip == "(Word ) Sep List_Word / (Word )"


  test "resolve non-literal part with one param and one capture":
    let ruleRef = newRuleRef("List", @["Word"])
    let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let grammar = newGrammar()
    addRule(grammar, rule)
    let resolver = applier(rule, ruleRef)
    let capture = parseRuleRef("Sep")
    let spec = subSpec( grammar, @[capture])
    resolveRuleRes(spec, resolver, subs, buf)
    setPosition(buf, 0)
    check buf.readAll().strip == "(Word ) {Sep} List_Word / (Word )"


  test "resolve non-literal part with one param and one param capture":
    let ruleRef = newRuleRef("List", @["Word"])
    let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let grammar = newGrammar()
    addRule(grammar, rule)
    let resolver = applier(rule, ruleRef)
    let capture = parseRuleRef("Word")
    let spec = subSpec( grammar, @[capture])
    resolveRuleRes(spec, resolver, subs, buf)
    setPosition(buf, 0)
    check buf.readAll().strip == "({Word} ) Sep List_Word / ({Word} )"

  test "resolve non-literal part with one param and one parameterized rule capture":
    let ruleRef = newRuleRef("List", @["Word"])
    let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let grammar = newGrammar()
    addRule(grammar, rule)
    let resolver = applier(rule, ruleRef)
    let capture = newRuleRef("List", @["Word"])
    let spec = subSpec( grammar, @[capture])
    resolveRuleRes(spec, resolver, subs, buf)
    setPosition(buf, 0)
    let value = buf.readAll().strip
    check value == "(Word ) Sep {List_Word} / (Word )"


  test "resolve non-literal part with 2 params":
    let ruleRef = newRuleRef("List", @["Word", "Space"])
    let rule = newRule("List", "Item Sep List<Item, Sep> / Item", "", @["Item", "Sep"])
    let grammar = newGrammar()
    addRule(grammar, rule)
    let resolver = applier(rule, ruleRef)
    let spec = subSpec( grammar, @[])
    resolveRuleRes(spec, resolver, subs, buf)
    setPosition(buf, 0)
    let value = buf.readAll().strip
    check value == "(Word ) (Space ) List_Word_Space / (Word )"

  test "resolve rule with one literal arg":
    let ruleRef = newRuleRef("List", @["[a-z]+"])
    let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let grammar = newGrammar()
    addRule(grammar, rule)
    let resolver = applier(rule, ruleRef)
    let spec = subSpec( grammar, @[])
    resolveRuleRes(spec, resolver, subs, buf)
    setPosition(buf, 0)
    let resolved = buf.readAll().strip
    check resolved =~ peg"'([a-z]+) Sep List_p' [0-9]+ ' / ([a-z]+)'"

suite "recursive":
  enableLogging()
  setup:
    var buf = newStringStream()
    var subs: seq[RuleRef]

  test "listspacedword":
    let ruleRef = newRuleRef("List", @["Spaced<Word>"])
    let listRule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let spacedRule = newRule("Spaced", "Sp Thing Sp", "", @["Thing"])
    let grammar = newGrammar()
    addRule(grammar, listRule)
    addRule(grammar, spacedRule)
    let resolver = applier(listRule, ruleRef)
    let spec = subSpec( grammar, @[])
    resolveRuleRes(spec, resolver, subs, buf)
    setPosition(buf, 0)
    let resolved = buf.readAll().strip
    check resolved =~ peg"'(Spaced_Word ) Sep List_p' [0-9]+ ' / (Spaced_Word )'"
    check subs.filterIt(it.name == "Spaced" and sequal(it.args, @["Word"])).len() > 0

proc newRule(params: seq[string] = @[]): Rule =
  newRule("TestRule", "just a test", "", params)
#
# suite "ruleRefResolution":
#   test "no parameters, no targets":
#     let ruleRef = newRuleRef("TestRule", @[])
#     check applier(newRule(), ruleRef).resolveRuleRef(ruleRef).serialize == "TestRule"
#
#   test "one parameter, no targets":
#     let ruleRef = newRuleRef("TestRule", @["Een"])
#     check applier(newRule(@["One"]), ruleRef).resolveRuleRef(ruleRef).serialize == "TestRule_Een"
#
#   test "two parameters, no targets":
#     let ruleRef = newRuleRef("TestRule", @["Een", "Twee"])
#     check applier(newRule(@["One", "Two"]), ruleRef).resolveRuleRef(ruleRef).serialize == "TestRule_Een_Twee"
#
#   test "no parameters, one target":
#     let ruleRef = newRuleRef("TestRule", @[])
#     let target = newRuleRef("TestRule")
#     check applier(newRule(), ruleRef).resolveRuleRef(ruleRef, @[target]).serialize == "{TestRule}"
#
#
#   test "targeted ruleref with two parameters":
#     let ruleRef = newRuleRef("TestRule", @["Een", "Twee"])
#     check applier(newRule(@["One", "Two"]), ruleRef).resolveRuleRef(ruleRef, @[ruleRef]).serialize == "{TestRule_Een_Twee}"
#
#   test "two literal params, no targets":
#     let ruleRef = newRuleRef("TestRule", @["'a'", "'b'"])
#     check applier(newRule(@["One", "Two"]), ruleRef).resolveRuleRef(ruleRef).serialize == "TestRule_p2183385484_p517476560"
#
