import unittest, pegs, streams, strutils
import grammarian, grammarian/patterns

suite "patternresolution":

  test "resolve non-literal part with one param":
    var buf = newStringStream()
    var subs: seq[RuleRef]
    let ruleRef = newRuleRef("List", @["Word"])
    let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let resolver = applier(rule, ruleRef)
    resolver.resolvePatternSpec("Item Sep List<Item> / Item", @[], subs, buf)
    setPosition(buf, 0)
    check buf.readAll().strip == "Word Sep List_Word / Word"


  test "resolve non-literal part with one param and one capture":
    var buf = newStringStream()
    var subs: seq[RuleRef]
    let ruleRef = newRuleRef("List", @["Word"])
    let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let resolver = applier(rule, ruleRef)
    let capture = parseRuleRef("Sep")
    resolver.resolvePatternSpec("Item Sep List<Item> / Item", @[capture], subs, buf)
    setPosition(buf, 0)
    check buf.readAll().strip == "Word {Sep} List_Word / Word"


  test "resolve non-literal part with one param and one param capture":
    var buf = newStringStream()
    var subs: seq[RuleRef]
    let ruleRef = newRuleRef("List", @["Word"])
    let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let resolver = applier(rule, ruleRef)
    let capture = parseRuleRef("Word")
    resolver.resolvePatternSpec("Item Sep List<Item> / Item", @[capture], subs, buf)
    setPosition(buf, 0)
    check buf.readAll().strip == "{Word} Sep List_Word / {Word}"

  test "resolve non-literal part with one param and one parameterized rule capture":
    var buf = newStringStream()
    var subs: seq[RuleRef]
    let ruleRef = newRuleRef("List", @["Word"])
    let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let resolver = applier(rule, ruleRef)
    let capture = newRuleRef("List", @["Word"])
    resolver.resolvePatternSpec("Item Sep List<Item> / Item", @[capture], subs, buf)
    setPosition(buf, 0)
    let value = buf.readAll().strip
    check value == "Word Sep {List_Word} / Word"


  test "resolve non-literal part with 2 params":
    var buf = newStringStream()
    var subs: seq[RuleRef]
    let ruleRef = newRuleRef("List", @["Word", "Space"])
    let rule = newRule("List", "Item Sep List<Item, Sep> / Item", "", @["Item", "Sep"])
    let resolver = applier(rule, ruleRef)
    resolver.resolvePatternSpec("Item Sep List<Item, Sep> / Item", @[], subs, buf)
    setPosition(buf, 0)
    let value = buf.readAll().strip
    check value == "Word Space List_Word_Space / Word"

  test "resolve rule with one literal arg":
    var buf = newStringStream()
    var subs: seq[RuleRef]
    let ruleRef = newRuleRef("List", @["'a'"])
    let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let resolver = applier(rule, ruleRef)
    resolver.resolvePatternSpec("Item Sep List<Item> / Item", @[], subs, buf)
    setPosition(buf, 0)
    check buf.readAll().strip == "('a') Sep List_p2183385484 / ('a')"

proc newRule(params: seq[string] = @[]): Rule =
  newRule("TestRule", "just a test", "", params)

suite "ruleRefResolution":
  test "no parameters, no targets":
    let ruleRef = newRuleRef("TestRule", @[])
    check applier(newRule(), ruleRef).resolveRuleRef(ruleRef).serialize == "TestRule"

  test "one parameter, no targets":
    let ruleRef = newRuleRef("TestRule", @["Een"])
    check applier(newRule(@["One"]), ruleRef).resolveRuleRef(ruleRef).serialize == "TestRule_Een"

  test "two parameters, no targets":
    let ruleRef = newRuleRef("TestRule", @["Een", "Twee"])
    check applier(newRule(@["One", "Two"]), ruleRef).resolveRuleRef(ruleRef).serialize == "TestRule_Een_Twee"

  test "no parameters, one target":
    let ruleRef = newRuleRef("TestRule", @[])
    let target = newRuleRef("TestRule")
    check applier(newRule(), ruleRef).resolveRuleRef(ruleRef, @[target]).serialize == "{TestRule}"


  test "targeted ruleref with two parameters":
    let ruleRef = newRuleRef("TestRule", @["Een", "Twee"])
    check applier(newRule(@["One", "Two"]), ruleRef).resolveRuleRef(ruleRef, @[ruleRef]).serialize == "{TestRule_Een_Twee}"

  test "two literal params, no targets":
    let ruleRef = newRuleRef("TestRule", @["'a'", "'b'"])
    check applier(newRule(@["One", "Two"]), ruleRef).resolveRuleRef(ruleRef).serialize == "TestRule_p2183385484_p517476560"

