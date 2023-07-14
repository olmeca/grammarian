import unittest, pegs, streams, strutils, sequtils, logging
import grammarian, grammarian/patterns, grammarian/common


let mockerConfigSpec* = """
# This part also covers the Karate header syntax
KarateHeader <- ^ Space KaratePrefix HeaderName Space '=' HeaderValuePart
KaratePrefix <- ('*' / 'And' / 'Given') ' header '
HeaderName <- Space 'x-ing-wm-' MockerName
MockerName:header <- [a-zA-Z0-9-_]+
OnlyMockerName <- ^ MockerName !.
MockerName <- 'means' / 'mean' / 'individual' / 'party' / 'parties-lookup' / 'cronto-' ('latest' / 'provision' / 'activation') / 'access-mgt' / 'userreg'
HeaderValuePart <- Space DQuote MockerConfiguration DQuote
MockerConfiguration:header <- (!DQuote .)+

# This part describes the content of the header
MockerConfiguration <- ^ List<ResponseAlternative, AltSep> !.
ResponseAlternative:ResAlt <- UpToAltEnd
UpToAltEnd <- (!AltSep .)+
AltEnd <- AltSep / End
ResponseAlternative <- Condition Implies Response
Condition:altHighLevel <- (!Implies .)+
Response:altHighLevel <- .+ !.
Implies <- '=>' Space
Condition <- List<Predicate, PredSep>
Predicate:condition <- (!(Space (And / !.)) .)+
Predicate <- Name Equals ElementaryValue Space
Name:predicate <- (! Equals .)+
ElementaryValue:predicate <- .*
Response <- Status Spaced<And> BodySpecification / Status Space Empty / Empty BodySpecification
BodySpecification <- ObjectSpecification ObjectSep BodySpecification / ObjectSpecification / ''
BodySpecification:respHighLevel <- (![0-9] .) .+ !.
Status <- [2-5] \d \d
Status:respHighLevel <- \d+
ObjectSpecification <- PrototypeName State? (':' Attributes)? Space
ObjectSpecification:bodyHighLevel <- (!ObjectSep .)* / .*
State <- '@' ('past' / 'present')
Attributes <- Attribute (';' Attribute)*
Attribute <- Space Name '=' Values
Values <- Value (',' Value)*
Value <- CompositeValue / ElementaryValue
CompositeValue <- Type '/' ElementaryValue
Type <- [A-Z_]+
ElementaryValue <- Expression / LiteralValue
Expression <- UuidExpr / StaticObjectRef / ContextObjectRef / NullExpr
UuidExpr <- '$u' \d+
StaticObjectRef <- '$d' PrototypeName
ContextObjectRef <- '$c' Name
NullExpr <- '$n'
LiteralValue <- [a-zA-Z0-9_-]+
PrototypeName <- 'scanner' / 'card' / 'mtoken' / 'username' / 'individual' / 'userreg' / 'accprof' / 'prof' / 'provisioning' / 'party'
Comment <- Space '#' .*
List<Item, Sep> <- Item Sep List<Item, Sep> / Item Space
OptList<Item, Sep> <- Item Sep OptList<Item, Sep> / Item / Empty
AltSep <- Spaced<Pipe>
PredSep <- Spaced<And>
Equals <- Spaced<EqualsSign>
Spaced<Item> <- Space Item Space
UpTo<Limit> <- (! (Limit) .)+
UpToEnd <- .+ !.
ObjectSep <- ', '
And <- '&'
Pipe <- '|'
EqualsSign <- '='

# Common
Empty <- ''
Name <- \ident
Space <- ' '*
End <- !.
DQuote <- '"'
"""


proc subSpec(grammar: Grammar, captures: seq[RuleRef]): SubGrammarSpec =
  SubGrammarSpec(grammar: grammar, variant: "", captures: captures)


suite "nominal-param-capture":

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
    resolveRuleRes(spec, resolver, subs, buf, 0)
    setPosition(buf, 0)
    check buf.readAll().strip == "Word Sep List_Word / Word"


  test "resolve non-literal part with one param and one capture":
    let ruleRef = newRuleRef("List", @["Word"])
    let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let grammar = newGrammar()
    addRule(grammar, rule)
    let resolver = applier(rule, ruleRef)
    let capture = newRuleRef("Sep")
    let spec = subSpec( grammar, @[capture])
    resolveRuleRes(spec, resolver, subs, buf, 0)
    setPosition(buf, 0)
    check buf.readAll().strip == "Word {Sep} List_Word / Word"


  test "capture-param":
    let ruleRef = newRuleRef("List", @["Word"])
    let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let grammar = newGrammar()
    addRule(grammar, rule)
    let resolver = applier(rule, ruleRef)
    let capture = newRuleRef("Word")
    let spec = subSpec( grammar, @[capture])
    resolveRuleRes(spec, resolver, subs, buf, 0)
    setPosition(buf, 0)
    check buf.readAll().strip == "{Word} Sep List_Word / {Word}"

# TODO: for now only non-literals with no params can be captured.
  # test "resolve non-literal part with one param and one parameterized rule capture":
  #   let ruleRef = newRuleRef("List", @["Word"])
  #   let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
  #   let grammar = newGrammar()
  #   addRule(grammar, rule)
  #   let resolver = applier(rule, ruleRef)
  #   let capture = newRuleRef("List", @["Word"])
  #   let spec = subSpec( grammar, @[capture])
  #   resolveRuleRes(spec, resolver, subs, buf, 0)
  #   setPosition(buf, 0)
  #   let value = buf.readAll().strip
  #   check value == "Word Sep {List_Word} / Word"


  test "resolve non-literal part with 2 params":
    let ruleRef = newRuleRef("List", @["Word", "Space"])
    let rule = newRule("List", "Item Sep List<Item, Sep> / Item", "", @["Item", "Sep"])
    let grammar = newGrammar()
    addRule(grammar, rule)
    let resolver = applier(rule, ruleRef)
    let spec = subSpec( grammar, @[])
    resolveRuleRes(spec, resolver, subs, buf, 0)
    setPosition(buf, 0)
    let value = buf.readAll().strip
    check value == "Word Space List_Word_Space / Word"

  test "resolve rule with one literal arg":
    let ruleRef = newRuleRef("List", @["[a-z]+"])
    let rule = newRule("List", "Item Sep List<Item> / Item", "", @["Item"])
    let grammar = newGrammar()
    addRule(grammar, rule)
    let resolver = applier(rule, ruleRef)
    let spec = subSpec( grammar, @[])
    resolveRuleRes(spec, resolver, subs, buf, 0)
    setPosition(buf, 0)
    let resolved = buf.readAll().strip
    check resolved =~ peg"'([a-z]+) Sep List_p' [0-9]+ ' / ([a-z]+)'"

suite "recursive":
  # enableLogging()
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
    resolveRuleRes(spec, resolver, subs, buf, 0)
    setPosition(buf, 0)
    let resolved = buf.readAll().strip
    check resolved == "Spaced_Word Sep List_Spaced_Word / Spaced_Word"
    check subs.filterIt(it.name == "Spaced" and sequal(it.args, @["Word"])).len() > 0

  let grammarSpec = """
  pat <- 'val: ' list< a,s<z> > ' end' !.
  parpat<a,z> <- 'val: ' list< a,s<z> > ' end' !.
  list<i,j> <- i j list<i,j> / i
  s<x> <- '[' x ']'
  a <- 'A'
  z <- 'Z'
  x <- 'X'
  y <- 'Y'
  """

  test "cascadedruleref":
    let grammar = newGrammar(grammarSpec)
    let pegSpec = pegString(grammar, "pat")
    echo pegSpec

  test "cascadedruleref2":
    let grammar = newGrammar(grammarSpec)
    let spec = matcher(grammar, "pat")
    check("val: A end" =~ spec)
    check("val: A[Z]A end" =~ spec)
    check("val: A[Z]A[Z]A end" =~ spec)


  test "cascadedruleref3":
    let grammar = newGrammar(grammarSpec)
    let spec = matcher(grammar, "parpat", @["x", "y"])
    check("val: X end" =~ spec)
    check("val: X[Y]X end" =~ spec)
    check("val: X[Y]X[Y]X end" =~ spec)


  test "cascadedruleref4":
    let grammar = newGrammar(grammarSpec)
    let spec = matcher(grammar, "parpat", @["'H'", "y"])
    check("val: H end" =~ spec)
    check("val: H[Y]H end" =~ spec)
    check("val: H[Y]H[Y]H end" =~ spec)

suite "variants":

  let variantsSpec = """
  p <- ^ '[' c ']' !.
  c <- l<a,p<b>>
  l<x,y> <- x y l<x,y> / x
  p<i> <- '(' i ')'
  a:n <- 'N'
  a <- 'A'
  b <- 'B'
  """

  test "capture1":
    let grammar = newGrammar(variantsSpec)
    let spec = extractorPeg(grammar, "p", @[], @["c"], "n")
    if "[N]" =~ spec:
      check(matches[0] == "N")

  test "capture2":
    let grammar = newGrammar(variantsSpec)
    let spec = extractorPeg(grammar, "p", @[], @["c"], "n")
    if "[N(B)N]" =~ spec:
      check(matches[0] == "N(B)N")


  test "capture3":
    let grammar = newGrammar(mockerConfigSpec)
    let spec = pegString(grammar, "Response", @[], @["Status", "Empty", "BodySpecification"], "respHighLevel")
    enableLogging()
    debug(spec)

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
