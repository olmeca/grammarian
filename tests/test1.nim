# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest, pegs, tables, strutils, logging, streams, sequtils, sugar
import grammarian, grammarian/patterns

let peg_pegstring = """
  Pattern <- Alternative ('/' Sp  Alternative)*
  Alternative <- SequenceItem+
  SequenceItem <- SuccessorPrefix? Sp Suffix
  SuccessorPrefix <- [!&]
  Suffix <- Primary CardinalityIndicator? Sp
  CardinalityIndicator <- [*+?]
  Primary <- '(' Sp Pattern ')' Sp / '.' Sp / Literal / Charclass / NonTerminal !'<-'
  Literal <- ['] (!['] .)* ['] Sp
  Charclass <- '[' (!']' (. '-' . / .))+ ']' Sp
  NonTerminal <- Word Sp
  Word <- [a-zA-Z]+
  Sp <- ' '*
"""
let adres_spec = """
Adres <- StraatHuis ',' ' '+ Woonplaats
StraatHuis <- Straat ' '+ Huisnr
Straat <- Naam
Woonplaats <- Naam
Naam <- [a-zA-Z]+
Huisnr <- [0-9]+
"""

let select_statement_peg = """
SelectStatement <- ^ SelectClause FromClause WhereClause OrderClause !.
SelectClause <- SelectStart SelectContent
SelectContent <- (!FromStart .)+
FromClause <- FromStart FromContent
FromList <- FromFirstItem FromTail
FromFirstItem <- TableRef
FromTail <- .* !.
JoinSpec <- Join TableRef JoinCriterium Space
Join <- ('left' Space)? ('outer' / 'inner') Space 'join' Space
JoinCriterium <- 'on' Space ColumnRef '=' ColumnRef
TableRef <- Name (Space Name) Spc
FromContent <- (!FromEnd .)+
WhereClause <- (WhereStart WhereContent) / Spc
WhereContent <- (!WhereEnd .)+
OrderClause <- OrderStart OrderContent / ''
OrderContent <- .+ !.
SelectStart <- ('select' / 'SELECT') Space
FromStart <- ('from' / 'FROM') Space
FromEnd <- WhereStart
WhereStart <- ('where' / 'WHERE') Spc
WhereEnd <- OrderStart / !.
OrderStart <- ('order by' / 'ORDER BY') Spc
ColumnAlias <- ColumnRef (Space AliasSpec)? Spc
AliasSpec <- 'as' Space Name
ColumnRef <- Name '.' Name
Name <- [a-zA-Z] [a-zA-Z_0-9]*
Space <- ' '+
Spc <- ' '*
"""

proc enableLogging() =
  var logger = newConsoleLogger()
  addHandler(logger)
  setLogFilter(lvlDebug)


# enableLogging()

suite "NonTerminals":

  # test "Backslash chars":
  #   let pattern = """
  #   IssuerLine <- Sp Label ': ' Issuer Lf
  #   Label <- [A-Za-z]+
  #   Issuer <- (!Lf .)+
  #   Sp <- ' '+
  #   Lf <- \10
  #   """
  #   let source = """
  #      Issuer: Polifinario
  #   """
  #   let grammar = newGrammar(pattern)
  #   let extractor = grammar.newPatternExtractor("IssuerLine", @["Issue"])
  #   let extracted = extractor.extract(source)


  test "word pattern with space at end":
    check not "word ".match(single_word_pat)

  test "word pattern starting with space":
    check not " word ".match(single_word_pat)

  test "word pattern with two words":
    check not "word two".match(single_word_pat)

  test "word pattern with non-letter":
    check not "me.com".match(single_word_pat)

suite "namesection":
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
      debug(foldMatches(matches))
      check matches[0] == "name"
      check matches[1] == ""
      check matches[2] == "a, b"

suite "lineparsing":
#   test "name only":
#     let rule: Rule = read_peg_line("name <- value ")
#     check rule.name == "name"
#     check rule.pattern.contains("value")

  test "name variant":
    let rule: Rule = read_peg_line("name:var <- value ")
    debug("name variant: rule: [$#]" % $(rule))

  test "name variant params":
    let pred = read_peg_line("name:var<a,b> <- value ")
    check pred.name == "name"
    check pred.parameters.sequal(["a", "b"])
    check pred.variant == "var"

  test "name variant spaced params":
    let pred = read_peg_line("name:var< a, b > <- value ")
    check pred.name == "name"
    check pred.parameters.sequal(["a", "b"])
    check pred.variant == "var"

  test "name spaced params":
    let pred = read_peg_line("name< a, b > <- value ")
    check pred.name == "name"
    check pred.parameters.sequal(["a", "b"])

  test "name one params":
    let pred = read_peg_line("name< a > <- value ")
    check pred.name == "name"
    check pred.parameters.sequal(["a"])

suite "utils":

  test "one elem array of string":
    let params = @["een"]
    check sequal(params, params)

  test "two elem array of string":
    let params = @["een", "twee"]
    check sequal(params, params)

  test "one elem array of RuleRef with no params":
    let params1 = @["name"]
    let params2 = @["name"]
    check sequal(params1, params2)

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


proc mark4capture(pattern: string, targets: seq[string]): string =
  let rule = newRule("Dummy", pattern)
  let ruleRes = applier(rule, @[])
  var ruleRefs: seq[RuleRef] = @[]
  var buf = newStringStream()
  let targetRefs = targets.map(t => newRuleRef(t))
  resolvePatternSpec(ruleRes, targetRefs, ruleRefs, buf)
  setPosition(buf, 0)
  result = readAll(buf).strip

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
    debug("Value: '$#'" % value)
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
    debug("Value: '$#'" % value)
    check value == "Word Space List_Word_Space / Word"

proc testPattern(pattern: Peg, source: string) =
  debug("testing '$#'" % source)
  if source =~ pattern:
    debug("Pattern: |", foldMatches(matches))
  else:
    raise newException(ValueError, "No match for '$#'" % source)

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


suite "capture":
  test "mark4capture 1":
    check mark4capture("Word Sep Number", @["Number"]) == "Word Sep {Number}"

  test "mark4capture 1":
    check mark4capture("Word (Sep / Number)", @["Number"]) == "Word (Sep / {Number} )"

  test "quotedstrings 1":
    if "Some 'quoted' value 'again'." =~ quoted_string_pat:
      debug ("[$#]" % [foldMatches(matches)])

  test "quotedstrings 2":
    if "'quoted' value 'again'." =~ quoted_string_pat:
      debug ("[$#]" % [foldMatches(matches)])


suite "grammars":
  let valuesSpec = """
  Main <- 'Value:' Sp Value
  Value <- List<Ident, Dot> / List<Number, Comma>
  PackageName <- List<Ident, Dot>
  Csv <- List<Number, Comma>
  List<Item, Sep> <- Item Sep List<Item, Sep> / Item
  Word <- [a-zA-Z]+
  Ident <- [a-z]+
  Number <- [0-9]+
  Dot <- '.'
  Space <- ' '+
  Comma <- ','
  Test <- 'test'
  Sp <- ' '*
  """

  # test "create grammar":
  #   let grammar = newGrammar()
  #   let p1 = newRule("Passed", "'true'")
  #   let p2 = newRule("Failed", "'false'")
  #   grammar.addRule(newRule("Test", "Passed / Failed"))
  #   grammar.addRule(p1)
  #   grammar.addRule(p2)
  #   check grammar.getRule("Passed") == p1


  # test "serialize grammar":
  #   let grammar = newGrammar()
  #   let p1 = newRule("Passed", "'true'")
  #   let p2 = newRule("Failed", "'false'")
  #   grammar.addRule(newRule("Test", "Passed / Failed"))
  #   grammar.addRule(p1)
  #   grammar.addRule(p2)
  #   check grammar.getRule("Passed") == p1
  #
  #
  # test "read grammar":
  #   let grammar = newGrammar(peg_pegstring)
  #   # dump_grammar(base)
  #   echo grammar.pegString("Primary")
  #
  # test "grammar with parameterized rule":
  #   let spec = """
  #   Pattern <- Csv
  #   Csv <- List<Word, Comma>
  #   List<Item, Sep> <- Item Sep List<Item, Sep> / Item
  #   Word <- [a-zA-Z]+
  #   Space <- ' '+
  #   Comma <- ','
  #   """
  #   let grammar = newGrammar(spec)
  #   debug("**** DUMPING PEG ****")
  #   echo grammar.pegString("Csv")

  # test "grammar with parameterized rule 1":
  #   let spec = """
  #   Pattern <- 'Just a' Csv Test
  #   Csv <- List<Word, Comma>
  #   List<Item, Sep> <- Item Sep List<Item, Sep> / Item
  #   Word <- [a-zA-Z]+
  #   Space <- ' '+
  #   Comma <- ','
  #   Test <- 'test'
  #   """
  #   let grammar = newGrammar(spec)
  #   debug("**** DUMPING PEG ****")
  #   echo grammar.pegString("Pattern")
  #
  # test "grammar with parameterized rule 2":
  #   let spec = """
  #   Pattern <- 'Value: ' (PackageName / Csv)
  #   PackageName <- List<Ident, Dot>
  #   Csv <- List<Number, Comma>
  #   List<Item, Sep> <- Item Sep List<Item, Sep> / Item
  #   Word <- [a-zA-Z]+
  #   Ident <- [a-z]+
  #   Number <- [0-9]+
  #   Dot <- '.'
  #   Space <- ' '+
  #   Comma <- ','
  #   Test <- 'test'
  #   """
  #   let grammar = newGrammar(spec)
  #   debug("**** DUMPING PEG ****")
  #   echo grammar.pegString("Pattern")


  test "grammar matcher simple":
    let grammar = newGrammar("Main <- 'Value: ' [0-9]+")
    let pattern = grammar.matcher("Main")
    check "Value: 12345" =~ pattern

  test "grammar matcher complex":
    let pattern = newGrammar(valuesSpec).matcher("Main")
    check "Value: 12345" =~ pattern

  test "grammar extractor complex":
    let pattern = newGrammar(valuesSpec).extractorPeg(@["Value"])
    if "Value: 12345" =~ pattern:
      debug(foldMatches(matches))

  test "grammar extractor complex":
    let pattern = newGrammar(valuesSpec).extractorPeg(@["Value"])
    if "Value: 12345,67890" =~ pattern:
      debug(foldMatches(matches))

  test "grammar extractor list of numbers":
    let extractor = newGrammar(valuesSpec).extractorPeg(@["Value"])
    if "Value: 12345,67890" =~ extractor:
      check matches[0] == "12345,67890"
    else: discard

  test "grammar extractor package name":
    let extractor = newGrammar(valuesSpec).extractorPeg(@["Value"])
    if "Value: een.twee.drie" =~ extractor:
      check matches[0] == "een.twee.drie"
    else: discard


suite "Matchers":

  test "create a peg matcher":
    let grammar = newGrammar(adres_spec)
    let adr_matcher = grammar.matcher("Adres")
    let straat_matcher = grammar.matcher("StraatHuis")
    check "Caretaweg 980" =~ straat_matcher
    check "Zjoekowlaan 93, Delft" =~ adr_matcher


suite "extract":
  test "extraction two items":
    let grammar = newGrammar(adres_spec)
    let wp_extractor = grammar.extractorPeg("Adres", @["Straat", "Woonplaats"])
    if "Zjoekowlaan 93, Delft" =~ wp_extractor:
      debug(foldMatches(matches))
      check matches[1] == "Delft"
      check matches[0] == "Zjoekowlaan"
    else: discard




suite "sql":
  test "extraction select, from, where":
    let whereGrammar = """
    WhereClause <- (WhereStart WhereContent) / Spc
    WhereContent <- (!WhereEnd .)+
    WhereStart <- ('where' / 'WHERE') Spc
    WhereEnd <- OrderStart / !.
    OrderStart <- ('order by' / 'ORDER BY') Spc
    Spc <- ' '*
    """
    let grammar = newGrammar(whereGrammar)
    let wp_extractor = grammar.extractorPeg("WhereClause", @["WhereContent"])
    if "where a = 1 and b < 2 " =~ wp_extractor:
      check matches[0] == "a = 1 and b < 2 "
    else: discard

#
#   test "extraction select, from, where, order":
#     let grammar = newGrammar(select_statement_peg)
#     let wp_extractor = grammar.newPatternExtractor("SelectStatement",
#         @["SelectContent", "FromContent", "WhereContent", "OrderContent"])
#     let extracted = wp_extractor.extract("select myselect from myfrom where mywhere order by myorder ")
#     check extracted.len == 4
#     check extracted["SelectContent"] == "myselect "
#     check extracted["FromContent"] == "myfrom "
#     check extracted["WhereContent"] == "mywhere "
#     check extracted["OrderContent"] == "myorder "
#
#
#   test "extraction select, from, where, order":
#     let grammar = newGrammar(select_statement_peg)
#     let wp_extractor = grammar.newPatternExtractor("SelectStatement",
#         @["SelectContent", "FromContent", "WhereContent", "OrderContent"])
#     let extracted = wp_extractor.extract("select myselect from myfrom where mywhere order by myorder ")
#     check extracted.len == 4
#     check extracted["SelectContent"] == "myselect "
#     check extracted["FromContent"] == "myfrom "
#     check extracted["WhereContent"] == "mywhere "
#     check extracted["OrderContent"] == "myorder "
#
#
#   test "extraction from with a table ref":
#     let sql = "select myselect from tab t1 where blah "
#     let grammar = newGrammar(select_statement_peg)
#     let wp_extractor = grammar.newPatternExtractor("SelectStatement",
#         @["SelectContent", "FromContent", "WhereContent"])
#     let extracted = wp_extractor.extract(sql)
#     check extracted.len == 3
#     check extracted["FromContent"] == "tab t1 "
#
#
#   test "extraction from with a table join":
#     let sql = "select myselect from tab t1 inner join tab2 t2 on t1.x = t2.y  where blah "
#     let grammar = newGrammar(select_statement_peg)
#     let top_extractor = grammar.newPatternExtractor("SelectStatement",
#         @["SelectContent", "FromContent", "WhereContent"])
#     let extracted = top_extractor.extract(sql)
#     check extracted.len == 3
#     check extracted["FromContent"] == "tab t1 inner join tab2 t2 on t1.x = t2.y  "
#
#
#   test "extraction from with a table join":
#     let sql = "tab t1 inner join tab2 t2 on t1.x = t2.y "
#     let grammar = newGrammar(select_statement_peg)
#     let top_extractor = grammar.newPatternExtractor("FromList",
#         @["FromFirstItem", "FromTail"])
#     let extracted = top_extractor.extract(sql)
#     check extracted.len == 2
#     check extracted["FromFirstItem"] == "tab t1 "
#     check extracted["FromTail"] == "inner join tab2 t2 on t1.x = t2.y "
#
