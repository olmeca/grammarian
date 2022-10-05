# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest, pegs, tables, strutils, logging
import grammarian

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
SelectStatement <- ^ SelectClause FromClause WhereClause OrderClause? !.
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
OrderClause <- OrderStart OrderContent
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


enableLogging()

suite "NonTerminals":


  test "short name":
    let subs = subpatterns("B")
    check subs.contains("B")

  test "short names sequence":
    let subs = subpatterns("B  C  D")
    check subs.contains("B")
    check subs.contains("C")
    check subs.contains("D")

  test "long names sequence":
    let subs = subpatterns("Bert  Chris  Donna")
    check subs.contains("Bert")
    check subs.contains("Chris")
    check subs.contains("Donna")

  test "long names sequence":
    let subs = subpatterns("Bert  Chris  Donna")
    check subs.contains("Bert")
    check subs.contains("Chris")
    check subs.contains("Donna")

  test "long names sequence with group":
    let subs = subpatterns("(Bert  Chris)+  Donna")
    check subs.contains("Bert")
    check subs.contains("Chris")
    check subs.contains("Donna")

  test "short name alternatives":
    let subs = subpatterns("B / C / D")
    check subs.contains("B")

  test "long name alternatives":
    let subs = subpatterns("Bert / Chris / Donna")
    check subs.contains("Bert")
    check subs.contains("Chris")
    check subs.contains("Donna")

  test "long name alternatives with cardinality indicators":
    let subs = subpatterns("Bert* / Chris+ / Donna?")
    check subs.contains("Bert")
    check subs.contains("Chris")
    check subs.contains("Donna")


  test "long name alternatives with literals":
    let subs = subpatterns("Bert ? / 'a + be' / Donna")
    check subs.contains("Bert")
    check subs.contains("Donna")


  test "long name alternatives with charclass":
    let subs = subpatterns("Bert? / [A-za-z] / Donna")
    check subs.contains("Bert")
    check subs.contains("Donna")


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


  test "mark4x 1":
    check mark4x("Jij Bent Gek", @["Bent"]) == "Jij {Bent} Gek"

# suite "Grammars":
#   test "create grammar":
#     let grammar = newGrammar()
#     let p1 = (name: "Passed", pattern:"'true'")
#     let p2 = (name: "Failed", pattern:"'false'")
#     grammar.add((name: "Test", pattern:"Passed / Failed"))
#     grammar.add(p1)
#     grammar.add(p2)
#     check grammar.get("Passed") == p1
#
#
#   test "serialize grammar":
#     let grammar = newGrammar()
#     let p1 = (name: "Passed", pattern:"'true'")
#     let p2 = (name: "Failed", pattern:"'false'")
#     grammar.add((name: "Test", pattern:"Passed / Failed"))
#     grammar.add(p1)
#     grammar.add(p2)
#     check grammar.get("Passed") == p1
#
#   test "parse_predicate":
#     check read_peg_line("Anna  <- pweflyn2-3n*7&rv lweg", "") == (name: "Anna", pattern: "pweflyn2-3n*7&rv lweg")
#
#
#   test "read grammar":
#     let grammar = newGrammar(peg_pegstring)
#     # dump_grammar(base)
#     echo grammar.pegString("Primary")
#
#
# suite "Matchers":
#
#   test "create a peg matcher":
#     let grammar = newGrammar(adres_spec)
#     let adr_matcher = grammar.matcher("Adres")
#     let straat_matcher = grammar.matcher("StraatHuis")
#     check "Caretaweg 980" =~ straat_matcher
#     check "Zjoekowlaan 93, Delft" =~ adr_matcher


# suite "Extractors":
#   test "extraction one item":
#     let grammar = newGrammar(adres_spec)
#     let wp_extractor = grammar.newPatternExtractor("Adres", @["Woonplaats"])
#     let extracted = wp_extractor.extract("Zjoekowlaan 93, Delft")
#     check extracted.len == 1
#     check extracted["Woonplaats"] == "Delft"
#
#
#   test "extraction two items":
#     let wp_extractor = newGrammar(adres_spec).newPatternExtractor("Adres", @["Straat", "Woonplaats"])
#     let extracted = wp_extractor.extract("Zjoekowlaan 93, Delft")
#     check extracted.len == 2
#     check extracted["Straat"] == "Zjoekowlaan"
#     check extracted["Woonplaats"] == "Delft"


  # test "extraction select, from, where":
  #   let grammar = newGrammar(select_statement_peg)
  #   let wp_extractor = grammar.newPatternExtractor("SelectStatement",
  #       @["SelectContent", "FromContent", "WhereContent", "OrderContent"])
  #   let extracted = wp_extractor.extract("select myselect from myfrom where mywhere ")
  #   check extracted.len == 4
  #   check extracted["SelectContent"] == "myselect "
  #   check extracted["FromContent"] == "myfrom "
  #   check extracted["WhereContent"] == "mywhere "
  #   check extracted["OrderContent"].isEmpty


  # test "extraction select, from, where, order":
  #   let grammar = newGrammar(select_statement_peg)
  #   let wp_extractor = grammar.newPatternExtractor("SelectStatement",
  #       @["SelectContent", "FromContent", "WhereContent", "OrderContent"])
  #   let extracted = wp_extractor.extract("select myselect from myfrom where mywhere order by myorder ")
  #   check extracted.len == 4
  #   check extracted["SelectContent"] == "myselect "
  #   check extracted["FromContent"] == "myfrom "
  #   check extracted["WhereContent"] == "mywhere "
  #   check extracted["OrderContent"] == "myorder "


  # test "extraction from with a table ref":
  #   let sql = "select myselect from tab t1 where blah "
  #   let grammar = newGrammar(select_statement_peg)
  #   let wp_extractor = grammar.newPatternExtractor("SelectStatement",
  #       @["SelectContent", "FromContent", "WhereContent"])
  #   let extracted = wp_extractor.extract(sql)
  #   check extracted.len == 3
  #   check extracted["FromContent"] == "tab t1 "


  # test "extraction from with a table join":
  #   let sql = "select myselect from tab t1 inner join tab2 t2 on t1.x = t2.y  where blah "
  #   let grammar = newGrammar(select_statement_peg)
  #   let top_extractor = grammar.newPatternExtractor("SelectStatement",
  #       @["SelectContent", "FromContent", "WhereContent"])
  #   let extracted = top_extractor.extract(sql)
  #   check extracted.len == 3
  #   check extracted["FromContent"] == "tab t1 inner join tab2 t2 on t1.x = t2.y  "


  # test "extraction from with a table join":
  #   let sql = "tab t1 inner join tab2 t2 on t1.x = t2.y "
  #   let grammar = newGrammar(select_statement_peg)
  #   let top_extractor = grammar.newPatternExtractor("FromList",
  #       @["FromFirstItem", "FromTail"])
  #   let extracted = top_extractor.extract(sql)
  #   check extracted.len == 2
  #   check extracted["FromFirstItem"] == "tab t1 "
  #   check extracted["FromTail"] == "inner join tab2 t2 on t1.x = t2.y "
  #
