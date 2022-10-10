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


# enableLogging()

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
    check "name <- pattern".match(named_pattern_peg)

  test "found parts":
    if " name <- pattern " =~ named_pattern_peg:
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
  test "name only":
    let pred = $(read_peg_line("name <- value "))
    check pred == "name ''<> <- value "

  test "name variant":
    let pred = $(read_peg_line("name:var <- value "))
    check pred == "name 'var'<> <- value "

  test "name variant params":
    let pred = $(read_peg_line("name:var<a,b> <- value "))
    check pred == "name 'var'<a, b> <- value "

  test "name variant spaced params":
    let pred = $(read_peg_line("name:var< a, b > <- value "))
    check pred == "name 'var'<a, b> <- value "

  test "name spaced params":
    let pred = $(read_peg_line("name< a, b > <- value "))
    check pred == "name ''<a, b> <- value "

  test "name one params":
    let pred = $(read_peg_line("name< a > <- value "))
    check pred == "name ''<a> <- value "


suite "capture":
  test "mark4capture 1":
    check mark4capture("Jij Bent Gek", @["Bent"]) == "Jij {Bent} Gek"

  test "mark4capture 2":
    check mark4capture("Jij Bent Gek", @["Bent", "Gek"]) == "Jij {Bent} {Gek}"

  test "mark4capture 3":
    check mark4capture("Jij Bent Gek", @["Bent", "Gek", "Jij"]) == "{Jij} {Bent} {Gek}"

  test "mark4capture 4":
    check mark4capture("Jij, Bent:Gek", @["Bent", "Gek", "Jij"]) == "{Jij}, {Bent}:{Gek}"

  test "mark4capture 5":
    check mark4capture("(Jij), [Bent]:Gek!", @["Bent", "Gek", "Jij"]) == "({Jij}), [{Bent}]:{Gek}!"

  test "mark4capture 6":
    check mark4capture("(Jij) ", @["Jij"]) == "({Jij}) "

  test "mark4capture 7":
    check mark4capture("Jij) ", @["Jij"]) == "{Jij}) "

  test "mark4capture 8":
    check mark4capture("(Jij ) ", @["Jij"]) == "({Jij} ) "

  test "mark4capture 9":
    check mark4capture("Jij Bent Gek 'en Bento ook'", @["Bent"]) == "Jij {Bent} Gek 'en Bento ook'"

  test "quotedstrings 1":
    if "Some 'quoted' value 'again'." =~ quoted_string_pat:
      debug ("[$#]" % [foldMatches(matches)])

  test "quotedstrings 2":
    if "'quoted' value 'again'." =~ quoted_string_pat:
      debug ("[$#]" % [foldMatches(matches)])

  test "repnonterm 1":
    check replace_nonterminal("Ik ben 'ben' aan het zeggen", "ben") == "Ik {ben} 'ben' aan het zeggen"

  test "repnonterm 2":
    check replace_nonterminal("Ik ben 'ik ben' aan het zeggen", "ben") == "Ik {ben} 'ik ben' aan het zeggen"

  test "repnonterm 3":
    check replace_nonterminal("('left' Space)? ('outer' / 'inner') Space 'join' Space", "Space") == "('left' {Space})? ('outer' / 'inner') {Space} 'join' {Space}"


suite "Grammars":
  test "create grammar":
    let grammar = newGrammar()
    let p1 = newPegPredicate("Passed", "'true'")
    let p2 = newPegPredicate("Failed", "'false'")
    grammar.add(newPegPredicate("Test", "Passed / Failed"))
    grammar.add(p1)
    grammar.add(p2)
    check grammar.get("Passed") == p1


  test "serialize grammar":
    let grammar = newGrammar()
    let p1 = newPegPredicate("Passed", "'true'")
    let p2 = newPegPredicate("Failed", "'false'")
    grammar.add(newPegPredicate("Test", "Passed / Failed"))
    grammar.add(p1)
    grammar.add(p2)
    check grammar.get("Passed") == p1


  test "read grammar":
    let grammar = newGrammar(peg_pegstring)
    # dump_grammar(base)
    echo grammar.pegString("Primary")


suite "Matchers":

  test "create a peg matcher":
    let grammar = newGrammar(adres_spec)
    let adr_matcher = grammar.matcher("Adres")
    let straat_matcher = grammar.matcher("StraatHuis")
    check "Caretaweg 980" =~ straat_matcher
    check "Zjoekowlaan 93, Delft" =~ adr_matcher


suite "Extractors":
  test "extraction one item":
    let grammar = newGrammar(adres_spec)
    let wp_extractor = grammar.newPatternExtractor("Adres", @["Woonplaats"])
    let extracted = wp_extractor.extract("Zjoekowlaan 93, Delft")
    check extracted.len == 1
    check extracted["Woonplaats"] == "Delft"


  test "extraction two items":
    let wp_extractor = newGrammar(adres_spec).newPatternExtractor("Adres", @["Straat", "Woonplaats"])
    let extracted = wp_extractor.extract("Zjoekowlaan 93, Delft")
    check extracted.len == 2
    check extracted["Straat"] == "Zjoekowlaan"
    check extracted["Woonplaats"] == "Delft"


  test "extraction select, from, where":
    let grammar = newGrammar(select_statement_peg)
    let wp_extractor = grammar.newPatternExtractor("SelectStatement",
        @["SelectContent", "FromContent", "WhereContent", "OrderContent"])
    let extracted = wp_extractor.extract("select myselect from myfrom where mywhere ")
    check extracted.len == 4
    check extracted["SelectContent"] == "myselect "
    check extracted["FromContent"] == "myfrom "
    check extracted["WhereContent"] == "mywhere "
    check extracted["OrderContent"].isEmpty


  test "extraction select, from, where, order":
    let grammar = newGrammar(select_statement_peg)
    let wp_extractor = grammar.newPatternExtractor("SelectStatement",
        @["SelectContent", "FromContent", "WhereContent", "OrderContent"])
    let extracted = wp_extractor.extract("select myselect from myfrom where mywhere order by myorder ")
    check extracted.len == 4
    check extracted["SelectContent"] == "myselect "
    check extracted["FromContent"] == "myfrom "
    check extracted["WhereContent"] == "mywhere "
    check extracted["OrderContent"] == "myorder "


  test "extraction from with a table ref":
    let sql = "select myselect from tab t1 where blah "
    let grammar = newGrammar(select_statement_peg)
    let wp_extractor = grammar.newPatternExtractor("SelectStatement",
        @["SelectContent", "FromContent", "WhereContent"])
    let extracted = wp_extractor.extract(sql)
    check extracted.len == 3
    check extracted["FromContent"] == "tab t1 "


  test "extraction from with a table join":
    let sql = "select myselect from tab t1 inner join tab2 t2 on t1.x = t2.y  where blah "
    let grammar = newGrammar(select_statement_peg)
    let top_extractor = grammar.newPatternExtractor("SelectStatement",
        @["SelectContent", "FromContent", "WhereContent"])
    let extracted = top_extractor.extract(sql)
    check extracted.len == 3
    check extracted["FromContent"] == "tab t1 inner join tab2 t2 on t1.x = t2.y  "


  test "extraction from with a table join":
    let sql = "tab t1 inner join tab2 t2 on t1.x = t2.y "
    let grammar = newGrammar(select_statement_peg)
    let top_extractor = grammar.newPatternExtractor("FromList",
        @["FromFirstItem", "FromTail"])
    let extracted = top_extractor.extract(sql)
    check extracted.len == 2
    check extracted["FromFirstItem"] == "tab t1 "
    check extracted["FromTail"] == "inner join tab2 t2 on t1.x = t2.y "

