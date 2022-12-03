# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest, pegs, tables, strutils, logging, streams, sequtils, sugar
import grammarian, grammarian/patterns

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


enableLogging()


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
