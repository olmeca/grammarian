import unittest, pegs, logging
import grammarian/common, grammarian/patterns

let nimPeg = peg"""
grammar <- rule* / expr

identifier <- [A-Za-z][A-Za-z0-9_]*
charsetchar <- "\\" . / [^\]]
charset <- "[" "^"? (charsetchar ("-" charsetchar)?)+ "]"
stringlit <- identifier? ("\"" ("\\" . / [^"])* "\"" /
                          "'" ("\\" . / [^'])* "'")
builtin <- "\\" identifier / [^\13\10]

comment <- '#' @ \n
ig <- (\s / comment)* # things to ignore

rule <- identifier \s* "<-" expr ig
identNoArrow <- identifier !(\s* "<-")
prefixOpr <- ig '&' / ig '!' / ig '@' / ig '{@}' / ig '@@'
literal <- ig identifier? '$' [0-9]+ / '$' / '^' /
           ig identNoArrow /
           ig charset /
           ig stringlit /
           ig builtin /
           ig '.' /
           ig '_' /
           (ig "(" expr ig ")")
postfixOpr <- ig '?' / ig '*' / ig '+'
primary <- prefixOpr* (literal postfixOpr*)

# Concatenation has higher priority than choice:
# ``a b / c`` means ``(a b) / c``

seqExpr <- primary+
expr <- seqExpr (ig "/" expr)*
"""

enableLogging()

suite "patternmatch":

  test "pattern match composite":
    check "(! ([a-z] / [A-Z]) .)" =~ compositeSeqItemPeg

  test "pattern match item non-terminal simple":
    check "One" =~ seqItemPeg

  test "pattern match item non-terminal +":
    check "One+" =~ seqItemPeg

  test "pattern match item !non-terminal":
    check "!One" =~ seqItemPeg

  test "pattern match item @non-terminal +":
    check "@One+" =~ seqItemPeg

  test "pattern match item composite":
    check "(One / Two)+" =~ compositeSeqItemPeg

  test "pattern match item @composite":
    check "@(One / Two)+" =~ compositeSeqItemPeg

  # test "pattern match item case insensitive composite":
  #   check "i(One / Two)+" =~ compositeSeqItemPeg

  test "pattern match item charset +":
    check "[a-z]+" =~ seqItemPeg

  test "pattern match case insensitive charset +":
    check "i[a-z]+" =~ seqItemPeg

  test "pattern match backref":
    check "$12" =~ seqItemPeg

  test "pattern match item & charset +":
    check "&[a-z] +" =~ seqItemPeg

  test "pattern match item & literal +":
    check "& 'test' +" =~ seqItemPeg

  test "pattern match parameterized item":
    check "List<Word, Item>" =~ seqItemPeg

  test "pattern match parameterized item with space":
    check "List<Word, Item> " =~ seqItemPeg

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
    check "Value: 12345" =~ pattern

  test "nimpeg 1":
    check "[a-zA-Z_]+" =~ nimPeg