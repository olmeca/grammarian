import unittest, pegs

let pattern = peg"""
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
  element <- ig identifier? '$' [0-9]+ / '$' / '^' /
             ig identNoArrow /
             ig charset /
             ig stringlit /
             ig builtin /
             ig '.' /
             ig '_' /
             (ig "(" expr ig ")")
  postfixOpr <- ig '?' / ig '*' / ig '+'
  primary <- prefixOpr* (element postfixOpr*)

  # Concatenation has higher priority than choice:
  # ``a b / c`` means ``(a b) / c``

  seqExpr <- primary+
  expr <- seqExpr (ig "/" expr)*
"""

suite "Pegs":
  test "choice of literals":
    check "Test <- 'a' / 'b' 'd' / 'c'" =~ pattern

  test "choice of literals with comment":
    check "Test <- 'a' / 'b' 'd' / 'c' # Comment" =~ pattern

  test "choice of literal case insensitive":
    check "Test <- i 'a'" =~ pattern

  test "choice of literal case insensitive":
    check "Test <- '[' @ ']'" =~ pattern

