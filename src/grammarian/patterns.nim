import pegs

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

let firstRuleAndRestPeg* = peg"""
grammar <- ^ ig {ruleheader} arrow {rulecontent} {rest} !.
rest <- .*
ruleheader <- spc identifier variant? args?
variant <- ':' identifier
arrow <- spc "<-" ig
# Currently, string literal containing any angle bracket ('<' or '>') not accounted for
args <- abopen spc abcontent abclose
abcontent <- abrange (subargs abrange)*
abrange <- (!abrangeend .)*
subargs <- abopen spc abcontent abclose
abrangeend <- abopen / abclose
identifier <- [A-Za-z][A-Za-z0-9_]*
spc <- (\s)* # things to ignore
abopen <- spc '<' !'-'
abclose <- spc '>'
rulecontent <- nonliteral (literal nonliteral)*
nonliteral <- (! nonliteralend .)*
nonliteralend <- (ruleheader arrow / squote / dquote) # todo: handle '\"' and "\'"
literal <- (dquote ("\\" . / [^"])* dquote / squote ("\\" . / [^'])* squote)
ig <- (\s / comment)* # things to ignore
squote <- "'"
dquote <- '"'
comment <- '#' @ eol
end <- !.
eol <- \n / end
"""

let firstSeqItemAndRestPeg* = peg"""
seqExpr <- {primary} ig {rest} end
rest <- .*
primary <- prefixOpr* literal postfixOpr*
prefixOpr <- ig '&' / ig '!' / ig '@' / ig '{@}' / ig '@@'
postfixOpr <- ig '?' / ig '*' / ig '+'
literal <- ig identifier? '$' [0-9]+ / '$' / '^' /
           ig "(" parcontent ig ")" /
           ig ruleref /
           ig charset /
           ig stringlit /
           ig builtin /
           ig '.' /
           ig '_'
ruleref <- identifier args? !(\s* "<-")
# Currently no nesting of params supported
args <- '<' @ '>'
identifier <- [A-Za-z][A-Za-z0-9_]*
charset <- "[" "^"? (charsetchar ("-" charsetchar)?)+ "]"
charsetchar <- "\\" . / [^\]]
stringlit <- identifier? ("\"" ("\\" . / [^"])* "\"" /
                          "'" ("\\" . / [^'])* "'")
builtin <- "\\" identifier / [^\13\10]
parcontent <- subrange (sub subrange)*
subrange <- (!subrangeend .)*
subrangeend <- ig (paropen / parclose / '"' / "'")
sub <- ig (stringlit / parenthesized)
parenthesized <- paropen parcontent parclose
ig <- (\s / comment)* # things to ignore
comment <- '#' @ eol
eol <- \n / end
paropen <- '('
parclose <- ')'
end <- !.
"""

let seqItemPeg* = peg"""
primary <- {prefixOpr} ig {literal} ig {postfixOpr} ig
prefixOpr <- ig '&' / ig '!' / ig '@' / ig '{@}' / ig '@@' / ''
postfixOpr <- '?' / ig '*' / ig '+' / ''
literal <-  identifier? '$' [0-9]+ / '$' / '^' /
            ruleref /
            charset /
            stringlit /
            builtin /
            '.' /
            '_'
ruleref <- identifier args? !(\s* "<-")
# Currently, string literal containing any angle bracket ('<' or '>') not accounted for
args <- ig abopen abcontent abclose
abcontent <- abrange (args abrange)*
abrange <- (!abrangeend .)*
abrangeend <- abopen / abclose
identifier <- [A-Za-z][A-Za-z0-9_]*
charset <- "[" "^"? (charsetchar ("-" charsetchar)?)+ "]"
charsetchar <- "\\" . / [^\]]
stringlit <- identifier? ("\"" ("\\" . / [^"])* "\"" /
                          "'" ("\\" . / [^'])* "'")
builtin <- "\\" identifier / [^\13\10]
ig <- (\s / comment)* # things to ignore
comment <- '#' @ eol
abopen <- '<'
abclose <- '>'
eol <- \n / end
end <- !.
"""

let ruleRefPeg* = peg"""
ruleref <- spc {identifier} args !(spc "<-")
# Currently, string literal containing any angle bracket ('<' or '>') not accounted for
args <- abopen spc {abcontent} abclose / {''}
abcontent <- abrange (subargs abrange)*
abrange <- (!abrangeend .)*
subargs <- abopen spc abcontent abclose
abrangeend <- abopen / abclose
identifier <- [A-Za-z][A-Za-z0-9_]*
spc <- (\s)* # things to ignore
abopen <- spc '<'
abclose <- spc '>'
"""

let ruleHeaderPeg* = peg"""
ruleheader <- spc {rulename} flavor args
rulename <- identifier
flavor <- ':' {flavorname} / {''}
flavorname <- identifier
# Currently, string literal containing any angle bracket ('<' or '>') not accounted for
args <- abopen spc {abcontent} abclose / {''}
abcontent <- abrange (subargs abrange)*
abrange <- (!abrangeend .)*
subargs <- abopen spc abcontent abclose
abrangeend <- abopen / abclose
identifier <- [A-Za-z][A-Za-z0-9_]*
spc <- (\s)* # things to ignore
abopen <- spc '<'
abclose <- spc '>'
"""

let compositeSeqItemPeg* = peg"""
primary <- {prefixOpr} ig paropen {topparcontent} parclose ig {postfixOpr}
prefixOpr <- ig '&' / ig '!' / ig '@' / ig '{@}' / ig '@@' / ''
postfixOpr <- ig '?' / ig '*' / ig '+' / ''
stringlit <- ("\"" ("\\" . / [^"])* "\"" /
                          "'" ("\\" . / [^'])* "'")
topparcontent <- parcontent
parcontent <- subrange (sub subrange)*
subrange <- (!subrangeend .)*
subrangeend <- ig (paropen / parclose / '"' / "'")
sub <- ig (stringlit / parenthesized)
parenthesized <- paropen parcontent parclose
ig <- (\s / comment)* # things to ignore
comment <- '#' @ eol
eol <- \n / end
paropen <- '('
parclose <- ')'
end <- !.
"""

let firstAltAndRestPeg* = peg"""
pattern <- ^ ig {toplevelalt} ig optrest end
optrest <- (altsep ig)? {rest}
rest <- @ end
toplevelalt <- alttoprange (ig sub alttoprange)*
alttoprange <- (!alttoprangeend .)*
alttoprangeend <- ig (squote / dquote / paropen / abopen / altsep / end)
sub <- squoted / dquoted / parenthesized / anglebracketed
squoted <- ig squote @ squote
dquoted <- ig dquote @ dquote
parenthesized <- ig paropen parcontent ig parclose
parcontent <- partoprange (sub partoprange)*
partoprange <- (!partoprangeend .)*
partoprangeend <- ig (squote / dquote / abopen / paropen / parclose / end)
altsep <- '/'
ig <- (\s / comment)* # things to ignore
comment <- '#' @ eol
anglebracketed <- abopen abcontent ig abclose
abcontent <- abtoprange (sub abtoprange)*
abtoprange <- (!abtoprangeend .)*
abtoprangeend <- ig (squote / dquote / abopen / paropen / abclose / end)
abopen <- '<'
abclose <- '>'
#identifier <- [A-Za-z][A-Za-z0-9_]*
paropen <- '('
parclose <- ')'
squote <- "'"
dquote <- '"'
#empty <- ''
eol <- \n / end
end <- !.
#spc <- ' '*
"""


let firstRuleRefArgAndRestPeg* = peg"""
pattern <- ^ ig {toplevelexpr} ig optrest end
optrest <- (argsep ig)? {rest}
rest <- @ end
toplevelexpr <- alttoprange (ig sub alttoprange)*
alttoprange <- (!alttoprangeend .)*
alttoprangeend <- ig (squote / dquote/ paropen / altsep / argsep / end)
sub <- squoted / dquoted / parenthesized / altsep
squoted <- ig squote @ squote
dquoted <- ig dquote @ dquote
parenthesized <- ig paropen parcontent parclose
parcontent <- subrange (sub subrange)*
subrange <- (!subrangeend .)*
subrangeend <- ig (paropen / altsep / parclose)
altsep <- '/'
ig <- (\s / comment)* # things to ignore
comment <- '#' @ eol
argsep <- ','
paropen <- '('
parclose <- ')'
squote <- "'"
dquote <- '"'
#empty <- ''
eol <- \n / end
end <- !.
"""

let paramListPeg* = peg"""
ParamList <- Sp {identifier} Sep ParamList / Sp {identifier}
identifier <- [A-Za-z][A-Za-z0-9_]*
Sep <- ',' Sp
Sp <- ' '*
"""

let name_pattern* = peg"^ [a-z,A-Z] [a-z,A-Z0-9_]* !."

let rule_params_peg* = peg"""
Params <- {Pattern} ',' Sp Params / {Pattern}
Pattern <- Alternative '/' Sp Pattern / Alternative
Alternative <- SequenceItem+
SequenceItem <- SuccessorPrefix? Sp Suffix
SuccessorPrefix <- [!&]
Suffix <- Primary CardinalityIndicator? Sp
CardinalityIndicator <- [*+?]
Primary <- Composite / '^' Sp / '.' Sp / '_' Sp / LiteralOrBackRef / BuiltIn / EscapedChar / Charclass / NonTerminal !'<-'
LiteralOrBackRef <- Modifier? Literal Sp
Modifier <- [iyv]
Composite <- ParOpen Sp SubPattern ParClose Sp
SubPattern <- PatternTopLevelString (Composite PatternTopLevelString)*
PatternTopLevelString <- ( ! (ParOpen / ParClose) .)*
ParOpen <- '('
ParClose <- ')'
NonTerminal <- Word Sp
Literal <- ['] (!['] .)* ['] Sp
EscapedChar <- '\\' [0-9]+ Sp
BuiltIn <- '\\' [a-z]+ Sp
Charclass <- '[' (!']' (. '-' . / .))+ ']' Sp
Word <- [a-zA-Z]+
Sp <- ' '*
"""


