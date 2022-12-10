import pegs

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


let rule_peg_alternatives* = peg"""
Pattern <- {Alternative} '/' Sp Pattern / {Alternative}
Alternative <- SequenceItem+
SequenceItem <- SuccessorPrefix? Sp Suffix
SuccessorPrefix <- [!&]
Suffix <- Primary CardinalityIndicator? Sp
CardinalityIndicator <- [*+?]
Primary <- Composite / '^' Sp / '.' Sp / '_' Sp / LiteralOrBackRef / BuiltIn / EscapedChar / Charclass / NonTerminal !'<-' / CapturedSearch / Search
Search <- '@'
CapturedSearch <- '{@}'
LiteralOrBackRef <- Modifier? (Literal / BackRef) Sp
Modifier <- [iyv]
BackRef <- '$' \d+
Composite <- ParOpen Sp SubPattern ParClose Sp
SubPattern <- PatternTopLevelString (Composite PatternTopLevelString)*
PatternTopLevelString <- ( ! (ParOpen / ParClose) .)*
ParOpen <- '('
ParClose <- ')'
NonTerminal <- RuleRef RuleParams
Literal <- ['] (!['] .)* ['] Sp
BuiltIn <- '\\' [a-z]+ Sp
EscapedChar <- '\\' [0-9]+ Sp
Charclass <- '[' (!']' (. '-' . / .))+ ']' Sp
RuleRef <- Word Sp
RuleParams <- '<' RuleParList '>' / '' Sp
RuleParList <- (!'>' .)+
Word <- [a-zA-Z]+
Sp <- ' '*
"""

let rule_peg_sequence* = peg"""
Alternative <- {SequenceItem}+
SequenceItem <- SuccessorPrefix? Sp Suffix
SuccessorPrefix <- [!&]
Suffix <- Primary CardinalityIndicator? Sp
CardinalityIndicator <- [*+?]
Primary <- Composite / '^' Sp / '.' Sp / '_' Sp / LiteralOrBackRef / BuiltIn / EscapedChar / Charclass / NonTerminal !'<-' / CapturedSearch / Search
Search <- '@'
CapturedSearch <- '{@}'
LiteralOrBackRef <- Modifier? (Literal / BackRef) Sp
Modifier <- [iyv]
BackRef <- '$' \d+
Composite <- ParOpen Sp SubPattern ParClose Sp
SubPattern <- PatternTopLevelString (Composite PatternTopLevelString)*
PatternTopLevelString <- ( ! (ParOpen / ParClose) .)*
ParOpen <- '('
ParClose <- ')'
NonTerminal <- RuleRef RuleParams
Literal <- ['] (!['] .)* ['] Sp
BuiltIn <- '\\' [a-z]+ Sp
EscapedChar <- '\\' [0-9]+ Sp
Charclass <- '[' (!']' (. '-' . / .))+ ']' Sp
RuleRef <- Word Sp
RuleParams <- '<' RuleParList '>' / '' Sp
RuleParList <- (!'>' .)+
Word <- [a-zA-Z]+
Sp <- ' '*
"""


let rule_peg_item* = peg"""
SequenceItem <- {SuccessorPrefix} Sp {Primary} {CardinalityIndicator} Sp
SuccessorPrefix <- [!&] / ''
CardinalityIndicator <- [*+?] / ''
Primary <- Composite / '^' Sp / '.' Sp / '_' Sp / LiteralOrBackRef / BuiltIn / EscapedChar / Charclass / NonTerminal !'<-' / CapturedSearch / Search
Search <- '@'
CapturedSearch <- '{@}'
LiteralOrBackRef <- Modifier? (Literal / BackRef) Sp
Modifier <- [iyv]
BackRef <- '$' \d+
Composite <- ParOpen Sp SubPattern ParClose Sp
SubPattern <- PatternTopLevelString (Composite PatternTopLevelString)*
PatternTopLevelString <- ( ! (ParOpen / ParClose) .)*
ParOpen <- '('
ParClose <- ')'
NonTerminal <- RuleRef RuleParams
Literal <- ['] (!['] .)* ['] Sp
BuiltIn <- '\\' [a-z]+ Sp
EscapedChar <- '\\' [0-9]+ Sp
Charclass <- '[' (!']' (. '-' . / .))+ ']' Sp
RuleRef <- Word Sp
RuleParams <- '<' RuleParList '>' / '' Sp
RuleParList <- (!'>' .)+
Word <- [a-zA-Z]+
Sp <- ' '*
"""

let rule_peg_composite* = peg"""
Composite <- {SuccessorPrefix} Sp ParOpen {SubPattern} ParClose Sp {CardinalityIndicator}
SubPattern <- PatternTopLevelString (SubComposite PatternTopLevelString)*
SubComposite <- ParOpen Sp SubSubPattern ParClose Sp
SubSubPattern <- PatternTopLevelString (SubComposite PatternTopLevelString)*
PatternTopLevelString <- ( ! (ParOpen / ParClose) .)*
SuccessorPrefix <- [!&] / ''
CardinalityIndicator <- [*+?] / ''
ParOpen <- '('
ParClose <- ')'
Sp <- ' '*
"""


# let rule_peg_composite* = peg"""
# Composite <- ParOpen Sp {SubPattern} ParClose Sp
# SubPattern <- PatternTopLevelString (Composite PatternTopLevelString)*
# PatternTopLevelString <- ( ! (ParOpen / ParClose) .)*
# ParOpen <- '('
# ParClose <- ')'
# Sp <- ' '*
# """
