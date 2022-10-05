## Grammarian extends the utility of Nim's PEG library when handling complex text structures
## like e.g. the structure of a programming language, aimed at the use case where the
## text involved is created by hand. Human errors need to be handled in a user friendly way
## by providing more detailed feedback than 'The text does not match the pattern'. This requires
## breaking down the parsing process into smaller parts. When the parse of a part of the text
## fails the offending part can be pointed to. This means the user can look at a smaller
## haystack to find the offending needle. Though it is possible to divide the parsing process
## into small pieces using only the PEG library, I have found it challenging when dealing with
## recursive structures, like nested expressions (e.g. SQL query with subqueries). A recursive
## structure can be easily described in one PEG spec., that is not the problem addressed here.
## If a large handwritten text (with recursive structures) has an error somewhere it is desirable
## to be able to point the user to a small part of the text where it doesn't match the structure
## specification. Therefore it is desirable to break the matching of the whole text into smaller
## matches carried out on subparts. In general this can also be easily done with the PEG library,
## except when there is recursion involved. When you try to break down the PEG description of a
## recursive structure into smaller PEG specs. you will find yourself repeating the same PEG
## lines in multiple specs. This is a consequence of the recursive nature of the structure.
## Repeating yourself means redundancy and maintenance overhead. When you discover you need to
## improve a PEG line you will have to walk through all the separate PEG specs you used it in.
## Grammarian aims at minimizing or even eliminating such duplication. It does this by allowing
## you to describe your recursive structure in one meta-PEG specification, which I call a Grammar,
## and from there retrieve specific PEG matchers when needed. It also allows you to postpone marking
## the subpatterns of interest for extraction until the moment you retrieve a PEG

import pegs, tables, strutils, sequtils, streams, sets, logging

const
  cVariantKeySeparator = ":"

type
  PegPredicate* = tuple
    name: string
    pattern: string

  Grammar* = ref object of RootObj
    ## This is the object that stores all the PEG lines
    ## as 'grammar rules'.
    rules: OrderedTableRef[string, string]

  PatternExtractor* = ref object of RootObj
    ## This object wraps a Peg that can be used to
    ## extract parts of a matched text. It also knows
    ## the names of the marked subpatterns in the Peg.
    ## This is used when matching en extracting parts:
    ## the matches are returned as a table using these
    ## names as keys to the matched values.
    extractorPattern: Peg
    mainPatternName: string
    subPatternNames: seq[string]

  PegPredicatePatternError* = object of ValueError
    ## This error is raised when a Grammar object attempts
    ## to read a PEG string that does not conform to the
    ## syntax of a PEG expression.
  NoSuchPredicateError* = object of ValueError
    ## This error may be raised when the Grammar object
    ## processes a request for a grammar rule (PEG line)
    ## with an unrecognized name.
  NoMatchError* = object of ValueError


let pattern_peg = peg"""
Pattern <- ('^' ' '+)? Alternative ('/' Sp  Alternative)*
Alternative <- SequenceItem+
SequenceItem <- SuccessorPrefix? Sp Suffix
SuccessorPrefix <- [!&]
Suffix <- Primary CardinalityIndicator? Sp
CardinalityIndicator <- [*+?]
Primary <- '(' Sp Pattern ')' Sp / '.' Sp / Literal / EscapedChar / Charclass / Nonterminal !'<-'
Literal <- ['] (!['] .)* ['] Sp
EscapedChar <- '\\' [0-9]+ Sp
Charclass <- '[' (!']' (. '-' . / .))+ ']' Sp
Nonterminal <- {Word} Sp
Word <- [a-zA-Z]+
Sp <- ' '*
"""

let nonterminal_replacement_peg_template = "{(^ / \\W)} {'$#'} {(\\W / !.)}"


let named_pattern_peg = peg"""
PegLine <- Space Name '<-' Space Pattern !.
Name <- {Word (':' Word)?} Space
Pattern <- {.+}
Word <- [a-zA-Z]+
Space <- ' '*
"""

let whitespaceOrCommentLinePeg = peg"""
Pattern <- ^ Spc Comment? !.
Spc <- \s*
Comment <- '#' .*
"""


proc isEmpty*(value: string): bool =
    value == ""


proc notEmpty*(value: string): bool =
    not value.isEmpty


proc foldMatches*(source: array[0..19, string]): string =
    source.foldl(a & "|" & b)


proc nonterminal_replacement_peg(nonterminal: string): Peg =
  debug("ntrp: nt: '$#'" % nonterminal)
  let pegstring = nonterminal_replacement_peg_template % nonterminal
  debug("ntrp: pegstring = '$#'" % pegstring)
  peg(pegstring)


proc `$`(pred: PegPredicate): string =
  "$# <- $#" % [pred.name, pred.pattern]


proc pegKey(name: string, variant: string): string =
  if variant.isEmpty():
    name
  else:
    name & cVariantKeySeparator & variant

proc read_peg_line*(line: string, variant: string): PegPredicate =
  if line =~ named_pattern_peg:
    result = (name: pegKey(matches[0], variant), pattern: matches[1])
  else:
    raise newException(PegPredicatePatternError, "Invalid PEG line: '$#'" % line);


proc isNotCommentOrEmptyLine(line: string): bool =
  result = not (line =~ whitespaceOrCommentLinePeg)


proc subpatterns*(pattern: string): seq[string] =
  if pattern =~ pattern_peg:
    result = matches.filter(notEmpty)
    debug("subpatterns matches: $#" % foldMatches(matches))
  else:
    raise newException(PegPredicatePatternError, pattern)


proc subpatterns*(pred: PegPredicate): seq[string] =
  subpatterns(pred.pattern)


proc newGrammar*(): Grammar =
  Grammar(rules: newOrderedTable[string, string]())


proc get*(grammar: Grammar, key: string): PegPredicate =
  result = (name: key, pattern: grammar.rules[key])


proc add*(grammar: Grammar, pred: PegPredicate) =
  # result = not base.rules.hasKey(predicate.name)
  grammar.rules[pred.name] = pred.pattern


proc boolStr(value: bool): string =
  if value: "true" else: "false"


proc readPeg*(grammar: Grammar, peg_spec: string, variant: string = "") =
  for line in peg_spec.splitLines().filter(isNotCommentOrEmptyLine):
    let predicate = read_peg_line(line, variant)
    grammar.add(predicate)


proc newGrammar*(grammar_spec: string): Grammar =
  result = newGrammar()
  result.readPeg(grammar_spec)


proc append(buffer: Stream, predline: string) =
  write(buffer, "\n$#" % predline)


proc mark4x(nonterminal: string): string =
  "{$#}" % nonterminal

proc mark4x*(pattern: string, targets: seq[string]): string =
  result = pattern
  for target in targets:
    debug("mark4x: marking '$#' in '$#'" % [target, result])
    result = replacef(result, nonterminal_replacement_peg(target), "$1{$2}$3")
  debug("mark4x returning '$#'" % result)


# proc copy_sub(grammar:Grammar, root: string, dest: Grammar, targets: seq[string]) =
#   if not dest.rules.hasKey(root):
#     let pred = grammar.get(root)
#     let subpats = pred.subpatterns
#     let newpattern = pred.pattern.copy_marked_for_extraction(targets, subpats)
#     let predcopy = (name: pred.name, pattern: newpattern)
#     let subsstring = if len(subpats) == 0: "none" else: subpats.foldl(a & ", " & b)
#     echo "| $# >>> $#" % [$(predcopy), subsstring]
#     dest.add(predcopy)
#     for item in pred.subpatterns().items():
#       copy_sub(grammar, item, dest, targets)


proc getVariant(grammar: Grammar, predName: string, variant: string): PegPredicate =
  let variantKey = pegKey(predName, variant)
  if grammar.rules.hasKey(variantKey):
    grammar.get(variantKey)
  else:
    grammar.get(predName)


proc writePredicate(buffer: Stream, grammar:Grammar, predName: string, doneItems: var HashSet, extractables: seq[string], variant: string) =
  if not doneItems.contains(predName):
    let pred = getVariant(grammar, predName, variant)
    let subpats = subpatterns(pred)
    let newpattern = mark4x(pred.pattern, extractables)
    buffer.write("$# <- $#\n" % [predName, newpattern])
    doneItems.incl(predName)
    for item in subpats.items():
      writePredicate(buffer, grammar, item, doneItems, extractables, variant)


proc pegString*(grammar: Grammar, patternName: string, extractables: seq[string] = @[], variant: string = ""): string  =
  var doneItems = initHashSet[string]()
  var buffer: Stream = newStringStream()
  writePredicate(buffer, grammar, patternName, doneItems, extractables, variant)
  buffer.setPosition(0)
  result = buffer.readAll()
  echo "pegString ->\n" & result


proc matcher*(grammar: Grammar, patternName: string = "Main"): Peg =
  peg(pegString(grammar, patternName))


proc extractorPeg(grammar: Grammar, patternName: string, parts: seq[string], variant: string): Peg =
  peg(pegString(grammar, patternName, parts, variant))


proc newPatternExtractor*(grammar: Grammar, mainPattern: string, subpatterns: seq[string], variant: string = ""): PatternExtractor =
  let extractorPeg = grammar.extractorPeg(mainPattern, subpatterns, variant)
  PatternExtractor(extractorPattern: extractorPeg, mainPatternName: mainPattern, subPatternNames: subpatterns)


proc extract*(extractor: PatternExtractor, source: string): TableRef[string,string] =
  ## Attempts to match the given string (source). If successful, then
  ## the subparts are extracted and returned in a table, Every entry has the
  ## grammar rule (PEG line) name as its key and the matched string as its value.
  result = newTable[string, string]()
  if source =~ extractor.extractorPattern:
    echo foldMatches(matches)
    for i in 0..extractor.subPatternNames.len-1:
      result[extractor.subPatternNames[i]] = matches[i]
  else:
    raise newException(NoMatchError, "Pattern '$#' does not match string '$#'" %
                      [extractor.mainPatternName, source])