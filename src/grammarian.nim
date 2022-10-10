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
  NonTerminalNameError* = object of Exception
  NonTerminalCaptureError* = object of Exception

  PegPredicate* = object
    name: string
    variant: string
    parameters: seq[string]
    pattern: string

  Grammar* = ref object of RootObj
    ## This is the object that stores all the PEG lines
    ## as 'grammar rules'.
    rules: seq[PegPredicate]

  PatternExtractor* = ref object of RootObj
    ## This object wraps a Peg that can be used to
    ## extract parts of a matched text. It also knows
    ## the nonterminals in the Peg to be marked for extraction.
    ## This is used when matching en extracting parts:
    ## the matches are returned as a table using these
    ## names as keys to the matched values.
    extractorPattern: Peg
    mainPatternName: string
    targetNonterminals: seq[string]

  PegPredicatePatternError* = object of ValueError
    ## This error is raised when a Grammar object attempts
    ## to read a PEG string that does not conform to the
    ## syntax of a PEG expression.
  NoSuchPredicateError* = object of ValueError
    ## This error may be raised when the Grammar object
    ## processes a request for a grammar rule (PEG line)
    ## with an unrecognized name.
  NoMatchError* = object of ValueError
  MultipleMatchingPredicatesError* = object of ValueError

# pattern template for finding a given word in a string
let word_pat_tpl = """
Pattern <- ^ WordSection Pattern / WordSection
WordSection <- NonWord* Word WordEnd
NonWord <- (![a-zA-Z] .)*
Word <- {Target} / \w+
Target <- '$#'
WordEnd <- \w*
"""

let quoted_string_pat* = peg"""
QuotedString <- {UpToQuote} ({Quote} {UpToQuote} {Quote} {UpToEnd})?
UpToQuote <- (!Quote .)*
UpToEnd <- .* (!.)
Quote <- \39
"""

let single_word_pat* = peg"^ \w+ !."

let pattern_peg = peg"""
Pattern <- Sp ('^' ' '+)? Alternative ('/' Sp  Alternative)*
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

let nonterminal_replacement_peg_template = """
WordSection <- {Preamble} {Word} !Letter
Preamble <- (!Letter .)*
Word <- '$#'
Letter <- [a-zA-Z]
"""


  #"{(^ / \\W)} {'$#'} {(\\W / !.)}"


let named_pattern_peg* = peg"""
PegLine <- {NamePart} Arrow {PatternPart}
NamePart <- (!Arrow .)+
Arrow <- '<-'
PatternPart <- UpToEnd
UpToEnd <- .* (!.)
"""

let name_section_peg* = peg"""
NameSection <- Spc {RuleName} Spc OptVariant OptParams
RuleName <- Name
OptVariant <- (':' {Name}) / {Empty}
OptParams <- ('<' {UpToParEnd}) / {Empty}
UpToParEnd <- (!'>' .)+
Name <- [a-zA-Z]+
Spc <- \s*
Empty <- ''
"""

let whitespaceOrCommentLinePeg = peg"""
Pattern <- ^ Spc Comment? !.
Spc <- \s*
Comment <- '#' .*
"""

let peg_params_peg = peg"""
Params <- Sp {Param} Sep Params / Sp {Param}
Param <- [a-zA-Z]+
Sep <- ',' Sp
Sp <- ' '*
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
  debug("ntrp returning: pegstring = '$#'" % pegstring)
  peg(pegstring)


proc `$`*(pred: PegPredicate): string =
  "$# '$#'<$#> <-$#" % [pred.name, pred.variant, join(pred.parameters, ", "), pred.pattern]


proc newPegPredicate*(name: string, pattern: string, variant = "", params: seq[string] = @[]): PegPredicate =
  PegPredicate(name: name, variant: variant, parameters: params, pattern: pattern)

proc read_peg_params(source: string): seq[string] =
  debug("read_peg_params: '$#'" % source)
  if len(source) > 0:
    if source =~ peg_params_peg:
      debug(foldMatches(matches))
      result = matches.filter(notEmpty)
    else:
      raise newException(PegPredicatePatternError, "Invalid PEG parameter specification: <$#>" % source)
  else:
    result = @[]

proc read_peg_parts(namepart: string, patternpart: string): PegPredicate =
  if namepart =~ name_section_peg:
    debug(foldMatches(matches))
    let name = matches[0]
    let variant = matches[1]
    let params = if matches[2].notEmpty: read_peg_params(matches[2]) else: @[]
    result = newPegPredicate(name, patternpart, variant, params)
  else:
    raise newException(PegPredicatePatternError, "Invalid LHS: '$#'" % namepart);

proc read_peg_line*(line: string): PegPredicate =
  if line =~ named_pattern_peg:
    read_peg_parts(matches[0], matches[1])
  else:
    raise newException(PegPredicatePatternError, "Invalid PEG line: '$#'" % line);


proc isNotCommentOrEmptyLine(line: string): bool =
  result = not (line =~ whitespaceOrCommentLinePeg)


proc subpatterns*(pattern: string): seq[string] =
  debug("subpatterns: pattern: '$#'" % pattern)
  if pattern =~ pattern_peg:
    result = matches.filter(notEmpty)
    debug("subpatterns matches: $#" % foldMatches(matches))
  else:
    raise newException(PegPredicatePatternError, pattern)


proc subpatterns*(pred: PegPredicate): seq[string] =
  subpatterns(pred.pattern)


proc newGrammar*(): Grammar =
  Grammar(rules: @[])

proc hasValues(r: PegPredicate, name: string, variant: string, params: seq[string]): bool =
  r.name == name and r.variant == variant and len(r.parameters) == len params

proc get*(grammar: Grammar, name: string, variant: string = "", params: seq[string] = @[]): PegPredicate =
  var found = grammar.rules.filter(proc (r: PegPredicate): bool = r.hasValues(name, variant, params))
  if len(found) == 0 and variant != "":
    found = grammar.rules.filter(proc (r: PegPredicate): bool = r.hasValues(name, "", params))
  else: discard
  if len(found) == 1:
    result = found[0]
  elif len(found) == 0:
    raise newException(NoSuchPredicateError,
      "No predicate found matching '$#:$#' with $# parameters." % [name, variant, intToStr(len(params))])
  else:
    raise newException(MultipleMatchingPredicatesError,
    "More than one predicate found matching '$#:$#' with $# parameters." % [name, variant, intToStr(len(params))])


proc add*(grammar: Grammar, pred: PegPredicate) =
  # result = not base.rules.hasKey(predicate.name)
  grammar.rules.add(pred)


proc boolStr(value: bool): string =
  if value: "true" else: "false"


proc readPeg*(grammar: Grammar, peg_spec: string, variant: string = "") =
  for line in peg_spec.splitLines().filter(isNotCommentOrEmptyLine):
    let predicate = read_peg_line(line)
    if predicate.variant == "" or predicate.variant == variant:
      grammar.add(predicate)
    else: discard


proc newGrammar*(grammar_spec: string): Grammar =
  result = newGrammar()
  result.readPeg(grammar_spec)


proc append(buffer: Stream, predline: string) =
  write(buffer, "\n$#" % predline)



proc contains_word(source: string, target: string): bool =
  source.match(peg(word_pat_tpl % target))


proc replace_nonterminals(buf: StringStream, source: string, name: string) =
  if name.match(single_word_pat):
    if source =~ quoted_string_pat:
      buf.write(replacef(matches[0], nonterminal_replacement_peg(name), "$1{$2}"))
      # If a quoted string was found
      if matches[1] == "'":
        # Write opening quote
        buf.write(matches[1])
        buf.write(matches[2])
        # Write closing quote
        buf.write(matches[3])
        # If residual string exists then recurse
        if notEmpty(matches[4]):
          replace_nonterminals(buf, matches[4], name)
    else:
      buf.write(replacef(source, nonterminal_replacement_peg(name), "$1{$2}"))
  else:
    raise newException(NonTerminalNameError, name)

proc replace_nonterminal*(source: string, nonterm_name: string): string =
  var buf = newStringStream()
  replace_nonterminals(buf, source, nonterm_name)
  result = buf.data


proc mark4capture*(pattern: string, nonterminals: seq[string]): string =
  result = pattern
  for nonterm in nonterminals:
    debug("mark4capture: marking '$#' in '$#'" % [nonterm, result])
    result = replace_nonterminal(result, nonterm)
    #result = replacef(result, nonterminal_replacement_peg(nonterm), "$1{$2}")
  debug("mark4capture returning '$#'" % result)


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


proc writePredicate(buffer: Stream, grammar:Grammar, predName: string, doneItems: var HashSet, extractables: seq[string], variant: string) =
  if not doneItems.contains(predName):
    let pred = get(grammar, predName, variant)
    let subpats = subpatterns(pred)
    let newpattern = mark4capture(pred.pattern, extractables)
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
  debug ("pegString ->\n" & result)


proc matcher*(grammar: Grammar, patternName: string = "Main"): Peg =
  peg(pegString(grammar, patternName))


proc extractorPeg(grammar: Grammar, patternName: string, parts: seq[string], variant: string): Peg =
  peg(pegString(grammar, patternName, parts, variant))


proc newPatternExtractor*(grammar: Grammar, mainPattern: string, targets: seq[string], variant: string = ""): PatternExtractor =
  let extractorPeg = grammar.extractorPeg(mainPattern, targets, variant)
  PatternExtractor(extractorPattern: extractorPeg, mainPatternName: mainPattern, targetNonterminals: targets)


proc extract*(extractor: PatternExtractor, source: string): TableRef[string,string] =
  ## Attempts to match the given string (source). If successful, then
  ## the subparts are extracted and returned in a table, Every entry has the
  ## grammar rule (PEG line) name as its key and the matched string as its value.
  result = newTable[string, string]()
  if source =~ extractor.extractorPattern:
    debug(foldMatches(matches))
    for i in 0..extractor.targetNonterminals.len-1:
      result[extractor.targetNonterminals[i]] = matches[i]
  else:
    raise newException(NoMatchError, "Pattern '$#' does not match string '$#'" %
                      [extractor.mainPatternName, source])