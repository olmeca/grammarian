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

import pegs, tables, strutils, sequtils, streams, sets, logging, sugar, hashes
import grammarian/patterns

const
  cVariantKeySeparator = ":"


type
  NonTerminalNameError* = object of ValueError
  NonTerminalCaptureError* = object of ValueError

  Rule* = object
    name*: string
    variant*: string
    parameters*: seq[string]
    pattern*: string

  RuleRef* = object
    name*: string
    parameters*: seq[string]
    capture: bool

  RuleRes* = object
    name*: string # derived from rule and args
    rule*: Rule
    args*: seq[string]

  Grammar* = ref object of RootObj
    ## This is the object that stores all the PEG lines
    ## as 'grammar rules'.
    rules: seq[Rule]

  PatternExtractor* = ref object of RootObj
    ## This object wraps a Peg that can be used to
    ## extract parts of a matched text. It also knows
    ## the nonterminals in the Peg to be marked for extraction.
    ## This is used when matching en extracting parts:
    ## the matches are returned as a table using these
    ## names as keys to the matched values.
    extractorPattern: Peg
    targetNonterminals: seq[string]

  RulePatternError* = object of ValueError
    ## This error is raised when a Grammar object attempts
    ## to read a PEG string that does not conform to the
    ## syntax of a PEG expression.
  NoSuchRuleError* = object of ValueError
    ## This error may be raised when the Grammar object
    ## processes a request for a grammar rule (PEG line)
    ## with an unrecognized name.
  NoMatchError* = object of ValueError
  MultipleMatchingRulesError* = object of ValueError
  ParamListError* = object of ValueError

let nonTerminalPattern = peg"""
Pattern <- ^ {NonWord} {Word} {Remains}
NonWord <- (![a-zA-Z] .)*
Word <- \w+
Remains <- .* (!.)
"""

let quoted_string_pat* = peg"""
QuotedString <- {UpToQuote} ({Quote} {UpToQuote} {Quote} {UpToEnd})?
UpToQuote <- (!Quote .)*
UpToEnd <- .* (!.)
Quote <- \39
"""

let single_word_pat* = peg"^ \w+ !."


let ruleref_parts_peg = peg"""
Pattern <- ^ Sp RuleRefSpec !.
RuleRefSpec <- NameSection ParamSection Sp
NameSection <- {Word} Sp
ParamSection <- '<' Sp {Param_List} Sp '>' / {''}
Param_List <- (!'>' .)+
Word <- [a-zA-Z] [a-zA-Z0-9_]*
Sp <- ' '*
"""

let ruleref_peg = peg"""
Pattern <- {UpToRuleRef} {RuleRefSpec} {Remaining}
UpToRuleRef <- (!Word .)*
RuleRefSpec <- Word ParamSection?
ParamSection <- '<' Sp Param_List Sp '>'
Param_List <- Sp Word Sep Param_List / Sp Word
Sep <- Sp ',' Sp
Word <- [a-zA-Z] [a-zA-Z0-9_]*
Sp <- ' '*
Remaining <- .*
"""

let nonterminal_replacement_peg_template = """
WordSection <- {Preamble} {Word} !Letter
Preamble <- (!Letter .)*
Word <- '$#'
Letter <- [a-zA-Z]
"""


  #"{(^ / \\W)} {'$#'} {(\\W / !.)}"


let named_rule_peg* = peg"""
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

let word_list_peg = peg"""
Words <- Sp {Word} Sep Words / Sp {Word}
Word <- [a-zA-Z]+
Sep <- ',' Sp
Sp <- ' '*
"""

proc resolveChoice(ruleRes: RuleRes, patSpec: string, targets: seq[RuleRef], ruleRefAcc: var seq[RuleRef], patAccBuf: StringStream)

func nameForPattern(pattern: string): string =
  "Pat$#" % intToStr(hash(pattern))

func isEmpty*[T](list: seq[T]): bool =
  count(list) == 0

func notEmpty*[T](list: seq[T]): bool =
  not isEmpty(list)

proc isEmpty*(value: string): bool =
  value == ""


proc notEmpty*(value: string): bool =
  not value.isEmpty


proc foldMatches*(source: array[0..19, string]): string =
  source.foldl(a & "|" & b)


proc captured(name: string): string =
  "{$#}" % name

proc fuse(name: string, args: seq[string] = @[]): string =
  let fusedParams = args.map(x => "_$#" % x).join("")
  result = "$#$#" % [name, fusedParams]

proc serialize*(ruleref: RuleRef, paramValues: seq[string] = ruleref.parameters): string =
  result = fuse(ruleref.name, paramValues)
  if ruleref.capture:
    result = captured(result)
  else: discard

proc `$`*(ruleParams: seq[string]): string =
  "<$#>" % join(ruleParams, ", ")

proc `$`*(ruleRef: RuleRef): string =
  let paramStr = $(ruleRef.parameters)
  "$#$#" % [ruleRef.name, paramStr]

proc `$`*(rule: Rule): string =
  let paramStr = $(rule.parameters)
  let variantStr = ":$#" % rule.variant
  "$#$#$# <- $#" % [rule.name, variantStr, paramStr, rule.pattern]

proc `$`*(ruleRes: RuleRes): string =
  "$# ($#) -> <$#>" % [ruleRes.rule.name, join(ruleRes.rule.parameters, ", "), join(ruleRes.args, ", ")]

proc newRule*(name: string, pattern: string, variant = "", params: seq[string] = @[]): Rule =
  Rule(name: name, variant: variant, parameters: params, pattern: pattern)

proc newRuleRef*(name: string, params: seq[string] = @[], mark: bool = false): RuleRef =
  RuleRef(name: name, parameters: params, capture: mark)

proc parseRuleParams(source: string): seq[string] =
  if source.isEmpty:
    result = @[]
  elif source =~ word_list_peg:
    result = matches.filter(notEmpty)
  else:
    raise newException(ParamListError, source)

proc parseRuleRef*(ruleref: string): RuleRef =
  debug("parseRuleRef: '$#'" % ruleref)
  if ruleref =~ ruleref_parts_peg:
    debug("parseRuleRef - RuleRef parts: <$#>" % foldMatches(matches))
    let ruleName = matches[0]
    let ruleParams = if matches[1] == "": @[] else: parseRuleParams(matches[1])
    result = RuleRef(name: ruleName, parameters: ruleParams)
    debug("parseRuleRef -> [$#]" % $(result))
  else:
    debug("parseRuleRef: no match for <$#>" % ruleref)
    raise newException(NonTerminalNameError, ruleref)

func sequal*[T](s1: openArray[T], s2: openArray[T]): bool =
  s1.len == s2.len and (s1.len == 0 or zip(s1, s2).all(x => x[0] == x[1]))

func eqRuleRef(r1: RuleRef, r2: RuleRef): bool =
  r1.name == r2.name and sequal(r1.parameters, r2.parameters)

func applier*(rule: Rule, args: seq[string]): RuleRes =
  RuleRes(name: fuse(rule.name, args), rule: rule, args: args)

func applier*(rule: Rule, ruleRef: RuleRef): RuleRes =
  RuleRes(name: serialize(ruleRef), rule: rule, args: ruleRef.parameters)

func refersTo(ruleRef: RuleRef, rule: Rule): bool =
  ruleRef.name == rule.name and sequal(ruleRef.parameters, rule.parameters)

func replaceParamByValue*(ruleRes: RuleRes, param: string): string =
  let parindex = find(ruleRes.rule.parameters, param)
  if parindex < 0: param else: ruleRes.args[parindex]

proc resolveRuleRef*(ruleRes: RuleRes, ruleRef: RuleRef, targets: seq[RuleRef] = @[]): RuleRef =
  debug("resolveRuleRef - [$#]" % join(targets.map(t => serialize(t)), ", "))
  var theRuleRef: RuleRef
  # if ruleRef is a recursive reference
  if ruleRef.name == ruleRes.rule.name:
    # a fully recursive call (with same rule params as args)
    if sequal(ruleRef.parameters, ruleRes.rule.parameters):
      # ruleRes.name is the already resolved name
      theRuleRef = newRuleRef(ruleRes.name)
    elif ruleRef.parameters.len == ruleRes.rule.parameters.len:
      # This is another recursive call to this rule (with args differing from rule params)
      theRuleRef = ruleRef
    else:
      debug("resolveRuleRef - param discrepancy: [$#] vs [$#]" % [ruleRef.parameters.join(","), ruleRes.rule.parameters.join(",")])
      raise newException(ValueError, "Rule selfreference with wrong nr. of parameters.")
  elif ruleRef.parameters.len == 0:
    # just replace the name if it is a rule parameter name
    theRuleRef = newRuleRef(replaceParamByValue(ruleRes, ruleRef.name))
  else:
    let resolvedParams = ruleRef.parameters.map(p => replaceParamByValue(ruleRes, p))
    theRuleRef = newRuleRef(ruleRef.name, resolvedParams)
  # if targets contains rule ref equal to this
  debug("resolveRuleRef -> [$#]" % $(theRuleRef))
  if targets.filter(r => serialize(r) == serialize(theRuleRef)).len > 0:
    theRuleRef.capture = true
  result = theRuleRef


proc resolveRuleRefSpec(ruleRes: RuleRes, patSpec: string, targets: seq[RuleRef], ruleRefAcc: var seq[RuleRef], patAccBuf: StringStream) =
  let ruleRef: RuleRef = parseRuleRef(patSpec)
  debug("resolveRuleRefSpec - parsed ruleref [$#]" % ruleRef.serialize)
  let resolvedRuleRef = resolveRuleRef(ruleRes, ruleRef, targets)
  # Only add if not already containing item with same resolved name
  if ruleRefAcc.filter(r => r.name == resolvedRuleRef.name).len == 0:
    ruleRefAcc.add(resolvedRuleRef)
  else: discard
  debug("resolveRuleRefSpec - writing: [$#]" % $(resolvedRuleRef))
  patAccBuf.write(serialize(resolvedRuleRef) & ' ')


proc resolveSequenceItem(ruleRes: RuleRes, patSpec: string, targets: seq[RuleRef], ruleRefAcc: var seq[RuleRef], patAccBuf: StringStream) =
  debug("resolveSequenceItem: <$#>" % patSpec)
  if patSpec =~ rule_peg_composite:
    # write successor indicator (& or !)
    patAccBuf.write(matches[0])
    # open parenthesis
    patAccBuf.write("(")
    resolveChoice(ruleRes, matches[1], targets, ruleRefAcc, patAccBuf)
    patAccBuf.write(") ")
    # write cardinality indicator (*, + or ?)
    patAccBuf.write(matches[2])
  elif patSpec =~ rule_peg_item:
    debug("resolveSequenceItem: ruleref parts found: <$#>" % foldMatches(matches))
    try:
      let successorPrefix = matches[0]
      let primary = matches[1]
      let cardinalityIndicator = matches[2]
      let ruleRef: RuleRef = parseRuleRef(primary)
      let resolvedRuleRef = resolveRuleRef(ruleRes, ruleRef, targets)
      # write successor indicator (& or !)
      patAccBuf.write(successorPrefix)
      patAccBuf.write(serialize(resolvedRuleRef) & ' ')
      # write cardinality indicator (*, + or ?)
      patAccBuf.write(cardinalityIndicator)
      # Only add if not recursive
      if not ruleRef.refersTo(ruleRes.rule):
        # Only add if not already containing item with same resolved name
        if ruleRefAcc.filter(r => r.eqRuleRef(resolvedRuleRef)).len == 0:
          ruleRefAcc.add(resolvedRuleRef)
        else: discard
      else: discard
    except NonTerminalNameError:
      # This item is not a non-terminal
      patAccBuf.write(patSpec)

  else:
    patAccBuf.write(patSpec)

proc resolveSequence(ruleRes: RuleRes, patSpec: string, targets: seq[RuleRef], ruleRefAcc: var seq[RuleRef], patAccBuf: StringStream) =
  debug("resolveSequence: <$#>" % patSpec)
  if patSpec =~ rule_peg_sequence:
    debug("resolveSequence: sequences found: <$#>" % foldMatches(matches))
    for s in matches.filter(notEmpty):
      resolveSequenceItem(ruleRes, s, targets, ruleRefAcc, patAccBuf)
  else:
    raise newException(ValueError, "Invalid pattern: '$#'" % patSpec)


proc resolveChoice(ruleRes: RuleRes, patSpec: string, targets: seq[RuleRef], ruleRefAcc: var seq[RuleRef], patAccBuf: StringStream) =
  debug("resolveChoice: <$#>" % patSpec)
  if patSpec =~ rule_peg_alternatives:
    debug("resolveChoice: alternatives found: <$#>" % foldMatches(matches))
    let foundMatches = matches.filter(notEmpty)
    for i, alt in foundMatches:
      if i > 0:
        patAccBuf.write("/ ")
      else: discard
      resolveSequence(ruleRes, alt, targets, ruleRefAcc, patAccBuf)
  else:
    raise newException(ValueError, "Invalid pattern: '$#'" % patSpec)


proc resolvePatternSpec*(ruleRes: RuleRes, patSpec: string, targets: seq[RuleRef], ruleRefAcc: var seq[RuleRef], patAccBuf: StringStream) =
  resolveChoice(ruleRes, patSpec, targets, ruleRefAcc, patAccBuf)


proc resolvePatternSpec*(ruleRes: RuleRes, targets: seq[RuleRef], ruleRefAcc: var seq[RuleRef], patAccBuf: StringStream) =
  resolveChoice(ruleRes, ruleRes.rule.pattern, targets, ruleRefAcc, patAccBuf)


proc nonterminal_replacement_peg(nonterminal: string): Peg =
  debug("ntrp: nt: '$#'" % nonterminal)
  let pegstring = nonterminal_replacement_peg_template % nonterminal
  debug("ntrp returning: pegstring = '$#'" % pegstring)
  peg(pegstring)


proc readWordList(source: string): seq[string] =
  debug("readWordList: '$#'" % source)
  if len(source) > 0:
    if source =~ word_list_peg:
      debug(foldMatches(matches))
      result = matches.filter(notEmpty)
    else:
      raise newException(RulePatternError, "Invalid PEG parameter specification: <$#>" % source)
  else:
    result = @[]

proc readRuleParts(namepart: string, patternpart: string): Rule =
  if namepart =~ name_section_peg:
    debug("readRuleParts: name: <$#>" % foldMatches(matches))
    let name = matches[0]
    let variant = matches[1]
    let params = if matches[2].notEmpty: readWordList(matches[2]) else: @[]
    result = newRule(name, patternpart, variant, params)
    debug("readRuleParts -> [$#]" % $(result))
  else:
    raise newException(RulePatternError, "Invalid LHS: '$#'" % namepart);

proc read_peg_line*(line: string): Rule =
  if line =~ named_rule_peg:
    readRuleParts(matches[0], matches[1])
  else:
    raise newException(RulePatternError, "Invalid PEG line: '$#'" % line);


proc isNotCommentOrEmptyLine(line: string): bool =
  result = not (line =~ whitespaceOrCommentLinePeg)


# proc collectRuleRefs(acc: var seq[RuleRef], source: string) =
#   debug("subRuleRefs: '$#'" % source)
#   if source =~ ruleref_peg:
#     debug(foldMatches(matches))
#     acc.add(parseRuleRef(matches[1]))
#     if matches[2].notEmpty:
#       collectRuleRefs(acc, matches[2])
#   else:
#     raise newException(RulePatternError, pattern)
#
# proc subRuleRefs*(pattern: string): seq[RuleRef] =
#   result = @[]
#   collectRuleRefs(result, pattern)
#
# proc subRuleRefs*(rule: Rule): seq[RuleRef] =
#   subRuleRefs(rule.pattern)
#
#
# proc subpatterns*(pattern: string): seq[string] =
#   subRuleRefs(pattern).map(`$`)
#
# proc subpatterns*(rule: Rule): seq[string] =
#   subpatterns(rule.pattern)

proc applyParams(rule: Rule, params: seq[string], captures: seq[string]): Rule =
  let ruleName = fuse(rule.name, params)


proc newGrammar*(): Grammar =
  Grammar(rules: @[])

proc hasValues(rule: Rule, name: string, variant: string, params: seq[string]): bool =
  rule.name == name and rule.variant == variant and len(rule.parameters) == len params

proc getRule*(grammar: Grammar, name: string, variant: string = "", args: seq[string] = @[]): Rule =
  debug("getRule '$#', variant: '$#', args: $#" % [name, variant, args.join(",")])
  var okRules = grammar.rules.filter(r => r.hasValues(name, variant, args))
  # If not found then try without variant
  if len(okRules) == 0 and variant != "":
    okRules = grammar.rules.filter(r => r.hasValues(name, "", args))
  else: discard
  if len(okRules) == 1:
    okRules[0]
  elif len(okRules) == 0:
    raise newException(NoSuchRuleError,
      "No rule found matching '$#:$#' with $# arguments." % [name, variant, intToStr(len(args))])
  else:
    raise newException(MultipleMatchingRulesError,
    "More than one rule found matching '$#:$#' with $# parameters." % [name, variant, intToStr(len(args))])

proc getRule*(grammar: Grammar, ruleRef: RuleRef, variant: string = ""): Rule =
  getRule(grammar, ruleRef.name, variant, ruleRef.parameters)

proc getRuleRes*(grammar: Grammar, name: string, args: seq[string], variant: string = ""): RuleRes =
  applier(getRule(grammar, name, variant, args), args)

proc getRuleRes*(grammar: Grammar, ruleRef: RuleRef, variant: string = ""): RuleRes=
  applier(getRule(grammar, ruleRef.name, variant, ruleRef.parameters), ruleRef.parameters)

proc hasRule*(grammar: Grammar, rule: Rule): bool =
  try:
    discard getRule(grammar, rule.name, rule.variant, rule.parameters)
    result = true
  except NoSuchRuleError:
    result = false


proc addRule*(grammar: Grammar, rule: Rule) =
  debug("addRule: [$#]" % $(rule))
  if not grammar.hasRule(rule):
    grammar.rules.add(rule)
  else: discard

proc boolStr(value: bool): string =
  if value: "true" else: "false"


proc readPeg*(grammar: Grammar, grammarSpec: string, variant: string = "") =
  for line in grammarSpec.splitLines().filter(isNotCommentOrEmptyLine):
    debug("readPeg '$#'" % line)
    let rule = read_peg_line(line)
    if rule.variant == "" or rule.variant == variant:
      grammar.rules.add(rule)
    else: discard


proc newGrammar*(grammarSpec: string): Grammar =
  result = newGrammar()
  readPeg(result, grammarSpec)


proc hasRuleNamed(grammar: Grammar, ruleName: string): bool =
  grammar.rules.filter(rule => rule.name == ruleName).len > 0

proc copySubGrammar(srcGrammar: Grammar, destGrammar: Grammar, ruleRes: RuleRes, captureTargets: seq[RuleRef],
                    variant: string) =
  # Only if dest has no rule with resolved name
  if not destGrammar.hasRuleNamed(ruleRes.name):
    var patSpecBuf = newStringStream()
    var subRuleRefsAcc: seq[RuleRef] = @[]
    resolvePatternSpec(ruleRes, captureTargets, subRuleRefsAcc, patSpecBuf)
    patSpecBuf.setPosition(0)
    let resolvedRule = newRule(ruleRes.name, patSpecBuf.readAll())
    debug("copySubGrammar: resolvedRule: [$#]" % $(resolvedRule))
    destGrammar.rules.add(resolvedRule)
    for ruleRef in subRuleRefsAcc:
      debug("copySubGrammar: subRuleRef: [$#]" % $(ruleRef))
      let subRuleRes = getRuleRes(srcGrammar, ruleRef, variant)
      debug("copySubGrammar: subRuleRes: [$#]" % $(subRuleRes))
      copySubGrammar(srcGrammar, destGrammar, subRuleRes, captureTargets, variant)


proc getSubGrammar(srcGrammar: Grammar, captureTargets: seq[string], ruleName: string = "Main",
                    variant: string = "", args: seq[string] = @[]): Grammar =
  result = newGrammar()
  let rootRuleRes = srcGrammar.getRuleRes(ruleName, args, variant)
  debug("getSubGrammar root: [$#]" % $(rootRuleRes))
  let targets = captureTargets.map(t => parseRuleRef(t))
  copySubGrammar(srcGrammar, result, rootRuleRes, targets, variant)



proc pegString*(grammar: Grammar, ruleName: string = "Main", extractables: seq[string] = @[],
                variant: string = "", args: seq[string] = @[]): string  =
  let subGrammar = getSubGrammar(grammar, extractables, ruleName, variant, args)
  var buffer: Stream = newStringStream()
  for rule in subGrammar.rules:
    buffer.write("$# <- $#\n" % [rule.name, rule.pattern])
  buffer.setPosition(0)
  result = buffer.readAll()
  debug ("pegString ->\n" & result)


proc matcher*(grammar: Grammar, patternName: string = "Main"): Peg =
  peg(pegString(grammar, patternName))


proc extractorPeg*(grammar: Grammar, parts: seq[string], patternName: string = "Main", variant: string = ""): Peg =
  peg(pegString(grammar, patternName, parts, variant))

proc extractorPeg*(grammar: Grammar, patternName: string, parts: seq[string], variant: string = ""): Peg =
  peg(pegString(grammar, patternName, parts, variant))


proc newPatternExtractor*(grammar: Grammar, mainPattern: string, targets: seq[string], variant: string = ""): PatternExtractor =
  debug("newPatternExtractor: main: '$#', var: '$#', targets: [$#]" % [mainPattern, variant, targets.join(",")])
  let extractorPatPeg = grammar.extractorPeg(targets, mainPattern, variant)
  PatternExtractor(extractorPattern: extractorPatPeg, targetNonterminals: targets)

