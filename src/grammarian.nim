## Grammarian extends the utility of Nim's PEG library when handling complex text structures
## like e.g. the structure of a programming language. It aims to simplify the task of extracting
## data from such text structures, by means of a number of features:
## 1- It allows you to keep all PEG rules describing one OR MORE text structures together
## in one store, called a Grammar. When you need a PEG for a substructure you can retrieve
## it from the store by rule name (rule being one PEG line, name == left of the '<-'). Taking
## the given rule as root, all other rules reachable from the root are retrieved recursively.
## The result is a consistent set of rules: a valid PEG.
## 2- Rules can have variants. Imagine targetting SQL statements. Most of the language constructs
## are equal among the mainstream DBMS's. But here and there are small syntax variations between
## SQL 'dialects'. Besides a main rule describing the most common syntax you can add variants
## to the same rule, describing the vendor-specific syntax. Example:
## General syntax rule        :  DateString <- ......
## Vendor specific syntax rule:  DateString:oracle <- .....
## When requesting a PEG from the store, you specify the root rule name and also the variant.
## Now when retrieving all the rules the system will prefer the given variant, but will
## fall back to the non-specific version of the rule. Again, the result is a consistent PEG.
## 3- When requesting a PEG from the store you can also specify a list of rule names, a.k.a.
## 'extraction targets'. All occurrences of these names as non-terminals will be marked for
## capture. Given a rule "SelectClause <- 'SELECT' \s+ Columns FromClause ....", if you
## specify ["Colums"] as targets, the resulting rule in the PEG string retrieved will read:
## "SelectClause <- 'SELECT' \s+ {Columns} FromClause ....".
## 4- Parameterized rules help eliminate duplication. Imagine a structure that is identical
## among different use cases, but in some use cases should contain numbers, but in other use
## cases should be filled with textual content. Parameterized rules enable using one rule
## defining the overall structure, but allow you to specify the contents pattern as an
## argument in your non-terminal, referring to the parameterized rule. An elementary
## example would be the textual representation of some list. Generally speaking, this is
## textual items separated by some textual delimiter. Examples: words separated by spaces,
## or numbers separated by a comma. The general rule can be described as follows:
## "List<Item, Separator> <- Item Separator List<Item, Separator> / Item"
## Another rule could then specify a comma separated list of numbers as follows:
## "InClause <- 'IN' \s* '(' List<Number, Comma> ')' \s*".
## The resulting PEG would contain among others the following four rules:
## "List_Number_Comma <- Number Comma List_Number_Comma / Number"
## "InClause <- 'IN' \s* '(' List_Number_Comma ')' \s*".
## "Number <- ... (assuming you have such a rule in your store)
## "Comma <- ...  (idem ditto)
## In the above the parameterized rule was applied using non-terminals as arguments.
## However, you can also use terminal expressions as arguments, for example:
## "InClause <- 'IN' \s* '(' List<[0-9]+, ','> ')' \s*".
## For more example please review the unit tests.

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

let name_pattern = peg"^ [a-z,A-Z] [a-z,A-Z0-9_]* !."

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
Name <-  [a-zA-Z] [a-zA-Z0-9_]*
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

proc nominate(pattern: string): string =
  # Creates an identifying name for the given pattern
  # if it is not already a valid name (non-terminal)
  if pattern =~ name_pattern:
    pattern
  else:
    "p$#" % intToStr(hash(pattern))

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

proc serialize*(ruleref: RuleRef, args: seq[string] = ruleref.parameters): string =
  result = fuse(ruleref.name, args.map(a => nominate(a)))
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
  elif source =~ rule_params_peg:
    result = matches.filter(notEmpty)
  else:
    raise newException(ParamListError, source)

proc isNonTerminal(ruleRef: RuleRef): bool =
  ruleRef.name.match(name_pattern)

proc parseRuleRef*(ruleref: string): RuleRef =
  debug("parseRuleRef: '$#'" % ruleref)
  if ruleref =~ ruleref_parts_peg:
    debug("parseRuleRef - RuleRef parts found: <$#>" % foldMatches(matches))
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
  RuleRes(name: fuse(rule.name, args.map(a => nominate(a))), rule: rule, args: args)

func applier*(rule: Rule, ruleRef: RuleRef): RuleRes =
  RuleRes(name: serialize(ruleRef), rule: rule, args: ruleRef.parameters)

func refersTo(ruleRef: RuleRef, rule: Rule): bool =
  ruleRef.name == rule.name and sequal(ruleRef.parameters, rule.parameters)

proc replaceParamByValue*(ruleRes: RuleRes, param: string, doNominate: bool = false): string =
  let parindex = find(ruleRes.rule.parameters, param)
  debug("replaceParamByValue: '$#', index: $#" % [param, intToStr(parindex)])
  if parindex < 0:
    param
  elif ruleRes.args[parindex] =~ name_pattern:
    ruleRes.args[parindex]
  else:
    # Terminal expressions must be parenthesized (i.e. macro substitution)
    "($#)" % ruleRes.args[parindex]

proc resolveRuleRef*(ruleRes: RuleRes, ruleRef: RuleRef, targets: seq[RuleRef] = @[]): RuleRef =
  debug("resolveRuleRef resolver: [$#], ruleRef: [$#]" % [$(ruleRes), $(ruleRef)])
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
      if resolvedRuleRef.isNonTerminal and not ruleRef.refersTo(ruleRes.rule):
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


proc readPeg(grammar: Grammar, grammarSpec: string) =
  for line in grammarSpec.splitLines().filter(isNotCommentOrEmptyLine):
    debug("readPeg '$#'" % line)
    let rule = read_peg_line(line)
    grammar.rules.add(rule)


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

