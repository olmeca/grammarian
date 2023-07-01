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
import grammarian/common, grammarian/patterns

const
  cVariantKeySeparator = ":"



type
  NonTerminalNameError* = object of ValueError
  NonTerminalCaptureError* = object of ValueError
  RuleRefResolutionError* = object of ValueError

  PrimaryKind* = enum
    Literal, Nominal

  Rule* = object
    name*: string
    variant*: string
    parameters*: seq[string]
    pattern*: string

  RuleRef* = object
    name*: string
    args*: seq[string]

  RuleRes* = ref RuleResObj
  RuleResObj = object
    name*: string # derived from rule and args
    rule*: Rule
    args*: seq[string]
    parent*: RuleRes

  ResolvedRule* = object
    rule: Rule
    subs: seq[RuleRef]

  Grammar* = ref object of RootObj
    ## This is the object that stores all the PEG lines
    ## as 'grammar rules'.
    rules: seq[Rule]

  SubGrammarSpec* = object
    grammar*: Grammar
    variant*: string
    captures*: seq[RuleRef]

  Primary* = ref PrimaryObj
  PrimaryObj* = object of RootObj
    kind*: PrimaryKind


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
  MultipleMatchingRulesError* = object of ValueError
  ParamListError* = object of ValueError


proc resolveChoice( spec: SubGrammarSpec, ruleRes: RuleRes, patSpec: string,
                    ruleRefAcc: var seq[RuleRef], patAccBuf: StringStream)
proc parseRuleRef*(ruleref: string): RuleRef


proc nominate(pattern: string): string =
  # Creates an identifying name for the given pattern
  # if it is not already a valid name (non-terminal)
  if pattern =~ name_pattern:
    pattern
  else:
    "p$#" % intToStr(hash(pattern))


proc fuse(name: string, args: seq[string] = @[]): string =
  let fusedParams = args.map(x => "_$#" % x).join("")
  result = "$#$#" % [name, fusedParams]


proc nameWithArgs(name: string, args: seq[string]): string =
  fuse(name, args.map(a => nominate(a)))


proc captured(name: string): string =
  "{$#}" % name


proc serialize*(ruleref: RuleRef, capture: bool = false, successorPrefix = "", cardinality = ""): string =
  result = successorPrefix & nameWithArgs(ruleref.name, ruleRef.args) & cardinality
  if capture:
    result = captured(result)
  else: discard


proc `$`*(ruleParams: seq[string]): string =
  "<$#>" % join(ruleParams.map(p => "'$#'" % p), ", ")


proc `$`*(ruleRef: RuleRef, capture: bool = false): string =
  let paramStr = $(ruleRef.args)
  result = "$#$#" % [ruleRef.name, paramStr]
  if capture:
    result = captured(result)
  else: discard


proc `$`*(rule: Rule): string =
  let paramStr = $(rule.parameters)
  let variantStr = ":'$#'" % rule.variant
  "'$#'$#$# <- `$#`" % [rule.name, variantStr, paramStr, rule.pattern]


proc `$`*(ruleRes: RuleRes): string =
  "($#) -> $# ($#): `$#`" % [join(ruleRes.args, ", "), ruleRes.rule.name, join(ruleRes.rule.parameters, ", "), ruleRes.rule.pattern]


proc newRule*(name: string, pattern: string, variant = "", params: seq[string] = @[]): Rule =
  Rule(name: name, variant: variant, parameters: params, pattern: pattern)


proc newRuleRef*(name: string, params: seq[string] = @[]): RuleRef =
  RuleRef(name: name, args: params)


proc parseRuleRefArgs(source: string): seq[string] =
  result = @[]
  var argStream = newStream(firstRuleRefArgAndRestPeg, source)
  var arg = argStream.next()
  while arg.notEmpty():
    try:
      let subRuleRef = parseRuleRef(arg)
      debug("Found subruleref argument: `$#`" % serialize(subRuleRef))
      result.add(serialize(subRuleRef))
    except NonTerminalNameError:
      result.add(arg)
    arg = argStream.next()


proc isNonTerminal(ruleRef: RuleRef): bool =
  ruleRef.name.match(name_pattern)


proc parseRuleRef*(ruleref: string): RuleRef =
  debug("parseRuleRef: `$#`" % ruleref)
  if ruleref =~ ruleRefPeg:
    debug("parseRuleRef - RuleRef parts found: <$#>" % foldMatches(matches))
    let ruleName = matches[0]
    let ruleParams = if matches[1] == "": @[] else: parseRuleRefArgs(matches[1])
    result = RuleRef(name: ruleName, args: ruleParams)
    debug("parseRuleRef -> [$#]" % $(result))
  else:
    debug("parseRuleRef: no match for <$#>" % ruleref)
    raise newException(NonTerminalNameError, ruleref)


func sequal*[T](s1: openArray[T], s2: openArray[T]): bool =
  s1.len == s2.len and (s1.len == 0 or zip(s1, s2).all(x => x[0] == x[1]))


func eqRuleRef(r1: RuleRef, r2: RuleRef): bool =
  r1.name == r2.name and sequal(r1.args, r2.args)


func applier*(rule: Rule, args: seq[string], parent: RuleRes = nil): RuleRes =
  RuleRes(name: nameWithArgs(rule.name, args), rule: rule, args: args, parent: parent)


func applier*(rule: Rule, ruleRef: RuleRef, parent: RuleRes = nil): RuleRes =
  RuleRes(name: serialize(ruleRef), rule: rule, args: ruleRef.args, parent: parent)


func isSelfRef(ruleRef: RuleRef, rule: Rule): bool =
  ruleRef.name == rule.name and sequal(ruleRef.args, rule.parameters)


proc resolveParameter*( spec: SubGrammarSpec, ruleRes: RuleRes, param: string,
                        ruleRefAcc: var seq[RuleRef], doNominate: bool = false): string =
  let parindex = find(ruleRes.rule.parameters, param)
  debug("resolveParameter: '$#', index: $#" % [param, intToStr(parindex)])
  if parindex < 0:
    # param is not a rule parameter name
    param
  elif ruleRes.args[parindex] =~ name_pattern:
    # TODO: Here we have too process the arg as a pattern spec
    ruleRes.args[parindex]
  else:
    # Terminal expressions must be parenthesized (i.e. macro substitution)
    "($#)" % ruleRes.args[parindex]


proc refersTo(ruleRef: RuleRef, ruleRes: RuleRes): bool =
  ruleRef.name == ruleRes.rule.name


proc recurses(ruleRef: RuleRef, ruleRes: RuleRes): bool =
  ruleRef.refersTo(ruleRes) and sequal(ruleRef.args, ruleRes.rule.parameters)


proc shouldCapture(spec: SubGrammarSpec, ruleRef: RuleRef): bool =
  spec.captures.filter(r => serialize(r) == serialize(ruleRef)).len > 0


proc resolveRuleRef*( spec: SubGrammarSpec, ruleRes: RuleRes, ruleRef: RuleRef,
                      ruleRefAcc: var seq[RuleRef]): RuleRef =
  debug("resolveRuleRef resolver: $#, ruleRef: `$#`" % [$(ruleRes), $(ruleRef)])
  var theRuleRef: RuleRef
  # if ruleRef is a recursive reference
  if ruleRef.recurses(ruleRes):
    # a fully recursive call (with same rule params as args)
    theRuleRef = newRuleRef(ruleRes.name)
    # if ruleRef.recurses(ruleRes):
    #   debug("resolveRuleRef: full self reference.")
    #   # ruleRes.name is the already resolved name
    #   theRuleRef = newRuleRef(ruleRes.name)
    # else:
    #   debug("resolveRuleRef: recursive call with different args")
    #   # This is another recursive call to this rule (with args differing from rule params)
    #   theRuleRef = ruleRef
  elif ruleRef.args.len == 0:
    debug("resolveRuleRef: ruleref no args")
    # just replace the name if it is a rule parameter name
    theRuleRef = newRuleRef(resolveParameter(spec, ruleRes, ruleRef.name, ruleRefAcc))
  else:
    debug("resolveRuleRef: ruleref with args")
    # Can we not get the args from the ruleres?
    var resolvedParams: seq[string] = @[]
    for param in ruleRef.args:
      resolvedParams.add(resolveParameter(spec, ruleRes, param, ruleRefAcc))
    theRuleRef = newRuleRef(ruleRef.name, resolvedParams)
  debug("resolveRuleRef -> `$#`" % $(theRuleRef))
  result = theRuleRef


proc resolveSequenceItem( spec: SubGrammarSpec, ruleRes: RuleRes, patSpec: string,
                          ruleRefAcc: var seq[RuleRef], patAccBuf: StringStream) =
  debug("resolveSequenceItem: <$#>" % patSpec)
  if patSpec =~ compositeSeqItemPeg:
    # write successor indicator (& or !)
    patAccBuf.write(matches[0])
    # open parenthesis
    patAccBuf.write("(")
    resolveChoice(spec, ruleRes, matches[1], ruleRefAcc, patAccBuf)
    patAccBuf.write(")")
    # write cardinality indicator (*, + or ?)
    patAccBuf.write(matches[2] & ' ')
  elif patSpec =~ seqItemPeg:
    debug("resolveSequenceItem: sequence item parts found: <$#>" % foldMatches(matches))
    let successorPrefix = matches[0]
    let primary = matches[1]
    let cardinalityIndicator = matches[2]
    try:
      let ruleRef: RuleRef = parseRuleRef(primary)
      let resolvedRuleRef = resolveRuleRef(spec, ruleRes, ruleRef, ruleRefAcc)
      let captured = shouldCapture(spec, resolvedRuleRef)
      patAccBuf.write(serialize(resolvedRuleRef, captured, successorPrefix, cardinalityIndicator) & ' ')
      # Only add if it is not a recursive rule ref
      if resolvedRuleRef.isNonTerminal and not ruleRef.recurses(ruleRes):
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


proc resolveSequence( spec: SubGrammarSpec, ruleRes: RuleRes, patSpec: string,
                      ruleRefAcc: var seq[RuleRef], patAccBuf: StringStream) =
  debug("resolveSequence: <$#>" % patSpec)
  var seqStream = newStream(firstSeqItemAndRestPeg, patspec)
  var seqItem = seqStream.next()
  while seqItem.notEmpty():
    resolveSequenceItem(spec, ruleRes, seqItem, ruleRefAcc, patAccBuf)
    seqItem = seqStream.next()


proc resolveChoice( spec: SubGrammarSpec, ruleRes: RuleRes, patSpec: string,
                    ruleRefAcc: var seq[RuleRef], patAccBuf: StringStream) =
  debug("resolveChoice: rres: $#, pat: `$#`" % [$(ruleRes), patSpec])
  var choiceIndex = 0
  var altStream = newStream(firstAltAndRestPeg, patspec)
  var alt = altStream.next()
  while alt.notEmpty():
    if choiceIndex > 0:
      patAccBuf.write("/ ")
    else: discard
    inc(choiceIndex)
    resolveSequence(spec, ruleRes, alt, ruleRefAcc, patAccBuf)
    alt = altStream.next()


# proc resolvePatternSpec*(ruleRes: RuleRes, patSpec: string, targets: seq[RuleRef], ruleRefAcc: var seq[RuleRef], patAccBuf: StringStream) =
#   resolveChoice(ruleRes, patSpec, targets, ruleRefAcc, patAccBuf)


proc resolveRuleRes*(spec: SubGrammarSpec, ruleRes: RuleRes, ruleRefAcc: var seq[RuleRef], patAccBuf: StringStream) =
  resolveChoice(spec, ruleRes, ruleRes.rule.pattern, ruleRefAcc, patAccBuf)


proc readWordList(source: string): seq[string] =
  debug("readWordList: '$#'" % source)
  if len(source) > 0:
    if source =~ paramListPeg:
      debug(foldMatches(matches))
      result = matches.filter(notEmpty)
    else:
      raise newException(RulePatternError, "Invalid rule parameter list specification: <$#>" % source)
  else:
    result = @[]


proc readRuleParts(namepart: string, patternpart: string): Rule =
  if namepart =~ ruleHeaderPeg:
    debug("readRuleParts: name: <$#>" % foldMatches(matches))
    let name = matches[0]
    let variant = matches[1]
    let params = if matches[2].notEmpty: readWordList(matches[2]) else: @[]
    result = newRule(name, patternpart, variant, params)
    debug("readRuleParts -> [$#]" % $(result))
  else:
    raise newException(RulePatternError, "Invalid LHS: '$#'" % namepart);


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
  getRule(grammar, ruleRef.name, variant, ruleRef.args)


proc getRuleRes*(grammar: Grammar, name: string, args: seq[string], variant: string = ""): RuleRes =
  applier(getRule(grammar, name, variant, args), args)


proc getRuleRes*(grammar: Grammar, ruleRef: RuleRef, variant: string = ""): RuleRes =
  applier(getRule(grammar, ruleRef.name, variant, ruleRef.args), ruleRef.args)


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


proc splitFirstRule*(value: string): (string, string, string) =
  if value =~ firstRuleAndRestPeg:
    (matches[0], matches[1], matches[2])
  else:
    raise newException(NoMatchError, ("value: '$#'" % value))


proc readPeg(grammar: Grammar, grammarSpec: string) =
  var nextRuleSplit = splitFirstRule(grammarSpec)
  block processing:
    while true:
      grammar.rules.add(readRuleParts(nextRuleSplit[0], nextRuleSplit[1]))
      if nextRuleSplit[2].isEmpty():
        break processing
      else:
        nextRuleSplit = splitFirstRule(nextRuleSplit[2])


proc newGrammar*(grammarSpec: string): Grammar =
  result = newGrammar()
  readPeg(result, grammarSpec)


proc hasRuleNamed(grammar: Grammar, ruleName: string): bool =
  grammar.rules.filter(rule => rule.name == ruleName).len > 0


proc resolveRule(spec: SubGrammarSpec, ruleRes: RuleRes): ResolvedRule =
    var patSpecBuf = newStringStream()
    var subRuleRefsAcc: seq[RuleRef] = @[]
    resolveRuleRes(spec, ruleRes, subRuleRefsAcc, patSpecBuf)
    patSpecBuf.setPosition(0)
    ResolvedRule(rule: newRule(ruleRes.name, patSpecBuf.readAll()), subs: subRuleRefsAcc)


proc getRuleRes(spec: SubGrammarSpec, ruleRef: RuleRef): RuleRes =
  getRuleRes(spec.grammar, ruleRef, spec.variant)


proc copySubGrammar(spec: SubGrammarSpec, destGrammar: Grammar, ruleRef: RuleRef) =
  let ruleRes = spec.grammar.getRuleRes(ruleRef, spec.variant)
  # Only if dest has no rule with resolved name
  if not destGrammar.hasRuleNamed(ruleRes.name):
    let resolvedRule = resolveRule(spec, ruleRes)
    debug("copySubGrammar: resolvedRule: [$#]" % $(resolvedRule))
    destGrammar.rules.add(resolvedRule.rule)
    for subRuleRef in resolvedRule.subs:
      debug("copySubGrammar: subRuleRef: [$#]" % $(subRuleRef))
      copySubGrammar(spec, destGrammar, subRuleRef)


proc getSubGrammar(spec: SubGrammarSpec, ruleRef: RuleRef): Grammar =
  result = newGrammar()
  copySubGrammar(spec, result, ruleRef)


proc getSubGrammar(srcGrammar: Grammar, ruleRef: RuleRef, variant: string, targets: seq[string]): Grammar =
  let spec = SubGrammarSpec(grammar: srcGrammar, variant: variant, captures: targets.map(t => parseRuleRef(t)))
  getSubGrammar(spec, ruleRef)


proc getSubGrammar(srcGrammar: Grammar, ruleName: string = cDefaultRoot,
                    args: seq[string] = @[], variant: string = "", targets: seq[string]): Grammar =
  getSubGrammar(srcGrammar, newRuleRef(ruleName, args), variant, targets)


proc toString(grammar: Grammar): string =
  var buffer: Stream = newStringStream()
  for rule in grammar.rules:
    buffer.write("$# <- $#\n" % [rule.name, rule.pattern])
  buffer.setPosition(0)
  result = buffer.readAll()


proc pegString*(grammar: Grammar, ruleName: string = cDefaultRoot, extractables: seq[string] = @[],
                variant: string = "", args: seq[string] = @[]): string  =
  let subGrammar = getSubGrammar(grammar, ruleName, args, variant, extractables)
  result = subGrammar.toString()
  debug ("pegString ->\n" & result)


proc matcher*(grammar: Grammar, patternName: string = cDefaultRoot): Peg =
  peg(pegString(grammar, patternName))


proc extractorPeg*(grammar: Grammar, parts: seq[string], patternName: string = cDefaultRoot, variant: string = ""): Peg =
  peg(pegString(grammar, patternName, parts, variant))

proc extractorPeg*(grammar: Grammar, patternName: string, parts: seq[string], variant: string = ""): Peg =
  peg(pegString(grammar, patternName, parts, variant))


proc newPatternExtractor*(grammar: Grammar, mainPattern: string, targets: seq[string], variant: string = ""): PatternExtractor =
  debug("newPatternExtractor: main: '$#', var: '$#', targets: [$#]" % [mainPattern, variant, targets.join(",")])
  let extractorPatPeg = grammar.extractorPeg(targets, mainPattern, variant)
  PatternExtractor(extractorPattern: extractorPatPeg, targetNonterminals: targets)

proc extract*(extractor: PatternExtractor, source: string): TableRef[string, string] =
  # Supposing the matches found in the same order as in extractor.targetNonTerminals
  result = newTable[string, string]()
  if source =~ extractor.extractorPattern:
    for i, key in extractor.targetNonterminals:
      result[key] = matches[i]
  else:
    raise newException(NoMatchError, "String does not match pattern.")