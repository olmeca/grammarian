import unittest, pegs
import grammarian, grammarian/patterns


suite "grammars":
  let valuesSpec = """
  Main <- 'Value:' Sp Value
  Value <- List<Ident, Dot> / List<Number, Comma>
  PackageName <- List<Ident, Dot>
  Csv <- List<Number, Comma>
  List<Item, Sep> <- Item Sep List<Item, Sep> / Item
  Word <- [a-zA-Z]+
  Ident <- [a-z]+
  Number <- [0-9]+
  Dot <- '.'
  Space <- ' '+
  Comma <- ','
  Test <- 'test'
  Sp <- ' '*
  """

  # test "create grammar":
  #   let grammar = newGrammar()
  #   let p1 = newRule("Passed", "'true'")
  #   let p2 = newRule("Failed", "'false'")
  #   grammar.addRule(newRule("Test", "Passed / Failed"))
  #   grammar.addRule(p1)
  #   grammar.addRule(p2)
  #   check grammar.getRule("Passed") == p1


  # test "serialize grammar":
  #   let grammar = newGrammar()
  #   let p1 = newRule("Passed", "'true'")
  #   let p2 = newRule("Failed", "'false'")
  #   grammar.addRule(newRule("Test", "Passed / Failed"))
  #   grammar.addRule(p1)
  #   grammar.addRule(p2)
  #   check grammar.getRule("Passed") == p1
  #
  #
  # test "read grammar":
  #   let grammar = newGrammar(peg_pegstring)
  #   # dump_grammar(base)
  #   echo grammar.pegString("Primary")
  #
  # test "grammar with parameterized rule":
  #   let spec = """
  #   Pattern <- Csv
  #   Csv <- List<Word, Comma>
  #   List<Item, Sep> <- Item Sep List<Item, Sep> / Item
  #   Word <- [a-zA-Z]+
  #   Space <- ' '+
  #   Comma <- ','
  #   """
  #   let grammar = newGrammar(spec)
  #   debug("**** DUMPING PEG ****")
  #   echo grammar.pegString("Csv")

  # test "grammar with parameterized rule 1":
  #   let spec = """
  #   Pattern <- 'Just a' Csv Test
  #   Csv <- List<Word, Comma>
  #   List<Item, Sep> <- Item Sep List<Item, Sep> / Item
  #   Word <- [a-zA-Z]+
  #   Space <- ' '+
  #   Comma <- ','
  #   Test <- 'test'
  #   """
  #   let grammar = newGrammar(spec)
  #   debug("**** DUMPING PEG ****")
  #   echo grammar.pegString("Pattern")
  #
  # test "grammar with parameterized rule 2":
  #   let spec = """
  #   Pattern <- 'Value: ' (PackageName / Csv)
  #   PackageName <- List<Ident, Dot>
  #   Csv <- List<Number, Comma>
  #   List<Item, Sep> <- Item Sep List<Item, Sep> / Item
  #   Word <- [a-zA-Z]+
  #   Ident <- [a-z]+
  #   Number <- [0-9]+
  #   Dot <- '.'
  #   Space <- ' '+
  #   Comma <- ','
  #   Test <- 'test'
  #   """
  #   let grammar = newGrammar(spec)
  #   debug("**** DUMPING PEG ****")
  #   echo grammar.pegString("Pattern")


  test "grammar matcher simple":
    let grammar = newGrammar("Main <- 'Value: ' [0-9]+")
    let pattern = grammar.matcher("Main")
    check "Value: 12345" =~ pattern

  test "grammar matcher complex":
    let pattern = newGrammar(valuesSpec).matcher("Main")
    check "Value: 12345" =~ pattern

  test "grammar extractor complex":
    let pattern = newGrammar(valuesSpec).extractorPeg(@["Value"])
    if "Value: 12345" =~ pattern:
      check matches[0] == "12345"
    else:
      raise newException(NoMatchError, "No match.")

  test "grammar extractor complex":
    let pattern = newGrammar(valuesSpec).extractorPeg(@["Value"])
    if "Value: 12345,67890" =~ pattern:
      check matches[0] == "12345,67890"
    else:
      raise newException(NoMatchError, "No match.")

  test "grammar extractor list of numbers":
    let extractor = newGrammar(valuesSpec).extractorPeg(@["Value"])
    if "Value: 12345,67890" =~ extractor:
      check matches[0] == "12345,67890"
    else: discard

  test "grammar extractor package name":
    let extractor = newGrammar(valuesSpec).extractorPeg(@["Value"])
    if "Value: een.twee.drie" =~ extractor:
      check matches[0] == "een.twee.drie"
    else:
      raise newException(NoMatchError, "No match.")


let adres_spec = """
Adres <- StraatHuis ',' ' '+ Woonplaats
StraatHuis <- Straat ' '+ Huisnr
Straat <- Naam
Woonplaats <- Naam
Naam <- [a-zA-Z]+
Huisnr <- [0-9]+
"""
suite "Matchers":

  test "create a peg matcher":
    let grammar = newGrammar(adres_spec)
    let adr_matcher = grammar.matcher("Adres")
    let straat_matcher = grammar.matcher("StraatHuis")
    check "Caretaweg 980" =~ straat_matcher
    check "Zjoekowlaan 93, Delft" =~ adr_matcher


suite "extract":
  test "extraction two items":
    let grammar = newGrammar(adres_spec)
    let wp_extractor = grammar.extractorPeg("Adres", @["Straat", "Woonplaats"])
    if "Zjoekowlaan 93, Delft" =~ wp_extractor:
      check matches[1] == "Delft"
      check matches[0] == "Zjoekowlaan"
    else: discard


