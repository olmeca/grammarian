import unittest, pegs, tables, strutils, logging, streams, sequtils, sugar
import grammarian, grammarian/patterns, grammarian/common

let adres_spec = """
Adres <- StraatHuis ',' ' '+ Woonplaats
StraatHuis <- Straat ' '+ Huisnr
Straat <- Naam
Woonplaats <- Naam
Naam <- [a-zA-Z]+
Huisnr <- [0-9]+
"""

enableLogging()

proc listMatch(item: string, sep: string, source: string): string =
    let valuesSpec = """
    Pattern <- 'Value: ' Value
    Value <- List<$#, $#>
    List<Item, Sep> <- Item Sep List<Item, Sep> / Item
    """ % [item, sep]
    debug("Peg: $#" % valuesSpec)
    let extractor = newGrammar(valuesSpec).extractorPeg(@["Value"])
    if "Value: $#" % source =~ extractor:
      matches[0]
    else:
      raise newException(NoMatchError, "No match.")

suite "plainextraction":

  # test "list of charset and comma":
  #   check listMatch("[a-z]", "','", "a,b,c d") == "a,b,c"
  #
  # test "list of charset words and comma":
  #   check listMatch("[a-z]+", "','", "aa,b,ccc d") == "aa,b,ccc"
  #
  # test "list of numbers sep by space":
  #   check listMatch("[0-9]", "' '", "3 0 7 9  8") == "3 0 7 9"

  test "list of numbers or words sep by space 2":
    check listMatch("([0-9]+) / ([a-zA-Z]+)", "' '", "2 be 10 is l8") == "2 be 10 is l"

  # test "list of numbers or words sep by space":
  #   check listMatch("([0-9]+) ([a-zA-Z]+)", "' '", "28Ai 89Qw bin98") == "28Ai 89Qw"
  #
  # test "list of numbers sep by tab":
  #   check listMatch("[0-9]", "'\\9'", "3\t8\t2 \t5") == "3\t8\t2"
  #
  # test "list of builtins sep by space":
  #   check listMatch("\\d", "' '", "3 0 7 9  8") == "3 0 7 9"
  #
  #
  # test "grammar extractor comma sep list of char":
  #   let valuesSpec = """
  #   Pattern <- 'Value: ' Value
  #   Value <- List<[a-z], ','>
  #   List<Item, Sep> <- Item Sep List<Item, Sep> / Item
  #   """
  #   let extractor = newGrammar(valuesSpec).extractorPeg(@["Value"])
  #   if "Value: a,b,c" =~ extractor:
  #     check matches[0] == "a,b,c"
  #   else:
  #     raise newException(NoMatchError, "No match.")
  #
  # test "grammar extractor list of words":
  #   let valuesSpec = """
  #   Pattern <- 'Value: ' Value
  #   Value <- List<[a-z]+, ' '>
  #   List<Item, Sep> <- Item Sep List<Item, Sep> / Item
  #   """
  #   let extractor = newGrammar(valuesSpec).extractorPeg(@["Value"])
  #   if "Value: a bra cadabra" =~ extractor:
  #     check matches[0] == "a bra cadabra"
  #   else:
  #     raise newException(NoMatchError, "No match.")
  #
  # test "grammar extractor comma sep list of char":
  #   let valuesSpec = """
  #   Pattern <- 'Value: ' Value
  #   Value <- List< 'B' [a-z]+, ','>
  #   List<Item, Sep> <- Item Sep List<Item, Sep> / Item
  #   """
  #   let extractor = newGrammar(valuesSpec).extractorPeg(@["Value"])
  #   if "Value: Ba,Bra,Badabra" =~ extractor:
  #     check matches[0] == "Ba,Bra,Badabra"
  #   else:
  #     raise newException(NoMatchError, "No match.")
  #
  # test "grammar extractor comma sep list of char":
  #   let valuesSpec = """
  #   Pattern <- 'Value: ' Value
  #   Value <- List<[A-Z]+ / [a-z]+, ','>
  #   List<Item, Sep> <- Item Sep List<Item, Sep> / Item
  #   """
  #   let extractor = newGrammar(valuesSpec).extractorPeg(@["Value"])
  #   if "Value: bra,BRO,bri" =~ extractor:
  #     check matches[0] == "bra,BRO,bri"
  #   else:
  #     raise newException(NoMatchError, "No match.")
  #
