import unittest, pegs, logging

import grammarian/common, grammarian/patterns

var stream: PegChunkedStream

suite "PegChunkedStream tests":

  test " 1: <abc> gives <abc>":
    stream = newStream(firstAltAndRestPeg, "abc")
    check stream.next() == "abc"

  test " 2: <abc/d> yields <abc>, <d>":
    stream = newStream(firstAltAndRestPeg, "abc/d")
    check stream.next() == "abc"
    check stream.next() == "d"
    check stream.next() == ""

  test " 3: <abc/d > yields <abc>, <d>":
    stream = newStream(firstAltAndRestPeg, "abc/d ")
    check stream.next() == "abc"
    check stream.next() == "d"
    check stream.next() == ""

  test " 4: <abc/d #test> yields <abc>, <d>":
    stream = newStream(firstAltAndRestPeg, "abc/d #test")
    check stream.next() == "abc"
    check stream.next() == "d"
    check stream.next() == ""

  test " 5: <abc/d\\n> yields <abc>, <d>":
    stream = newStream(firstAltAndRestPeg, "abc/d\n")
    check stream.next() == "abc"
    check stream.next() == "d"
    check stream.next() == ""

  test " 6: <abc d > yields <abc>, <d>":
    stream = newStream(firstSeqItemAndRestPeg, "abc d ")
    check stream.next() == "abc"
    check stream.next() == "d"
    check stream.next() == ""

  test " 7: <!abc+ [a-z]? > yields <!abc+>, <[a-z]?>":
    stream = newStream(firstSeqItemAndRestPeg, "!abc+ [a-z]? ")
    check stream.next() == "!abc+"
    check stream.next() == "[a-z]?"
    check stream.next() == ""
