import unittest, pegs, tables, strutils, logging, streams, sequtils, sugar
import grammarian

suite "sequal":

  test "compare one elem array of string with itself":
    let params = @["een"]
    check sequal(params, params)

  test "compare two elem array of string with itself":
    let params = @["een", "twee"]
    check sequal(params, params)

  test "compare one elem array of strings with another":
    let params1 = @["name"]
    let params2 = @["name"]
    check sequal(params1, params2)


  test "compare one elem array of strings with another":
    let params1 = @["name"]
    let params2 = @["names"]
    check not sequal(params1, params2)


  test "compare two elem array of strings with another":
    let params1 = @["name", "value"]
    let params2 = @["name", "Value"]
    check not sequal(params1, params2)

