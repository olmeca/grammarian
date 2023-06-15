import logging, sequtils, pegs

type
  PegChunkedStream* = object of RootObj
    source: string
    pattern: Peg
  NoMatchError* = object of ValueError

const
  cDefaultRoot* = "Pattern"

func isEmpty*[T](list: seq[T]): bool =
  count(list) == 0

func notEmpty*[T](list: seq[T]): bool =
  not isEmpty(list)

proc isEmpty*(value: string): bool =
  value == ""


proc notEmpty*(value: string): bool =
  not value.isEmpty


proc boolToStr*(value: bool): string =
    if value: "true" else: "false"

proc foldMatches*(source: array[0..19, string]): string =
  "|" & source.foldl(a & "|" & b)

proc enableLogging*() =
  var logger = newConsoleLogger()
  addHandler(logger)
  setLogFilter(lvlDebug)

proc newStream*(pattern: Peg, value: string): PegChunkedStream =
  PegChunkedStream(pattern: pattern, source: value)

proc hasNext*(stream: PegChunkedStream): bool =
  stream.source.notEmpty()

proc next*(stream: var PegChunkedStream): string =
  if stream.source.isEmpty():
    result = ""
  elif stream.source =~ stream.pattern:
    result = matches[0]
    stream.source = matches[1]
  else:
    raise newException(NoMatchError, "'$#' does not match pattern")

