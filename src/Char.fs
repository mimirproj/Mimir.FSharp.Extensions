module Mimir.FSharp.Extensions.Char

open System

let inline isBackTick c =
    c = '`'

let inline isSingleQuote c =
    c = '''

let inline isDoubleQuote c =
    c = '"'

let inline isEmphasis c =
    c = '_'

let inline isDigit c =
    Char.IsDigit c

let inline isLower c =
    Char.IsLower c

let inline isUpper c =
    Char.IsUpper c

let isLeftRoundBrace c =
    c = '('

let inline isRightRoundBrace c =
    c = ')'

let isRoundBrace =
    isLeftRoundBrace
    >|| isRightRoundBrace

let isLeftAngleBrace c =
    c = '<'

let inline isRightAngleBrace c =
    c = '>'

let isAngleBrace =
    isLeftAngleBrace
    >|| isRightAngleBrace

let isLeftCurlyBrace c =
    c = '{'

let inline isRightCurlyBrace c =
    c = '}'

let isCurlyBrace =
    isLeftCurlyBrace
    >|| isRightCurlyBrace

let isLeftSquareBrace c =
    c = '['

let inline isRightSquareBrace c =
    c = ']'

let isSquareBrace =
    isLeftSquareBrace
    >|| isRightSquareBrace

let isBrace =
    isRoundBrace
    >|| isAngleBrace
    >|| isCurlyBrace
    >|| isSquareBrace

let inline isDash c =
    c = '-'

let inline isColon c =
    c = ':'

let inline isSemiColon c =
    c = ';'

let inline isComma c =
    c = ','

let inline isPeriod c =
    c = '.'

let inline isExclamation c =
    c = '!'

let inline isQuestion c =
    c = '?'

let isPunctuation =
    isDash
    >|| isColon
    >|| isSemiColon
    >|| isComma
    >|| isPeriod
    >|| isExclamation
    >|| isQuestion