module Day06.Input exposing (parsedInput, Input)


type alias Input =
    List Int


rawInput : String
rawInput =
    "14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4"


parsedInput : Input
parsedInput =
    parse rawInput


parse : String -> Input
parse =
    List.filterMap (Result.toMaybe << String.toInt) << String.words
