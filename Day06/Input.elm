module Day06.Input exposing (Input, parsedInput)


type alias Input =
    List Int


rawInput : String
rawInput =
    "14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4"


parsedInput : Input
parsedInput =
    String.words rawInput
        |> List.filterMap String.toInt
