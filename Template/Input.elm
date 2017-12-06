module Template.Input exposing (parsedInput, Input)


type alias Input =
    String


rawInput : String
rawInput =
    ""


parsedInput : Input
parsedInput =
    parse rawInput


parse : String -> Input
parse =
    identity
