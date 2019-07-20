module Example exposing (main)

import Parser.Advanced as P exposing (Parser)


main =
    Platform.worker
        { init = init
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


init () =
    ( let
        _ =
            Debug.log "debugRun succeeds" <|
                P.debugRun int "[ 1, 2, 3 ]"

        _ =
            Debug.log "debugRun fails" <|
                P.debugRun int "[ 1, 2. 3 ]"
      in
      ()
    , Cmd.none
    )


type Problem
    = ExpectedInt
    | InvalidInt
    | ExpectingLeftBracket
    | ExpectingRightBracket
    | ExpectingListSeparator


type Context
    = InInt
    | InSpaces
    | InList


int : Parser Context Problem Int
int =
    P.int ExpectedInt InvalidInt
        |> P.inContext InInt


listOfInts : Parser Context Problem (List Int)
listOfInts =
    P.sequence
        { start = P.Token "[" ExpectingLeftBracket
        , separator = P.Token "," ExpectingListSeparator
        , end = P.Token "]" ExpectingRightBracket
        , spaces = spaces
        , item = int
        , trailing = P.Forbidden
        }
        |> P.inContext InList


spaces : Parser Context Problem ()
spaces =
    P.chompWhile ((==) ' ')
        |> P.inContext InSpaces
