module Tests exposing
    ( charCodeAtTest
    , findSubStringTest
    , isSubCharTest
    , isSubStringTest
    )

import Expect
import String.LowLevel
import Test exposing (Test)


findSubStringTest : Test
findSubStringTest =
    let
        runCase : Int -> ( ( String, ( Int, Int, Int ), String ), ( Int, Int, Int ) ) -> Test
        runCase i ( ( smallString, ( offset, row, col ), bigString ), output ) =
            Test.test ("example " ++ String.fromInt i) <|
                \() ->
                    String.LowLevel.findSubString smallString offset row col bigString
                        |> Expect.equal output
    in
    Test.describe "findSubString" <|
        List.indexedMap runCase
            [ ( ( "42", ( 0, 0, 0 ), "Is 42 the answer?" ), ( 3, 1, 4 ) )
            , ( ( "42", ( 7, 0, 0 ), "Is 42 the answer?" ), ( -1, 1, 18 ) )
            ]


isSubStringTest : Test
isSubStringTest =
    -- TODO test also newline handling
    -- TODO test also the UTF-16 multiChar 0xF800 0xD800 weirdness
    let
        runCase : Int -> ( ( String, ( Int, Int, Int ), String ), ( Int, Int, Int ) ) -> Test
        runCase i ( ( smallString, ( offset, row, col ), bigString ), output ) =
            Test.test ("example " ++ String.fromInt i) <|
                \() ->
                    String.LowLevel.isSubString smallString offset row col bigString
                        |> Debug.log "got"
                        |> Expect.equal (Debug.log "needed" output)
    in
    Test.describe "isSubString" <|
        List.indexedMap runCase
            [ ( ( "42", ( 0, 0, 0 ), "Is 42 the answer?" ), ( -1, 0, 0 ) )
            , ( ( "42", ( 3, 0, 0 ), "Is 42 the answer?" ), ( 5, 0, 2 ) )
            , ( ( "42", ( 4, 0, 0 ), "Is 42 the answer?" ), ( -1, 0, 0 ) )
            , ( ( "42", ( 0, 1, 1 ), "Is 42 the answer?" ), ( -1, 1, 1 ) )
            , ( ( "42", ( 3, 1, 1 ), "Is 42 the answer?" ), ( 5, 1, 3 ) )
            , ( ( "42", ( 4, 1, 1 ), "Is 42 the answer?" ), ( -1, 1, 1 ) )
            ]


isSubCharTest : Test
isSubCharTest =
    Test.todo "isSubCharTest"


charCodeAtTest : Test
charCodeAtTest =
    Test.todo "charCodeAtTest"
