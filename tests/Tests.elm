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
    Test.todo "isSubStringTest"


isSubCharTest : Test
isSubCharTest =
    Test.todo "isSubCharTest"


charCodeAtTest : Test
charCodeAtTest =
    Test.todo "charCodeAtTest"
