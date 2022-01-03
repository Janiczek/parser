module String.LowLevel exposing
    ( charCodeAt
    , findSubString
    , isSubChar
    , isSubString
    )

{-| Functions we needed to port from Elm.Kernel.Parser.js
-}

import Bitwise
import List.Extra


{-| Find a substring after a given offset.

    findSubString "42" offset row col "Is 42 the answer?"
        --==> (newOffset, newRow, newCol)

If `offset = 0` we would get `(3, 1, 4)`
If `offset = 7` we would get `(-1, 1, 18)`

  - offset == -1: substring not found after the given starting offset
  - offset == -2: substring found after a newline? TODO check this. Do we actually
    even do this in this function?

See docs for isSubString for more.

-}
findSubString : String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
findSubString smallString offset row col bigString =
    let
        newOffset =
            String.indexes smallString bigString
                |> List.Extra.dropWhile (\index -> index < offset)
                |> List.head
                |> Maybe.withDefault -1

        target =
            if newOffset < 0 then
                String.length bigString

            else
                String.length smallString + newOffset

        ( newRow, newCol ) =
            findSubStringHelp row col target offset bigString
    in
    ( newOffset, newRow, newCol )


findSubStringHelp : Int -> Int -> Int -> Int -> String -> ( Int, Int )
findSubStringHelp row col target offset bigString =
    if offset >= target then
        ( row, col )

    else
        let
            code =
                charCodeAt offset bigString

            offset1 =
                offset + 1

            ( newRow, newCol, newOffset ) =
                if code == 0x0A then
                    ( row + 1, 1, offset1 )

                else
                    ( row
                    , col + 1
                    , if Bitwise.and code 0xF800 == 0xD800 then
                        offset1 + 1

                      else
                        offset1
                    )
        in
        findSubStringHelp newRow newCol target newOffset bigString


charCodeAt : Int -> String -> Int
charCodeAt offset string =
    string
        |> String.slice offset (offset + 1)
        |> String.uncons
        |> Maybe.map (Tuple.first >> Char.toCode)
        -- shouldn't happen?
        |> Maybe.withDefault 0


{-| When making a fast parser, you want to avoid allocation as much as
possible. That means you never want to mess with the source string, only
keep track of an offset into that string.

You use `isSubString` like this:

    isSubString "let" offset row col "let x = 4 in x"
        --==> ( newOffset, newRow, newCol )

You are looking for `"let"` at a given `offset`. On failure, the
`newOffset` is `-1`. On success, the `newOffset` is the new offset. With
our `"let"` example, it would be `offset + 3`.

You also provide the current `row` and `col` which do not align with
`offset` in a clean way. For example, when you see a `\n` you are at
`row = row + 1` and `col = 1`. Furthermore, some UTF16 characters are
two words wide, so even if there are no newlines, `offset` and `col`
may not be equal.

-}
isSubString : String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
isSubString =
    Debug.todo "isSubString"


{-| Again, when parsing, you want to allocate as little as possible.
So this function lets you say:

    isSubChar isSpace offset "this is the source string"
        --==> newOffset

The `(Char -> Bool)` argument is called a predicate.
The `newOffset` value can be a few different things:

  - `-1` means that the predicate failed
  - `-2` means the predicate succeeded with a `\n`
  - otherwise you will get `offset + 1` or `offset + 2`
    depending on whether the UTF16 character is one or two
    words wide.

-}
isSubChar : (Char -> Bool) -> Int -> String -> Int
isSubChar =
    Debug.todo "isSubChar"
