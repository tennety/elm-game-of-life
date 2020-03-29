module RleParserTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import RleParser
import Test exposing (..)


suite : Test
suite =
    describe "RLE Parser"
        [ test "parses one dead cell" <|
            \_ ->
                Expect.equal (Ok []) (RleParser.parse "b!")
        , test "parses multiple dead cells" <|
            \_ ->
                Expect.equal (Ok []) (RleParser.parse "3b!")
        , test "parses one live cell" <|
            \_ ->
                Expect.equal (Ok [ ( 0, 0 ) ]) (RleParser.parse "o!")
        , test "parses multiple live cells" <|
            \_ ->
                Expect.equal (Ok [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]) (RleParser.parse "3o!")
        , test "parses combination of dead and live cells" <|
            \_ ->
                Expect.equal (Ok [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]) (RleParser.parse "3o2b!")
        , test "parses multiple lines" <|
            \_ ->
                Expect.equal
                    (Ok
                        [ ( 0, 0 )
                        , ( 1, 0 )
                        , ( 2, 0 )
                        , ( 0, 1 )
                        , ( 2, 1 )
                        ]
                    )
                    (RleParser.parse "3o$obo!")
        , test "does not fail without trailing bang" <|
            \_ ->
                Expect.equal (Ok [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ) ]) (RleParser.parse "2o$ob")
        , test "errors on an invalid rle string" <|
            \_ ->
                Expect.err (RleParser.parse "$#df%%fsd$!$$")
        ]
