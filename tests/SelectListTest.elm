module SelectListTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import SelectList exposing (SelectList)


mock : SelectList String
mock =
    { preceding = [ "previous" ]
    , current = "current"
    , following = [ "next" ]
    }


mock2 =
    { preceding = [ 3, 2, 1, 0 ]
    , current = 4
    , following = [ 5, 6, 7, 8 ]
    }


suite : Test
suite =
    describe "SelectList"
        [ test "singleton" <|
            \_ ->
                let
                    expected =
                        { preceding = []
                        , current = "current"
                        , following = []
                        }
                in
                    "current"
                        |> SelectList.singleton
                        |> Expect.equal expected
        , test "fromList" <|
            \_ ->
                let
                    expected =
                        Just
                            { preceding = []
                            , current = 0
                            , following = [ 1, 2 ]
                            }
                in
                    [ 0, 1, 2 ]
                        |> SelectList.fromList
                        |> Expect.equal expected
        , test "preceding" <|
            \_ ->
                mock2
                    |> SelectList.preceding
                    |> Expect.equal [ 0, 1, 2, 3 ]
        , test "current" <|
            \_ ->
                mock2
                    |> SelectList.current
                    |> Expect.equal 4
        , test "following" <|
            \_ ->
                mock2
                    |> SelectList.following
                    |> Expect.equal [ 5, 6, 7, 8 ]
        , test "toList" <|
            \_ ->
                let
                    expected =
                        [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
                in
                    mock2
                        |> SelectList.toList
                        |> Expect.equal expected
        , test "next" <|
            \_ ->
                let
                    expected =
                        { preceding = [ "current", "previous" ]
                        , current = "next"
                        , following = []
                        }
                in
                    mock
                        |> SelectList.next
                        |> Expect.equal expected
        , test "jump 3" <|
            \_ ->
                let
                    expected =
                        { preceding = [ 6, 5, 4, 3, 2, 1, 0 ]
                        , current = 7
                        , following = [ 8 ]
                        }
                in
                    mock2
                        |> SelectList.jump 3
                        |> Expect.equal expected
        , test "jump back 3" <|
            \_ ->
                let
                    expected =
                        { preceding = [ 0 ]
                        , current = 1
                        , following = [ 2, 3, 4, 5, 6, 7, 8 ]
                        }
                in
                    mock2
                        |> SelectList.jump -3
                        |> Expect.equal expected
        , test "jump 10 (out of bounds)" <|
            \_ ->
                let
                    expected =
                        { preceding = [ 7, 6, 5, 4, 3, 2, 1, 0 ]
                        , current = 8
                        , following = []
                        }
                in
                    mock2
                        |> SelectList.jump 10
                        |> Expect.equal expected
        , test "previous" <|
            \_ ->
                let
                    expected =
                        { preceding = []
                        , current = "previous"
                        , following = [ "current", "next" ]
                        }
                in
                    mock
                        |> SelectList.previous
                        |> Expect.equal expected
        , test "append" <|
            \_ ->
                let
                    expected =
                        { preceding = [ "previous" ]
                        , current = "current"
                        , following = [ "new", "next" ]
                        }
                in
                    mock
                        |> SelectList.append "new"
                        |> Expect.equal expected
        , test "prepend" <|
            \_ ->
                let
                    expected =
                        { preceding = [ "new", "previous" ]
                        , current = "current"
                        , following = [ "next" ]
                        }
                in
                    mock
                        |> SelectList.prepend "new"
                        |> Expect.equal expected
        , test "size" <|
            \_ ->
                mock2
                    |> SelectList.size
                    |> Expect.equal 9
        , test "map" <|
            \_ ->
                let
                    expected =
                        { preceding = [ 5, 4, 3, 2 ]
                        , current = 6
                        , following = [ 7, 8, 9, 10 ]
                        }
                in
                    SelectList.map ((+) 2) mock2
                        |> Expect.equal expected
        , test "updateCurrent" <|
            \_ ->
                let
                    expected =
                        { preceding = [ 3, 2, 1, 0 ]
                        , current = 6
                        , following = [ 5, 6, 7, 8 ]
                        }
                in
                    SelectList.updateCurrent ((+) 2) mock2
                        |> Expect.equal expected
        , test "removeCurrent" <|
            \_ ->
                let
                    expected =
                        { preceding = [ 3, 2, 1, 0 ]
                        , current = 5
                        , following = [ 6, 7, 8 ]
                        }
                in
                    SelectList.removeCurrent mock2
                        |> Expect.equal expected
        , test "removeCurrent with no following" <|
            \_ ->
                let
                    localmock =
                        { preceding = [ 3, 2, 1, 0 ]
                        , current = 4
                        , following = []
                        }

                    expected =
                        { preceding = [ 2, 1, 0 ]
                        , current = 3
                        , following = []
                        }
                in
                    SelectList.removeCurrent localmock
                        |> Expect.equal expected
        , test "removeCurrent with singleton" <|
            \_ ->
                let
                    localmock =
                        { preceding = []
                        , current = 4
                        , following = []
                        }
                in
                    SelectList.removeCurrent localmock
                        |> Expect.equal localmock
        ]
