module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Util


all : Test
all =
    describe "utils"
        [ describe "getIndex"
            [ test "finding index of number"
                <| \() -> Expect.equal (Util.getIndex ((==) 3) [ 1, 2, 3 ]) (Just 2)
            , test "not finding index of number"
                <| \() -> Expect.equal (Util.getIndex ((==) 4) [ 1, 2, 3 ]) Nothing
            ]
        ]
