module Util exposing (..)


getIndex : (a -> Bool) -> List a -> Maybe Int
getIndex pred l =
    case l of
        [] ->
            Nothing

        head :: tail ->
            if (pred head) then
                Just 0
            else
                Maybe.map ((+) 1) (getIndex pred tail)
