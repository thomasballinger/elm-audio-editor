module Util exposing (..)

import String


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


toClockTime : Float -> String
toClockTime t =
    let
        whole =
            round t

        hours =
            whole // (60 * 60)

        minutes =
            whole // (60) % 60

        seconds =
            whole % 60

        hundreths =
            floor (t * 100)
    in
        String.join ""
            [ (if hours > 0 then
                (toString whole) ++ ":"
               else
                ""
              )
            , (if hours > 0 then
                (String.slice -3 -1 ("0" ++ (toString minutes) ++ "!"))
               else
                toString minutes
              )
            , ":"
            , (String.slice -3 -1 ("0" ++ (toString seconds) ++ "!"))
            , "."
            , (String.slice -3 -1 ("0" ++ (toString hundreths) ++ "!"))
            ]
