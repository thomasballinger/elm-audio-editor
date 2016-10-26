module RemixSpec exposing (..)

import String
import Util
import Model exposing (..)


remixSpec : Model -> String
remixSpec model =
    let
        uses =
            scheduledUses model

        acc =
            ( -1, [] )

        ( _, body ) =
            List.foldl
                (\( _, clip, source ) ( prevSourceId, lines ) ->
                    ( source.id
                    , (lines
                        ++ (if source.id /= prevSourceId then
                                [ "url = \"" ++ model.urlBase ++ source.url ++ "\"" ]
                            else
                                []
                           )
                        ++ [ "play from " ++ (Util.toClockTime clip.sourceStart) ++ " to " ++ (Util.toClockTime (clip.sourceStart + clip.length)) ]
                      )
                    )
                )
                acc
                uses
    in
        "remix version 0.1\n\n" ++ (String.join "\n" body)
