module ReusableViews exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events


deleteBoxView : Float -> Float -> Float -> Float -> a -> Svg a
deleteBoxView xVal yVal w h msg =
    let
        left =
            xVal - w / 2

        top =
            yVal - w / 2

        right =
            xVal + w / 2

        bottom =
            yVal + w / 2
    in
        (g []
            [ (rect
                [ fill "red"
                , x (toString left)
                , y (toString top)
                , width (toString w)
                , height (toString h)
                , Html.Events.onClick msg
                ]
                []
              )
            ]
        )
