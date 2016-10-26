module View exposing (..)

import ReusableViews
import Html.Events exposing (onMouseDown)
import Html.Attributes exposing (src)
import Html
import Mouse exposing (Position)
import Json.Decode as Json
import Time
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Model exposing (..)
import Util
import RemixSpec


view model =
    let
        s =
            (ScaleInfo (hScale model.zoom) 0)
    in
        Html.div []
            ([ svg
                [ version "1.1"
                , x "0"
                , y "0"
                , viewBox ("0 0 " ++ (toString model.viewboxWidth) ++ " " ++ (toString model.viewboxHeight))
                ]
                ((List.indexedMap (\index source -> sourceView { s | y = (sourceYPos model source) } source (sourcePlayPosition model.playPosition source)) model.sources)
                    ++ (dragGuides model)
                    ++ [ (unusedClipsView (toFloat (List.length model.sources) * 15 + 15) (unscheduledClips model)) ]
                    ++ [ (scheduledClipsView (toFloat (List.length model.sources) * 15 + 15 + (toFloat (List.length (unscheduledClips model) * 15)))
                            (scheduledUses model)
                         )
                       ]
                )
             ]
                ++ List.indexedMap audioEmbed model.sourceUrls
                ++ [ Html.textarea [] [ text (RemixSpec.remixSpec model) ] ]
            )


targetDuration : Json.Decoder Float
targetDuration =
    Json.at [ "target", "duration" ] Json.float


onLoadedMetadata msg =
    Html.Events.on "loadedmetadata" (Json.map msg targetDuration)


targetCurrentTime : Json.Decoder Float
targetCurrentTime =
    Json.at [ "target", "currentTime" ] Json.float


onTimeUpdate msg =
    Html.Events.on "timeupdate" (Json.map msg targetCurrentTime)


audioEmbed i url =
    let
        cls =
            ("audio-source-" ++ (toString i))
    in
        Html.audio
            [ src url
            , onLoadedMetadata (AudioLengthKnown url cls)
            , onTimeUpdate (AudioOffsetUpdate url cls)
            , class cls
            ]
            []


sourceYPos : Model -> Source -> Float
sourceYPos model source =
    case Util.getIndex ((==) source) model.sources of
        Nothing ->
            Debug.crash "Don't know where to put nonexistant source"

        Just index ->
            toFloat (index * 15 + 10)


dragGuides : Model -> List (Svg Msg)
dragGuides model =
    let
        modClip =
            \clipId ->
                (allClipsWithSources model)
                    |> List.filter (\( clip, source ) -> clip.id == clipId)
                    |> List.map (\( clip, source ) -> ( clipWithDrag model clip, source ))
                    |> List.map (\( clip, source ) -> shadow (sourceYPos model source) clip)
    in
        case model.drag of
            Nothing ->
                []

            Just drag ->
                case drag.dragType of
                    MoveClipDrag clipId ->
                        modClip clipId

                    ResizeClipLeftDrag clipId ->
                        modClip clipId

                    ResizeClipRightDrag clipId ->
                        modClip clipId

                    NewClipDrag sourceId ->
                        model.sources
                            |> List.concatMap (\source -> (List.map (\clip -> ( clip, source )) (newClipsFromDrag model source)))
                            |> List.map (\( clip, source ) -> (shadow (sourceYPos model source) clip))


type alias ScaleInfo =
    { hScale : Float
    , y : Float
    }


sourceView : ScaleInfo -> Source -> Maybe Float -> Svg Msg
sourceView s source sourcePos =
    (g []
        ([ (rect
                [ fill "blue"
                , x "0"
                , y (toString s.y)
                , width (toString source.length)
                , height "10"
                , Html.Events.on "mousedown" (Json.map (DragStart (\pos -> Drag (NewClipDrag source.id) pos pos)) Mouse.position)
                ]
                []
           )
         , (text'
                [ x "2"
                , y (toString (s.y - 1))
                , fontFamily "Verdana"
                , fontSize "4"
                ]
                [ text source.url ]
           )
         ]
            ++ (List.map (clipWithResizeControls s.y) source.clips)
            ++ (case sourcePos of
                    Nothing ->
                        []

                    Just offset ->
                        [ rect
                            [ fill "red"
                            , x (toString offset)
                            , y (toString s.y)
                            , width "1"
                            , height "10"
                            ]
                            []
                        ]
               )
        )
    )


clipWithResizeControls : Float -> Clip -> Svg Msg
clipWithResizeControls yVal clip =
    (g []
        [ (rect
            [ fill "green"
            , stroke "black"
            , strokeWidth ".2"
            , x (toString clip.sourceStart)
            , y (toString yVal)
            , width (toString clip.length)
            , height "10"
            , class "moveCursor"
            , Html.Events.on "mousedown" (Json.map (DragStart (\pos -> Drag (MoveClipDrag clip.id) pos pos)) Mouse.position)
            ]
            []
          )
        , (rect
            [ fill "darkgreen"
            , x (toString clip.sourceStart)
            , y (toString (yVal - 3))
            , width "3"
            , height "5"
            , class "horzCursor"
            , Html.Events.on "mousedown" (Json.map (DragStart (\pos -> Drag (ResizeClipLeftDrag clip.id) pos pos)) Mouse.position)
            ]
            []
          )
        , (rect
            [ fill "darkgreen"
            , x (toString (clip.sourceStart + clip.length))
            , y (toString (yVal + 7))
            , width "3"
            , height "5"
            , class "horzCursor"
            , Html.Events.on "mousedown" (Json.map (DragStart (\pos -> Drag (ResizeClipRightDrag clip.id) pos pos)) Mouse.position)
            ]
            []
          )
        ]
    )


shadow : Float -> Clip -> Svg Msg
shadow yVal clip =
    (rect
        [ fill "gray"
        , opacity "0.5"
        , x (toString clip.sourceStart)
        , y (toString yVal)
        , width (toString clip.length)
        , height "10"
        ]
        []
    )


unusedClipRect : Float -> ( Clip, Source ) -> Svg Msg
unusedClipRect yVal ( clip, source ) =
    (g []
        [ (rect
            [ fill "green"
            , x "0"
            , y (toString yVal)
            , width (toString (clip.length))
            , height "10"
            , Html.Events.onClick (Schedule clip.id)
            ]
            []
          )
        , (line
            [ x1 (toString (clip.length / 2))
            , y1 (toString (yVal + 5))
            , x2 (toString (clip.sourceStart + clip.length / 2))
            , y2 (toString (0 + 5))
            , strokeWidth "0.2"
            , stroke "black"
            ]
            []
          )
        , (text'
            [ x (toString (clip.length + 2))
            , y (toString (yVal + 6))
            , fontFamily "Verdana"
            , fontSize "4"
            ]
            [ text (source.url ++ "   " ++ (Util.toClockTime clip.length)) ]
          )
        , (ReusableViews.deleteBoxView (clip.length + 2) yVal 2 2 (DeleteClip clip.id))
        ]
    )


usedClipRect : Float -> ( Int, Clip, Source ) -> Svg Msg
usedClipRect yOffset ( index, clip, source ) =
    let
        yVal =
            index * 20 + yOffset

        xVal =
            index * 20
    in
        (g []
            [ (rect
                [ fill "green"
                , x (toString xVal)
                , y (toString yVal)
                , width (toString (clip.length))
                , height "10"
                , Html.Events.onClick (UnSchedule index)
                ]
                []
              )
            , (text'
                [ x (toString xVal)
                , y (toString (yVal - 1))
                , fontFamily "Verdana"
                , fontSize "4"
                ]
                [ text (source.url ++ "   " ++ (Util.toClockTime clip.length)) ]
              )
            ]
        )


unusedClipsView : Float -> List ( Clip, Source ) -> Svg Msg
unusedClipsView yVal clipSourcePairs =
    (g []
        (List.indexedMap (\i ( clip, source ) -> (unusedClipRect (yVal + (toFloat i * 15)) ( clip, source )))
            clipSourcePairs
        )
    )


scheduledClipsView : Float -> List ( Int, Clip, Source ) -> Svg Msg
scheduledClipsView yVal uses =
    (g []
        (List.map (usedClipRect yVal) uses)
    )


hScale : Int -> Float
hScale zoom =
    (toFloat zoom) * (2 ^ (toFloat zoom))
