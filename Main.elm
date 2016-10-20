module Main exposing (..)

import AnimationFrame
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mouse exposing (Position)
import Time
import Html.Events exposing (onMouseDown)
import Html.Attributes exposing (src)
import Html
import Json.Decode as Json
import Window
import Task
import String


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    ([ Window.resizes WinSize ]
                        ++ case model.drag of
                            Nothing ->
                                []

                            Just _ ->
                                [ Mouse.moves DragAt
                                , Mouse.ups DragEnd
                                ]
                    )
        }



-- Since we have to keep track of drags ourselves, a drag handler should be able to specify
-- what kind of Drag, and what message it wants sent at DragEnd.
-- DragStart (Drag -> Msg) (Drag -> Msg) Position


type Msg
    = DragStart (Position -> Drag) Position
    | DragAt Position
    | DragEnd Position
    | WinSize Window.Size
    | NoOp
    | AudioLengthKnown String Float
    | Schedule Int
    | UnSchedule Int


init =
    ( { sources = []
      , sourceUrls = [ "audio1.ogg", "audio2.ogg", "audio3.ogg" ]
      , drag = Nothing
      , windowSize = ( 100, 100 )
      , viewboxWidth = 400.0
      , viewboxHeight = 200.0
      }
    , Task.perform (\_ -> NoOp) WinSize Window.size
    )


allClips : Model -> List Clip
allClips model =
    List.concatMap (.clips) model.sources


allClipsWithSources : Model -> List ( Clip, Source )
allClipsWithSources model =
    List.concatMap (\source -> List.map (\clip -> ( clip, source )) source.clips) model.sources


type alias Model =
    { sources : List Source
    , sourceUrls : List String
    , drag : Maybe Drag
    , windowSize : ( Int, Int )
    , viewboxWidth : Float
    , viewboxHeight : Float
    }


type alias Source =
    { id : Int
    , url : String
    , length : Float
    , clips : List Clip
    , yPos : Float
    }


type alias Clip =
    { id : Int
    , sourceStart : Float
    , length : Float
    , start : Float
    , uses : List Int
    }


type alias Drag =
    { dragType : DragType
    , start : Position
    , current : Position
    }


type DragType
    = MoveClipDrag Int
    | NewClipDrag Int
    | ResizeClipLeftDrag Int
    | ResizeClipRightDrag Int


newClipsFromDrag : Model -> Source -> List Clip
newClipsFromDrag model source =
    case model.drag of
        Nothing ->
            []

        Just drag ->
            case drag.dragType of
                NewClipDrag sourceId ->
                    let
                        ( winWidth, winHeight ) =
                            model.windowSize

                        widthRatio =
                            model.viewboxWidth
                                / (toFloat winWidth)

                        heightRatio =
                            model.viewboxHeight / (toFloat winHeight)

                        end =
                            (Basics.max (toFloat drag.start.x) (toFloat drag.current.x)) * widthRatio

                        start =
                            (Basics.min (toFloat drag.start.x) (toFloat drag.current.x)) * widthRatio
                    in
                        if source.id == sourceId then
                            [ (Clip (nextId model) start (end - start) start []) ]
                        else
                            []

                _ ->
                    []


clipWithDrag : Model -> Clip -> Clip
clipWithDrag model clip =
    let
        ( dx, dxLeft, dxRight, clipId ) =
            case model.drag of
                Nothing ->
                    ( 0, 0, 0, -1 )

                Just drag ->
                    case drag.dragType of
                        MoveClipDrag clipId ->
                            ( ((toFloat drag.current.x) - (toFloat drag.start.x)), 0, 0, clipId )

                        ResizeClipLeftDrag clipId ->
                            ( 0, ((toFloat drag.current.x) - (toFloat drag.start.x)), 0, clipId )

                        ResizeClipRightDrag clipId ->
                            ( 0, 0, ((toFloat drag.current.x) - (toFloat drag.start.x)), clipId )

                        _ ->
                            ( 0, 0, 0, -1 )
    in
        if clipId == clip.id then
            clipMovedByScreenX model dx dxLeft dxRight clip
        else
            clip


clipMovedByScreenX : Model -> Float -> Float -> Float -> Clip -> Clip
clipMovedByScreenX model dx dxLeft dxRight clip =
    let
        ( winWidth, winHeight ) =
            model.windowSize

        widthRatio =
            model.viewboxWidth
                / (toFloat winWidth)

        heightRatio =
            model.viewboxHeight / (toFloat winHeight)

        a =
            clip.start + (dx + dxLeft) * widthRatio

        b =
            clip.start + clip.length + (dx + dxRight) * widthRatio
    in
        { clip
            | start = Basics.min a b
            , length = abs (a - b)
        }


nextId : Model -> Int
nextId model =
    let
        ids =
            (List.map (.id) model.sources)
                ++ (List.concatMap (\source -> List.map (.id) source.clips) model.sources)
    in
        case List.maximum (ids) of
            Nothing ->
                0

            Just id ->
                id + 1


updateClips : (Clip -> Clip) -> List Source -> List Source
updateClips update sources =
    List.map
        (\source ->
            { source
                | clips =
                    List.map update
                        source.clips
            }
        )
        sources


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( (updateHelp msg model), Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg model =
    case msg of
        DragStart dragBuilder xy ->
            { model | drag = Just (dragBuilder xy) }

        DragAt xy ->
            case model.drag of
                Nothing ->
                    model

                Just drag ->
                    { model | drag = (Just { drag | current = xy }) }

        DragEnd position ->
            case noDragIfSmall model.drag of
                Nothing ->
                    { model | drag = Nothing }

                Just drag ->
                    let
                        updatedWithDrag clip model =
                            { model
                                | sources =
                                    model.sources
                                        |> updateClips (clipWithDrag model)
                            }

                        afterDrag =
                            case drag.dragType of
                                MoveClipDrag _ ->
                                    updatedWithDrag drag model

                                ResizeClipLeftDrag _ ->
                                    updatedWithDrag drag model

                                ResizeClipRightDrag _ ->
                                    updatedWithDrag drag model

                                NewClipDrag sourceId ->
                                    { model
                                        | sources =
                                            List.map
                                                (\source ->
                                                    { source | clips = source.clips ++ (newClipsFromDrag model source) }
                                                )
                                                model.sources
                                    }
                    in
                        { afterDrag | drag = Nothing }

        WinSize size ->
            { model | windowSize = ( size.width, size.height ) }

        NoOp ->
            model

        AudioLengthKnown url length ->
            { model
                | sources =
                    (Source (nextId model) url length [] (toFloat (List.length model.sources) * 15 + 10))
                        :: model.sources
            }

        Schedule clipId ->
            scheduleClipAtEnd clipId model

        UnSchedule index ->
            unscheduleClip index model


noDragIfSmall : Maybe Drag -> Maybe Drag
noDragIfSmall drag =
    case drag of
        Nothing ->
            Nothing

        Just drag ->
            if drag.start == drag.current then
                Nothing
            else
                Just drag


scheduledUses : Model -> List ( Int, Clip, Source )
scheduledUses model =
    allClipsWithSources model
        |> List.concatMap (\( clip, source ) -> (List.map (\use -> ( use, clip, source )) clip.uses))
        |> List.sortBy (\( use, clip, source ) -> use)


unscheduledClips : Model -> List ( Clip, Source )
unscheduledClips model =
    allClipsWithSources model
        |> List.filter (\( clip, source ) -> (List.length clip.uses) == 0)


unscheduleClip : Int -> Model -> Model
unscheduleClip index model =
    { model
        | sources =
            updateClips
                (\clip ->
                    { clip
                        | uses =
                            clip.uses
                                |> List.filter (\i -> i /= index)
                                |> List.map
                                    (\i ->
                                        if i > index then
                                            i - 1
                                        else
                                            i
                                    )
                    }
                )
                model.sources
    }


scheduleClipAtEnd : Int -> Model -> Model
scheduleClipAtEnd clipId model =
    { model
        | sources =
            List.map
                (\source ->
                    { source
                        | clips =
                            List.map
                                (\clip ->
                                    if clip.id == clipId then
                                        { clip | uses = ((nextScheduledId model) :: clip.uses) }
                                    else
                                        clip
                                )
                                source.clips
                    }
                )
                model.sources
    }


nextScheduledId : Model -> Int
nextScheduledId model =
    (Maybe.withDefault 0 (List.maximum (List.concatMap .uses (allClips model)))) + 1



-- Maybe each clip knows where it appears
-- Sources have Clips, Clips have Uses - nah how about they only have one use? Lists are easier to deal with I guess.
-- But the common use is that clips have 1 or 0 uses.
-- Layout: Sources with clips, then list of all unused clips, then layout of used clips.
-- Vie


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
            , x2 (toString (clip.start + clip.length / 2))
            , y2 (toString (source.yPos + 5))
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
            [ text (source.url ++ "   " ++ (floatToTime clip.length)) ]
          )
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
                [ text (source.url ++ "   " ++ (floatToTime clip.length)) ]
              )
            ]
        )


floatToTime : Float -> String
floatToTime t =
    (toString ((round t) // 60)) ++ ":" ++ (String.padLeft 2 '0' (toString ((round t) % 60))) ++ "." ++ (toString (round (t * 100) % 100))


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


view model =
    Html.div []
        ([ svg
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox ("0 0 " ++ (toString model.viewboxWidth) ++ " " ++ (toString model.viewboxHeight))
            ]
            ((List.map sourceView model.sources)
                ++ (dragGuides model)
                ++ [ (unusedClipsView (toFloat (List.length model.sources) * 15 + 15) (unscheduledClips model)) ]
                ++ [ (scheduledClipsView (toFloat (List.length model.sources) * 15 + 15 + (toFloat (List.length (unscheduledClips model) * 15)))
                        (scheduledUses model)
                     )
                   ]
            )
         ]
            ++ List.map audioEmbed model.sourceUrls
        )


targetDuration : Json.Decoder Float
targetDuration =
    Json.at [ "target", "duration" ] Json.float


onLoadedMetadata msg =
    Html.Events.on "loadedmetadata" (Json.map msg targetDuration)


audioEmbed url =
    Html.audio
        [ src url
        , onLoadedMetadata (AudioLengthKnown url)
        ]
        []


dragGuides : Model -> List (Svg Msg)
dragGuides model =
    let
        modClip =
            \clipId ->
                (allClipsWithSources model)
                    |> List.filter (\( clip, source ) -> clip.id == clipId)
                    |> List.map (\( clip, source ) -> ( clipWithDrag model clip, source ))
                    |> List.map (\( clip, source ) -> shadow source.yPos clip)
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
                            |> List.map (\( clip, source ) -> (shadow source.yPos clip))


sourceView : Source -> Svg Msg
sourceView source =
    (g []
        ([ (rect
                [ fill "blue"
                , x "0"
                , y (toString source.yPos)
                , width (toString source.length)
                , height "10"
                , Html.Events.on "mousedown" (Json.map (DragStart (\pos -> Drag (NewClipDrag source.id) pos pos)) Mouse.position)
                ]
                []
           )
         , (text'
                [ x "2"
                , y (toString (source.yPos - 1))
                , fontFamily "Verdana"
                , fontSize "4"
                ]
                [ text source.url ]
           )
         ]
            ++ (List.map (clipWithResizeControls source.yPos) source.clips)
        )
    )


clipWithResizeControls : Float -> Clip -> Svg Msg
clipWithResizeControls yVal clip =
    (g []
        [ (rect
            [ fill "green"
            , stroke "black"
            , strokeWidth ".2"
            , x (toString clip.start)
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
            , x (toString clip.start)
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
            , x (toString (clip.start + clip.length))
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
        , x (toString clip.start)
        , y (toString yVal)
        , width (toString clip.length)
        , height "10"
        ]
        []
    )
