port module Main exposing (..)

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


port playAt : ( String, Float ) -> Cmd msg


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
    | AudioLengthKnown String String Float
    | AudioOffsetUpdate String String Float
    | Schedule Int
    | UnSchedule Int
    | PlaySource Source Float


init =
    ( { sources = []
      , sourceUrls = [ "audio1.ogg", "audio2.ogg", "audio3.ogg" ]
      , drag = Nothing
      , windowSize = ( 100, 100 )
      , viewboxWidth = 400.0
      , viewboxHeight = 200.0
      , playPosition = Nothing
      , playStatus = Paused
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
    , playPosition : Maybe PlayPosition
    , playStatus : PlayStatus
    }


type PlayPosition
    = SourcePos { sourceId : Int, offset : Float }
    | ClipPos { sourceId : Int, clipId : Int, offset : Float }
    | MixPos { offset : Float }


type PlayStatus
    = Playing
    | Paused


type alias Source =
    { id : Int
    , url : String
    , elementClass : String
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


dxToDuration : Model -> Float -> Float
dxToDuration model dx =
    let
        ( winWidth, winHeight ) =
            model.windowSize

        widthRatio =
            model.viewboxWidth
                / (toFloat winWidth)
    in
        dx * widthRatio


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


clipSource : Model -> Int -> Maybe Source
clipSource model clipId =
    model.sources
        |> List.filter
            (\source ->
                (source.clips
                    |> List.filter (\clip -> clip.id == clipId)
                    |> List.length
                )
                    == 1
            )
        |> List.head


sourceById : Model -> Int -> Maybe Source
sourceById model sourceId =
    model.sources
        |> List.filter (\source -> source.id == sourceId)
        |> List.head


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart dragBuilder xy ->
            ( { model | drag = Just (dragBuilder xy) }, Cmd.none )

        DragAt xy ->
            ( case model.drag of
                Nothing ->
                    model

                Just drag ->
                    { model | drag = (Just { drag | current = xy }) }
            , Cmd.none
            )

        DragEnd position ->
            case model.drag of
                Nothing ->
                    ( { model | drag = Nothing }, Cmd.none )

                Just drag ->
                    if dragIsSmall drag then
                        let
                            ( newValue, newCmd ) =
                                update (clickFromDrag model drag) { model | drag = Nothing }
                        in
                            ( newValue, newCmd )
                    else
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
                            ( { afterDrag | drag = Nothing }, Cmd.none )

        WinSize size ->
            ( { model | windowSize = ( size.width, size.height ) }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        AudioLengthKnown url elClass length ->
            ( { model
                | sources =
                    (Source (nextId model) url elClass length [] (toFloat (List.length model.sources) * 15 + 10))
                        :: model.sources
              }
            , Cmd.none
            )

        AudioOffsetUpdate url elClass offset ->
            case List.head (model.sources |> List.filter (\src -> src.url == url)) of
                Nothing ->
                    ( model, Cmd.none )

                Just source ->
                    ( updatePlayPos source offset model, Cmd.none )

        Schedule clipId ->
            ( scheduleClipAtEnd clipId model, Cmd.none )

        UnSchedule index ->
            ( unscheduleClip index model, Cmd.none )

        PlaySource source offset ->
            ( { model
                | playStatus = Playing
                , playPosition = Just (SourcePos { sourceId = source.id, offset = dxToDuration model offset })
              }
            , playAt ( source.elementClass, dxToDuration model offset )
            )


updatePlayPos : Source -> Float -> Model -> Model
updatePlayPos source offset model =
    case model.playPosition of
        Nothing ->
            model

        Just (SourcePos { sourceId }) ->
            if source.id == sourceId then
                { model | playPosition = Just (SourcePos { sourceId = sourceId, offset = offset }) }
            else
                model

        Just _ ->
            model


dragIsSmall : Drag -> Bool
dragIsSmall drag =
    drag.start == drag.current


clickFromDrag : Model -> Drag -> Msg
clickFromDrag model drag =
    let
        source =
            case drag.dragType of
                NewClipDrag sourceId ->
                    sourceById model sourceId

                MoveClipDrag clipId ->
                    clipSource model clipId

                ResizeClipLeftDrag clipId ->
                    clipSource model clipId

                ResizeClipRightDrag clipId ->
                    clipSource model clipId
    in
        case source of
            Nothing ->
                NoOp

            Just src ->
                (PlaySource src (toFloat drag.start.x))


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


sourcePlayPosition : Maybe PlayPosition -> Source -> Maybe Float
sourcePlayPosition playPos source =
    case playPos of
        Nothing ->
            Nothing

        Just (SourcePos data) ->
            if data.sourceId == source.id then
                Just data.offset
            else
                Nothing

        Just _ ->
            Nothing


view model =
    Html.div []
        ([ svg
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox ("0 0 " ++ (toString model.viewboxWidth) ++ " " ++ (toString model.viewboxHeight))
            ]
            ((List.map (\source -> sourceView source (sourcePlayPosition model.playPosition source)) model.sources)
                ++ (dragGuides model)
                ++ [ (unusedClipsView (toFloat (List.length model.sources) * 15 + 15) (unscheduledClips model)) ]
                ++ [ (scheduledClipsView (toFloat (List.length model.sources) * 15 + 15 + (toFloat (List.length (unscheduledClips model) * 15)))
                        (scheduledUses model)
                     )
                   ]
            )
         ]
            ++ List.indexedMap audioEmbed model.sourceUrls
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


sourceView : Source -> Maybe Float -> Svg Msg
sourceView source sourcePos =
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
            ++ (case sourcePos of
                    Nothing ->
                        []

                    Just offset ->
                        [ rect
                            [ fill "red"
                            , x (toString offset)
                            , y (toString source.yPos)
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
