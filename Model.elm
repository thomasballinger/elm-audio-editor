module Model exposing (..)

import Window
import Keyboard
import Mouse exposing (Position)


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
    | PlayClip Clip Float
    | DeleteClip Int
    | Keypress Keyboard.KeyCode


type alias Model =
    { sources : List Source
    , sourceUrls : List String
    , drag : Maybe Drag
    , windowSize : ( Int, Int )
    , urlBase : String
    , viewboxWidth : Float
    , viewboxHeight : Float
    , playPosition : Maybe PlayPosition
    , playStatus : PlayStatus
    , zoom : Int
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



-- Clip functions


allClips : Model -> List Clip
allClips model =
    List.concatMap (.clips) model.sources


allClipsWithSources : Model -> List ( Clip, Source )
allClipsWithSources model =
    List.concatMap (\source -> List.map (\clip -> ( clip, source )) source.clips) model.sources


nextId : Model -> Int
nextId model =
    (maxId model) + 1


maxId : Model -> Int
maxId model =
    let
        ids =
            (List.map (.id) model.sources)
                ++ (List.concatMap (\source -> List.map (.id) source.clips) model.sources)
    in
        case List.maximum (ids) of
            Nothing ->
                0

            Just id ->
                id


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


deleteClip : Int -> Model -> Model
deleteClip clipId model =
    { model
        | sources =
            List.map
                (\source ->
                    { source
                        | clips =
                            List.filter (\clip -> clip.id /= clipId)
                                source.clips
                    }
                )
                model.sources
    }


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


clipById : Model -> Int -> Maybe Clip
clipById model clipId =
    model.sources
        |> List.concatMap .clips
        |> List.filter (\clip -> clip.id == clipId)
        |> List.head



-- Source Functions


sourceById : Model -> Int -> Maybe Source
sourceById model sourceId =
    model.sources
        |> List.filter (\source -> source.id == sourceId)
        |> List.head


sourceAndClipByClipId : Model -> Int -> Maybe ( Source, Clip )
sourceAndClipByClipId model clipId =
    model.sources
        |> List.concatMap (\source -> (List.map (\clip -> ( source, clip )) source.clips))
        |> List.filter (\( source, clip ) -> clip.id == clipId)
        |> List.head


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

        Just (ClipPos clipPosData) ->
            case List.head (List.filter (\c -> c.id == clipPosData.clipId) source.clips) of
                Nothing ->
                    Nothing

                Just clip ->
                    if clipPosData.sourceId == source.id then
                        Just (clipPosData.offset + clip.sourceStart)
                    else
                        Nothing

        Just _ ->
            Nothing



-- Scheduling Clips


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



--- Uh, Drag?


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
                            [ (Clip (nextId model) start (end - start) 0 []) ]
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



--TODO get rid of any screen translations in Model.elm


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
            clip.sourceStart + (dx + dxLeft) * widthRatio

        b =
            clip.sourceStart + clip.length + (dx + dxRight) * widthRatio
    in
        { clip
            | sourceStart = Basics.min a b
            , length = abs (a - b)
        }
