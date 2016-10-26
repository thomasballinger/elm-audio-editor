port module Main exposing (..)

import Html.App as App
import Mouse exposing (Position)
import Window
import Task
import Keyboard
import Char
import Util
import Model exposing (..)
import View


port playAt : ( String, Float ) -> Cmd msg


port pause : Int -> Cmd msg


main =
    App.programWithFlags
        { init = init
        , view = View.view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    ([ Window.resizes WinSize
                     , Keyboard.presses Keypress
                     ]
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


init : { urlBase : String } -> ( Model, Cmd Msg )
init flags =
    ( { sources = []
      , sourceUrls = [ "audio1.ogg", "audio2.ogg", "audio3.ogg" ]
      , drag = Nothing
      , windowSize = ( 100, 100 )
      , urlBase = flags.urlBase
      , viewboxWidth =
            400.0
            --TODO make these 1, then remove?
      , viewboxHeight = 200.0
      , playPosition = Nothing
      , playStatus = Paused
      , zoom = 0
      }
    , Cmd.batch [ Task.perform (\_ -> NoOp) WinSize Window.size ]
    )


dxToDuration : Model -> Float -> Float
dxToDuration model dx =
    let
        ( winWidth, winHeight ) =
            model.windowSize

        widthRatio =
            model.viewboxWidth
                / (toFloat winWidth)
    in
        dx * widthRatio * 1



--TODO test ^this hScale (1), might be inverse


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
                            updatedWithDrag : Model -> Model
                            updatedWithDrag model =
                                { model
                                    | sources =
                                        model.sources
                                            |> updateClips (clipWithDrag model)
                                    , drag = Nothing
                                }

                            ( afterDrag, clipId, offset ) =
                                case drag.dragType of
                                    MoveClipDrag clipId ->
                                        ( updatedWithDrag model, clipId, 0.0 )

                                    ResizeClipLeftDrag clipId ->
                                        ( updatedWithDrag model, clipId, 0.0 )

                                    ResizeClipRightDrag clipId ->
                                        ( updatedWithDrag model, clipId, -1.0 )

                                    NewClipDrag sourceId ->
                                        let
                                            newModel =
                                                { model
                                                    | sources =
                                                        List.map
                                                            (\source ->
                                                                { source | clips = source.clips ++ (newClipsFromDrag model source) }
                                                            )
                                                            model.sources
                                                    , drag = Nothing
                                                }
                                        in
                                            ( newModel, maxId newModel, 0.0 )
                        in
                            let
                                clip =
                                    (clipById afterDrag clipId)
                            in
                                case clip of
                                    Nothing ->
                                        ( afterDrag, Cmd.none )

                                    Just c ->
                                        if offset < 0.0 then
                                            update (PlayClip c (Basics.max 0 (c.length + offset))) afterDrag
                                        else
                                            update (PlayClip c offset) afterDrag

        WinSize size ->
            ( { model | windowSize = ( size.width, size.height ) }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        AudioLengthKnown url elClass length ->
            ( { model
                | sources =
                    (Source (nextId model) url elClass length [])
                        :: model.sources
              }
            , Cmd.none
            )

        AudioOffsetUpdate url elClass offset ->
            case List.head (model.sources |> List.filter (\src -> src.url == url)) of
                Nothing ->
                    ( { model
                        | playPosition = Nothing
                        , playStatus = Paused
                      }
                    , pause 1
                    )

                Just source ->
                    updatePlayPos source offset model

        Schedule clipId ->
            ( scheduleClipAtEnd clipId model, Cmd.none )

        UnSchedule index ->
            ( unscheduleClip index model, Cmd.none )

        DeleteClip clipId ->
            ( deleteClip clipId model, Cmd.none )

        Keypress code ->
            ( case Char.fromCode code of
                '=' ->
                    { model | zoom = model.zoom + 1 }

                '+' ->
                    { model | zoom = model.zoom + 1 }

                '-' ->
                    { model | zoom = model.zoom - 1 }

                '0' ->
                    { model | zoom = 0 }

                _ ->
                    model
            , Cmd.none
            )

        PlaySource source offset ->
            ( { model
                | playStatus = Playing
                , playPosition = Just (SourcePos { sourceId = source.id, offset = dxToDuration model offset })
              }
            , playAt ( source.elementClass, dxToDuration model offset )
            )

        PlayClip clip offset ->
            case clipSource model clip.id of
                Nothing ->
                    ( model, Cmd.none )

                Just source ->
                    ( { model
                        | playStatus = Playing
                        , playPosition = Just (ClipPos { sourceId = source.id, clipId = clip.id, offset = offset })
                      }
                    , playAt ( source.elementClass, clip.sourceStart + offset )
                    )


updatePlayPos : Source -> Float -> Model -> ( Model, Cmd Msg )
updatePlayPos source offset model =
    case model.playPosition of
        Nothing ->
            ( model, Cmd.none )

        Just (SourcePos { sourceId }) ->
            if source.id == sourceId then
                -- TODO check for reaching of the end here?
                ( { model | playPosition = Just (SourcePos { sourceId = sourceId, offset = offset }) }, Cmd.none )
            else
                ( model, Cmd.none )

        Just (ClipPos { sourceId, clipId }) ->
            if source.id == sourceId && List.member clipId (List.map .id source.clips) then
                case clipById model clipId of
                    Nothing ->
                        Debug.crash "we just checked that it existed!"

                    Just clip ->
                        let
                            clipOffset =
                                offset - clip.sourceStart
                        in
                            if clipOffset > clip.length then
                                ( { model
                                    | playPosition = Nothing
                                    , playStatus = Paused
                                  }
                                , (pause 1)
                                )
                            else
                                ( { model | playPosition = Just (ClipPos { sourceId = sourceId, clipId = clipId, offset = clipOffset }) }, Cmd.none )
            else
                ( { model
                    | playPosition = Just (SourcePos { sourceId = sourceId, offset = offset })
                    , playStatus = Playing
                  }
                , Cmd.none
                )

        Just _ ->
            ( model, Cmd.none )


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

            Just s ->
                (PlaySource s (toFloat drag.start.x))
