module Main exposing (..)

import AnimationFrame
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mouse exposing (Position)
import Time
import Html.Events exposing (onMouseDown)
import Json.Decode as Json
import Window
import Task


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
                            --Nothing -> AnimationFrame.times Tick
                            Nothing ->
                                []

                            Just drag ->
                                [ Mouse.moves (DragAt drag.clipId)
                                , Mouse.ups (DragEnd drag.clipId)
                                ]
                    )
        }


type
    Msg
    --    = Tick Time.Time
    = DragStart Int Position
    | DragAt Int Position
    | DragEnd Int Position
    | WinSize Window.Size
    | NoOp


init =
    ( { sources = [ Source "The star spangled banner" [ (Clip 0 0 120 10) ] ]
      , drag = Nothing
      , windowSize = ( 100, 100 )
      , viewboxWidth = 200.0
      , viewboxHeight = 200.0
      }
    , Task.perform (\_ -> NoOp) WinSize Window.size
    )


allClips : Model -> List Clip
allClips model =
    List.concatMap (.clips) model.sources


type alias Model =
    { sources : List Source
    , drag : Maybe Drag
    , windowSize : ( Int, Int )
    , viewboxWidth : Float
    , viewboxHeight : Float
    }


type alias Source =
    { url : String
    , clips : List Clip
    }


type alias Clip =
    { id : Int
    , sourceStart : Float
    , length : Float
    , start : Float
    }


type alias Drag =
    { clipId : Int
    , start : Position
    , current : Position
    }


clipWithDrag : Model -> Clip -> Clip
clipWithDrag model clip =
    case model.drag of
        Just drag ->
            let
                ( winWidth, winHeight ) =
                    model.windowSize

                widthRatio =
                    model.viewboxWidth
                        / (toFloat winWidth)

                heightRatio =
                    model.viewboxHeight / (toFloat winHeight)
            in
                { clip | start = clip.start + ((toFloat drag.current.x) - (toFloat drag.start.x)) * widthRatio }

        Nothing ->
            clip


applyDrag : Model -> Model
applyDrag model =
    case model.drag of
        Nothing ->
            model

        Just drag ->
            { model
                | sources =
                    updateClips
                        (\clip ->
                            if clip.id == drag.clipId then
                                clipWithDrag model clip
                            else
                                clip
                        )
                        model.sources
                , drag = Nothing
            }


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
        DragStart id xy ->
            { model | drag = Just (Drag id xy xy) }

        DragAt id xy ->
            { model | drag = (Maybe.map (\{ start, clipId } -> Drag clipId start xy) model.drag) }

        --TODO this update should be based on the current zoom
        DragEnd id _ ->
            applyDrag model

        WinSize size ->
            { model | windowSize = ( size.width, size.height ) }

        NoOp ->
            model



-- View


clipView : Clip -> Svg Msg
clipView =
    clipRectOfOpacityColor "1.0" "green"


view model =
    svg
        [ version "1.1"
        , x "0"
        , y "0"
        , viewBox ("0 0 " ++ (toString model.viewboxWidth) ++ " " ++ (toString model.viewboxHeight))
        ]
        ((List.map clipView (allClips model)) ++ (dragGuides model))


dragGuides : Model -> List (Svg Msg)
dragGuides model =
    case model.drag of
        Nothing ->
            []

        Just drag ->
            (allClips model)
                |> List.filter (\clip -> clip.id == drag.clipId)
                |> List.map (clipWithDrag model)
                |> List.map shadow


clipRectOfOpacityColor : String -> String -> Clip -> Svg Msg
clipRectOfOpacityColor o c clip =
    (rect
        [ fill c
        , opacity o
        , x (toString clip.start)
        , y "107.392"
        , width (toString clip.length)
        , height "10"
        , Html.Events.on "mousedown" (Json.map (DragStart clip.id) Mouse.position)
        ]
        []
    )


shadow : Clip -> Svg Msg
shadow =
    clipRectOfOpacityColor "0.5" "gray"


nop =
    svg
        [ version "1.1"
        , x "0"
        , y "0"
        , viewBox "0 0 323.141 322.95"
        ]
        [ polygon [ fill "#F0AD00", points "161.649,152.782 231.514,82.916 91.783,82.916" ] []
        , polygon [ fill "#7FD13B", points "8.867,0 79.241,70.375 232.213,70.375 161.838,0" ] []
        , rect
            [ fill "#7FD13B"
            , x "192.99"
            , y "107.392"
            , width "107.676"
            , height "108.167"
            , transform "matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)"
            ]
            []
        , polygon [ fill "#60B5CC", points "323.298,143.724 323.298,0 179.573,0" ] []
        , polygon [ fill "#5A6378", points "152.781,161.649 0,8.868 0,314.432" ] []
        , polygon [ fill "#F0AD00", points "255.522,246.655 323.298,314.432 323.298,178.879" ] []
        , polygon [ fill "#60B5CC", points "161.649,170.517 8.869,323.298 314.43,323.298" ] []
        ]
