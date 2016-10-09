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
    | AudioLengthKnown String Float


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
    }


type alias Drag =
    { clipId :
        Int
        --TODO change this to id or something, it could be a clip or a source
    , start : Position
    , current : Position
    }


newClipsFromDrag : Model -> Source -> List Clip
newClipsFromDrag model source =
    case model.drag of
        Nothing ->
            []

        Just drag ->
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
                [ (Clip (nextId model) start (end - start) start) ]


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


applyDrag : Model -> Drag -> Model
applyDrag model drag =
    if drag.start == drag.current then
        { model | drag = Nothing }
    else
        { model
            | sources =
                model.sources
                    |> updateClips
                        (\clip ->
                            if clip.id == drag.clipId then
                                clipWithDrag model clip
                            else
                                clip
                        )
                    |> List.map
                        (\source ->
                            if source.id == drag.clipId then
                                { source
                                    | clips =
                                        List.append (newClipsFromDrag model source)
                                            source.clips
                                }
                            else
                                source
                        )
            , drag = Nothing
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



--TODO different kinds of drags


type DragType
    = NewClip Int
    | MoveClip Int
    | ResizeClipLeft Int
    | ResizeClipRight Int


updateHelp : Msg -> Model -> Model
updateHelp msg model =
    case msg of
        DragStart id xy ->
            { model | drag = Just (Drag id xy xy) }

        DragAt id xy ->
            { model | drag = (Maybe.map (\{ start, clipId } -> Drag clipId start xy) model.drag) }

        --TODO this update should be based on the current zoom
        DragEnd id _ ->
            case model.drag of
                Nothing ->
                    model

                Just drag ->
                    if drag.start == drag.current then
                        { model | drag = Nothing }
                    else
                        applyDrag model drag

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



-- Maybe each clip knows where it appears
-- Sources have Clips, Clips have Uses - nah how about they only have one use? Lists are easier to deal with I guess.
-- But the common use is that clips have 1 or 0 uses.
-- Layout: Sources with clips, then list of all unused clips, then layout of used clips.
-- View


unusedClipRect : Float -> ( Clip, Source ) -> Svg Msg
unusedClipRect yVal ( clip, source ) =
    (g []
        [ (rect
            [ fill "green"
            , x "0"
            , y (toString yVal)
            , width (toString clip.length)
            , height "10"
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


floatToTime : Float -> String
floatToTime t =
    (toString ((round t) // 60)) ++ ":" ++ (String.padLeft 2 '0' (toString ((round t) % 60))) ++ "." ++ (toString (round (t * 100) % 100))


clipsView : Float -> List ( Clip, Source ) -> Svg Msg
clipsView yVal clipSourcePairs =
    (g []
        (List.indexedMap (\i ( clip, source ) -> (unusedClipRect (yVal + (toFloat i * 15)) ( clip, source )))
            clipSourcePairs
        )
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
                ++ [ (clipsView (toFloat (List.length model.sources) * 15 + 15) (allClipsWithSources model)) ]
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
    case model.drag of
        Nothing ->
            []

        Just drag ->
            (((allClipsWithSources model)
                |> List.filter (\( clip, source ) -> clip.id == drag.clipId)
                |> List.map (\( clip, source ) -> ( clipWithDrag model clip, source ))
                |> List.map (\( clip, source ) -> shadow source.yPos clip)
             )
                ++ (model.sources
                        |> List.filter (\source -> source.id == drag.clipId)
                        |> List.concatMap (\source -> (List.map (\clip -> ( clip, source )) (newClipsFromDrag model source)))
                        |> List.map (\( clip, source ) -> (shadow source.yPos clip))
                   )
            )


sourceView : Source -> Svg Msg
sourceView source =
    (g []
        ([ (rect
                [ fill "blue"
                , x "0"
                , y (toString source.yPos)
                , width (toString source.length)
                , height "10"
                , Html.Events.on "mousedown" (Json.map (DragStart source.id) Mouse.position)
                ]
                []
           )
         , (text'
                [ x "2"
                , y (toString (source.yPos + 5))
                , fontFamily "Verdana"
                , fontSize "4"
                ]
                [ text source.url ]
           )
         ]
            ++ (List.map (clipRectOfOpacityColorY "1" "green" source.yPos) source.clips)
        )
    )


clipRectOfOpacityColorY : String -> String -> Float -> Clip -> Svg Msg
clipRectOfOpacityColorY o c yVal clip =
    (rect
        [ fill c
        , opacity o
        , x (toString clip.start)
        , y (toString yVal)
        , width (toString clip.length)
        , height "10"
        , Html.Events.on "mousedown" (Json.map (DragStart clip.id) Mouse.position)
        ]
        []
    )


shadow : Float -> Clip -> Svg Msg
shadow =
    clipRectOfOpacityColorY "0.5" "gray"


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
