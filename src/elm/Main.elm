module Main exposing (..)

import Config exposing (config)
import Dict
import Hexagons.Hex as Hex exposing (Hex, (===), (!==))
import Hexagons.Map as Map
import Hexagons.Layout as Layout
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class, style)
import Html.Events exposing (onMouseOver, onMouseOut, onClick)
import Guards exposing (..)


-- APP


main : Program Never
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL
--type alias Hex =
--( Hexagons.Hex.Hex, Int )


type alias Model =
    { map : Dict.Dict Map.Hash Hex.Hex
    , layout : Layout.Layout
    , hoverHex : Maybe Hex.Hex
    , activeHex : Maybe Hex.Hex
    }


model : Model
model =
    { map = Map.rectangularPointyTopMap config.fieldSize.x config.fieldSize.y
    , layout =
        { orientation =
            Layout.orientationLayoutPointy
            --{ forward_matrix = ( 2 / sqrt 3.0, 1 / (sqrt 3), 0.0, 1.0 )
            --, inverse_matrix = ( (sqrt 3.0) / 3.0, -1.0 / 3.0, 0.0, 2.0 / 3.0 )
            --, start_angle = 0.5
            --}
        , size = ( 30.0, 30.0 )
        , origin = ( 0.0, 0.0 )
        }
    , hoverHex = Nothing
    , activeHex = Nothing
    }



-- UPDATE


type Msg
    = SetActive (Maybe Hex.Hex)
    | SetHover (Maybe Hex.Hex)


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetActive hex ->
            { model | activeHex = hex }

        SetHover hex ->
            { model | hoverHex = hex }


{-| Linear interpolation of hexes
-}
hexLerp : Hex -> Hex -> Float -> Hex
hexLerp a b t =
    let
        a_ =
            Hex.toFloatHex a

        b_ =
            Hex.toFloatHex b

        q1 =
            Hex.q a_

        q2 =
            Hex.q b_

        r1 =
            Hex.r a_

        r2 =
            Hex.r b_

        q =
            q1 + ((q2 - q1) * t)

        r =
            r1 + ((r2 - r1) * t)
    in
        Hex.floatFactory ( q, r )


drawLine : Hex -> Hex -> List Hex
drawLine a b =
    let
        n =
            toFloat <| Hex.distance a b

        step =
            1.0 / (max n 1.0)

        steps =
            List.map ((*) step) [0.0..n]
    in
        List.map (Hex.toIntHex << (hexLerp a b)) steps


view : Model -> Html Msg
view model =
    let
        hexagon ( hash, hex ) =
            let
                ( x, y ) =
                    Layout.hexToPoint model.layout hex

                isHex =
                    Maybe.map ((===) hex) >> Maybe.withDefault False

                inLine h =
                    Maybe.map2 drawLine model.activeHex model.hoverHex
                        |> Maybe.withDefault []
                        |> List.member h

                klass =
                    "hexagon "
                        ++ (inLine hex => "active " |= "")
                        ++ (isHex model.activeHex => "active " |= "")
                        ++ (isHex model.hoverHex => "hover " |= "")
            in
                div
                    [ class klass
                    , onClick (SetActive <| Just hex)
                    , onMouseOver (SetHover <| Just hex)
                    , onMouseOut (SetHover Nothing)
                    , style
                        [ ( "position", "absolute" )
                        , ( "left", toString x ++ "px" )
                        , ( "top", toString y ++ "px" )
                        ]
                    ]
                    --[ text <| toString hash ]
                    []
    in
        model.map
            |> Dict.toList
            |> List.map hexagon
            |> div []
