module Main exposing (..)

import Dict exposing (Dict)
import Hexagons.Hex as Hex exposing (Hex, (===), (!==))
import Hexagons.Map as Map
import Hexagons.Layout as Layout exposing (drawLine)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class, style)
import Html.Events exposing (onMouseOver, onMouseOut, onClick)
import Guards exposing (..)
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Time exposing (every, second, millisecond, Time)


-- APP


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : a -> Sub Msg
subscriptions model =
    Sub.batch
        [ every (33 * millisecond) Tick
        , every (33 * millisecond) UpgradeHexes
        ]


type alias Model =
    { map : Dict Map.Hash Hex
    , layout : Layout.Layout
    , hoverHex : Maybe Hex.Hex
    , activeHex : Maybe Hex.Hex
    , ship : Ship
    , score : Float
    }


type alias Hex =
    { value : Float
    , hex : Hex.Hex
    }


type alias Ship =
    { acceleration : Float
    , pos : Vec2
    , velocity : Vec2
    }


init : ( Model, Cmd Msg )
init =
    { map =
        Map.rectangularPointyTopMap 15 15
            |> Dict.map (\i e -> { hex = e, value = 100 })
    , layout =
        { orientation = Layout.orientationLayoutPointy
        , size = ( 30, 30 )
        , origin = ( 0, 0 )
        }
    , hoverHex = Nothing
    , activeHex = Nothing
    , score = 0
    , ship =
        { pos = vec2 30 15
        , velocity = vec2 0 0
        , acceleration = 1.001
        }
    }
        ! []



-- UPDATE


type Msg
    = SetActive Hex.Hex
    | SetHover (Maybe Hex.Hex)
    | Tick Time
    | UpgradeHexes Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetActive hex ->
            let
                shipOnTile =
                    Hex.eq hex <|
                        Layout.pointToHex model.layout (Vec.toTuple model.ship.pos)

                map i tile =
                    if Hex.distance hex tile.hex <= 1 && shipOnTile then
                        { tile | value = tile.value - 20 }
                    else
                        tile

                score idx tile int =
                    if Hex.distance hex tile.hex <= 1 && shipOnTile then
                        int + min tile.value 20
                    else
                        int
            in
                { model
                    | activeHex = Just hex
                    , map = Dict.map map model.map
                    , score = Dict.foldl score model.score model.map
                }
                    ! []

        SetHover hex ->
            { model | hoverHex = hex } ! []

        Tick _ ->
            let
                vDist =
                    model.activeHex
                        |> Maybe.map (Layout.hexToPoint model.layout)
                        |> Maybe.map Vec.fromTuple
                        |> Maybe.withDefault (vec2 0 0)
                        |> (\h -> Vec.sub h model.ship.pos)

                v1 =
                    vDist
                        |> Vec.normalize
                        |> Vec.scale model.ship.acceleration
                        |> Vec.add model.ship.velocity
                        |> dampWhenApproachingDestination
                        |> capVelocity

                capVelocity v =
                    Vec.normalize v |> Vec.scale (Vec.length v |> min 10)

                dampWhenApproachingDestination v =
                    if 5 * Vec.length v > Vec.length vDist then
                        Vec.scale 0.2 v
                    else
                        v

                ship' ship =
                    { ship
                        | velocity = v1
                        , pos = Vec.add v1 model.ship.pos
                    }
            in
                ( model |> (\m -> { m | ship = ship' m.ship })
                , Cmd.none
                )

        UpgradeHexes _ ->
            let
                incrHex h =
                    h.value * 1.002 |> min 100

                mapMap i e =
                    { hex = e.hex, value = incrHex e }
            in
                { model | map = Dict.map mapMap model.map } ! []


view : Model -> Html Msg
view model =
    let
        hexagon ( hash, { hex, value } ) =
            let
                ( x, y ) =
                    Layout.hexToPoint model.layout hex

                isHex =
                    Maybe.map ((===) hex) >> Maybe.withDefault False

                klass =
                    "hexagon "
                        ++ (isHex model.activeHex => "active " |= "")
                        ++ (isHex model.hoverHex => "hover " |= "")
                        ++ (model.hoverHex
                                |> Maybe.map (\h -> Hex.distance hex h == 1 => "hover " |= "")
                                |> Maybe.withDefault ""
                           )
            in
                div
                    [ class klass
                    , onClick (SetActive <| hex)
                    , onMouseOver (SetHover <| Just hex)
                    , onMouseOut (SetHover Nothing)
                    , style
                        [ ( "position", "absolute" )
                        , ( "left", toString x ++ "px" )
                        , ( "top", toString y ++ "px" )
                        , ( "opacity", toString (value / 100) )
                        ]
                    ]
                    []
    in
        div []
            [ model.map
                |> Dict.toList
                |> List.map hexagon
                |> div []
            , let
                ( x, y ) =
                    Vec.toTuple model.ship.pos
              in
                div
                    [ class "ship"
                    , style
                        [ ( "transform", "translate3d(" ++ toString x ++ "px, " ++ toString y ++ "px, 0)" ) ]
                    ]
                    [ text <| toString model.score ]
            ]
