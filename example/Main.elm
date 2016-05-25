module Main exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Transition.Discrete as Discrete exposing (NodeId)
import Ease exposing (inQuad)

import Dict exposing (Dict)
import Time exposing (second, Time)
import Color exposing (hsl, Color, toRgb)
import Task


type alias Model =
  { transition : Discrete.Model Msg Msg
  , node1Color : Color
  , node2Color : Color
  , node3Color : Color
  , isTransitioning : Bool
  , between : Maybe NodeId
  }

type Msg
  = DiscreteMsg (Discrete.Msg Msg)
  | Start
  | Between NodeId
  | Finished
  | ChangeNode1Color Color
  | ChangeNode2Color Color
  | ChangeNode3Color Color
  | Clicked NodeId

node1 : NodeId
node1 = 1
node2 : NodeId
node2 = 2
node3 : NodeId
node3 = 3

nodes : Dict NodeId (Discrete.Node Msg Msg)
nodes =
  let onward : Time -> Cmd Color
      onward t =
        Task.perform (\_ -> Debug.crash "") identity <|
          Task.succeed <| hsl (degrees <| 360 * (t / second)) 1 0.5
  in  Dict.fromList [ (node1, Discrete.mkNode second <|
                                Cmd.map ChangeNode1Color << onward)
                    , (node2, Discrete.mkNode second <|
                                Cmd.map ChangeNode2Color << onward)
                    , (node3, Discrete.mkNode second <|
                                Cmd.map ChangeNode3Color << onward)
                    ]

init : (Model, Cmd Msg)
init =
  ( { transition = Discrete.init node1 nodes
    , node1Color = hsl (degrees 360) 1 0.5
    , node2Color = hsl (degrees 0) 1 0.5
    , node3Color = hsl (degrees 0) 1 0.5
    , isTransitioning = False
    , between = Nothing
    }
  , Cmd.none
  )

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    DiscreteMsg a ->
      let (newDiscrete, eff) = Discrete.update
                                 (Ok << DiscreteMsg)
                                 a
                                 model.transition
      in  ( { model | transition = newDiscrete }
          , Cmd.map (\r -> case r of
                             Err x -> x
                             Ok  x -> x) eff
          )
    ChangeNode1Color c ->
      ( { model | node1Color = c }
      , Cmd.none
      )
    ChangeNode2Color c ->
      ( { model | node2Color = c }
      , Cmd.none
      )
    ChangeNode3Color c ->
      ( { model | node3Color = c }
      , Cmd.none
      )
    Between n ->
      ( { model | between = Just n }
      , Cmd.none
      )
    Start ->
      ( { model | isTransitioning = True }
      , Cmd.none
      )
    Finished ->
      ( { model | isTransitioning = False
                , between = Nothing
        }
      , Cmd.none
      )
    Clicked n ->
      ( model
      , Cmd.batch
          [ mkCmd Start
          , mkCmd <| DiscreteMsg <| Discrete.GoTo n <|
              { onChange        = \_ -> mkCmd Finished
              , onBetweenChange = \_ -> mkCmd <| Between n
              }
          ]
      )

mkCmd : a -> Cmd a
mkCmd x = Task.perform (\_ -> Debug.crash "") identity <|
            Task.succeed x

showRgb : Color -> String
showRgb c =
  let { red, green, blue } = toRgb c
  in  "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")"

view : Model -> Html Msg
view model =
  div []
    [ div [ style [("background-color", showRgb model.node1Color)]
          , onClick <| Clicked node1
          ] [text "Node 1"]
    , div [ style [("background-color", showRgb model.node2Color)]
          , onClick <| Clicked node2
          ] [text "Node 2"]
    , div [ style [("background-color", showRgb model.node3Color)]
          , onClick <| Clicked node3
          ] [text "Node 3"]
    , div [] <|
        if model.isTransitioning
        then [text "Is Transitioning..."]
        else []
    , div [] <|
        case model.between of
          Nothing -> []
          Just n  -> [text <| "toward: " ++ toString n]
    , div []
          [ text <| "Current: "
              ++ toString model.transition.current
          ]
    ]

main = App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = \m -> Sub.map DiscreteMsg <| Discrete.subscriptions m.transition
  }
