module Transition.Discrete exposing
  ( Node
  , mkNode
  , NodeId
  , Model
  , init
  , Msg (GoTo)
  , MsgSettings
  , emptyMsgSettings
  , update
  , subscriptions
  )

{-|

## A Discrete State

@docs Node, mkNode, NodeId

## Model

@docs Model, init

## Msg

@docs Msg, MsgSettings, emptyMsgSettings

## Update

@docs update, subscriptions

-}


import Duration

import Time exposing (Time)
import Task
import IntDict exposing (IntDict)



-- only ever issuing more on completion
type alias DurationMsg b   = Duration.Msg (Msg b)
type alias DurationModel b = Duration.Model (Msg b)


{-| The discrete state's transition settings -}
type Node a b = Node
  { duration : Time
  , actions  : Time -> Cmd a
  , onward   : DurationModel b
  }

{-| Build a discrete state -}
mkNode : Time -> (Time -> Cmd a) -> Node a b
mkNode d a = Node
  { duration = d
  , actions  = a
  , onward   = Duration.init
  }

{-| The identifier for a discrete state - note that you should
    keep these unique; if not, the behaviour is unspecified
-}
type alias NodeId = Int


type TargetState
  = Replenish
  | Transfer { hasStarted : Bool
             , toward     : NodeId
             }

type alias PendingMsg b =
  { onChange        : Cmd b
  , onBetweenChange : Cmd b
  }

{-| Actions to issue when a state transition is finished, or between
    a state change.
-}
type alias MsgSettings b =
  { onChange        : Cmd b -> Cmd b
  , onBetweenChange : Cmd b -> Cmd b
  }

{-| -}
emptyMsgSettings : MsgSettings b
emptyMsgSettings =
  { onChange        = identity
  , onBetweenChange = identity
  }

applyPending : MsgSettings b -> PendingMsg b -> PendingMsg b
applyPending m a =
  { onChange        = m.onChange        a.onChange
  , onBetweenChange = m.onBetweenChange a.onBetweenChange
  }


{-| The state of the transition system -}
type alias Model a b =
  { current : NodeId
  , target  : Maybe TargetState
  , nodes   : IntDict (Node a b)
  , actions : PendingMsg b
  }

{-| -}
init : NodeId -> List (NodeId, (Node a b)) -> Model a b
init first nodes_ =
  { current = first
  , target  = Nothing
  , nodes   = IntDict.fromList nodes_
  , actions = { onChange        = Cmd.none
              , onBetweenChange = Cmd.none
              }
  }

{-| Call `GoTo node settings` to initialize the transition. -}
type Msg b
  = GoTo NodeId (MsgSettings b)
  | DurationMsg NodeId (DurationMsg b)
  | Complete
  | HasReversed NodeId

{-| Where `a` are the actions issued during transition, and `b`
    are the actions issued when the state is changed (or between)
-}
handle : (Msg b -> Result a b) -> TransitionResults a b -> Result a b
handle f r =
  case r of
    More m -> f m
    Done b -> Ok b
    Tick a -> Err a

type TransitionResults a b
  = More (Msg b)
  | Done b
  | Tick a

{-| -}
update : (Msg b -> Result a b)
      -> Msg b
      -> Model a b
      -> (Model a b, Cmd (Result a b))
update f a m =
  let (m_, eff) = update_ a m
  in  (m_, Cmd.map (handle f) eff)


update_ : Msg b
      -> Model a b
      -> (Model a b, Cmd (TransitionResults a b))
update_ action model =
  case action of
    DurationMsg k a ->
      let (newNodes, eff) = updateTransition k a model.nodes
      in  ( { model | nodes = newNodes }
          , eff
          )
    Complete ->
      case model.target of
        Nothing -> (model, Cmd.none)
        Just Replenish ->
          ( { model | target = Nothing }
          , Cmd.map Done model.actions.onChange
          )
        Just (Transfer x) ->
          ( { model | current = x.toward
                    , target = Nothing
            }
          , Cmd.map Done model.actions.onChange
          )
    HasReversed n ->
      if n == model.current
      then case model.target of
             Nothing -> Debug.crash "somehow don't have a target"
             Just Replenish -> Debug.crash "somehow removing self"
             Just (Transfer x) ->
               ( { model | target = Just <| Transfer { x | hasStarted = True } }
               , Cmd.batch
                   [ Cmd.map More <| mkCmd <| DurationMsg x.toward <|
                       Duration.Forward <| \_ -> mkCmd Complete
                   , Cmd.map Done model.actions.onBetweenChange
                   ]
               )
      else (model, Cmd.none)
    GoTo to m ->
      let newActions = applyPending m model.actions
      in if to == model.current
      then case model.target of
             Nothing ->
               ( { model | actions = newActions }
               , Cmd.none
               )
             Just s ->
               case s of
                 Replenish ->
                   ( { model | actions = newActions }
                   , Cmd.none
                   )
                 Transfer x ->
                   let oldTo = x.toward -- oldTo /= model.current
                   in ( { model | target  = Just Replenish
                                , actions = newActions
                        }
                      , Cmd.batch
                          [ if x.hasStarted
                            then Cmd.batch
                                   [ Cmd.map More <| mkCmd <| DurationMsg oldTo <|
                                       Duration.Reverse <| \_ -> Cmd.none
                                   , Cmd.map Done newActions.onBetweenChange
                                   ]
                            else Cmd.none
                          , Cmd.map More <| mkCmd <| DurationMsg model.current <|
                              Duration.Forward <| \_ -> mkCmd Complete
                          ]
                      )
      else case model.target of
             Nothing ->
               ( { model | actions = newActions
                         , target = Just <| Transfer { hasStarted = False
                                                     , toward = to
                                                     }
                 }
               , Cmd.map More <| mkCmd <| DurationMsg model.current <|
                   Duration.Reverse <| \_ -> mkCmd <| HasReversed model.current
               )
             Just s ->
               case s of
                 Replenish ->
                   ( { model | actions = newActions
                             , target = Just <| Transfer { hasStarted = False
                                                         , toward = to
                                                         }
                     }
                   , Cmd.map More <| mkCmd <| DurationMsg model.current <|
                       Duration.Reverse <| \_ -> mkCmd <| HasReversed model.current
                   )
                 Transfer x -> -- current is already leaving / has already left
                   let oldTo = x.toward
                   in  ( { model | actions = newActions
                                 , target = Just <| Transfer { hasStarted = x.hasStarted
                                                             , toward = to
                                                             }
                         }
                       , if x.hasStarted
                         then Cmd.batch -- current already left
                                [ Cmd.map More <| mkCmd <| DurationMsg oldTo <|
                                    Duration.Reverse <| \_ -> Cmd.none
                                , Cmd.map More <| mkCmd <| DurationMsg to <|
                                    Duration.Forward <| \_ -> mkCmd Complete
                                , Cmd.map Done newActions.onBetweenChange
                                ]
                         else Cmd.none -- when current leaves then it will initialize to
                       )




-- Utilities

updateTransition : NodeId
                -> DurationMsg b
                -> IntDict (Node a b)
                -> ( IntDict (Node a b)
                   , Cmd (TransitionResults a b)
                   )
updateTransition k action xs =
  case IntDict.get k xs of
    Nothing -> Debug.crash "key not found!"
    Just (Node x) ->
      let (newDuration, eff) =
            Duration.update
              x.actions
              x.duration
              action
              x.onward
          new = Node { x | onward = newDuration }
      in  ( IntDict.insert k new xs
          , Cmd.map (\r -> case r of
                             Err a -> Tick a
                             Ok m  -> More m) eff
          )


mkCmd : a -> Cmd a
mkCmd = Task.perform identity << Task.succeed

{-| -}
subscriptions : Model a b -> Sub (Msg b)
subscriptions model =
  Sub.batch <|
    List.map (\(n, Node v) -> Sub.map (DurationMsg n)
                           <| Duration.subscriptions v.onward
             )
          <| IntDict.toList model.nodes
