module Main where

import Prelude (Unit, bind, discard, pure, unit, void, ($), (<>))

import Data.Array (updateAt, (!!))
import Data.Maybe
import Data.Show (class Show, show)
import Data.Traversable (traverse)
import Data.Unfoldable (replicate)

import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM

import React as React
import React.SyntheticEvent as Event
import React.DOM as React.DOM
import React.DOM.Props as Props
import ReactDOM as ReactDOM

import Effect (Effect)
import Effect.Console (log)

data SquareValue = X | O | None

instance showSquareValue :: Show SquareValue where
  show val =
    case val of
      X -> "X"
      O -> "O"
      None -> ""

squareClass :: React.ReactClass {value :: SquareValue, onClick :: Event.SyntheticMouseEvent -> Effect Unit}
squareClass =
  let
    onClick this event = do
      props <- React.getProps this
      props.onClick event

    render this = do
      props <- React.getProps this
      pure $ React.DOM.button [Props.className "square", Props.onClick (onClick this)] [
        React.DOM.text (show props.value)
      ]

    component this = pure {state: {}, render: render this}
  in React.component "Square" component

boardClass :: React.ReactClass {}
boardClass =
  let
    chooseNextPlayer squareValue =
      case squareValue of
        X -> O
        _ -> X

    handleClick this n event = do
      state <- React.getState this
      case updateAt n state.nextPlayer state.squareState of
        Nothing -> log $ "failed to handle click for out-of-bounds square " <> show n
        Just newState -> React.setState this {squareState: newState, nextPlayer: chooseNextPlayer state.nextPlayer}

    mkSquare this n = do
      state <- React.getState this
      squareValue <- case state.squareState !! n of
        Nothing -> do
          log $ "failed to get state for out-of-bounds square " <> show n
          pure None
        Just val -> pure val
      pure $ React.createLeafElement squareClass {value: squareValue, onClick: handleClick this n}

    render this = do
      state <- React.getState this
      firstRow <- traverse (mkSquare this) [0, 1, 2]
      secondRow <- traverse (mkSquare this) [3, 4, 5]
      thirdRow <- traverse (mkSquare this) [6, 7, 8]
      pure $ React.DOM.div' [
        React.DOM.div [Props.className "status"] [React.DOM.text $ "Next player: " <> show state.nextPlayer],
        React.DOM.div [Props.className "board-row"] firstRow,
        React.DOM.div [Props.className "board-row"] secondRow,
        React.DOM.div [Props.className "board-row"] thirdRow
      ]

    component this = pure {state: {squareState: replicate 9 None, nextPlayer: X}, render: render this}
  in React.component "Board" component

gameClass :: React.ReactClass {}
gameClass =
  let
    render = pure $
      React.DOM.div [Props.className "game"] [
        React.DOM.div [Props.className "game-board"] [
          React.createLeafElement boardClass {}
        ],
        React.DOM.div [Props.className "game-info"] [
          React.DOM.div' [],
          React.DOM.ol' []
        ]
      ]

    component this = pure {state: {}, render: render}
  in React.component "Game" component

main :: Effect Unit
main = do
  window <- DOM.window
  document <- DOM.document window
  maybeAppElement <- DOM.getElementById "app" (DOM.toNonElementParentNode document)
  case maybeAppElement of
    Nothing -> pure unit
    Just appElement -> void $ ReactDOM.render (React.createLeafElement gameClass {}) appElement
