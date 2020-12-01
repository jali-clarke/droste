module Main where

import Prelude (Unit, bind, pure, show, unit, void, ($), (<>))

import Data.Maybe

import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Window (alert, document) as DOM

import React as React
import React.DOM as React.DOM
import React.DOM.Props as Props
import ReactDOM as ReactDOM

import Effect (Effect)

squareClass :: React.ReactClass {value :: Int}
squareClass =
  let
    onClick this event = do
      state <- React.getState this
      window <- DOM.window
      DOM.alert ("i am square " <> show state.value) window

    render this = do
      state <- React.getState this
      pure $ React.DOM.button [Props.className "square", Props.onClick (onClick this)] [
        React.DOM.text (show state.value)
      ]

    component this = do
      props <- React.getProps this
      pure {state: {value: props.value}, render: render this}
  in React.component "Square" component

boardClass :: React.ReactClass {}
boardClass =
  let
    renderSquare n = React.createLeafElement squareClass {value: n}

    status = "Next player: X"
    render = pure $
      React.DOM.div' [
        React.DOM.div [Props.className "status"] [React.DOM.text status],
        React.DOM.div [Props.className "board-row"] [
          renderSquare(0),
          renderSquare(1),
          renderSquare(2)
        ],
        React.DOM.div [Props.className "board-row"] [
          renderSquare(3),
          renderSquare(4),
          renderSquare(5)
        ],
        React.DOM.div [Props.className "board-row"] [
          renderSquare(6),
          renderSquare(7),
          renderSquare(8)
        ]
      ]

    component this = pure {state: {}, render: render}
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
