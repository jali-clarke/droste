module Main where

import Prelude

import Data.Maybe

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

appClass :: React.ReactClass {}
appClass =
  let
    render = pure $ React.DOM.div' []
    component this = pure {state: {}, render: render}
  in React.component "App" component

main :: Effect Unit
main = do
  window <- DOM.window
  document <- DOM.document window
  maybeAppElement <- DOM.getElementById "app" (DOM.toNonElementParentNode document)
  case maybeAppElement of
    Nothing -> pure unit
    Just appElement -> void $ ReactDOM.render (React.createLeafElement appClass {}) appElement
