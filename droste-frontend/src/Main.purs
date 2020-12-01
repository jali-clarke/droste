module Main where

import Prelude (Unit, bind, pure, unit, void, ($))

import Data.Maybe

import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM

import React as React
import React.DOM as React.DOM
import ReactDOM as ReactDOM

import Effect (Effect)

-- class Square extends React.Component {
--   render() {
--     return (
--       <button className="square">
--         {/* TODO */}
--       </button>
--     );
--   }
-- }

-- class Board extends React.Component {
--   renderSquare(i) {
--     return <Square />;
--   }

--   render() {
--     const status = 'Next player: X';

--     return (
--       <div>
--         <div className="status">{status}</div>
--         <div className="board-row">
--           {this.renderSquare(0)}
--           {this.renderSquare(1)}
--           {this.renderSquare(2)}
--         </div>
--         <div className="board-row">
--           {this.renderSquare(3)}
--           {this.renderSquare(4)}
--           {this.renderSquare(5)}
--         </div>
--         <div className="board-row">
--           {this.renderSquare(6)}
--           {this.renderSquare(7)}
--           {this.renderSquare(8)}
--         </div>
--       </div>
--     );
--   }
-- }

-- class Game extends React.Component {
--   render() {
--     return (
--       <div className="game">
--         <div className="game-board">
--           <Board />
--         </div>
--         <div className="game-info">
--           <div>{/* status */}</div>
--           <ol>{/* TODO */}</ol>
--         </div>
--       </div>
--     );
--   }
-- }

-- // ========================================

asdfClass :: React.ReactClass {}
asdfClass =
  let component this = pure {state: {}, render: (pure $ React.DOM.text "asdf")}
  in React.component "Asdf" component

main :: Effect Unit
main = do
  window <- DOM.window
  document <- DOM.document window
  maybeAppElement <- DOM.getElementById "app" (DOM.toNonElementParentNode document)
  case maybeAppElement of
    Nothing -> pure unit
    Just appElement -> void $ ReactDOM.render (React.createLeafElement asdfClass {}) appElement
