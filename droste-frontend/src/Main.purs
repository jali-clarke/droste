module Main where

import Prelude (Unit, bind, discard, map, pure, unit, void, ($), (<>))

import Data.Either
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
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import DrosteApi
import DrosteTypes

imageListElementClass :: React.ReactClass {image :: Image}
imageListElementClass =
    let render props =
            let Image {path: imagePath} = props.image
            in React.DOM.li' [
                React.DOM.text imagePath,
                React.DOM.img [
                    Props.src $ "/static/" <> imagePath,
                    Props.className "thumbnail"
                ]
            ]
    in React.statelessComponent render

imageListClass :: React.ReactClass {images :: Array Image}
imageListClass =
    let mkImageListElement image = React.createLeafElement imageListElementClass {image: image}
        render props = React.DOM.div' [
            React.DOM.text "Images",
            React.DOM.ul' (map mkImageListElement props.images)
        ]
    in React.statelessComponent render

appClass :: React.ReactClass {}
appClass =
    let render this = do
            state <- React.getState this
            pure $ React.DOM.div' [
                React.createLeafElement imageListClass {
                    images: state.images
                }
            ]

        componentDidMount this = launchAff_ $ do
            maybeImages <- getImages
            case maybeImages of
                Left err -> log $ "error when getting images: " <> err
                Right images -> liftEffect $ React.setState this {images: images}

        component this = pure {
            state: {images: []},
            render: render this,
            componentDidMount: componentDidMount this
        }
    in React.component "App" component

main :: Effect Unit
main = do
    window <- DOM.window
    document <- DOM.document window
    maybeAppElement <- DOM.getElementById "app" (DOM.toNonElementParentNode document)
    case maybeAppElement of
        Nothing -> pure unit
        Just appElement -> void $ ReactDOM.render (React.createLeafElement appClass {}) appElement
