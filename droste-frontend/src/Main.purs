module Main where

import Prelude (Unit, bind, map, pure, unit, void, ($), (<>))

import Data.Array (catMaybes)
import Data.Either
import Data.Maybe (Maybe(..))

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

import DrosteApi (getImagePaths)

imageListElementClass :: React.ReactClass {imagePath :: String, onClick :: Event.SyntheticMouseEvent -> Effect Unit}
imageListElementClass =
    let render props = React.DOM.li' [
            React.DOM.button [Props.className "imageButton", Props.onClick props.onClick] [
                React.DOM.text props.imagePath
            ],
            React.DOM.img [
                Props.src $ "/static/" <> props.imagePath,
                Props.className "thumbnail"
            ]
        ]
    in React.statelessComponent render

imageListClass :: React.ReactClass {imageSelectOnClick :: String -> Event.SyntheticMouseEvent -> Effect Unit}
imageListClass =
    let mkImageListElement onClickConstructor imagePath = React.createLeafElement imageListElementClass {imagePath: imagePath, onClick: onClickConstructor imagePath}

        refreshImageList this = launchAff_ $ do
            maybeImagePaths <- getImagePaths
            case maybeImagePaths of
                Left err -> log $ "error when getting image paths: " <> err
                Right imagePaths -> liftEffect $ React.setState this {imagePaths: imagePaths}

        render this = do
            props <- React.getProps this
            state <- React.getState this
            pure $ React.DOM.div' [
                React.DOM.button [Props.onClick $ (\_ -> refreshImageList this)] [
                    React.DOM.text "refresh"
                ],
                React.DOM.text "Images",
                React.DOM.ul' (map (mkImageListElement props.imageSelectOnClick) state.imagePaths)
            ]

        component this = pure {
            state: {imagePaths: []},
            render: render this,
            componentDidMount: refreshImageList this
        }
    in React.component "ImageList" component

imageViewportClass :: React.ReactClass {imagePath :: String}
imageViewportClass =
    let render props = React.DOM.div' [
            React.DOM.img [
                Props.src $ "/static/" <> props.imagePath
            ]
        ]
    in React.statelessComponent render

appClass :: React.ReactClass {}
appClass =
    let setSelectedImage this imagePath = do
            state <- React.getState this
            React.setState this {selectedImagePath: Just imagePath}

        render this = do
            state <- React.getState this
            pure $ React.DOM.div' $ catMaybes [
                Just $ React.createLeafElement imageListClass {
                    imageSelectOnClick: (\imagePath _ -> setSelectedImage this imagePath)
                },
                map (\imagePath -> React.createLeafElement imageViewportClass {imagePath: imagePath}) state.selectedImagePath
            ]

        component this = pure {
            state: {selectedImagePath: Nothing},
            render: render this
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
