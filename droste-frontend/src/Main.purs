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

import DrosteApi (getImages)
import DrosteTypes (Image(..))

imageListElementClass :: React.ReactClass {image :: Image, onClick :: Event.SyntheticMouseEvent -> Effect Unit}
imageListElementClass =
    let render props =
            let Image {path: imagePath} = props.image
            in React.DOM.li' [
                React.DOM.button [Props.className "imageButton", Props.onClick props.onClick] [
                    React.DOM.text imagePath
                ],
                React.DOM.img [
                    Props.src $ "/static/" <> imagePath,
                    Props.className "thumbnail"
                ]
            ]
    in React.statelessComponent render

imageListClass :: React.ReactClass {imageSelectOnClick :: Image -> Event.SyntheticMouseEvent -> Effect Unit}
imageListClass =
    let mkImageListElement onClickConstructor image = React.createLeafElement imageListElementClass {image: image, onClick: onClickConstructor image}

        refreshImageList this = launchAff_ $ do
            maybeImages <- getImages
            case maybeImages of
                Left err -> log $ "error when getting images: " <> err
                Right images -> liftEffect $ React.setState this {images: images}

        render this = do
            props <- React.getProps this
            state <- React.getState this
            pure $ React.DOM.div' [
                React.DOM.button [Props.onClick $ (\_ -> refreshImageList this)] [
                    React.DOM.text "refresh"
                ],
                React.DOM.text "Images",
                React.DOM.ul' (map (mkImageListElement props.imageSelectOnClick) state.images)
            ]

        component this = pure {
            state: {images: []},
            render: render this,
            componentDidMount: refreshImageList this
        }
    in React.component "ImageList" component

imageViewportClass :: React.ReactClass {image :: Image}
imageViewportClass =
    let render props =
            let Image {path: imagePath} = props.image
            in React.DOM.div' [
                React.DOM.img [
                    Props.src $ "/static/" <> imagePath
                ]
            ]
    in React.statelessComponent render

appClass :: React.ReactClass {}
appClass =
    let setSelectedImage this image = do
            state <- React.getState this
            React.setState this {selectedImage: Just image}

        render this = do
            state <- React.getState this
            pure $ React.DOM.div' $ catMaybes [
                Just $ React.createLeafElement imageListClass {
                    imageSelectOnClick: (\image _ -> setSelectedImage this image)
                },
                map (\image -> React.createLeafElement imageViewportClass {image: image}) state.selectedImage
            ]

        component this = pure {
            state: {selectedImage: Nothing},
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
