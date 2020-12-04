module Main where

import Prelude (Unit, bind, map, pure, unit, void, ($), (<>))

import Data.Array (catMaybes)
import Data.Either
import Data.Maybe (Maybe(..))
import Data.Set as Set

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

import DrosteApi (deleteImage, getImagePaths, getImage)
import DrosteTypes (Image(..))

imageListElementClass :: React.ReactClass {imagePath :: String, displayOnClick :: Event.SyntheticMouseEvent -> Effect Unit, deleteOnClick :: Event.SyntheticMouseEvent -> Effect Unit}
imageListElementClass =
    let render props = React.DOM.li' [
            React.DOM.button [Props.className "imageButton", Props.onClick props.displayOnClick] [
                React.DOM.text props.imagePath
            ],
            React.DOM.button [Props.className "deleteButton", Props.onClick props.deleteOnClick] [
                React.DOM.text "X"
            ],
            React.DOM.img [
                Props.src $ "/static/" <> props.imagePath,
                Props.className "thumbnail"
            ]
        ]
    in React.statelessComponent render

imageListClass :: React.ReactClass {imageSelectOnClick :: String -> Event.SyntheticMouseEvent -> Effect Unit}
imageListClass =
    let mkImageListElement displayOnClickConstructor deleteOnClickConstructor imagePath = React.createLeafElement imageListElementClass {
            imagePath: imagePath,
            displayOnClick: displayOnClickConstructor imagePath,
            deleteOnClick: deleteOnClickConstructor imagePath
        }

        refreshImageList this = launchAff_ $ do
            maybeImagePaths <- getImagePaths
            case maybeImagePaths of
                Left err -> log $ "error when getting image paths: " <> err
                Right imagePaths -> liftEffect $ React.setState this {imagePaths: imagePaths}

        deleteImageFromList this imagePath event = launchAff_ $ do
            maybeResponse <- deleteImage imagePath
            case maybeResponse of
                Left err -> log $ "error when deleting image " <> imagePath <> " : " <> err
                Right _ -> liftEffect $ refreshImageList this

        render this = do
            props <- React.getProps this
            state <- React.getState this
            pure $ React.DOM.div' [
                React.DOM.button [Props.onClick $ (\_ -> refreshImageList this)] [
                    React.DOM.text "refresh"
                ],
                React.DOM.text "Images",
                React.DOM.button [] [
                    React.DOM.text "upload"
                ],
                React.DOM.ul' $ map (mkImageListElement props.imageSelectOnClick (deleteImageFromList this)) (Set.toUnfoldable state.imagePaths)
            ]

        component this = pure {
            state: {imagePaths: Set.empty},
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
    let setSelectedImage this imagePath = launchAff_ $ do
            maybeImage <- getImage imagePath
            case maybeImage of
                Left err -> log $ "failed to fetch image " <> imagePath <> " : " <> err
                Right image -> liftEffect $ React.setState this {selectedImage: Just image}

        render this = do
            state <- React.getState this
            pure $ React.DOM.div' $ catMaybes [
                Just $ React.createLeafElement imageListClass {
                    imageSelectOnClick: (\imagePath _ -> setSelectedImage this imagePath)
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
