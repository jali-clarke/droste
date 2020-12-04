module DrosteApi where

import Prelude (Unit, bind, map, pure, show, unit, ($), (<>))

import Affjax (delete_, get, printError) as AX
import Affjax.ResponseFormat (string) as AX

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Set as Set
import Effect.Aff (Aff)
import Foreign.Generic (decodeJSON)

import DrosteTypes (Image)

getImagePaths :: Aff (Either String (Set.Set String))
getImagePaths = do
    maybeResponse <- AX.get AX.string "/api/images"
    pure $ case map (\r -> decodeJSON r.body) maybeResponse of
        Left err -> Left $ AX.printError err
        Right maybeImagePaths -> case runExcept maybeImagePaths of
            Left err -> Left $ show err
            Right imagePaths -> Right (Set.fromFoldable (imagePaths :: Array String))

getImage :: String -> Aff (Either String Image)
getImage imagePath = do
    maybeResponse <- AX.get AX.string ("/api/images/" <> imagePath)
    pure $ case map (\r -> decodeJSON r.body) maybeResponse of
        Left err -> Left $ AX.printError err
        Right maybeImage -> case runExcept maybeImage of
            Left err -> Left $ show err
            Right image -> Right image

deleteImage :: String -> Aff (Either String Unit)
deleteImage imagePath = do
    maybeResponse <- AX.delete_ ("/api/images/" <> imagePath)
    pure $ case maybeResponse of
        Left err -> Left $ AX.printError err
        Right _ -> Right unit
