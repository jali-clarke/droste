module DrosteApi where

import Prelude (bind, map, pure, show, ($))

import Affjax (get, printError) as AX
import Affjax.ResponseFormat (string) as AX

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Foreign.Generic (decodeJSON)

getImagePaths :: Aff (Either String (Array String))
getImagePaths = do
    maybeResponse <- AX.get AX.string "/api/images"
    pure $ case map (\r -> decodeJSON r.body) maybeResponse of
        Left err -> Left $ AX.printError err
        Right maybeImagePaths -> case runExcept maybeImagePaths of
            Left err -> Left $ show err
            Right imagePaths -> Right imagePaths
