module DrosteApi where

import Prelude (bind, map, pure, show, ($))

import Affjax (get, printError) as AX
import Affjax.ResponseFormat (string) as AX

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Foreign.Generic (decodeJSON)

import DrosteTypes (Image)

getImages :: Aff (Either String (Array Image))
getImages = do
    maybeResponse <- AX.get AX.string "/api/images"
    pure $ case map (\r -> decodeJSON r.body) maybeResponse of
        Left err -> Left $ AX.printError err
        Right maybeVal -> case runExcept maybeVal of
            Left err -> Left $ show err
            Right val -> Right val
