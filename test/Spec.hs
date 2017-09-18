{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
import GHCJS.Fetch
import Test.Hspec.Core.Runner (Config(..), hspecWith, defaultConfig, ColorMode(..))
import Test.Hspec
import Test.QuickCheck
import Control.Exception (finally)
import GHCJS.Marshal
import GHCJS.Types
import Data.Aeson (Value)

main :: IO ()
main = do
  flip finally seleniumAsync $ hspecWith defaultConfig { configColorMode = ColorNever } $ do
    describe "fetch" $ do
      it "can GET" $ do
        resp <- fetch (Request "https://httpbin.org/get" RequestOptions)
        val <- responseJSON resp
        print (val :: Maybe Value)

foreign import javascript safe "window.seleniumCallback();"
               seleniumAsync :: IO ()
