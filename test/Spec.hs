{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception (finally)
import           Data.Aeson (Value(..))
import qualified Data.HashMap.Lazy as HashMap
import           GHCJS.Fetch
import           GHCJS.Marshal
import           GHCJS.Types
import           Test.Hspec
import           Test.Hspec.Core.Runner (Config(..), hspecWith, defaultConfig, ColorMode(..))
import           Test.QuickCheck

main :: IO ()
main = do
  flip finally seleniumAsync $
    hspecWith defaultConfig {configColorMode = ColorNever} $ do
      describe "fetch" $ do
        it "can GET" $ do
          resp <- fetch (Request "https://httpbin.org/get" defaultRequestOptions)
          val <- responseJSON resp
          case val of
            Just (Object obj) ->
              HashMap.lookup "url" obj `shouldBe`
              Just (String "https://httpbin.org/get")
            _ -> expectationFailure ("Expected Object but got: " ++ show val)
        it "should throw on nonexisting URL" $
          fetch (Request "https://example.com" defaultRequestOptions) `shouldThrow`
          (\(PromiseException _) -> True)

foreign import javascript safe "window.seleniumCallback();"
               seleniumAsync :: IO ()
