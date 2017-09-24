{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception (catch, finally)
import           Data.Aeson (Value(..))
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.JSString as JSString
import           Data.Text (Text)
import           GHCJS.Fetch
import           GHCJS.Marshal
import           GHCJS.Types
import           Network.HTTP.Types
import           Test.Hspec
import           Test.Hspec.Core.Runner (Config(..), hspecWith, defaultConfig, ColorMode(..))
import           Test.QuickCheck

main :: IO ()
main = do
  flip finally seleniumAsync $
    hspecWith defaultConfig {configColorMode = ColorNever} $ do
      describe "fetch" $ do
        it "can GET" $ do
          resp <-
            fetch (Request "https://httpbin.org/get" defaultRequestOptions)
          val <- responseJSON resp
          case val of
            Just (Object obj) ->
              HashMap.lookup "url" obj `shouldBe`
              Just (String "https://httpbin.org/get")
            _ -> expectationFailure ("Expected Object but got: " ++ show val)
        it "should throw on nonexisting URL" $
          fetch (Request "https://nonexistent.AA" defaultRequestOptions) `shouldThrow`
          (\(PromiseException _) -> True)
        it "can’t POST using GET" $ do
          resp <-
            fetch (Request "https://httpbin.org/post" defaultRequestOptions)
          responseText resp `shouldReturn`
            JSString.unlines
              [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">"
              , "<title>405 Method Not Allowed</title>"
              , "<h1>Method Not Allowed</h1>"
              , "<p>The method is not allowed for the requested URL.</p>"
              ]
        it "can POST" $ do
          resp <-
            fetch
              (Request
                 "https://httpbin.org/post"
                 defaultRequestOptions {reqOptMethod = methodPost})
          val <- responseJSON resp
          case val of
            Just (Object obj) ->
              HashMap.lookup "url" obj `shouldBe`
              Just (String "https://httpbin.org/post")
            _ -> expectationFailure ("Expected Object but got: " ++ show val)
        it "can POST text/plain" $ do
          resp <-
            fetch
              (Request
                 "https://httpbin.org/post"
                 defaultRequestOptions
                 { reqOptMethod = methodPost
                 , reqOptBody = Just (jsval ("my-text" :: JSString))
                 })
          val <- responseJSON resp
          case val of
            Just (Object obj) -> do
              (lookupKey "Content-Type" =<< HashMap.lookup "headers" obj) `shouldBe`
                Just (String "text/plain;charset=UTF-8")
              HashMap.lookup "data" obj `shouldBe` Just (String "my-text")
            Nothing ->
              expectationFailure ("Expected Object but got: " ++ show val)

lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object obj) = HashMap.lookup k obj
lookupKey _ _ = Nothing

foreign import javascript safe "console.log($1);"
  consoleLog :: JSVal -> IO ()
foreign import javascript safe "window.seleniumCallback();"
               seleniumAsync :: IO ()
