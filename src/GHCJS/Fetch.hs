{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module GHCJS.Fetch
  ( fetch
  , responseJSON
  , responseText
  , Request(..)
  , RequestOptions(..)
  , defaultRequestOptions
  , Response(..)
  , PromiseException(..)
  ) where

import           Control.Exception
import           Data.Aeson hiding (Object)
import qualified Data.ByteString.Char8 as Char8
import           Data.Foldable
import qualified Data.JSString as JSString
import           Data.Typeable
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Object (setProp, getProp)
import qualified JavaScript.Object as Object
import           JavaScript.Object.Internal (Object(..))
import           Network.HTTP.Types
import           System.IO.Unsafe

data RequestOptions = RequestOptions
  { reqOptMethod :: !Method
  , reqOptBody :: !(Maybe JSVal)
  }

data Request = Request
  { requestUrl :: !JSString
  , requestOptions :: !RequestOptions
  }

defaultRequestOptions :: RequestOptions
defaultRequestOptions =
  RequestOptions {reqOptMethod = methodGet, reqOptBody = Nothing}

instance ToJSVal RequestOptions where
  toJSVal (RequestOptions { reqOptMethod, reqOptBody }) = do
    obj <- Object.create
    -- TODO verify that this can only be an ASCII string
    setProp "method" ((jsval . JSString.pack . Char8.unpack) reqOptMethod) obj
    traverse_ (\body -> setProp "body" body obj) reqOptBody
    return (jsval obj)

toJSRequest :: Request -> IO JSRequest
toJSRequest (Request url opts) = do
  opts' <- toJSVal opts
  js_newRequest url opts'

instance Show PromiseException where
  show _ = "PromiseException"

data PromiseException =
  PromiseException !JSVal
  deriving (Typeable)

instance Exception PromiseException

newtype Response = Response JSVal
  deriving FromJSVal

-- | Throws 'PromiseException' when the request fails
fetch :: Request -> IO Response
fetch req = do
  Response <$> (await =<< js_fetch =<< toJSRequest req)

responseJSON :: Response -> IO (Maybe Value)
responseJSON resp =
  fromJSVal =<< await =<< js_responseJSON resp

responseText :: Response -> IO JSString
responseText resp =
  fromJSValUnchecked =<< await =<< js_responseText resp

newtype JSRequest = JSRequest JSVal

newtype Promise a = Promise JSVal

await :: Promise a -> IO JSVal
await p = do
  obj <- js_await p
  Just success <- fromJSVal =<< getProp "success" obj
  if success
    then getProp "val" obj
    else do
      err <- getProp "val" obj
      throwIO (PromiseException err)

foreign import javascript safe "new Request($1, $2)" js_newRequest
               :: JSString -> JSVal -> IO JSRequest

foreign import javascript interruptible
               "$1.then(function(a) { $c({ 'val': a, 'success': true }); }, function(e) { $c({ 'val': e, 'success': false }); });"
               js_await :: Promise a -> IO Object

foreign import javascript safe "fetch($1)" js_fetch ::
               JSRequest -> IO (Promise Response)

foreign import javascript safe "$1.json()" js_responseJSON ::
               Response -> IO (Promise JSVal)

foreign import javascript safe "$1.text()" js_responseText ::
               Response -> IO (Promise JSString)

foreign import javascript safe "console.log(JSON.stringify($1));"
               consoleLog :: JSVal -> IO ()
