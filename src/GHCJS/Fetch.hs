{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module GHCJS.Fetch
  ( fetch
  , responseJSON
  , Request(..)
  , RequestOptions(..)
  , Response(..)
  , PromiseException(..)
  ) where

import           Control.Exception
import           Data.Aeson hiding (Object)
import           Data.Typeable
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Object (setProp, getProp)
import qualified JavaScript.Object as Object
import           JavaScript.Object.Internal (Object(..))
import           System.IO.Unsafe

data TypeError

data USVString

data Mode = Cors | NoCors | SameOrigin

data Credentials
  = CredOmit
  | CredSameOrigin
  | CredInclude
  -- | FederatedCredential
  -- | PasswordCredential

data CacheMode
  = Default
  | NoStore
  | Reload
  | NoCache
  | ForceCache
  | OnlyIfCached

data RedirectMode
  = Follow
  | Error
  | Manual

data Referrer =
  Referrer !JSVal

data ReferrerPolicy
  = NoReferrer
  | NoReferrerWhenDownGrade
  | Origin
  | OriginWhenCrossOrigin
  | UnsafeUrl

data Integrity =
  Integrity !JSVal

-- signal and observe have not been standarized

data RequestOptions = RequestOptions
  deriving (Show, Eq, Ord)

data Request = Request
  { requestUrl :: !JSString
  , requestOptions :: !RequestOptions
  }

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
fetch (Request url _) = do
  reqOpts <- Object.create
  req <- js_newRequest url reqOpts
  Response <$> (await =<< js_fetch req)

responseJSON :: Response -> IO (Maybe Value)
responseJSON resp =
  fromJSVal =<< await =<< js_responseJSON resp

newtype JSRequest = JSRequest JSVal

foreign import javascript safe "new Request($1)" js_newRequest ::
               JSString -> Object -> IO JSRequest

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

foreign import javascript interruptible
               "$1.then(function(a) { $c({ 'val': a, 'success': true }); }, function(e) { $c({ 'val': e, 'success': false }); });"
               js_await :: Promise a -> IO Object

foreign import javascript safe "fetch($1)" js_fetch ::
               JSRequest -> IO (Promise Response)

foreign import javascript safe "$1.json()" js_responseJSON ::
               Response -> IO (Promise JSVal)
