{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Fetch
  ( fetch
  , responseJSON
  , Request(..)
  , RequestOptions(..)
  , Response(..)
  ) where

import Control.Exception
import Data.Aeson
import Data.Typeable
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types
import System.IO.Unsafe

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

-- | Throws 'PromiseException' when the request fails
fetch :: Request -> IO Response
fetch (Request url _) = js_fetch url exceptionHandler

{-# NOINLINE exceptionHandler #-}
exceptionHandler :: Callback (JSVal -> IO ())
exceptionHandler = (unsafePerformIO . asyncCallback1) (\e -> throwIO (PromiseException e))

responseJSON :: Response -> IO (Maybe Value)
responseJSON resp = fromJSVal =<< js_responseJSON resp exceptionHandler

foreign import javascript interruptible
               "fetch($1).then($c, $2);" js_fetch ::
               JSString -> Callback (JSVal -> IO ()) -> IO Response

foreign import javascript interruptible "$1.json().then($c, $2);"
               js_responseJSON ::
               Response -> Callback (JSVal -> IO ()) -> IO JSVal
