{-# LANGUAGE OverloadedStrings #-}

module GHCJS.Fetch.ByteString where

import GHCJS.Fetch
import GHCJS.Fetch.FFI
import GHCJS.Fetch.Types
import GHCJS.Buffer
import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer (..),MutableArrayBuffer (..),unsafeFreeze)
import Data.Binary
import Data.ByteString (ByteString(..))
import qualified Data.ByteString.Lazy as BL
import Data.String

fetchByteString :: Request -> IO ByteString
fetchByteString req = do
    response <- fetch req
    arr <- responseArrayBuffer response
    let bs = toByteString 0 Nothing $ createFromArrayBuffer arr
    return bs

fetchBinary :: Binary a => Request -> IO a
fetchBinary req = do
    bs <- fetchByteString req
    return $ decode (BL.fromStrict bs)
    
fetchBinaryFile :: Binary a => FilePath -> IO a
fetchBinaryFile = fetchBinaryFileWith opts
    where opts = defaultRequestOptions { reqOptHeaders = [("origin","http://anywhere.com")] }

fetchBinaryFileWith :: Binary a => RequestOptions -> FilePath -> IO a
fetchBinaryFileWith opts file = do
    let url = fromString file
    let req = Request url opts
    fetchBinary req
