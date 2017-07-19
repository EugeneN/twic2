{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

module Foreign.JavaScript.Utils
  ( synchronously
  , freeRequestAnimationFrameCallback
  , js_dataView
  , bsFromMutableArrayBuffer
  , bsToArrayBuffer
  ) where

import GHCJS.Concurrent
import GHCJS.DOM.Types (JSM, JSVal, RequestAnimationFrameCallback (..))
import GHCJS.Foreign.Callback (releaseCallback)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified GHCJS.Buffer as JS
import GHCJS.DOM.Types (ArrayBuffer (..))
import qualified JavaScript.TypedArray.ArrayBuffer as JS
import Language.Javascript.JSaddle.Types (MonadJSM, ghcjsPure, jsval, liftJSM)

freeRequestAnimationFrameCallback :: RequestAnimationFrameCallback -> JSM ()
freeRequestAnimationFrameCallback (RequestAnimationFrameCallback cb) = releaseCallback cb

foreign import javascript safe "new DataView($3,$1,$2)"
  js_dataView :: Int -> Int -> JSVal -> IO JSVal

{-# INLINABLE bsFromMutableArrayBuffer #-}
bsFromMutableArrayBuffer :: MonadJSM m => JS.MutableArrayBuffer -> m ByteString
bsFromMutableArrayBuffer ab = liftJSM $ JS.unsafeFreeze ab >>=
    ghcjsPure . JS.createFromArrayBuffer >>= ghcjsPure . JS.toByteString 0 Nothing

{-# INLINABLE bsToArrayBuffer #-}
bsToArrayBuffer :: MonadJSM m => ByteString -> m ArrayBuffer
bsToArrayBuffer bs = liftJSM $ do
  (b, off, len) <- ghcjsPure $ JS.fromByteString bs
  ArrayBuffer <$> if BS.length bs == 0 --TODO: remove this logic when https://github.com/ghcjs/ghcjs-base/issues/49 is fixed
                  then JS.create 0 >>= ghcjsPure . JS.getArrayBuffer >>= ghcjsPure . jsval
                  else do
                    ref <- ghcjsPure (JS.getArrayBuffer b) >>= ghcjsPure . jsval
                    js_dataView off len ref
