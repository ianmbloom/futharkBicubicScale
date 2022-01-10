
module Futhark.Entries where
import qualified Futhark.Raw as Raw
import qualified Futhark.Context as C
import Futhark.Fut (FutT)
import qualified Futhark.Fut as Fut
import qualified Futhark.Wrap as U
import Futhark.Types
import qualified Futhark.TypeClasses as T
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Foreign as F
import Foreign.C.Types
bicubicInterpolationImage ::
  Monad m => Int64 -> Int64 -> F32_3d c -> FutT c m (F32_3d c)
bicubicInterpolationImage newSizeY newSizeX input
  = Fut.unsafeLiftFromIO
      $ (\ context
           -> T.withFO input
                $ (\ input'
                     -> do out0 <- F.malloc
                           C.inContextWithError
                             context
                             (\ context'
                                -> Raw.entry_bicubicInterpolationImage
                                     context' out0 newSizeY newSizeX input')
                           out0' <- U.peekFreeWrapIn context out0
                           return out0'))
shuffler ::
  Monad m => Int64 -> Int64 -> Int64 -> FutT c m (I64_3d c)
shuffler seed h w
  = Fut.unsafeLiftFromIO
      $ (\ context
           -> do out0 <- F.malloc
                 C.inContextWithError
                   context (\ context' -> Raw.entry_shuffler context' out0 seed h w)
                 out0' <- U.peekFreeWrapIn context out0
                 return out0')
