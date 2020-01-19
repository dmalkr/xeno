{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}

-- | Shared types.

module Xeno.Types where

import           Control.DeepSeq
import           Control.Exception
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Unsafe as SU
import           Data.Data
import           Data.String     (IsString(..))
import           Data.Word
import           GHC.Generics

#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail

-- It is recommended to use more specific `failHere` instead
instance MonadFail (Either Xeno.Types.XenoException) where
  fail = Left . XenoParseError 0 . S8.pack
#endif

data XenoException
  = XenoStringIndexProblem { stringIndex :: Int, inputString :: ByteString }
  | XenoParseError         { inputIndex  :: Int, message     :: ByteString }
  | XenoExpectRootNode
  deriving (Show, Data, Typeable, NFData, Generic)

instance Exception XenoException where displayException = show

-- | ByteString guaranted have '\NUL' at the end
newtype ByteStringZeroTerminated =
  BSZT ByteString deriving (Eq, Show, Typeable, Generic, NFData)

instance IsString ByteStringZeroTerminated where
  fromString = fromBS
             . fromString

fromBS  :: S.ByteString -> ByteStringZeroTerminated
fromBS s = BSZT $ S.init $ s `S.snoc` 0

-- | VectorizedString class defines
--   minimal efficient interface for
--   **character arrays**.
--   It is a set of operations sufficient
--   for extremely fast parsing.
--
--   Operations are so chosen as to
--   facilitate vectorization by LLVM,
--   and extraction of ByteString result.
class ( Show           str
      , Eq             str
      , IsString       str
      )
   => VectorizedString str where
    s_index'       ::          str -> Int -> Word8
    elemIndexFrom' :: Word8 -> str -> Int -> Maybe Int
    drop'          :: Int   -> str ->                     str
    substring'     ::          str -> Int ->       Int -> str
    s_null         ::          str -> Bool
    --vs_fromBS      :: ByteString   ->                     str
    --toBS           ::          str -> ByteString

instance VectorizedString ByteString where
    s_null         = S.null
    {-# INLINE s_null #-}
    s_index'       = s_index
    {-# INLINE s_index' #-}
    elemIndexFrom' = elemIndexFrom
    {-# INLINE elemIndexFrom' #-}
    drop'          = S.drop
    {-# INLINE drop' #-}
    substring'     = substring
    {-# INLINE substring' #-}

-- | Same as ByteString, but with additional
--   invariant that requires it to be null terminated.
instance VectorizedString ByteStringZeroTerminated where
    s_null           (BSZT ps)     = S.null ps
    {-# INLINE s_null #-}
    s_index'         (BSZT ps) n   = ps `SU.unsafeIndex` n
    {-# INLINE s_index' #-}
    elemIndexFrom' w (BSZT bs) i   = elemIndexFrom w bs i
    {-# INLINE elemIndexFrom' #-}
    drop'          i (BSZT bs)     = BSZT $ S.drop i bs
    {-# INLINE drop' #-}
    substring'       (BSZT bs) s t = BSZT $ substring bs s t
    {-# INLINE substring' #-}

-- | A set of callbacks used for SAX parsing.
--   High level interface that allows one
--   to easily implement extension,
--   inheritance, and composition on event parser
--   without sacrificing performance.
data VectorizedString s
  => Process s a =
  Process {
      openF    :: !(s ->      a) -- ^ Open tag.
    , attrF    :: !(s -> s -> a) -- ^ Tag attribute.
    , endOpenF :: !(s ->      a) -- ^ End open tag.
    , textF    :: !(s ->      a) -- ^ Text.
    , closeF   :: !(s ->      a) -- ^ Close tag.
    , cdataF   :: !(s ->      a) -- ^ CDATA.
    }

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
s_index :: ByteString -> Int -> Word8
s_index ps n
    | n < 0            = throw (XenoStringIndexProblem n ps)
    | n >= S.length ps = throw (XenoStringIndexProblem n ps)
    | otherwise        = ps `SU.unsafeIndex` n
{-# INLINE s_index #-}

-- | Get a substring of a string.
substring :: ByteString -> Int -> Int -> ByteString
substring s start end = S.take (end - start) (S.drop start s)
{-# INLINE substring #-}

-- | Get index of an element starting from offset.
elemIndexFrom :: Word8 -> ByteString -> Int -> Maybe Int
elemIndexFrom c str offset = fmap (+ offset) (S.elemIndex c (S.drop offset str))
-- Without the INLINE below, the whole function is twice as slow and
-- has linear allocation. See git commit with this comment for
-- results.
{-# INLINE elemIndexFrom #-}
