{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SAX parser and API for XML.

module Xeno.SAX
  ( process
  , Process(..)
  , fold
  , validate
  , validateEx
  , dump
  , skipDoctype
  ) where

import           Control.Exception
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Control.Spork
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Unsafe as SU
import           Data.Char(isSpace)
import           Data.Functor.Identity
import           Data.STRef
import           Data.Word
import           Xeno.Types

data Process (m :: * -> *)  =
  Process {
      openF    :: !(ByteString ->               m (Process m)) -- ^ Open tag.
    , attrF    :: !(ByteString -> ByteString -> m ()) -- ^ Tag attribute.
    , endOpenF :: !(ByteString ->               m ()) -- ^ End open tag.
    , textF    :: !(ByteString ->               m ()) -- ^ Text.
    , closeF   :: !(ByteString ->               m (Process m)) -- ^ Close tag.
    , cdataF   :: !(ByteString ->               m ()) -- ^ CDATA.
    }

--------------------------------------------------------------------------------
-- Helpful interfaces to the parser

-- | Parse the XML but return no result, process no events.
--
-- N.B.: Only the lexical correctness of the input string is checked, not its XML semantics (e.g. only if tags are well formed, not whether tags are properly closed)
--
-- > > :set -XOverloadedStrings
-- > > validate "<b>"
-- > True
--
-- > > validate "<b"
-- > False
validate :: ByteString -> Bool
validate s =
  case spork
         (runIdentity
            (let pr = Process {
                 openF    = \_   -> pure pr
               , attrF    = \_ _ -> pure ()
               , endOpenF = \_   -> pure ()
               , textF    = \_   -> pure ()
               , closeF   = \_   -> pure pr
               , cdataF   = \_   -> pure ()
               }
            in process pr s)) of
    Left (_ :: XenoException) -> False
    Right _ -> True


-- | Parse the XML and checks tags nesting.
--
validateEx :: ByteString -> Bool
validateEx s =
  case spork
         (runST $ do
            tags <- newSTRef []
            let pr = Process {
                 openF    = \tag   -> modifySTRef' tags (tag:) >> return pr
               , attrF    = \_ _ -> pure ()
               , endOpenF = \_   -> pure ()
               , textF    = \_   -> pure ()
               , closeF   = \tag  -> do
                   modifySTRef' tags $ \case
                      [] -> fail $ "Unexpected close tag \"" ++ show tag ++ "\""
                      (expectedTag:tags') ->
                          if expectedTag == tag
                          then tags'
                          else fail $ "Unexpected close tag. Expected \"" ++ show expectedTag ++ "\", but got \"" ++ show tag ++ "\""
                   return pr
               , cdataF   = \_   -> pure ()
               }
            (process pr s)
            readSTRef tags >>= \case
                [] -> return ()
                tags' -> fail $ "Not all tags closed: " ++ show tags'
         ) of
    Left (_ :: XenoException) -> False
    Right _ -> True


-- | Parse the XML and pretty print it to stdout.
dump :: ByteString -> IO ()
dump str = evalStateT (process pr str) (0 :: Int)
  where
      pr = Process {
         openF = \name -> do
          level <- get
          lift (S8.putStr (S8.replicate level ' ' <> "<" <> name <> ""))
          return pr
       , attrF = \key value -> lift (S8.putStr (" " <> key <> "=\"" <> value <> "\""))
       , endOpenF = \_ -> do
          level <- get
          let !level' = level + 2
          put level'
          lift (S8.putStrLn (">"))
       , textF = \text -> do
          level <- get
          lift (S8.putStrLn (S8.replicate level ' ' <> S8.pack (show text)))
       , closeF = \name -> do
          level <- get
          let !level' = level - 2
          put level'
          lift (S8.putStrLn (S8.replicate level' ' ' <> "</" <> name <> ">"))
          return pr
       , cdataF = \cdata -> do
          level <- get
          lift (S8.putStrLn (S8.replicate level ' ' <> "CDATA: " <> S8.pack (show cdata)))
       }

-- | Fold over the XML input.
fold
  :: (s -> ByteString -> s) -- ^ Open tag.
  -> (s -> ByteString -> ByteString -> s) -- ^ Attribute key/value.
  -> (s -> ByteString -> s) -- ^ End of open tag.
  -> (s -> ByteString -> s) -- ^ Text.
  -> (s -> ByteString -> s) -- ^ Close tag.
  -> (s -> ByteString -> s) -- ^ CDATA.
  -> s
  -> ByteString
  -> Either XenoException s
fold openF attrF endOpenF textF closeF cdataF s str =
  let pr = Process {
            openF    = \name -> pr <$ modify (\s' -> openF s' name)
          , attrF    = \key value -> modify (\s' -> attrF s' key value)
          , endOpenF = \name -> modify (\s' -> endOpenF s' name)
          , textF    = \text -> modify (\s' -> textF s' text)
          , closeF   = \name -> pr <$ modify (\s' -> closeF s' name)
          , cdataF   = \cdata -> modify (\s' -> cdataF s' cdata)
        }
  in spork (execState (process pr str) s)

--------------------------------------------------------------------------------
-- Main parsing function

-- | Process events with callbacks in the XML input.
process
  :: Monad m
  => Process m
  -> ByteString -> m ()
process {- !(Process {openF, attrF, endOpenF, textF, closeF, cdataF}) -} !prcs str' = findLT prcs 0
  where
    -- We add \NUL to omit length check in `s_index`
    -- Also please see https://gitlab.com/migamake/xeno/issues/1
    str = str' `S.snoc` 0
    findLT !pr@Process{textF} index =
      case elemIndexFrom openTagChar str index of
        Nothing -> unless (S.null text) (textF text)
          where text = S.drop index str
        Just fromLt -> do
          unless (S.null text) (textF text)
          checkOpenComment pr (fromLt + 1)
          where text = substring str index fromLt
    -- Find open comment, CDATA or tag name.
    checkOpenComment !pr@Process{} index =
      if | s_index this 0 == bangChar -- !
           && s_index this 1 == commentChar -- -
           && s_index this 2 == commentChar -> -- -
           findCommentEnd pr (index + 3)
         | s_index this 0 == bangChar -- !
           && s_index this 1 == openAngleBracketChar -- [
           && s_index this 2 == 67 -- C
           && s_index this 3 == 68 -- D
           && s_index this 4 == 65 -- A
           && s_index this 5 == 84 -- T
           && s_index this 6 == 65 -- A
           && s_index this 7 == openAngleBracketChar -> -- [
           findCDataEnd pr (index + 8) (index + 8)
         | otherwise ->
           findTagName pr index
      where
        this = S.drop index str
    findCommentEnd !pr index =
      case elemIndexFrom commentChar str index of
        Nothing -> throw $ XenoParseError index "Couldn't find the closing comment dash."
        Just fromDash ->
          if s_index this 0 == commentChar && s_index this 1 == closeTagChar
            then findLT pr (fromDash + 2)
            else findCommentEnd pr (fromDash + 1)
          where this = S.drop index str
    findCDataEnd !pr@Process{cdataF} cdata_start index =
      case elemIndexFrom closeAngleBracketChar str index of
        Nothing -> throw $ XenoParseError index "Couldn't find closing angle bracket for CDATA."
        Just fromCloseAngleBracket ->
          if s_index str (fromCloseAngleBracket + 1) == closeAngleBracketChar
             then do
               cdataF (substring str cdata_start fromCloseAngleBracket)
               findLT pr (fromCloseAngleBracket + 3) -- Start after ]]>
             else
               -- We only found one ], that means that we need to keep searching.
               findCDataEnd pr cdata_start (fromCloseAngleBracket + 1)
    findTagName !pr@Process{openF} index0 =
      let spaceOrCloseTag = parseName str index
      in if | s_index str index0 == questionChar ->
              case elemIndexFrom closeTagChar str spaceOrCloseTag of
                Nothing -> throw $ XenoParseError index "Couldn't find the end of the tag."
                Just fromGt -> do
                  findLT pr (fromGt + 1)
            | s_index str spaceOrCloseTag == closeTagChar ->
              do let tagname = substring str index spaceOrCloseTag
                 pr' <- if s_index str index0 == slashChar
                   then closeF pr tagname
                   else do
                     pr''@Process{endOpenF} <- openF tagname
                     endOpenF tagname
                     return pr''
                 findLT pr' (spaceOrCloseTag + 1)
            | otherwise ->
              do let tagname = substring str index spaceOrCloseTag
                 pr'@Process{endOpenF,closeF} <- openF tagname
                 result <- findAttributes pr' spaceOrCloseTag
                 endOpenF tagname
                 case result of
                   Right closingTag -> findLT pr' (closingTag + 1)
                   Left closingPair -> do
                     pr'' <- closeF tagname
                     findLT pr'' (closingPair + 2)
      where
        index =
          if s_index str index0 == slashChar
            then index0 + 1
            else index0
    findAttributes !pr@Process{attrF} index0 =
      if s_index str index == slashChar &&
         s_index str (index + 1) == closeTagChar
        then pure (Left index)
        else if s_index str index == closeTagChar
               then pure (Right index)
               else let afterAttrName = parseName str index
                    in if s_index str afterAttrName == equalChar
                         then let quoteIndex = afterAttrName + 1
                                  usedChar = s_index str quoteIndex
                              in if usedChar == quoteChar ||
                                    usedChar == doubleQuoteChar
                                   then case elemIndexFrom
                                               usedChar
                                               str
                                               (quoteIndex + 1) of
                                          Nothing ->
                                            throw
                                              (XenoParseError index "Couldn't find the matching quote character.")
                                          Just endQuoteIndex -> do
                                            attrF
                                              (substring str index afterAttrName)
                                              (substring
                                                 str
                                                 (quoteIndex + 1)
                                                 (endQuoteIndex))
                                            findAttributes pr (endQuoteIndex + 1)
                                   else throw
                                         (XenoParseError index("Expected ' or \", got: " <> S.singleton usedChar))
                         else throw (XenoParseError index ("Expected =, got: " <> S.singleton (s_index str afterAttrName) <> " at character index: " <> (S8.pack . show) afterAttrName))
      where
        index = skipSpaces str index0
{-# INLINE process #-}
{-# SPECIALISE process :: Process Identity -> ByteString -> Identity ()
               #-}
{-# SPECIALISE process :: Process (State s) -> ByteString -> State s ()
               #-}
{-# SPECIALISE process :: Process (ST s) -> ByteString -> ST s ()
               #-}
{-# SPECIALISE process :: Process IO -> ByteString -> IO ()
               #-}

--------------------------------------------------------------------------------
-- ByteString utilities

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
s_index :: ByteString -> Int -> Word8
s_index ps n
    -- 1) `n` is always non-negative;
    -- 2) parser will stop on last `\NUL` (added in `process`) so we don't need to check crossing string boundary.
    --    Also please see https://gitlab.com/migamake/xeno/issues/1
    --
    -- | n < 0            = throw (XenoStringIndexProblem n ps)
    -- | n >= S.length ps = throw (XenoStringIndexProblem n ps)
    | otherwise      = ps `SU.unsafeIndex` n
{-# INLINE s_index #-}

-- | A fast space skipping function.
skipSpaces :: ByteString -> Int -> Int
skipSpaces str i =
  if isSpaceChar (s_index str i)
    then skipSpaces str (i + 1)
    else i
{-# INLINE skipSpaces #-}

-- | Get a substring of a string.
substring :: ByteString -> Int -> Int -> ByteString
substring s start end = S.take (end - start) (S.drop start s)
{-# INLINE substring #-}

-- | Basically @findIndex (not . isNameChar)@, but doesn't allocate.
parseName :: ByteString -> Int -> Int
parseName str index =
  if not (isNameChar1 (s_index str index))
     then index
     else parseName' str (index + 1)
{-# INLINE parseName #-}

-- | Basically @findIndex (not . isNameChar)@, but doesn't allocate.
parseName' :: ByteString -> Int -> Int
parseName' str index =
  if not (isNameChar (s_index str index))
     then index
     else parseName' str (index + 1)
{-# INLINE parseName' #-}

-- | Get index of an element starting from offset.
elemIndexFrom :: Word8 -> ByteString -> Int -> Maybe Int
elemIndexFrom c str offset = fmap (+ offset) (S.elemIndex c (S.drop offset str))
-- Without the INLINE below, the whole function is twice as slow and
-- has linear allocation. See git commit with this comment for
-- results.
{-# INLINE elemIndexFrom #-}

--------------------------------------------------------------------------------
-- Character types

isSpaceChar :: Word8 -> Bool
isSpaceChar = testBit (0b100000000000000000010011000000000 :: Int) . fromIntegral
--                       |                  |  ||  bits:
--                       |                  |  |+-- 9
--                       |                  |  +--- 10
--                       |                  +------ 13
--                       +------------------------- 32
{-# INLINE isSpaceChar #-}

-- | Is the character a valid first tag/attribute name constituent?
-- 'a'-'z', 'A'-'Z', '_', ':'
isNameChar1 :: Word8 -> Bool
isNameChar1 c =
  (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c == 95 || c == 58
{-# INLINE isNameChar1 #-}

-- isNameCharOriginal :: Word8 -> Bool
-- isNameCharOriginal c =
--   (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c == 95 || c == 58 ||
--   c == 45 || c == 46 || (c >= 48 && c <= 57)
-- {-# INLINE isNameCharOriginal #-}
--
-- TODO Strange, but highMaskIsNameChar, lowMaskIsNameChar don't calculate fast... FIX IT
-- highMaskIsNameChar, lowMaskIsNameChar :: Word64
-- (highMaskIsNameChar, lowMaskIsNameChar) =
--     foldl (\(hi,low) char -> (hi `setBit` (char - 64), low `setBit` char)) -- NB: `setBit` can process overflowed values (where char < 64; -- TODO fix it
--           (0::Word64, 0::Word64)
--           (map fromIntegral (filter isNameCharOriginal [0..128]))
-- {-# INLINE highMaskIsNameChar #-}
-- {-# INLINE lowMaskIsNameChar #-}

-- | Is the character a valid tag/attribute name constituent?
-- isNameChar1 + '-', '.', '0'-'9'
isNameChar :: Word8 -> Bool
isNameChar char = (lowMaskIsNameChar `testBit` char'low) || (highMaskIsNameChar `testBit` char'high)
   -- TODO 1) change code to use W# instead of Word64
   --      2) Document `ii - 64` -- there is underflow, but `testBit` can process this!
  where
    char'low  = fromIntegral char
    char'high = fromIntegral (char - 64)
    highMaskIsNameChar :: Word64
    highMaskIsNameChar = 0b11111111111111111111111111010000111111111111111111111111110
    --                     ------------+------------- |    ------------+-------------
    --                                 |              |                |  bits:
    --                                 |              |                +-- 65-90
    --                                 |              +------------------- 95
    --                                 +---------------------------------- 97-122
    lowMaskIsNameChar :: Word64
    lowMaskIsNameChar =  0b11111111111011000000000000000000000000000000000000000000000
    --                     -----+----- ||
    --                          |      ||  bits:
    --                          |      |+-- 45
    --                          |      +--- 46
    --                          +---------- 48-58
{-# INLINE isNameChar #-}

-- | Char for '\''.
quoteChar :: Word8
quoteChar = 39

-- | Char for '"'.
doubleQuoteChar :: Word8
doubleQuoteChar = 34

-- | Char for '='.
equalChar :: Word8
equalChar = 61

-- | Char for '?'.
questionChar :: Word8
questionChar = 63

-- | Char for '/'.
slashChar :: Word8
slashChar = 47

-- | Exclaimation character !.
bangChar :: Word8
bangChar = 33

-- | The dash character.
commentChar :: Word8
commentChar = 45 -- '-'

-- | Open tag character.
openTagChar :: Word8
openTagChar = 60 -- '<'

-- | Close tag character.
closeTagChar :: Word8
closeTagChar = 62 -- '>'

-- | Open angle bracket character.
openAngleBracketChar :: Word8
openAngleBracketChar = 91

-- | Close angle bracket character.
closeAngleBracketChar :: Word8
closeAngleBracketChar = 93

-- | Skip initial DOCTYPE declaration
skipDoctype :: ByteString -> ByteString
skipDoctype arg =
    if "<!DOCTYPE" `S8.isPrefixOf` bs
      then let (_, rest)=">" `S8.breakSubstring` bs
           in skipBlanks $ S8.drop 1 rest
      else bs
  where
    bs = skipBlanks arg
    skipBlanks = S8.dropWhile isSpace
