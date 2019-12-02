{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Streamed SAX parser and API for XML.

module Xeno.StreamedSAX
  ( process
  , SAXEvent(..)
  , validate
  , validateEx
  -- , dump
  ) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Spork
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Unsafe as SU
import           Data.Word
import           GHC.Generics
import           Xeno.Types


data SAXEvent = OpenElt    !ByteString
              | AttrElt    !ByteString !ByteString
              | EndOpenElt !ByteString
              | TextElt    !ByteString
              | CloseElt   !ByteString
              | CdataElt   !ByteString
              deriving (Eq, Ord, Show, Read, Generic, NFData)

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
validate str =
  case spork $ force $ process str of
    Left (_ :: XenoException) -> False
    Right _ -> True


-- | Parse the XML and checks tags nesting.
--
validateEx :: ByteString -> Bool
validateEx str =
  case spork $ checkStream [] $ process str of
    Left (_ :: XenoException) -> False
    Right _ -> True
  where
    checkStream :: [ByteString] -> [SAXEvent]  -> ()
    checkStream []          [] = ()
    checkStream tags        [] = error $ concat ["Some tags not matched: ", show tags]
    checkStream tags        (OpenElt tag  : events) = checkStream (tag : tags) events
    checkStream []          (CloseElt tag : _)      = error $ concat ["Unexpected close tag \"", show tag, "\""]
    checkStream (tag':tags) (CloseElt tag : events)
      | tag == tag' = checkStream tags events
      | otherwise   = error $ concat ["Expected \"", show tag', "\" tag, but got \"", show tag, "\""]
    checkStream tags (_ : events) = checkStream tags events



-- TODO : restore with streamed interface
---- | Parse the XML and pretty print it to stdout.
--dump :: ByteString -> IO ()
--dump str =
--  evalStateT
--    (process
--       Process {
--         openF = \name -> do
--          level <- get
--          lift (S8.putStr (S8.replicate level ' ' <> "<" <> name <> ""))
--       , attrF = \key value -> lift (S8.putStr (" " <> key <> "=\"" <> value <> "\""))
--       , endOpenF = \_ -> do
--          level <- get
--          let !level' = level + 2
--          put level'
--          lift (S8.putStrLn (">"))
--       , textF = \text -> do
--          level <- get
--          lift (S8.putStrLn (S8.replicate level ' ' <> S8.pack (show text)))
--       , closeF = \name -> do
--          level <- get
--          let !level' = level - 2
--          put level'
--          lift (S8.putStrLn (S8.replicate level' ' ' <> "</" <> name <> ">"))
--       , cdataF = \cdata -> do
--          level <- get
--          lift (S8.putStrLn (S8.replicate level ' ' <> "CDATA: " <> S8.pack (show cdata)))
--       }
--       str)
--    (0 :: Int)


--------------------------------------------------------------------------------
-- Main parsing function

-- TODO transform to FUSION

-- | Process events with callbacks in the XML input.
process :: ByteString -> [SAXEvent]
process str' = findLT 0
  where
    preText text next | S.null text    =                next
                      | text == "\NUL" =                next
                      | otherwise      = TextElt text : next
    -- We add \NUL to omit length check in `s_index`
    -- Also please see https://gitlab.com/migamake/xeno/issues/1
    str = str' `S.snoc` 0
    findLT index =
      case elemIndexFrom openTagChar str index of
        Nothing ->
            preText (S.drop index str) []
        Just fromLt ->
            preText (substring str index fromLt) (checkOpenComment (fromLt + 1))
    -- Find open comment, CDATA or tag name.
    checkOpenComment index =
      if | s_index this 0 == bangChar -- !
           && s_index this 1 == commentChar -- -
           && s_index this 2 == commentChar -> -- -
           findCommentEnd (index + 3)
         | s_index this 0 == bangChar -- !
           && s_index this 1 == openAngleBracketChar -- [
           && s_index this 2 == 67 -- C
           && s_index this 3 == 68 -- D
           && s_index this 4 == 65 -- A
           && s_index this 5 == 84 -- T
           && s_index this 6 == 65 -- A
           && s_index this 7 == openAngleBracketChar -> -- [
           findCDataEnd (index + 8) (index + 8)
         | otherwise ->
           findTagName index
      where
        this = S.drop index str
    findCommentEnd index =
      case elemIndexFrom commentChar str index of
        Nothing -> throw $ XenoParseError index "Couldn't find the closing comment dash."
        Just fromDash ->
          if s_index this 0 == commentChar && s_index this 1 == closeTagChar
            then findLT (fromDash + 2)
            else findCommentEnd (fromDash + 1)
          where this = S.drop index str
    findCDataEnd cdata_start index =
      case elemIndexFrom closeAngleBracketChar str index of
        Nothing -> throw $ XenoParseError index "Couldn't find closing angle bracket for CDATA."
        Just fromCloseAngleBracket ->
          if s_index str (fromCloseAngleBracket + 1) == closeAngleBracketChar
             then
               CdataElt (substring str cdata_start fromCloseAngleBracket) :
               findLT (fromCloseAngleBracket + 3) -- Start after ]]>
             else
               -- We only found one ], that means that we need to keep searching.
               findCDataEnd cdata_start (fromCloseAngleBracket + 1)
    findTagName index0 =
      let spaceOrCloseTag = parseName str index
      in if | s_index str index0 == questionChar ->
              case elemIndexFrom closeTagChar str spaceOrCloseTag of
                Nothing -> throw $ XenoParseError index "Couldn't find the end of the tag."
                Just fromGt ->
                  findLT (fromGt + 1)
            | s_index str spaceOrCloseTag == closeTagChar ->
                 let tagname = substring str index spaceOrCloseTag
                     next = findLT (spaceOrCloseTag + 1)
                 in if s_index str index0 == slashChar
                   then (CloseElt tagname                      : next)
                   else (OpenElt  tagname : EndOpenElt tagname : next)
            | otherwise ->
              let tagname = substring str index spaceOrCloseTag
                  (result, next) = findAttributes spaceOrCloseTag
              in OpenElt tagname : (next ++ EndOpenElt tagname : (case result of -- TODO change `++` with some fusion
                   Right closingTag ->                    findLT (closingTag  + 1)
                   Left closingPair -> CloseElt tagname : findLT (closingPair + 2)))
      where
        index =
          if s_index str index0 == slashChar
            then index0 + 1
            else index0
    findAttributes index0 =
      if s_index str index == slashChar &&
         s_index str (index + 1) == closeTagChar
        then (Left index, [])
        else if s_index str index == closeTagChar
               then (Right index, [])
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
                                          Just endQuoteIndex ->
                                            let (result, next) = findAttributes (endQuoteIndex + 1)
                                            in  (result, AttrElt (substring str index afterAttrName)
                                                                 (substring
                                                                     str
                                                                     (quoteIndex + 1)
                                                                     (endQuoteIndex)) : next)
                                   else throw
                                         (XenoParseError index("Expected ' or \", got: " <> S.singleton usedChar))
                         else throw (XenoParseError index ("Expected =, got: " <> S.singleton (s_index str afterAttrName) <> " at character index: " <> (S8.pack . show) afterAttrName))
      where
        index = skipSpaces str index0
{-# INLINE process #-}

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
