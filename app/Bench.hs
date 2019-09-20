{-# LANGUAGE QuasiQuotes #-}
import Control.Monad
import Data.Time.Clock
import System.IO.Posix.MMap
import Text.InterpolatedString.Perl6 (qc)
import Xeno.SAX
import qualified Data.ByteString as BS


main :: IO ()
main = do
    let prefix = "/i/p/migamake/xeno/data/"
        files = map (prefix ++)
                [ {- 921 Mb -} "1htq.xml"
                , {- 190 Mb -} "enwiki-20190901-abstract10.xml"
                , {- 1.6 Gb -} "enwiki-20190901-pages-logging1.xml"
                , {- 4.0 Gb -} "enwiki-20190901-pages-meta-current24.xml-p30503451p32003451.xml"
                -- , {-  21 Gb -} "enwiki-20190901-pages-meta-history2.xml"
                --
                --
                , {- 921 Mb -} "1htq.xml"
                , {- 190 Mb -} "enwiki-20190901-abstract10.xml"
                , {- 1.6 Gb -} "enwiki-20190901-pages-logging1.xml"
                , {- 4.0 Gb -} "enwiki-20190901-pages-meta-current24.xml-p30503451p32003451.xml"

                , {- 921 Mb -} "1htq.xml"
                , {- 190 Mb -} "enwiki-20190901-abstract10.xml"
                , {- 1.6 Gb -} "enwiki-20190901-pages-logging1.xml"
                , {- 4.0 Gb -} "enwiki-20190901-pages-meta-current24.xml-p30503451p32003451.xml"

                , {- 921 Mb -} "1htq.xml"
                , {- 190 Mb -} "enwiki-20190901-abstract10.xml"
                , {- 1.6 Gb -} "enwiki-20190901-pages-logging1.xml"
                , {- 4.0 Gb -} "enwiki-20190901-pages-meta-current24.xml-p30503451p32003451.xml"

                , {- 921 Mb -} "1htq.xml"
                , {- 190 Mb -} "enwiki-20190901-abstract10.xml"
                , {- 1.6 Gb -} "enwiki-20190901-pages-logging1.xml"
                , {- 4.0 Gb -} "enwiki-20190901-pages-meta-current24.xml-p30503451p32003451.xml"

                , {- 921 Mb -} "1htq.xml"
                , {- 190 Mb -} "enwiki-20190901-abstract10.xml"
                , {- 1.6 Gb -} "enwiki-20190901-pages-logging1.xml"
                , {- 4.0 Gb -} "enwiki-20190901-pages-meta-current24.xml-p30503451p32003451.xml"
                ]
        --files = map (prefix ++)
        --        [ [> 4.0 Gb <] "enwiki-20190901-pages-meta-current24.xml-p30503451p32003451.xml"
        --        ]
    --
    deltas <- forM files $ \fn -> do
        putStrLn [qc|Processing file '{fn}'|]
        --
        -- NOTE: It is need to cache file in memory BEFORE start test.
        --       It can be done with `vmtouch` utility for example (`vmtouch -vtL *`).
        --
        bs <- unsafeMMapFile fn
        -- bs <- BS.readFile fn
        putStrLn [qc|  size: {BS.length bs `div` (1024*1024)} Mb|]
        -- print $ fold (\m _ -> m + 1) (\m _ _ -> m) const const const const 0 bs
        start <- getCurrentTime
        let res = validate bs
        putStrLn [qc|  process result: {res}|]
        finish <- getCurrentTime
        let delta = finish `diffUTCTime` start
        putStrLn [qc|  processing time: {delta}|]
        return delta
    --
    putStrLn "------"
    putStrLn [qc|Total: {sum deltas}|]
