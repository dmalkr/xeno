import qualified Data.ByteString as BS
import System.IO.Posix.MMap
import Xeno.SAX
import Data.Time.Clock

main :: IO ()
main = do
    -- let fn = "data/1htq.xml"
    let fn = "data/enwiki-20190901-pages-logging1.xml"
    -- let fn = "data/enwiki-20190901-pages-meta-current24.xml-p30503451p32003451.xml"
    putStrLn fn
    bs <- unsafeMMapFile fn
    -- bs <- BS.readFile fn
    print (BS.length bs)
    --
    -- print $ fold (\m _ -> m + 1) (\m _ _ -> m) const const const const 0 bs
    --
    start <- getCurrentTime
    let res = validate bs
    print res
    finish <- getCurrentTime
    let delta = finish `diffUTCTime` start
    print delta
