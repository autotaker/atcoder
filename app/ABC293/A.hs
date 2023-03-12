module ABC293.A where

import           Data.Function ((&))
import qualified Data.Text     as T
import qualified Data.Text.IO  as T

main :: IO ()
main = do
    s <- T.getLine
    T.putStrLn $ solve s

solve :: T.Text -> T.Text
solve s =
    s & T.chunksOf 2
      & map T.reverse
      & T.concat


