{-# LANGUAGE ImplicitParams #-}
import Data.List
import Data.Char
import Control.Monad

shrinkDictionary :: [String] -> [String] -> [String]
shrinkDictionary wl dl =  let dict (w:wx) d dc = (search w d) : dict wx d dc
                              dict []     _      dc = dc
                              --dict _      []     dc = dc
                              search w (d:dx) = if w == (takeWhile (\l -> l /= '/') d) -- regex?  BAH!!!
                                                then d
                                                else search w dx
                              search w []     = w
                          in  dict wl dl []

sortDict :: [String] -> [String]
sortDict dict = let werdz = map (\x -> (map (\c -> toLower c) ((takeWhile (\l -> l /= '/') x)), x)) dict
  in map (\(_,l) -> l) $ sortBy (\(a,_) (b,_) -> compare a b) werdz


getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (lines contents)

--save :: (Show a) => a -> FilePath -> IO ()
save x f = writeFile f (unlines x)

main :: IO ()
main = do
  wordList <- getWords "Up-Goer-5.wordlist"
  dictionary <- getWords "en_US.dic"
  --sortedDict = sortDict dictionary
  --finalDict <- liftM (shrinkDictionary wordList $ sortDict dictionary
  save (shrinkDictionary wordList $ sortDict dictionary) "Up-Goer-5.dic"
