{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List (find)
import Database.SQLite.Simple

data NaturalWord = NaturalWord
  { infinitive :: String,
    mood :: String,
    tense :: String,
    translation :: String,
    fps :: String,
    sps :: String,
    tps :: String,
    fpp :: String,
    spp :: String,
    tpp :: String
  }
  deriving (Show)

instance FromRow NaturalWord where
  fromRow = NaturalWord <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

moods :: [String]
moods = ["Indicativo", "Subjuntivo", "Imperativo Afirmativo", "Imperativo Negativo"]

persons :: [NaturalWord -> String]
persons = [fps, sps, tps, fpp, spp, tpp]

tenses :: [String]
tenses = ["Presente", "Pretérito", "Imperfecto", "Imperfecto", "Futuro"]
 
padR :: Int -> String -> String
padR n s
  | length s < n = s ++ replicate (n - length s) ' '
  | otherwise = s

formatLine :: [String] -> String
formatLine line = foldl1 (++) (map (padR 15) line)

filterTense :: [NaturalWord] -> String -> [NaturalWord]
filterTense nws t = filter (\nw -> tense nw == t) nws

tensesForPerson :: (NaturalWord -> String) -> [NaturalWord] -> String
tensesForPerson personFn nws = formatLine $ map personFn $ concat $ map (filterTense nws) tenses

printMood :: [NaturalWord] -> String -> IO ()
printMood nws m = do
  putStrLn $ "\n" ++ m ++ "\n"
  putStrLn $ formatLine $ filter (\t -> length (filterTense indicatives t) > 0) tenses
  mapM_ (\personFn -> putStrLn $ tensesForPerson personFn indicatives) persons
  where
    indicatives = filter (\nw -> mood nw == m) nws

main :: IO ()
main = do
  conn <- open "conjugation.db"

  rows <- query_ conn "SELECT * from verbs where infinitive=\"correr\"" :: IO [NaturalWord]
  mapM_ (printMood rows) moods

  close conn
