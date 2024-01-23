{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List (find)
import Database.SQLite.Simple 
import System.Environment (getArgs)

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

type Mood = String
type Tense = String

moods :: [Mood]
moods = ["Indicativo", "Subjuntivo", "Imperativo Afirmativo", "Imperativo Negativo"]

persons :: [NaturalWord -> String]
persons = [fps, sps, tps, fpp, spp, tpp]

tenses :: [Tense]
tenses = ["Presente", "Pretérito", "Imperfecto", "Imperfecto", "Futuro"]
 
padR :: Int -> String -> String
padR n s
  | length s < n = s ++ replicate (n - length s) ' '
  | otherwise = s

formatLine :: [String] -> String
formatLine line = foldl1 (++) (map (padR 15) line)

filterTense :: [NaturalWord] -> Tense -> [NaturalWord]
filterTense nws t = filter (\nw -> tense nw == t) nws

tensesForPerson :: (NaturalWord -> String) -> [NaturalWord] -> String
tensesForPerson personFn nws = formatLine $ map personFn $ concat $ map (filterTense nws) tenses

printMood :: [NaturalWord] -> Mood -> IO ()
printMood nws m = do
  putStrLn $ "\n" ++ m ++ "\n"
  putStrLn $ formatLine $ filter (\t -> length (filterTense currentMood t) > 0) tenses
  mapM_ (\personFn -> putStrLn $ tensesForPerson personFn currentMood) persons
  where
    currentMood = filter (\nw -> mood nw == m) nws

main :: IO ()
main = do
  verb:args <- getArgs 
  conn <- open "conjugation.db"

  rows <- query conn "SELECT * from verbs where infinitive=?" (Only (verb :: String)) :: IO [NaturalWord]
  mapM_ (printMood rows) moods

  close conn
