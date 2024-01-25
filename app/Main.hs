{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List (find)
import Database.SQLite.Simple
import System.Environment (getArgs)

type Mood = String

type Tense = String

type Prop = NaturalWord -> String

data NaturalWord = NaturalWord
  { infinitive :: String,
    mood :: Mood,
    tense :: Tense,
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

moods :: [Mood]
moods = ["Indicativo", "Subjuntivo", "Imperativo Afirmativo", "Imperativo Negativo"]

persons :: [(String, Prop)]
persons = [("Yo", fps), ("Tú", sps), ("él/ella/Ud.", tps), ("nosotros", fpp), ("vosotros", spp), ("ellos/ellas/Uds.", tpp)]

tenses :: [Tense]
tenses = ["Presente", "Pretérito", "Imperfecto", "Imperfecto", "Futuro"]

padR :: Int -> String -> String
padR n s
  | length s < n = s ++ replicate (n - length s) ' '
  | otherwise = s

formatLine :: [String] -> String
formatLine line = foldl1 (++) (map (padR 16) line)

filterTense :: [NaturalWord] -> Tense -> [NaturalWord]
filterTense nws t = filter ((== t) . tense) nws

printMood :: [NaturalWord] -> Mood -> IO ()
printMood nws m = do
  putStrLn $ "\n" ++ m ++ "\n"
  putStrLn $ formatLine $ filter (\t -> length (filterTense moodMatches t) > 0) tenses
  mapM_ printPerson persons
  where
    moodMatches = filter ((== m) . mood) nws
    tenseValues = concat $ map (filterTense moodMatches) tenses
    personLabels = map fst persons
    tensesForPerson personFn = map personFn tenseValues
    printPerson (label, personFn) = putStrLn $ formatLine $ label:tensesForPerson personFn

main :: IO ()
main = do
  verb : args <- getArgs
  conn <- open "conjugation.db"

  rows <- query conn "SELECT * from verbs where infinitive=?" (Only (verb :: String)) :: IO [NaturalWord]
  mapM_ (printMood rows) moods

  close conn
