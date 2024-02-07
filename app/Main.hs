{-# LANGUAGE OverloadedStrings #-}

import Data.List (transpose)
import Database.SQLite.Simple
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import Control.Applicative
import Text.Layout.Table (gridString, left, fixedUntil, column, def)

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

data Table = Table 
  {
    cells :: [[Maybe String]],
    rowHeader :: [String],
    columnHeader :: [String]
  }
  deriving (Show)
 
filterBlankRows :: Table -> Table
filterBlankRows g = g { cells =(map snd filtered), rowHeader = (map fst filtered) }
  where filtered = filterAllBlank $ zip (rowHeader g) (cells g)

transposeTable :: Table -> Table
transposeTable g = Table (transpose (cells g)) (columnHeader g) (rowHeader g)

filterBlankColumns :: Table -> Table
filterBlankColumns = transposeTable . filterBlankRows . transposeTable

toArray :: Table -> [[String]]
toArray g = getZipList $ 
  (:)
  <$> ZipList ("":(rowHeader g)) 
  <*> ZipList ((columnHeader g):(map (map (fromMaybe "")) (cells g)))
 
filterTense :: [NaturalWord] -> Tense -> Maybe NaturalWord
filterTense nws t = listToMaybe $ filter ((== t) . tense) nws
 
applyProp :: (a -> String) -> Maybe a -> Maybe String
applyProp f m = if x == "" then Nothing else Just x 
  where x = fromMaybe "" (f <$> m)
 
filterAllBlank :: [(String, [Maybe String])] -> [(String, [Maybe String])]
filterAllBlank = filter ((any isJust) . snd) 
 
printMood :: [NaturalWord] -> Mood -> IO ()
printMood nws m = do
  putStrLn $ "\n" ++ m ++ "\n"
  putStrLn $ gridString (take 6 (repeat (column (fixedUntil 12) left def def))) array
  where
    moodMatches :: [NaturalWord]
    moodMatches = filter ((== m) . mood) nws

    tenseValues :: [Maybe NaturalWord]
    tenseValues = map (filterTense moodMatches) tenses

    tensesForPerson :: (Maybe NaturalWord -> Maybe String) -> [Maybe String]
    tensesForPerson = flip map tenseValues
 
    grid = Table 
      { cells = map (tensesForPerson . applyProp . snd) persons,
        columnHeader = tenses,
        rowHeader = map fst persons
      }

    array :: [[String]]
    array = toArray $ filterBlankRows $ filterBlankColumns grid 
 
main :: IO ()
main = do
  verb : _ <- getArgs
  conn <- open "conjugation.db"

  rows <- query conn "SELECT * from verbs where infinitive=?" (Only (verb :: String)) :: IO [NaturalWord]
  mapM_ (printMood rows) moods

  close conn
