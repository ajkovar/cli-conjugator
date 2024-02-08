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
 
persons :: [(String, Prop)]
persons = [("Yo", fps), ("Tú", sps), ("él/ella/Ud.", tps), ("nosotros", fpp), ("vosotros", spp), ("ellos/ellas/Uds.", tpp)]

basicTenses :: [Tense]
basicTenses = ["Presente", "Pretérito", "Imperfecto", "Imperfecto", "Futuro"]

data Table = Table 
  {
    cells :: [[Maybe String]],
    rowHeader :: [String],
    columnHeader :: [String]
  }
  deriving (Show, Eq)

instance Semigroup Table where
  t1 <> t2 = Table 
    {
      cells = getZipList $ (<>) <$> ZipList (cells t1) <*> ZipList (cells t2),
      rowHeader = rowHeader t1 <> rowHeader t2,
      columnHeader = columnHeader t1 <> columnHeader t2
    }

instance Monoid Table where
  mempty = Table { cells = repeat [], rowHeader = [], columnHeader = [] }
 
filterBlankRows :: Table -> Table
filterBlankRows g = g { cells = map snd filtered, rowHeader = map fst filtered }
  where filtered = filter ((any isJust) . snd) $ zip (rowHeader g) (cells g)

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
   
data DisplayMood = DisplayMood 
  {
    title :: String,
    moods :: [Mood],
    tenses :: [Tense],
    customHeaders :: Maybe [String]
  }
 
displayMood :: [NaturalWord] -> DisplayMood -> IO ()
displayMood nws dm = do
  putStrLn $ "\n" ++ (title dm) ++ "\n"
  putStrLn $ gridString (take 6 (repeat columnDef)) array
  where 
    columnDef = column (fixedUntil 12) left def def

    combined :: Table
    combined =  mconcat (map (generateTable nws (tenses dm)) (moods dm))

    filtered :: Table
    filtered = filterBlankColumns $ filterBlankRows combined
 
    array :: [[String]]
    array = toArray $ case customHeaders dm of
      Nothing -> filtered
      Just headers -> filtered{columnHeader = headers}

generateTable :: [NaturalWord] -> [Tense] -> Mood -> Table
generateTable nws tenses' m = Table 
  { cells = map ((flip map tenseValues) . applyProp . snd) persons,
    columnHeader = tenses',
    rowHeader = map fst persons
  }
  where
    moodMatches :: [NaturalWord]
    moodMatches = filter ((== m) . mood) nws

    tenseValues :: [Maybe NaturalWord]
    tenseValues = map (filterTense moodMatches) tenses'
 
main :: IO ()
main = do
  verb : _ <- getArgs
  conn <- open "conjugation.db"

  rows <- query conn "SELECT * from verbs where infinitive=?" (Only (verb :: String)) :: IO [NaturalWord]
  mapM_ (displayMood rows)
    [
      DisplayMood
        { 
        title = "Indicativo",
        moods = ["Indicativo"], 
        tenses = basicTenses,
        customHeaders = Nothing
        }, 
      DisplayMood
        { 
        title = "Subjuntivo",
        moods = ["Subjuntivo"], 
        tenses = basicTenses,
        customHeaders = Nothing
        }, 
      DisplayMood
        {
        title = "Imperativo",
        moods = ["Imperativo Afirmativo",  "Imperativo Negativo"], 
        tenses = basicTenses,
        customHeaders = Just ["Afirmativo", "Negativo"]
        } 
    ]

  close conn  
