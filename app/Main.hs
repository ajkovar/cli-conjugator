{-# LANGUAGE OverloadedStrings #-}

import Data.List (transpose)
import Database.SQLite.Simple
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import Control.Applicative
import Text.Layout.Table (left, expandUntil, column, def, tableString, unicodeS, fullTableS, titlesH, justifyText, center, colsAllG)
import System.Console.ANSI
import Text.Layout.Table.Cell.Formatted
import System.IO (stdout)

type Mood = String

type Tense = String

type Prop = Verb -> String

data Verb = Verb
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

instance FromRow Verb where
  fromRow = Verb <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
 
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
 
persons :: [(String, Prop)]
persons = [("Yo", fps), ("Tú", sps), ("él/ella/Ud.", tps), ("nosotros", fpp), ("vosotros", spp), ("ellos/ellas/Uds.", tpp)]

basicTenses :: [Tense]
basicTenses = ["Presente", "Pretérito", "Imperfecto", "Condicional", "Futuro"]

filterTense :: [Verb] -> Tense -> Maybe Verb
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

paint :: Color -> a -> Formatted a
paint color s = formatted (setSGRCode [SetColor Foreground Dull color]) (plain s) (setSGRCode [Reset])
 
displayMood :: [Verb] -> DisplayMood -> IO ()
displayMood nws dm = do
  setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity]
  putStrLn $ "\n" ++ (title dm) ++ "\n"
  setSGR [Reset]

  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if stdoutSupportsANSI
    then putStrLn $ tableString t
    else putStrLn "Standard output does not support 'ANSI' escape codes."
  where 
    combined :: Table
    combined =  mconcat (map (generateTable nws (tenses dm)) (moods dm))

    filtered :: Table
    filtered = filterBlankColumns $ filterBlankRows combined
 
    table :: Table
    table = case customHeaders dm of
      Nothing -> filtered
      Just headers -> filtered{columnHeader = headers}

    cs = repeat (column (expandUntil 20) left def def)
    ch = titlesH $ map (paint Yellow) (columnHeader table)
    rh = titlesH $ map (paint Yellow) (rowHeader table)
    rgs = map ((colsAllG center) . (map ((justifyText 10) . (fromMaybe "")))) (cells table)
    t = fullTableS cs unicodeS rh ch rgs

generateTable :: [Verb] -> [Tense] -> Mood -> Table
generateTable nws tenses' m = Table 
  { cells = map ((flip map tenseValues) . applyProp . snd) persons,
    columnHeader = tenses',
    rowHeader = map fst persons
  }
  where
    moodMatches :: [Verb]
    moodMatches = filter ((== m) . mood) nws

    tenseValues :: [Maybe Verb]
    tenseValues = map (filterTense moodMatches) tenses'
 
main :: IO ()
main = do
  verb : _ <- getArgs
  conn <- open "conjugation.db" 
  rows <- query conn "SELECT * from verbs where infinitive=?" (Only (verb :: String)) :: IO [Verb]
  close conn  
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
        },
      DisplayMood
        {
        title = "Perfecto",
        moods = ["Indicativo"], 
        tenses = ["Presente perfecto", "Pretérito anterior", "Pluscuamperfecto", "Condicional perfecto", "Futuro perfecto"],
        customHeaders = Just basicTenses 
        }, 
      DisplayMood
        {
        title = "Perfecto Subjunctivo",
        moods = ["Subjuntivo"], 
        tenses = ["Presente perfecto", "Pluscuamperfecto", "Futuro perfecto"],
        customHeaders = Just ["Presente", "Imperfecto", "Futuro"]  
        }
    ]  
