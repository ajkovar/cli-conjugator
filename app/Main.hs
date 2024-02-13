{-# LANGUAGE OverloadedStrings #-}

import Data.List (transpose)
import Database.SQLite.Simple
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import Control.Applicative
import Text.Layout.Table (left, fixedUntil, column, def, tableString, unicodeS, columnHeaderTableS, rowG, titlesH)
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

toArray :: Table -> [[String]]
toArray g = getZipList $ 
  (:)
  <$> ZipList ("":(rowHeader g)) 
  <*> ZipList ((columnHeader g):(map (map (fromMaybe "")) (cells g)))
 
persons :: [(String, Prop)]
persons = [("Yo", fps), ("Tú", sps), ("él/ella/Ud.", tps), ("nosotros", fpp), ("vosotros", spp), ("ellos/ellas/Uds.", tpp)]

basicTenses :: [Tense]
basicTenses = ["Presente", "Pretérito", "Imperfecto", "Imperfecto", "Futuro"]

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

paint :: Color -> String -> Formatted String
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
    columnDef = column (fixedUntil 10) left def def

    combined :: Table
    combined =  mconcat (map (generateTable nws (tenses dm)) (moods dm))

    filtered :: Table
    filtered = filterBlankColumns $ filterBlankRows combined
 
    array :: [[String]]
    array = toArray $ case customHeaders dm of
      Nothing -> filtered
      Just headers -> filtered{columnHeader = headers}

    cs = repeat columnDef
    h = (titlesH (map (paint Yellow) (head array)))
    paintFns = (paint Yellow):repeat plain
    rgs = map (rowG . (zipWith (\f d -> f d) paintFns)) (tail array)
    t = columnHeaderTableS cs unicodeS h rgs

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
        } 
    ]  
