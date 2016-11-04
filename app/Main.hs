-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Main
-- | Author  : Xiao Ling
-- | Date    : 10/31/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Main where

 
import System.Directory
import Data.Text (Text, unpack, pack, splitOn)
import Data.Set  (Set, union, fromList, toList)
import qualified System.IO as S
import qualified Data.Conduit.Text as CT


import Src
import Lib
import Scripts

{-----------------------------------------------------------------------------
  words
------------------------------------------------------------------------------}

--suffi = ["sufficient","good", "wide","full"]
--near  = ["nearby", "near","close","adjacent"]
--dim   = ["dim","gloomy","dark","black"]
--known = ["known","famous","legendary"]
--first = ["first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth"]
--close = ["close","near","intimate"]


--real        = ["real","solemn","serious","grave"]
--interesting = ["interesting","intriguing","amusing", "entertaining","fascinating", "exciting"]
--far         = ["far","further", "farther","removed"]
--acceptable  = ["acceptable", "okay", "alright","right"]
--cracked     = ["cracked","broken","crushed"]
--full        = ["full","stuffed","overloaded","overflowing"]
--flavorful   = ["flavorful","zesty","hot", "spicy"]
--unstable    = ["unstable","crazy","insane"]
--good        = ["good","real","authentic"]
--clean       = ["clean","spotless", "immaculate"]
--possible    = ["possible","realistic","feasible","practical"]
--sick        = ["sick", "ill","impaired","disabled"]

--uncommon    = ["uncommon","unusual","rare","extraordinary","exceptional"]
--impractical =["impractical","unrealistic","impossible"]
--global      = ["transnational","international","global"]
--concerned   = ["concerned","preoccupied","obsessed"]
--interesting = ["interesting","moving","exciting","thrilling"]


--low         = ["low","subdued","quiet"]
--sweet       = ["sweet","sugary","syrupy"]
--strong      = ["strong","intense","terrible","overwhelming","violent"]
--bleak       = ["bleak", "desperate","hopeless"]
--unfortunate = ["unfortunate","disastrous","fatal"]
--negligent   = ["negligent","careless","reckless"]
--strange     = ["strange", "funny","unusual","weird","eerie"]
--reasonable  = ["reasonable","valid","sound"]
--affordable  = ["affordable","inexpensive","cheap"]
--available   = ["available","accessible","visible"]
--cool        = ["cool", "refrigerated", "chilly","cold","frigid","icy", "frozen"]
--content     = ["content", "satisfied","pleased", "happy"]

--misleading  = ["misleading","deceptive", "fraudulent","false"]
--needed    = ["needed","necessary","required", "essential","indispensable"]
--some      = ["some","many","numerous","galore"]
--quiet     = ["quiet","inaudible", "imperceptible","silent"]
--wrong     = ["wrong","immoral","sinful","evil"]
--lean      = ["lean","slim","thin","skinny","gaunt","emaciated","skeletal"]
--statewide = ["statewide","nationwide","worldwide"]
--warm      = ["warm","hot","overheated","stifling", "sultry"]
--easy      = ["easy", "simple", "smooth","painless","effortless"]
--plain     = ["plain","unattractive","ugly"]
--like      = ["like","equal","same"]
--broad     = ["broad", "widespread","general","universal"]
--cheap     = ["cheap", "mediocre","bad","worst"]
--handsome  = ["handsome", "lovely", "gorgeous","beautiful", "pretty", "attractive"]
--thick     = ["thick","dense","impenetrable"]
--soft      = ["soft","quiet","inaudible","silent"]
--cool      = ["cool", "chilly","unfriendly","hostile"]
--sizeable  = ["sizeable","big", "large","huge","colossal"]

--dual       = ["dual", "double", "doubled","triple", "treble","quadruple"]
--creepy     = ["creepy","scary", "frightening","terrifying","sinister"]
--some       = ["some","few","fewer"]
--neglected  = ["neglected","ignored", "overlooked","forgotten"]
--old        = ["old","obsolete", "outdated"]
--necessary  = ["necessary","vital","indispensable","critical"]
--attractive = ["attractive","beautiful","mesmerizing", "seductive"]
--closed     = ["closed","shut","sealed"]
--unexpected = ["unexpected","astonishing", "stunning"]
--indecent   = ["indecent","profane","obscene"]
--sexy       = ["attractive","sexy","seductive"]
--humbled    = ["humbled","humiliated","crushed"]
--bright     = ["bright", "intelligent", "smart", "clever","brilliant"]

violent    = ["violent","homicidal", "murderous"]
personal   = ["personal","private","secret"]
high       = ["high","higher","soaring"]
harmful    = ["harmful","toxic","deadly"]
small       = ["small","smaller","midget","minute","tiny", "minuscule","micro", "microscopic"]
impractical = ["impractical", "impracticable","unrealistic","infeasible","impossible"]
mature      = ["mature","ripe","overripe"]
overweight  = ["overweight","chubby","fat","obese"]

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

main :: IO ()
main = do
  print "start collecting statistics [violent..."

  collect violent
  collect personal
  collect high
  collect harmful
  collect small
  collect impractical
  collect mature
  collect overweight

{-----------------------------------------------------------------------------
  routine
------------------------------------------------------------------------------}

collect :: [String] -> IO ()
collect wrds = do
  con        <- sysConfig
  let inpath    = corpus con
  let weakin    = weakStrong con
  let strongin  = strongWeak con
  let wordin    = corpus con ++ "vocab.txt"
  let weakout   = dir_out_l ++ "weak"
  let strongout = dir_out_l ++ "strong"
  let wordout   = dir_out_l ++ "words"


  count_phrase strongin inpath strongout `mapM` pset wrds
  count_phrase weakin   inpath weakout   `mapM` pset wrds
  count_words wordin wordout wrds

  print $ "collected statistics over words : "
  mapM print wrds

  return ()


pset :: Eq a => [a] -> [(a,a)]
pset xs = [(u,v) | u <- xs, v <- xs]



{-----------------------------------------------------------------------------
  Paths
------------------------------------------------------------------------------}

sysConfig :: IO Config
sysConfig = do
  d <- take 6 <$> getCurrentDirectory 
  if d == "/Users" then config_l
  else config_r


config_l :: IO Config
config_l = do
    Just con <- config corpus_l patterns_l
    return con

config_r :: IO Config
config_r = do
    Just con <- config corpus_r patterns_r
    return con

-- * local

dir_out_l  = "/Users/lingxiao/Documents/research/data/good-great-outputs/"
corpus_l   = "/Users/lingxiao/Documents/research/data/ngrams/corpus/"
patterns_l = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"

-- * remote
corpus_r   = "/nlp/data/xiao/ngrams/corpus/"
patterns_r = "/home1/l/lingxiao/xiao/good-great-excellent/inputs/"





















