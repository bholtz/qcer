import QuickChecker hiding (main)
import System.Process
import System.Posix.IO
import System.IO
import Text.Regex.Posix
import Data.List
import Data.List.Split

-- Keywords that often signify a return value.
-- Can update list with more synonyms as needed.
returnKeywords :: [String]
returnKeywords = ["return", "output", "returns", "outputs"]

simplifyParse :: String -> String
simplifyParse str = (splitOn "-" ((splitOn "," str) !! 1)) !! 0

filterByAmod :: String -> Bool
filterByAmod str = 
  let ptype = splitOn "(" str in
  (ptype !! 0) == "amod"

-- Generate NLP parse for comment and
-- Return list of prioritized words.
runNLP :: IO ([String])
runNLP = do
  let cmd = shell "java -mx150m -cp \"parse.txt\" edu.stanford.nlp.parser.lexparser.LexicalizedParser  -outputFormat \"typedDependencies\" edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz $*"
  _ <- createProcess cmd
  h <- openFile "parse.txt" ReadMode
  contents <- hGetContents h
  let lines = splitOn "\n" contents
  let amods = filter (\line -> filterByAmod line) lines
  let toRet =  map (\line -> simplifyParse line) amods
  return toRet

statTemplateFile :: String
statTemplateFile = "rankerTemplate.hs"

statParse :: String -> [String]
statParse stats = [stats]
  

-- Here, we run some QuickChecks and examine 
-- how many tests we pass/fail for each possible 
-- property. Right now, we toss properties that fail
-- more than x times. 
statQuickCheck :: IO ([String])
statQuickCheck = do
  -- Generate the quickCheckResult file  
  _ <- genTestsForFile "NextPrime.hs" statTemplateFile  "IO (Result)"
--  let cmd = shell "ghc NextPrimeAutoTest.hs"
--  _ <- createProcess cmd
  let run = shell "./NextPrimeAutoTest >> verboseQC.txt"
  _ <- createProcess run
  h <- openFile "verboseQC.txt" ReadMode
  contents <- hGetContents h
  hClose h
  return $ statParse contents



main :: IO ()
main = do
  firstPris <- runNLP 
  hPutStrLn stderr $ show firstPris -- | Prioritize these
  _ <- statQuickCheck
  return ()



{- 
TODO
- get stats of qucikcheckreults, generate ordered lit of props
- make less disgusting all he createProcess
- replace cp with correct copy/pasted NLP command because
i am bad at copy and paste?
-}