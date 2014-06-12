module QuickChecker (substitute, extractFn, extractFile, searchName,
                     dbFile, genTestsForFile) where

import System.IO
import System.Environment
import System.Process
import Text.Regex.Posix
import Data.List
import Data.List.Split
import Data.Char
import Data.Map.Lazy hiding (map, filter)
import Hoogle

type Comment = String
type TypeSig = String
type Type = String
type Property = String
type FnName = String

-- Take the contents of a file and return the list of functions with metadata     
extractFile :: String -> [(Property, FnName, Comment, [Type])]
extractFile str = 
  let pattern = "((^--.*$\n+)*(^[ ]*[a-zA-z][^ \t\n\r\v\f]*[ \t]::.*$))"
      matches = map (\(x:xs) -> x) (str =~ pattern :: [[String]])
  in concatMap extractFn matches

-- Take the contents of a single function and split it into comment, typesig, property list
extractFn :: String -> [(Property, FnName, Comment, [Type])]
extractFn str =
  let delimiter = "%%%"
      comment = str =~ "(^[ \t]*--.*$\n+)*" :: String
      fnNameTypeSig = str =~ "^[ ]*[a-zA-z][^ \t\n\r\v\f]*[ \t]::.*$" :: String
      chunks = splitOn delimiter comment
      propChunks (x1:x2:xs) = x2:(propChunks xs)
      propChunks _          = []
      props = propChunks chunks
      (fnName, typeSig) = splitSig fnNameTypeSig
  in [(prop, fnName, comment, typeSig) | prop <- props]


genTests :: Property -> FnName -> Comment -> [Type] -> Database -> Map String String
genTests p fnName c types db = 
  let results = filter (\(_, ts, _, _, _) -> (last ts == "Bool") && (length ts == 2)) $ searchName p db
      (resultName, resultType, _, resultLoc, _) = head results
      prop = resultName ++ " $ " ++ fnName ++" x"
      propName = filter (\c -> isAlpha c || c == '_') $ intercalate "_" $ words p
  in fromList [("propName", propName), ("fnName", fnName), ("vb", "x"), ("vbtype", head types), ("prop", prop), ("lib", resultLoc)]
      

-- The file containing a template for each test
templateFile :: String
templateFile = "template.hs"


splitSig :: String -> (FnName, [Type])
splitSig s = 
  let fnName':typeSig':[] = splitOn "::" s
      fnName = trim fnName'
      typeSig = trim typeSig'
  in (fnName, typeList typeSig)
     
     
trim :: String -> String
trim = reverse . (dropWhile isSpace) . reverse . (dropWhile isSpace)

searchName :: String -> Database -> [(FnName, [Type], String, String, Score)]
searchName queryString db = 
  let (Right query) = parseQuery Haskell queryString
      results = search db query
      simplifyLoc = snd . last . snd . head 
      clean (sc, (Result locs' ent' doc')) = 
        let ent = showTagText ent' 
            doc = showTagText doc'
            loc = simplifyLoc locs'
            (fnName, typeSig) = splitSig ent
        in (fnName, typeSig, doc, loc, sc)
  in  map clean results


genTestsForFile :: String -> String -> String -> IO ()
genTestsForFile filename tempFileName retType = do
  hPutStrLn stderr $ "Generating tests for file " ++ filename
  h <- openFile filename ReadMode
  contents <- hGetContents h
  let fns = extractFile contents
  db <- loadDatabase dbFile
  ht <- openFile tempFileName ReadMode
  template <- hGetContents ht
  let modul = takeWhile (/= '.') filename
      maps = map (\((p, fn, cm, types)) -> genTests p fn cm types db ) fns
      filledTemps = map (substitute template) maps
      -- delete on break
      libs = concat $ map (\mp -> "import " ++ (mp ! "lib") ++ "\n") maps
      testFile = "import Test.QuickCheck\nimport " ++ modul ++ "\n" ++ libs ++ "\nmain :: " ++ retType ++"\nmain = do\n" ++ concat filledTemps
  t <- openFile (modul ++ "AutoTest.hs") WriteMode
  hPutStr t testFile
  hClose t
  hClose ht
  hClose h
  
-- Modify as needed to get the proper database file from the proper place
dbFile :: String
dbFile = "/afs/ir/users/b/h/bholtz/.cabal/share/hoogle-4.2.32/databases/primes.hoo"
  
-- Take a template and replace keywords   
substitute :: String -> Map String String -> String 
substitute [] mp       = ""
substitute template mp =  
  let delim = "%%"
      chunks = splitOn delim template
      templateChunks (x:xs) = x:replaceChunks xs
      templateChunks _      = []
      replaceChunks (x:xs)  = (mp ! x):templateChunks xs
      replaceChunks _       = []
  in concat $ templateChunks chunks

-- Split the type signature into list of types
typeList :: TypeSig -> [Type]
typeList = map trim . splitOn "->"

--Comment in to run the simple quick checker
main :: IO ()
main = do
  files <- getArgs
  mapM_ (\file -> genTestsForFile file templateFile "IO ()") files
  
