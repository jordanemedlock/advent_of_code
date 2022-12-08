module Main where
import Control.Monad (foldM)
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Data.List (sort)


data FSElem = Directory { elems :: HM.HashMap String FSElem }
            | File { size :: Int }
            deriving (Show)

type Path = [String] -- front to back

data WorkingDirectory = WorkingDirectory 
    { rootElem :: FSElem
    , currentDir :: Path
    } deriving Show

execInst :: String -> WorkingDirectory -> WorkingDirectory
execInst str wd = case words str of
    ["$", "cd", dir] -> changeDir dir wd
    ["$", "ls"] -> wd -- Do nothing...
    ["dir", dir] -> addDir dir wd
    [num, fname] -> addFile (read num) fname wd
    edge -> error $ "No edge cases: " <> show edge

changeDir :: String -> WorkingDirectory -> WorkingDirectory
changeDir "/" (WorkingDirectory root _) = WorkingDirectory root []
changeDir ".." (WorkingDirectory root []) = error "no directory to go up to"
changeDir ".." (WorkingDirectory root path) = WorkingDirectory root (init path)
changeDir dir (WorkingDirectory root path) = WorkingDirectory root (path <> [dir])

addDir :: String -> WorkingDirectory -> WorkingDirectory
addDir dir (WorkingDirectory root path) = WorkingDirectory (addDirAtPath dir root path) path

addFile :: Int -> String -> WorkingDirectory -> WorkingDirectory
addFile size name (WorkingDirectory root path) = WorkingDirectory (addFileToPath size name root path) path


updateElemAtPath :: Path -> (FSElem -> FSElem) -> FSElem -> FSElem
updateElemAtPath [] f dir = f dir
updateElemAtPath (top:down) f (Directory elems) = Directory $ HM.update (Just . updateElemAtPath down f) top elems
updateElemAtPath path _ _ = error $ "Cannot find path in file: " <> show path

addDirAtPath :: String -> FSElem -> Path -> FSElem
addDirAtPath dir root path = updateElemAtPath path (\(Directory elems) -> Directory $ HM.insert dir (Directory HM.empty) elems) root

addFileToPath :: Int -> String -> FSElem -> Path -> FSElem
addFileToPath size fname root path = updateElemAtPath path (\(Directory elems) -> Directory $ HM.insert fname (File size) elems) root

tree :: FSElem -> String
tree root = innerTree 0 "/" root
    where
        innerTree n name dir@(Directory elems) = concat $ [replicate n ' ', "- ", name, " (dir, size=", show (dirSize dir), ")\n"] <> (uncurry (innerTree (n+4)) <$> HM.toList elems)
        innerTree n name (File size) = concat [replicate n ' ', "- ", name, " (file, size=", show size, ")\n"]



dirSize :: FSElem -> Int
dirSize (File s) = s
dirSize (Directory elems) = sum $ dirSize.snd <$> HM.toList elems



flatSizes :: FSElem -> [Int]
flatSizes (File _) = []
flatSizes dir = dirSize dir : concatMap (flatSizes . snd) (HM.toList $ elems dir)
    


main :: IO ()
main = do
    input <- readFile "./data/day7.txt"
    let root = Directory HM.empty
    let wd = WorkingDirectory root []

    -- let root' = addFileToPath 113975 "bqpslnv" root ["/"] 

    wd' <- foldM execInst' wd (lines input)

    let totalSize = dirSize $ rootElem wd'
    let diff = totalSize - 40000000
    let sizes = flatSizes $ rootElem wd'


    putStrLn $ tree $ rootElem wd'
    putStrLn $ show $ sum $ filter (<100000) $ sizes

    putStrLn $ "We need to cut out: " <> show diff
    putStrLn $ show $ sort $ filter (>diff) $ sizes
    -- hspec $ do
    --     describe "addFileToPath" $ do
    --         it "returns the sum of its two arguments" $ do
    --             add 1 2 `shouldBe` 3

execInst' :: WorkingDirectory -> String -> IO WorkingDirectory
execInst' wd inst = do
    -- putStrLn $ "Running instruction: "<>inst
    let wd' = execInst inst wd
    -- putStrLn $ tree $ rootElem wd'
    return wd'
