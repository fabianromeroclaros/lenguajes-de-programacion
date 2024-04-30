import System.IO
import System.Random
import Control.Monad (liftM)
-- import qualified Data.Text    as Text
-- import qualified Data.Text.IO as Text


type StudentName = String

data Student = Student{
    lastName::String,
    name::StudentName
    } deriving (Show, Eq,Ord)

data SchoolHouse = Griffindor
                    | Hufflepuff
                    | Ravenclaw 
                    | Slytherin 
                    deriving (Eq, Show, Ord, Read)

data HouseChosenTimes = HouseChosenTimes {
    house :: SchoolHouse,
    chosenTimes :: Int
} deriving (Show)

incrementChosenTimes :: SchoolHouse -> [HouseChosenTimes] -> [HouseChosenTimes]
incrementChosenTimes h [] = []
incrementChosenTimes h (x:xs)
    | house x == h = HouseChosenTimes (house x) (chosenTimes x + 1) : xs
    | otherwise = x : incrementChosenTimes h xs

getChosenTimes :: SchoolHouse -> [HouseChosenTimes] -> Int
getChosenTimes _ [] = 0
getChosenTimes h (x:xs)
    | house x == h = chosenTimes x
    | otherwise = getChosenTimes h xs

assignSchoolHouse :: Int -> [HouseChosenTimes] -> IO SchoolHouse
assignSchoolHouse n xs = do
    let houses = map (\h -> house h) (filter (\h -> chosenTimes h < n) xs)
    pickRandom houses

pickRandom :: [a] -> IO a
pickRandom xs = do
    idx <- randomRIO (0, length xs - 1) 
    return (xs !! idx) 

split' :: Eq a => a -> [a] -> [[a]]
split' d [] = []
split' d s = x : split' d (drop 1 y)
             where (x,y) = span (/= d) s

parseLine :: String -> Student
parseLine line =
    let [lastName, name] = split' ',' line
    in Student{lastName=lastName, name=name}

readFileToList :: FilePath -> IO [Student]
readFileToList filePath = do
    contents <- readFile filePath
    let students = map parseLine (lines contents)
    return students

main :: IO()
main = do
    records <- readFileToList "list.txt"
    let maxPersonsPerHouse = length records `div` 4 + length records `mod` 4
    let studentsList [] _ = return ()
        studentsList (x:xs) houses = do
            schoolHouse <- assignSchoolHouse maxPersonsPerHouse houses
            putStrLn $ show (x, schoolHouse)
            let updatedHouses = incrementChosenTimes schoolHouse houses
            studentsList xs updatedHouses
    let initialHouses = map (\house -> HouseChosenTimes house 0)
                            [Griffindor, Hufflepuff, Ravenclaw, Slytherin]
    studentsList records initialHouses
