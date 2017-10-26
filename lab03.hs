{-
Lab3 - Grading your Quiz with Haskell

Deadline : 27th Oct 2017 (Friday 7pm)

The intention of this lab exercise is to provide
some familiarity with some monadic programming with Haskell.
You are also strongly encouraged to use higher-order
functions where possible. We gave you an example of this in:

print_file ll =
    foldr (\ a m -> putStrLn a >> m) (return ()) ll

which simply prints all the data just read from the file.

You are also to use the compiler rather than the interpreter
for this exercise. To compile the program, please use ghc:

    >$ ghc lab03.hs
    [1 of 1] Compiling Main             ( lab03.hs, lab03.o )
    Linking lab03 ...

This will produce an executable lab03. You can now run it
as follows:

    >$ ./lab03
    cs2104.csv

    Type Your Command Option
    0 - Re-print the file
    1 - Compute Score and Sort by Student Number
    2 - Compute Score and Sort by Marks (decreasing order)
    3 - Return Min:XX,Max:XX,Median:XXXX,Average:XX.XXX
    Your Option >
    0
    ,35,B,C,E,D,C,D,E,B,D,A,A,B,B,D,D,B,E,D,A,D,C,B,D,B,E,C,B,B,D,A,C,C,D,D,E,,,,,,,,,,,,,,,
    A0133869R,30,B,C,E,D,C,D,E,B,D,A,A,B,B,D,D,B,E,D,B,D,B,B,D,B,E,C,B,B,D,A,C,C,C,D,D,,,,,,,,,,,,,,,
    A0124402Y,19,B,C,A,B,B,B,E,C,D,D,C,B,B,E,C,D,E,D,B,E,A,B,D,B,A,C,B,B,D,A,C,C,D,D,D,,,,,,,,,,,,,,,

    Complete the rest of your options and submit lab03.hs by the deadline.
    For the rest of the options, you are to use parsec to parse the
    csv (comma separated value) format of each line. Please remember to
    use higher-order functions (from the Haskell libraries), where possible.
-}

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Text as T
import Text.Parsec
import Text.ParserCombinators.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as P

type SParsec = Parsec String ()

lexer  = P.makeTokenParser haskellDef
comma  = P.commaSep lexer
lexeme = P.lexeme   lexer

joinStuAns :: String -> [Char] -> String
joinStuAns stu ans = stu ++ " " ++ show ans

line :: SParsec (String, [Char])
line = do
        spaces
        matricNumber <- many alphaNum
        skipMany (char ',')
        skipMany digit
        skipMany (char ',')
        answers <- comma (upper <|> char '*' <|> char ',')
        skipMany (char ',')
        return (joinStuAns matricNumber answers, answers)

parseLine :: String -> (String, [Char])
{-
parseLine ln =
    let parsed = case (parse line "" ln) of
                 Left err -> (show err, [])
                 Right (s, cs) -> (show s, cs)
    in parsed
-}
parseLine ln =
    let trimmedLn = T.dropAround (\ c -> isSpace c || isPunctuation c) (T.pack ln)
        splitLn = splitOn "," (T.unpack trimmedLn)
    in
    (head splitLn, intercalate "," (tail splitLn))

countMatch :: [Char] -> [Char] -> Int
countMatch xs [] = 0
countMatch [] ys = 0
countMatch xs ys =
    if (head xs == head ys)
    then 1 + countMatch (tail xs) (tail ys)
    else countMatch (tail xs) (tail ys)

parseAndCount :: String -> [Char] -> (String, Int)
parseAndCount answerKey ln =
    let pair = parseLine ln
    in
    (fst pair, countMatch (snd pair) answerKey)

sortByStudent :: [(String, Int)] -> [(String, Int)]
sortByStudent xs = sortBy (compare `on` fst)  xs

sortByMarks :: [(String, Int)] -> [(String, Int)]
sortByMarks xs = sortBy (flip compare `on` snd) xs

joinPair :: (String, Int) -> String
joinPair p = (fst p) ++ " " ++ show (snd p)

main = do
        -- x <- getLine
        ll <- read_file "cs2104.csv"
        putStrLn "\nType Your Command Option"
        putStrLn "0 - Re-print the file"
        putStrLn "1 - Compute Score and Sort by Student Number"
        putStrLn "2 - Compute Score and Sort by Marks (decreasing order)"
        putStrLn "3 - Return Min:XX,Max:XX,Median:XXXX,Average:XX.XXX"
        putStrLn "Your Option >"
        y <- getLine 
        grade_quiz y ll

read_file :: FilePath -> IO [String]
read_file x =
    do
    content <- readFile x
    let llcontent = lines content
        lz = length llcontent
        in do
            return llcontent

grade_quiz :: [Char] -> [String] -> IO ()
grade_quiz x ll =
    do
    if (x=="0")
    then print_file ll
    else
        if (x=="1")
        then sort_by_student ll
        else
            if (x=="2")
            then sort_by_marks ll
            else
                if (x=="3")
                then print_statistics ll
                else putStrLn "Invalid option"

print_file :: [String] -> IO ()
print_file ll =
    foldr (\ a m -> putStrLn a >> m) (return ()) ll

sort_by_student :: [String] -> IO ()
sort_by_student ll =
    let answerKey = snd (parseLine (head ll))
        in
        let parseAndMatch = parseAndCount answerKey
            finalSortedList = sortByStudent (map parseAndMatch (tail ll))
    in
    foldr (\ a m -> putStrLn a >> m) (return ()) (map joinPair finalSortedList)

sort_by_marks :: [String] -> IO ()
sort_by_marks ll =
    let answerKey = snd (parseLine (head ll))
        in
        let parseAndMatch = parseAndCount answerKey
            finalSortedList = sortByMarks (map parseAndMatch (tail ll))
    in
    foldr (\ a m -> putStrLn a >> m) (return ()) (map joinPair finalSortedList)

print_statistics :: [String] -> IO ()
print_statistics ll =
    foldr (\ a m -> putStrLn a >> m) (return ()) ll

