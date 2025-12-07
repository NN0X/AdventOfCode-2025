import Data.List

mapSymbols :: Char -> Int
mapSymbols symbol =
        case symbol of
                '.' -> 0
                'S' -> 1
                '^' -> 2
                n -> -1

updateAt :: Int -> Int -> [Int] -> [Int]
updateAt index newValue list
    | index < 0 || index >= length list = list
    | otherwise = take index list ++ [newValue] ++ drop (index + 1) list

safeGet :: [Int] -> Int -> Int
safeGet list index
        | index < 0 = 0
        | index >= length list = 0
        | otherwise = list !! index

transformLineCount :: [Int] -> [Int] -> [Int]
transformLineCount prevCounts currSymbols =
        let
                generateCountContributions :: [Int] -> [Int] -> [(Int, Int)]
                generateCountContributions prevN currS =
                        concat $ zipWith (\i prevN ->
                        let
                                currSymbol = safeGet currS i
                        in
                                case currSymbol of
                                        0 -> [(i, prevN)]
                                        1 -> [(i, prevN)]
                                        2 -> [ (i - 1, prevN), (i + 1, prevN) ]
                                        _ -> [ ]
                        ) [ 0.. ] prevN

                allContributions = generateCountContributions prevCounts currSymbols
                lineLength = length currSymbols
                initialResult = replicate lineLength 0
                finalResult = foldl (\acc (targetIndex, count) ->
                        let
                                currentCount = safeGet acc targetIndex
                                newCount = currentCount + count
                        in
                                if targetIndex >= 0 && targetIndex < lineLength
                                then updateAt targetIndex newCount acc
                                else acc
                        ) initialResult allContributions
                in
                        finalResult

calculateTotalPaths :: [[Int]] -> Int
calculateTotalPaths intLines =
        let
                initialCountLine = head intLines
                symbolLines = tail intLines
                evaluatedCounts = scanl transformLineCount initialCountLine symbolLines
                finalCountLine = last evaluatedCounts
                totalPaths = sum finalCountLine
        in
                totalPaths

main = do
        fileData <- readFile "../inputs/day7"
        let dataLines = lines $ fileData
        let intLines = map (\str -> map mapSymbols str) dataLines
        let totalPaths = calculateTotalPaths intLines

        print totalPaths
