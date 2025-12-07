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

generateContributions :: [Int] -> [Int] -> [(Int, Int)]
generateContributions prevLine currLine =
    concat $ zipWith (\i prevI ->
        let
            currI = safeGet currLine i
        in
        case prevI of
            1 -> case currI of
                0 -> [ (i, 1) ]
                2 -> [ (i - 1, 1), (i + 1, 1) ]
                _ -> [  ]
            _ -> [  ]

    ) [ 0.. ] prevLine

transformLine :: [Int] -> [Int] -> [Int]
transformLine prevLine currLine =
        let
                allContributions = generateContributions prevLine currLine
                initialResult = currLine
                finalResult = foldl (\acc (targetIndex, newValue)
                                -> updateAt targetIndex newValue acc ) initialResult allContributions
        in
        finalResult

evalLines :: [[Int]] -> [[Int]] -> [[Int]]
evalLines prevLines currLines =
        tail $ scanl transformLine (head prevLines) currLines

getNumSplits :: [[Int]] -> Int
getNumSplits evaluatedLines =
        sum $ map (\(lineIndex, line) ->
                sum $ map (\(colIndex, cell) ->
                        case cell of
                                2 -> if safeGet (evaluatedLines !! (lineIndex - 1)) colIndex == 1 then 1 else 0
                                _ -> 0
                    ) (zip [0..] line)
            ) (zip [0..] evaluatedLines)

main = do
        fileData <- readFile "../inputs/day7"
        let dataLines = lines $ fileData
        let intLines = map (\str -> map mapSymbols str) dataLines
        let prevLines = init intLines
        let currLines = tail intLines
        let evaluatedLines = evalLines prevLines currLines
        let numSplits = getNumSplits evaluatedLines

        print numSplits
