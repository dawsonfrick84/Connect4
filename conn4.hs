type Board=[[Int]]

printBoard :: Board-> IO ()
printBoard [] = putStrLn ""
printBoard (x:xs) | (x !! 2) == 1 && (x !! 1)/=7 = putStr (" [X] ") >> printBoard xs
                  | (x !! 2) == 2 && (x !! 1)/=7 = putStr (" [0] ") >> printBoard xs
                  | (x !! 2) == 0 && (x !! 1)/=7 = putStr (" [ ] ") >> printBoard xs
                  | (x !! 2) == 1 && (x !! 1)==7 = putStr (" [X]\n") >> printBoard xs
                  | (x !! 2) == 2 && (x !! 1)==7 = putStr (" [0]\n") >> printBoard xs
                  | otherwise = putStr (" [ ]\n") >> printBoard xs

updateBoard :: Board -> Int -> Int -> Board
updateBoard [] z y = []
updateBoard a z y | a !! ((y-1)+35) !! 2 == 0 = replace a 6 y z
                  | a !! ((y-1)+28) !! 2 == 0 = replace a 5 y z
                  | a !! ((y-1)+21) !! 2 == 0 = replace a 4 y z
                  | a !! ((y-1)+14) !! 2 == 0 = replace a 3 y z
                  | a !! ((y-1)+7) !! 2 == 0 = replace a 2 y z
                  | a !! ((y-1)) !! 2 == 0 = replace a 1 y z

replace :: Board -> Int -> Int -> Int -> Board
replace [] x y z = []
replace (a:as) x y z | ((a:as) !! 0 !! 0)==x && ((a:as) !! 0 !! 1)==y = ([x, y, z] : replace as x y z)
                     | otherwise = (a : replace as x y z)

inputCol :: IO Int
inputCol = do
    int <- getLine
    return (read int)

coords=[ [1,1,0], [1,2,0], [1,3,0], [1,4,0], [1,5,0], [1,6,0], [1,7,0],
             [2,1,0], [2,2,0], [2,3,0], [2,4,0], [2,5,0], [2,6,0], [2,7,0],
             [3,1,0], [3,2,0], [3,3,0], [3,4,0], [3,5,0], [3,6,0], [3,7,0],
             [4,1,0], [4,2,0], [4,3,0], [4,4,0], [4,5,0], [4,6,0], [4,7,0],
             [5,1,0], [5,2,0], [5,3,0], [5,4,0], [5,5,0], [5,6,0], [5,7,0],
             [6,1,0], [6,2,0], [6,3,0], [6,4,0], [6,5,0], [6,6,0], [6,7,0]
          ]

markedCoords=[ [1,1,0], [1,2,0], [1,3,0], [1,4,0], [1,5,0], [1,6,0], [1,7,0],
               [2,1,0], [2,2,0], [2,3,0], [2,4,0], [2,5,0], [2,6,0], [2,7,0],
               [3,1,0], [3,2,0], [3,3,0], [3,4,0], [3,5,0], [3,6,0], [3,7,0],
               [4,1,0], [4,2,0], [4,3,0], [4,4,0], [4,5,0], [4,6,0], [4,7,0],
               [5,1,0], [5,2,0], [5,3,0], [5,4,0], [5,5,0], [5,6,0], [5,7,0],
               [6,1,2], [6,2,1], [6,3,1], [6,4,2], [6,5,2], [6,6,1], [6,7,2]
            ]

toString :: Int -> [Char]
toString 1 = "1"
toString 2 = "2"

player=1

main = do
        putStrLn ("Player " ++ (toString player) ++ ": What column do you want to place?")
        column <- inputCol
        if column > 7 || column < 1
        then do
            putStrLn ("Try a different column!")
            main
        else do
            let coords2 = coords
            let coords2 = updateBoard coords player column
            printBoard coords2
            let player = 2
            main
