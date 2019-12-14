type Board=[(Int, Int, Int)]

printBoard :: Board-> IO ()
printBoard [] = putStrLn ""
printBoard ((x,y,z):rs) | z==1 && y/=7 = putStr (" [X] ") >> printBoard rs
                        | z==2 && y/=7 = putStr (" [0] ") >> printBoard rs
                        | z==0 && y/=7 = putStr (" [ ] ") >> printBoard rs
                        | z==1 && y==7 = putStr (" [X]\n") >> printBoard rs
                        | z==2 && y==7 = putStr (" [0]\n") >> printBoard rs
                        | otherwise = putStr (" [ ]\n") >> printBoard rs

updateBoard :: Board -> Int -> Int -> Board
updateBoard [] z y = []
updateBoard brd@((a, b, c):rs) z y | b==y && (bottomOfColumn brd a b)==True = ((a,b,z):rs)
                                   | otherwise = ((a,b,c):rs) >> updateBoard rs z y

bottomOfColumn :: Board -> Int -> Int -> Bool
bottomOfColumn ((a, b, c):rs) x y | (x+1)==a && y==b && (c==1 || c==2) = True
                                  | x==0 && y==0 = True
                                  | otherwise = False

inputCol :: IO Int
inputCol = do
    int <- getLine
    return (read int)

coords=[ (1,1,0), (1,2,0), (1,3,0), (1,4,0), (1,5,0), (1,6,0), (1,7,0),
             (2,1,0), (2,2,0), (2,3,0), (2,4,0), (2,5,0), (2,6,0), (2,7,0),
             (3,1,0), (3,2,0), (3,3,0), (3,4,0), (3,5,0), (3,6,0), (3,7,0),
             (4,1,0), (4,2,0), (4,3,0), (4,4,0), (4,5,0), (4,6,0), (4,7,0),
             (5,1,0), (5,2,0), (5,3,0), (5,4,0), (5,5,0), (5,6,0), (5,7,0),
             (6,1,0), (6,2,0), (6,3,0), (6,4,0), (6,5,0), (6,6,0), (6,7,0)
          ]

markedCoords=[ (1,1,0), (1,2,0), (1,3,0), (1,4,0), (1,5,0), (1,6,0), (1,7,0),
               (2,1,0), (2,2,0), (2,3,0), (2,4,0), (2,5,0), (2,6,0), (2,7,0),
               (3,1,0), (3,2,0), (3,3,0), (3,4,0), (3,5,0), (3,6,0), (3,7,0),
               (4,1,0), (4,2,0), (4,3,0), (4,4,0), (4,5,0), (4,6,0), (4,7,0),
               (5,1,0), (5,2,0), (5,3,0), (5,4,0), (5,5,0), (5,6,0), (5,7,0),
               (6,1,2), (6,2,1), (6,3,1), (6,4,2), (6,5,2), (6,6,1), (6,7,2)
            ]

toString :: Int -> [Char]
toString 1 = "1"
toString 2 = "2"

player=1

main = do
        printBoard coords
        putStrLn ("Player " ++ (toString player) ++ ": What column do you want to place?")
        column <- inputCol
        if column > 7 || column < 1
        then do
            putStrLn ("Try a different column!")
            main
        else do
            let coords = updateBoard coords player column
            let player = 2
            main
