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

columnFull :: Board -> Int -> Bool
columnFull a y | a !! ((y-1)) !! 2 == 0 = False
               | otherwise = True

checkWinRow :: Board -> Int -> Int -> Bool
checkWinRow a z r  | (a!!(r+0)!!2)==z && (a!!(r+1)!!2)==z && (a!!(r+2)!!2)==z && (a!!(r+3)!!2)==z = True
                   | (a!!(r+1)!!2)==z && (a!!(r+2)!!2)==z && (a!!(r+3)!!2)==z && (a!!(r+4)!!2)==z = True
                   | (a!!(r+2)!!2)==z && (a!!(r+3)!!2)==z && (a!!(r+4)!!2)==z && (a!!(r+5)!!2)==z = True
                   | (a!!(r+3)!!2)==z && (a!!(r+4)!!2)==z && (a!!(r+5)!!2)==z && (a!!(r+6)!!2)==z = True
                   | otherwise = False

checkWinDiagLR :: Board -> Int -> Int -> Bool
checkWinDiagLR a z d | (a!!(d)!!2)==z && (a!!(d-6)!!2)==z && (a!!(d-12)!!2)==z && (a!!(d-18)!!2)==z = True
                     | otherwise = False

checkWinDiagRL :: Board -> Int -> Int -> Bool
checkWinDiagRL a z d | (a!!(d)!!2)==z && (a!!(d-8)!!2)==z && (a!!(d-16)!!2)==z && (a!!(d-24)!!2)==z = True
                     | otherwise = False

checkWinCol :: Board -> Int -> Int -> Bool
checkWinCol a z c | (a!!(c+0)!!2)==z && (a!!(c+7)!!2)==z && (a!!(c+14)!!2)==z && (a!!(c+21)!!2)==z = True
                  | (a!!(c+7)!!2)==z && (a!!(c+14)!!2)==z && (a!!(c+21)!!2)==z && (a!!(c+28)!!2)==z = True
                  | (a!!(c+14)!!2)==z && (a!!(c+21)!!2)==z && (a!!(c+28)!!2)==z && (a!!(c+35)!!2)==z = True
                  | otherwise = False

inputCol :: IO Int
inputCol = do
    int <- getLine
    return (read int)

toString :: Int -> [Char]
toString 1 = "1"
toString 2 = "2"

coords= [ [1,1,0], [1,2,0], [1,3,0], [1,4,0], [1,5,0], [1,6,0], [1,7,0],
        [2,1,0], [2,2,0], [2,3,0], [2,4,0], [2,5,0], [2,6,0], [2,7,0],
        [3,1,0], [3,2,0], [3,3,0], [3,4,0], [3,5,0], [3,6,0], [3,7,0],
        [4,1,0], [4,2,0], [4,3,0], [4,4,0], [4,5,0], [4,6,0], [4,7,0],
        [5,1,0], [5,2,0], [5,3,0], [5,4,0], [5,5,0], [5,6,0], [5,7,0],
        [6,1,0], [6,2,0], [6,3,0], [6,4,0], [6,5,0], [6,6,0], [6,7,0]
        ]

startPlayer=1

start = do
        printBoard coords
        play coords startPlayer

play board player = do
            putStrLn ("Player " ++ (toString player) ++ ": What column do you want to place?")
            column <- inputCol
            if (column > 7 || column < 1) || (columnFull board column)
            then do
                putStrLn ("Try a different column!")
                play board player
            else do
                let board2 = board
                let board2 = updateBoard board player column
                printBoard board2
                if not ((checkWinRow board2 player 0)==True
                       || (checkWinRow board2 player 7)==True
                       || (checkWinRow board2 player 14)==True
                       || (checkWinRow board2 player 21)==True
                       || (checkWinRow board2 player 28)==True
                       || (checkWinRow board2 player 35)==True
                       || (checkWinRow board2 player 35)==True

                       || (checkWinDiagLR board2 player 38)==True || (checkWinDiagLR board2 player 37)==True || (checkWinDiagLR board2 player 36)==True || (checkWinDiagLR board2 player 35)==True
                       || (checkWinDiagLR board2 player 31)==True || (checkWinDiagLR board2 player 30)==True || (checkWinDiagLR board2 player 29)==True || (checkWinDiagLR board2 player 28)==True
                       || (checkWinDiagLR board2 player 24)==True || (checkWinDiagLR board2 player 23)==True || (checkWinDiagLR board2 player 22)==True || (checkWinDiagLR board2 player 21)==True

                       || (checkWinDiagRL board2 player 38)==True || (checkWinDiagRL board2 player 39)==True || (checkWinDiagRL board2 player 40)==True || (checkWinDiagRL board2 player 41)==True
                       || (checkWinDiagRL board2 player 31)==True || (checkWinDiagRL board2 player 32)==True || (checkWinDiagRL board2 player 33)==True || (checkWinDiagRL board2 player 34)==True
                       || (checkWinDiagRL board2 player 24)==True || (checkWinDiagRL board2 player 25)==True || (checkWinDiagRL board2 player 26)==True || (checkWinDiagRL board2 player 27)==True

                       || (checkWinCol board2 player 0)==True
                       || (checkWinCol board2 player 1)==True
                       || (checkWinCol board2 player 2)==True
                       || (checkWinCol board2 player 3)==True
                       || (checkWinCol board2 player 4)==True
                       || (checkWinCol board2 player 5)==True
                       || (checkWinCol board2 player 6)==True
                       )
                then do
                    if player == 1
                    then do
                        let player = 2
                        play board2 player
                    else do
                        let player = 1
                        play board2 player
                 else do
                      putStrLn (" _______________________________________________________ ")
                      putStrLn ("|                                                       |")
                      putStrLn ("|               Player " ++ (toString player) ++ " is the WINNER!!!               |")
                      putStrLn ("|_______________________________________________________|")
                      putStrLn ("\n")
