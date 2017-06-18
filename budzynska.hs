import Data.Char
import Data.Maybe
import Data.List
import Data.Ord

testb = Board 19 [Field (1,1) X,
				 Field (1,2) O,
				 Field (2,1) X, 
				 Field (2,2) X,
				 Field (3,3) X,
				 Field (1,19) O,
				 Field (2,18) O,
				 Field (3,17) O,
				 Field (4,16) O,
				 Field (19,1) O]

-------Stworzenie planszy---------------

data Sign = X | O deriving (Show, Eq)

data Field = Field {
	coordinate :: (Int, Int),
	sign :: Sign 
} deriving (Show, Eq)

data Board = Board{
	size :: Int,
	fields :: [Field] -- lista przechowuje tylko zapełnione pola
} deriving (Eq, Show)

---------Wyświetlanie planszy-------------------
 
printBoard :: Board -> IO ()
printBoard board = putStr (boardToString board)

boardToString :: Board -> String
boardToString board =
	" " ++ unwords bs ++ bottom ++ "\n"
	where
		tab = [1..(size board)]
		bs = map (\x -> (printRow board x)) tab
		bottom = "    " ++ concat ( map (\x -> [chr (ord 'A' + (x - 1))] ++ " " ) tab )

printRow :: Board -> Int -> String
printRow board a = 
	(numToString a) ++ " " ++ res ++ "\n"
	where
		tab = [1..(size board)]
		signs = map (\x -> fromBoard (a,x) board) tab --tablica signów
		res = concat $ map (\x -> if x == Nothing then "  " else [signToChar (fromJust x)] ++ " " ) signs

signToChar :: Sign -> Char
signToChar sign
	|sign == X = 'X'
	|sign == O = 'O'

fromBoard :: (Int,Int) -> Board -> Maybe Sign
fromBoard coord board 
	| result == [] = Nothing
	| otherwise = Just $ sign (head result)
	where
		result = filter (\x -> (coordinate x) == coord) (fields board)

numToString :: Int -> String
numToString a
	| a < 10 = " " ++ (singleIntToString a)
	| otherwise = (singleIntToString firNum) ++ (singleIntToString secNum)
	where
		secNum = a `mod` 10
		firNum = a `div` 10

singleIntToString :: Int -> String
singleIntToString a = [chr (ord '0' + a)]

----------Wstawianie elementu - dodanie do listy--------------------------------

addElem :: Field -> Board -> Maybe Board
addElem f board 
	| (a < 1) || (b < 1) || (a > (size board )) || (b > (size board )) = Nothing
	| fromBoard (coordinate f) board  == Nothing = Just $ Board (size board) (fields board ++ [f])
	| otherwise = Nothing 
	where
		(a,b) = coordinate f

-----------Ocenianie planszy-------------

checkRight :: Board -> Field -> Sign -> Int -> Int
checkRight board field s i
	| nextElem == Nothing || fromJust nextElem /= s = i 
	| otherwise = checkRight board (Field (a, b+1) (fromJust nextElem)) s (i + 1) 
	where
		(a,b) = coordinate field 
		nextElem = fromBoard (a,b+1) board

checkDown :: Board -> Field -> Sign -> Int -> Int
checkDown board field s i
	| nextElem == Nothing || fromJust nextElem /= s = i 
	| otherwise = checkDown board (Field (a+1, b) (fromJust nextElem)) s (i + 1) 
	where
		(a,b) = coordinate field 
		nextElem = fromBoard (a+1, b) board

checkLeftCant :: Board -> Field -> Sign -> Int -> Int
checkLeftCant board field s i
	| nextElem == Nothing || fromJust nextElem /= s = i 
	| otherwise = checkLeftCant board (Field (a+1, b-1) (fromJust nextElem)) s (i + 1) 
	where
		(a,b) = coordinate field 
		nextElem = fromBoard (a+1, b-1) board

checkRightCant :: Board -> Field -> Sign -> Int -> Int
checkRightCant board field s i
	| nextElem == Nothing || fromJust nextElem /= s = i 
	| otherwise = checkRightCant board (Field (a+1, b+1) (fromJust nextElem)) s (i + 1) 
	where
		(a,b) = coordinate field 
		nextElem = fromBoard (a+1, b+1) board

checkBoard :: Board -> Sign -> Int
checkBoard board s =
	sum (map (\x -> convertPoints x) (b ++ c ++ d ++ e))
	where
		b = map (\x -> (checkRight board x s 1)) filtered
		c = map (\x -> (checkDown board x s 1)) filtered
		d = map (\x -> (checkRightCant board x s 1)) filtered
		e = map (\x -> (checkLeftCant board x s 1)) filtered
		filtered = (filter (\x -> (sign x) == s ) (fields board))

convertPoints :: Int -> Int
convertPoints a
	| a == 1 = 5
	| a == 2 = 15
	| a == 3 = 40
	| a == 4 = 100
	| a == 5 = 1000
	| otherwise = 0

-----------Drzewo możliwych ruchów-----

data Tree a = Node a [Tree a] deriving Show

generateBoards :: Board -> Field -> Sign -> [Board]
generateBoards board field sign = 
	map (\x -> (fromJust x)) (filter (\x -> x /= Nothing) list)
	where
		(a,b) = coordinate field
		list = 	[addElem (Field (a-1, b-1) sign) board,
				 addElem (Field (a-1, b) sign) board,
	 			 addElem (Field (a-1, b+1) sign) board,
	 			 addElem (Field (a, b-1) sign) board,
	 			 addElem (Field (a, b+1) sign) board,
	 			 addElem (Field (a+1, b-1) sign) board,
	 			 addElem (Field (a+1, b) sign) board,
	 			 addElem (Field (a+1, b+1) sign) board
				]

generateTree :: Board -> Int -> Sign -> Tree Board
generateTree board depth sign
	| depth == 0 = Node board []
	| otherwise = Node board nodeList
	where
		nodeList = map (\x -> generateTree x (depth - 1) (reverseSign sign)) possibleBoards
		possibleBoards = nub $ concat $ map (\x -> generateBoards board x sign) (fields board) 

reverseSign :: Sign -> Sign
reverseSign sign
	| sign == X = O
	| sign == O = X

----------Min(max)----------------------

minMax :: Tree Board -> Sign -> Board
minMax (Node board boards) s = 
	bestBoard
	where
		list = map (\(Node x b) -> (x , (checkBoard x s))) boards 
		sortedList = sortBy (comparing snd) list
		(bestBoard, rating) = last sortedList

-----------Sprawdzanie wygranej--------------------

checkWin :: Board -> Sign -> Bool
checkWin board s  
		| length win /= 0 = True
		| otherwise = False
	where
		b = map (\x -> (checkRight board x s 1)) filtered
		c = map (\x -> (checkDown board x s 1)) filtered
		d = map (\x -> (checkRightCant board x s 1)) filtered
		e = map (\x -> (checkLeftCant board x s 1)) filtered
		filtered = (filter (\x -> (sign x) == s ) (fields board)) -- zostawia tylko pola wybranego znaku
		win = filter (\x -> x == 5) (b ++ c ++ d ++ e) -- zwróci tablicę z miejscami wystąpienia 5


-----------Główna pętla gry, interakcja z użytkownikiem------

parseInput :: String ->  Sign -> Maybe Field
parseInput string sig
	| length input /= 2 = Nothing
	| length (input !! 1) /= 1 = Nothing
	| otherwise = Just (Field (strToNum (head input), letterToNum (last input)) sig) 
	where
		input = words string

strToNum :: String -> Int
strToNum string 
	| length string == 1 = (ord (head string)) - (ord ('0'))
	| otherwise = ((ord (head string)) - (ord ('0'))) * 10 + ((ord (string !! 1)) - (ord ('0')))

letterToNum :: String -> Int
letterToNum string =
	 (ord (head string)) - (ord ('A')) + 1

endGame :: Sign -> IO ()
endGame s 
	| s == X = putStr("Gratulacje, wygrałeś!\n")
	| otherwise = putStr("Wygrał komputer\n")


gameBoard = Board 19 []

gameLoop :: Board -> IO ()
gameLoop board = do
	putStr ("Twój ruch - jesteś X\n")
	printBoard board
	line <- getLine
	let maybeFieldToPut = parseInput line X
	if maybeFieldToPut == Nothing then do
		putStr("Niewłaściwe współrzedne, prawidłowy format to: numer litera\n")
		gameLoop board
	else do

	let fieldToPut = fromJust maybeFieldToPut		
	let maybeNewBoard = addElem fieldToPut board
	if maybeNewBoard == Nothing then do
		putStr("Przekroczony zakres planszy lub to miejsce jest już zajęte\n")
		gameLoop board
	else do

	let newBoard = fromJust maybeNewBoard
	printBoard newBoard
	if (checkWin newBoard X) then do
		endGame X
	else do
	putStr ("Ruch komputera\n")
	let boardAfterCompMove = minMax (generateTree newBoard 1 O) O
	printBoard boardAfterCompMove
	if (checkWin boardAfterCompMove O) then do
		endGame O
	else do
	gameLoop boardAfterCompMove

main = gameLoop gameBoard
