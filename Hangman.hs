import System.IO
import Data.Char					-- toUpper, isAlpha
import System.Random  				-- Random Word

main :: IO()
main = do
		putStr "\n\
		\++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\
		\|                                 Hangman 1.0!                                 |\n\
		\|                               By Atif Chaudhry                               |\n\
		\++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\
		\# Game Instructions: \n\
		\ To guess a letter, type the letter and press enter.\n\
		\ - You have seven (7) chances to guess the word, before death!\n\
		\++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
		startGame
		
playHangman :: String -> Int -> [Char] -> IO ()
playHangman theWord no allGuesses
	| all (`elem` allGuesses) theWord = putStrLn ("Hangman> You escaped death! The word was: " ++ theWord ++ "\n")	-- A Match
	| no >= 7    = putStrLn ("Hangman> You were hanged! The word was: " ++ theWord ++ "\n")							-- Out of lives
	| otherwise = do
		drawPic no
		putStrLn (" THE WORD: " ++ (updateWord theWord allGuesses))
		putStrLn (" BAD GUESSES: " ++ badGuesses theWord allGuesses ++ "\n")
		
		letter <- takeGuess
		let no'
			| elem letter theWord = no		-- Correct guess
			| otherwise = succ no			-- Add to allGuesses

		let allGuesses'
			| letter `elem` allGuesses = allGuesses
			| otherwise = letter:allGuesses				-- Add to allGuesses'

		playHangman theWord no' allGuesses'
	
startGame = do
	wordList <- getWords "words.txt"
	tempWord <- getWord wordList
	let pickedWord = capitalize tempWord
	putStrLn "Hangman> A word has been picked for you... Let's get this started!"
	-- putStrLn pickedWord				-- Display picked word
	playHangman pickedWord 0 []		-- Picked Word, Hangman Drawing, Guesses

takeGuess = do
	putStr "Hangman> Guess a letter: "
	tempLetter <- getLine
	if (null tempLetter) || (length tempLetter < 1) then takeGuess
		else do let letter:gs = tempLetter
			if isAlpha letter then return $ toUpper letter else takeGuess

updateWord :: String -> String -> String
updateWord pickedWord allGuesses = map (\x -> if x `elem` allGuesses then x else '-') pickedWord -- goo.gl/hj1wwE

badGuesses :: String -> String -> [Char]
badGuesses theWord letters = [x | x <- letters, not $ x `elem` theWord]
			
-- Given file, loads words into a list 		# goo.gl/PT4SHn
getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (lines contents)

-- Given a list, randomly picks an element 	# goo.gl/i4V2Mq
getWord :: [a] -> IO a
getWord xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

-- Capitalizes entire string				# goo.gl/GykFUw
capitalize :: String -> String
capitalize str = map toUpper str

drawPic :: Int -> IO ()
drawPic n
    |n == 0 = putStrLn "\n"

    |n == 1 = putStrLn "\n\
\+-----+     \n\
\|           \n\
\|           \n\
\|           \n\
\|           \n\
\|           \n\
\+_________| \n"

    |n == 2 = putStrLn "\n\
\+-----+     \n\
\|     |     \n\
\|     O     \n\
\|           \n\
\|           \n\
\|           \n\
\+_________| \n"

    |n == 3 = putStrLn "\n\
\+-----+     \n\
\|     |     \n\
\|     O     \n\
\|     |     \n\
\|           \n\
\|           \n\
\+_________| \n"

    |n == 4 = putStrLn "\n\
\+-----+     \n\
\|     |     \n\
\|     O     \n\
\|    /|     \n\
\|           \n\
\|           \n\
\+_________| \n"

    |n == 5 = putStrLn "\n\
\+-----+     \n\
\|     |     \n\
\|     O     \n\
\|    /|\\    \n\
\|           \n\
\|           \n\
\+_________| \n"

    |n == 6 = putStrLn "\n\
\+-----+     \n\
\|     |     \n\
\|     O     \n\
\|    /|\\    \n\
\|     |     \n\
\|    /      \n\
\+_________| \n"

    |n == 7 = putStrLn "\n\
\+-----+     \n\
\|     |     \n\
\|     O     \n\
\|    /|\\    \n\
\|     |     \n\
\|    / \\    \n\
\+_________| \n"

	|otherwise = putStrLn "Out of bounds error"
