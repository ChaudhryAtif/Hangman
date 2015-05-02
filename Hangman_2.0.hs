import System.IO
import Data.Char					-- isDigit, toUpper, isAlpha
import System.Exit
import System.Random  				-- Random Word
import System.Process 				-- Clear Screen

main :: IO()
main = do
		system "cls"			-- Clear Screen (Windows)
		putStr howTo			-- Instructions
		startHangman
		
playHangman :: String -> Int -> [Char] -> IO ()
playHangman theWord no allGuesses
	| (all (`elem` allGuesses) theWord) = displayResult theWord no 1 allGuesses		-- A Match
	| (no >= 9) = displayResult theWord no 0 allGuesses 					-- Out of lives
	| otherwise = do
--		putStrLn ("\nHangman> ### Chosen Word: " ++ theWord)
		
		displayResult theWord no 2 allGuesses			-- 2 = Ignored state
		
		putStr "Hangman> "
		letter <- takeGuess	
		let no'
			| elem letter theWord = no			-- Correct guess
			| elem letter allGuesses = no			-- Already guessed
			| otherwise = succ no				-- Decrement num of lives

		let allGuesses'
			| letter `elem` allGuesses = allGuesses		-- Already an element
			| otherwise = letter:allGuesses			-- Add to allGuesses'

		playHangman theWord no' allGuesses'

-- ### Game Related
-- Compares pickedWord's letter to allGuesses', and update view
updateWord :: String -> String -> [Char]
updateWord pickedWord allGuesses =
	concatMap (\x -> if x `elem` allGuesses then [x] else " - ") pickedWord 	-- goo.gl/8kxNhH + goo.gl/72HEQG

-- Accumulate bad guesses
badGuesses :: String -> String -> [Char]
badGuesses theWord allGuesses = [x | x <- allGuesses, not (x `elem` theWord)]

-- Display current state (Helps avoid incomplete view in win/lose cases)
displayResult theWord no state allGuesses = do
	drawPic no
	putStrLn (" THE WORD:    " ++ updateWord theWord allGuesses)
	putStrLn (" BAD GUESSES: " ++ badGuesses theWord allGuesses ++ "\n")

	if (state == 1) then putStrLn ("Hangman> You escaped death! The word was '" ++ theWord ++ "'")
		else if (state == 0) then putStrLn ("Hangman> You were hanged! The word was '" ++ theWord ++ "'") else return()
	if (state == 1) || (state == 0) then do
		putStrLn "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
		putStr "Hangman> "
		startHangman								-- Restart/New game
		else return()

howTo = 
	"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\
	\|                                 Hangman 2.0!                                 |\n\
	\|                               By Atif Chaudhry                               |\n\
	\++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\
	\Hangman> Game Instructions: \n\
	\ (1) To begin, choose a playing option Single/Multi player.\n\
	\ - Single Player: A word is picked for you, and you (try to) guess the word!\n\
	\ + Multi  Player: Player 1 (P1) chooses a word, Player 2 (P2) guesses the word!\n\
	\\n\
	\ (2) To guess a letter, type the letter and press enter.\n\
	\ - Just like cats, you also get nine (9) lives before death!\n\
	\ + To quit at any point, enter ':Q', where Q is any character!\n\
	\++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\
	\Hangman> "
	
-- Single/Multi-Player menu (and execute choice)
startHangman = do
	putStrLn "Select a playing option. <Enter 0, 1, or 2>:\n\
	\ 0 Exit\n\
	\ 1 Single player\n\
	\ 2 Multi  player\n"
	
	putStr "Hangman> " 
	option <- getLine
	if (null option) then reStart						-- If empty, re-Prompt
		else do let head:tail = option
			if head == ':' && not (null tail) then exitGame
			else if (not (checkDigit option)) then reStart
				else do let opt = read option :: Int  		-- Convert to Int
					if opt == 1 then singlePlayer
						else if opt == 2 then multiPlayer
							else if opt == 0 then exitGame else reStart
	
-- ### Playing Options
singlePlayer = do
	putStr "Hangman> "
	wordLen <- takeLength	
	
	-- Get filtered word list (by length), and then pick a random word
	wordList <- fmap (filter (\x -> length x == wordLen) . lines) $ readFile "words.txt"
	tempWord <- randomRIO (0, length wordList - 1) >>= return . (wordList !!) 			-- goo.gl/i4V2Mq 
	
	let pickedWord = capitalize tempWord
	putStrLn "Hangman> A word has been picked for you... Let's get this started!"
	playHangman pickedWord 0 []

multiPlayer = do
	putStr "Hangman> "
	hSetEcho stdin False 			-- Hide input
	pickedWord <- takeWord
	hSetEcho stdin True
	putStrLn "Hangman> P1's word has been saved... Let's get this started!"
	playHangman pickedWord 0 []

exitGame = do
	putStrLn "Hangman> Thank you for playing Hangman by Atif Chaudhry!"
	exitWith ExitSuccess

-- ### Get input (length, word, guess)
-- Get length from player
takeLength = do
	putStr "Pick a length of word to guess (2-30): " 
	tempLength <- getLine
	if (null tempLength) then reLength					-- If empty, re-Prompt
	else if ((checkDigit tempLength) && (length tempLength >= 1)) then do
			let len = read tempLength :: Int			-- Read as int
			if (len >= 2 && len <= 30) then return len else reLength
		else do let head:tail = tempLength
			if head == ':' && not (null tail) then exitGame else reLength

-- Get word from P1
takeWord = do
	putStr "P1, enter a word: " 
	tempWord <- getLine
	putStrLn ""
	
	if (null tempWord) then reWord						-- If empty, re-Prompt
		else do let head:tail = tempWord
			if head == ':' && not (null tail) then exitGame
			else if (null tempWord) || (checkDigit tempWord) || not (checkAlpha tempWord) then reWord
				else if head == ':' && not (null tail) then exitGame else return (capitalize tempWord)

-- Get letter from Player
takeGuess = do
	putStr "Guess a letter (or :Q to quit): "
	tempLetter <- getLine
	if (null tempLetter) || (length tempLetter < 1) then reGuess
		else do let head:tail = tempLetter				-- Split input (head:tail)
			if isAlpha head then return $ toUpper head		-- Return head of input
			else if head == ':' && not (null tail) then exitGame else reGuess

-- ### Utilities
-- Checks if input is/are digit(s) (0-9)
checkDigit :: [Char] -> Bool 
checkDigit xs = all isDigit xs

-- Checks if input are letters (a-z)
checkAlpha :: [Char] -> Bool 
checkAlpha xs = all isAlpha xs

-- Capitalizes given string (entirely)						# goo.gl/GykFUw
capitalize :: String -> String
capitalize str = map toUpper str

-- ### Re-Prompters
reStart = do
	putStr "Hangman> Bad choice. Try again... "
	startHangman
reLength = do
	putStr "Hangman> Invalid length. Try again... "
	takeLength
reWord = do
	putStr "Hangman> Invalid word. Try again... "
	takeWord
reGuess = do
	putStr "Hangman> Invalid Guess. Try again... "
	takeGuess

-- ### Hangman Drawing
drawPic :: Int -> IO ()
drawPic n
    |n == 0 = putStrLn "\n\
\  _____________ \n\
\ | # Lives: 09 |\n\
\ |             |\n\
\ |             |\n\
\ |             |\n\
\ |             |\n\
\ |             |\n\
\ |             |\n\
\ |             |\n\
\ |_____________|\n"

    |n == 1 = putStrLn "\n\
\  _____________ \n\
\ | # Lives: 08 |\n\
\ | +-----+     |\n\
\ | |           |\n\
\ | |           |\n\
\ | |           |\n\
\ | |           |\n\
\ | |           |\n\
\ | +_________| |\n\
\ |_/\\_______/\\_|\n"

    |n == 2 = putStrLn "\n\
\  _____________ \n\
\ | # Lives: 07 |\n\
\ | +-----+     |\n\
\ | |     |     |\n\
\ | |     O     |\n\
\ | |           |\n\
\ | |           |\n\
\ | |           |\n\
\ | +_________| |\n\
\ |_/\\_______/\\_|\n"

    |n == 3 = putStrLn "\n\
\  _____________ \n\
\ | # Lives: 06 |\n\
\ | +-----+     |\n\
\ | |     |     |\n\
\ | |     O     |\n\
\ | |     |     |\n\
\ | |           |\n\
\ | |           |\n\
\ | +_________| |\n\
\ |_/\\_______/\\_|\n"

    |n == 4 = putStrLn "\n\
\  _____________ \n\
\ | # Lives: 05 |\n\
\ | +-----+     |\n\
\ | |     |     |\n\
\ | |     O     |\n\
\ | |    /|     |\n\
\ | |           |\n\
\ | |           |\n\
\ | +_________| |\n\
\ |_/\\_______/\\_|\n"

    |n == 5 = putStrLn "\n\
\  _____________ \n\
\ | # Lives: 04 |\n\
\ | +-----+     |\n\
\ | |     |     |\n\
\ | |     O     |\n\
\ | |    /|\\    |\n\
\ | |           |\n\
\ | |           |\n\
\ | +_________| |\n\
\ |_/\\_______/\\_|\n"

    |n == 6 = putStrLn "\n\
\  _____________ \n\
\ | # Lives: 03 |\n\
\ | +-----+     |\n\
\ | |     |     |\n\
\ | |     O     |\n\
\ | |    /|\\    |\n\
\ | |     |     |\n\
\ | |    /      |\n\
\ | +_________| |\n\
\ |_/\\_______/\\_|\n"

    |n == 7 = putStrLn "\n\
\  _____________ \n\
\ | # Lives: 02 |\n\
\ | +-----+     |\n\
\ | |     |     |\n\
\ | |     O     |\n\
\ | |    /|\\    |\n\
\ | |     |     |\n\
\ | |    / \\    |\n\
\ | +_________| |\n\
\ |_/\\_______/\\_|\n"

    |n == 8 = putStrLn "\n\
\  _____________ \n\
\ | # Lives: 01 |\n\
\ | +-----+     |\n\
\ | |     |     |\n\
\ | |     O     |\n\
\ | |    /|\\    |\n\
\ | |     |     |\n\
\ | |   _/ \\    |\n\
\ | +_________| |\n\
\ |_/\\_______/\\_|\n"

    |n == 9 = putStrLn "\n\
\  _____________ \n\
\ | # Lives: 00 |\n\
\ | +-----+     |\n\
\ | |     |     |\n\
\ | |     X     |\n\
\ | |    /|\\    |\n\
\ | |     |     |\n\
\ | |   _/ \\_   |\n\
\ | +_________| |\n\
\ |_/\\_______/\\_|\n"

	|otherwise = return()
