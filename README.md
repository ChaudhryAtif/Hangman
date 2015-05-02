# Program Overview
* A Multi-feature Hangman game written in Haskell, to be played in the console.
  * *Some Features:*
    * Visual hangman drawing,
    * Quit at anytime, Heavy error-checking,
    * Single (w/ word length) and multi-player playing option!
* **By Atif Chaudhry** (Programming Languages @ Saint Louis University, Spring 2015).

# Hangman Overview
- Hangman a game of guessing a word (or a phrase), one letter at a time
- In this version of the game, the player has 8 lives/chances to guess all the letters in the word.

# Game: Start
* Download Haskell Platform: http://goo.gl/bfyWAW (Windows for me)
* Either Double click Hangman_2.0.hs or Right Click -> 'Open with GHCI'
  * **Alternative:** run ghci command in console and enter :load Hangman_2.0.hs
* Call 'main' function to begin
* **Example:**
```
    F:\Directory>ghci
    -- Some text shows automatically
    Prelude> :load Hangman_2.0.hs
    -- Loaded module(s) are shown
    *Main> main
    -- Game begins
```

- The game starts off with showing game's how-to, along with a game menu
![alt tag](http://i59.tinypic.com/2vjaq3b.jpg) </br>
○ *Option 1:* Given a length, a random word of that length is picked for you </br>
○ *Option 2:* Player 2 picks a word (word is not shown/printed) for Player 1 to guess

- After the word is picked, the following set-up is shown
![alt tag](http://i61.tinypic.com/28ahz5v.jpg) </br>
○ The total number of ' _ ' represent the length of the word, and each ' _ ' represents a letter

# Game: Guessing Logic
* Each turn the player guesses a letter. Only the first letter of input is considered (i.e. in 'abc' only 'a' is taken)
* If the guess is correct, all occurrences of the letter are revealed.
  * *For example*, if the hidden word is 'paisa' and the guess was  'a' , the view becomes  _ A _  _ A.
* The number of lives do *NOT* decrease on correct, invalid (i.e. #, /, $, 3), or duplicate guess(es).
* Gradually, the wrong guesses will accumulate, and the hangman drawing will continue to become complete
![alt tag](http://i61.tinypic.com/90626h.jpg)

# Game: End
- (1) The player *WINS* if all letters are guessed with one or more lives to spare
![alt tag](http://i61.tinypic.com/2qu3c42.jpg)

- (2) The player *LOSES* if the player runs out of lives (the picked word IS shown)
![alt tag](http://i58.tinypic.com/2a8me4z.jpg)

- (3) The player *QUITS* (any time) without without completing the game (the picked word is NOT shown)
![alt tag](http://i57.tinypic.com/148lr7t.jpg)
