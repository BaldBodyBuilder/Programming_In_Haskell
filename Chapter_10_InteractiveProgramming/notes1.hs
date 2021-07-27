module Test where
{-
type IO = World -> World
type IO a = World -> (a, World)
Expressions of type IO a are actions

IO Char gives us an action with a return result of char
IO () is the type of actions tha return the empty tuple () as a dummy result value.

We'll consider IO as such:
    data IO a = . . .

-}

-- Three basic actions of dealing with IO
{-
reads from keyboard and echos to screen
getChar :: IO Char
getChar = . . .

Writes the char c to the screen 
putChar :: Char -> IO ()
putChar c = . . .

Returns the result value v without performing any interaction with the user
return :: a -> IO a
return v = . . .

Return provides a bridge from pure expressions without side effects to impure actions with side effects
There is no bridge back once we are impure we are impure for forever!
Impurity quickly impermeates most programs 

-}

-- Sequencing
{-
do v1 <- a1
   v2 <- a2
   .
   .
   .
   vn <- an
   return (f v1 v2 . . . vn)

Perform action a 1 and call the result then a2 and the result and so on.

Example and action that reads three characters and then discards the second and returns the first aan third as a pair:
act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)

If we failed to include the return we'd get a type error becuase it would be expecting the (Char,Char)
-}

-- Derived primitives
{-
getLine :: IO String
getLine = do x <- getChar
            if x == '\n' then
                return []
            else 
                do xs <- getLine
                    return (x:xs)
Reads a string of characters from the keyboard and terminates with an \n

putStr :: String -> IO ()
putStr []   = return ()
putStr (x:xs) = do putChar x
                   putStr xs

putStrLn :: String -> IO ()
putStrLn xs = do putStr xs
                 putChar '\n'

strlen :: IO ()
strlen = do putStr "Enter a string: "
                    xs <- getLine
                    putStr  "The String Has "
                    putStr (show (length xs))
                    putStrLn " characters"
-- EX: strlen "Haskell" "The string has 7 characters."
-}

