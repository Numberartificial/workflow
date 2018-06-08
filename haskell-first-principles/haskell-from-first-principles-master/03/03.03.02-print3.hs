module Print3 where

--The following line reads "myGreetings has a type String"
myGreetings :: String
myGreetings = "hello " ++ "world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
  putStrLn myGreetings
  putStrLn secondGreeting
  where secondGreeting = concat [hello, " ", world]
