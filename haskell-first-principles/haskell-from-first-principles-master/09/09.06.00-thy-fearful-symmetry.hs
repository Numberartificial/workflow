{-
I'm not super happy with this implementation,
so far I learned that:
dropWhile (== ' ') "a b c" result in
"a b c"

dropWhile (/= ' ') "a b c" results in
" b c"

takeWhile (/= ' ') "a b c"
"a"
-}

myWords x = go x []
  where
    go str result
      | (dropWhile (== ' ') str) == "" = result
      | take 1 str == " " = go (dropWhile (== ' ') str) result
      | otherwise = go (dropWhile (/= ' ') str) (result ++ [(takeWhile (/= ' ') str)])
