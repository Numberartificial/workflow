x = (+)
-- fixes:
-- wrap arguments in parentheses
-- if function is applied to a variable wrap it in backticks in order to use it
f (xs) = w `x` 1
  where w = length xs

-- original:
-- x = (+)
-- F xs = w'x'1
--   where w = length xs
