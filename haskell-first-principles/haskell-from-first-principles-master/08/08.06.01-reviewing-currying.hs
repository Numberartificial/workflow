cattyConny :: String -> String -> String
cattyConny x y = x ++ "mrow" ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"


-- appedCatty "woohoo!"
-- cattyConny "woops" "wooho!"
-- "woops mrow wooho!"

-- frappe "1"
-- flippy "haha" "1"
-- flip cattyConny "haha" "1"
-- cattyConny "1" "haha"
-- "1 mrow haha"

-- frappe (appedCatty "2")
-- frappe (cattyConny "woops" "2")
-- (flippy "haha") (cattyConny "woops" "2")
-- (flip cattyConny "haha") (cattyConny "woops" "2")
-- flip cattyConny "haha" "woops mrow 2"
-- cattyConny "woops mrow 2" "haha"
-- "woops mrow 2 mrow haha"


-- appedCatty (frappe "blue")
-- appedCatty (flippy "haha" "blue")
-- appedCatty (flip cattyConny "haha" "blue")
-- appedCatty (cattyConny "blue" "haha")
-- cattyConny "woops" "blue mrow haha"
-- woops mrow blue mrow haha

{-cattyConny (frappe "pink")
  (cattyConny "green" (appedCatty "blue"))-}
{-cattyConny (flippy "haha" "pink")
  (cattyConny "green" (cattyConny "woops" "blue"))-}
{-cattyConny (flip cattyConny "haha" "pink")
  (cattyConny "green" "woops mrow blue")-}
{-cattyCony (cattyCony "pink" "haha")
  "green mrow woops mrow blue"-}
-- cattyCony "pink mrow haha" "green mrow woops mrow blue"
-- "pink mrow haha mrow green mrow woops mrow blue"

-- cattyConny (flippy "Pugs" "are") "awesome"
-- cattyConny (flip cattyConny "Pugs" "are") "awesome"
-- cattyConny (cattyConny "are" "Pugs") "awesome"
-- cattyConny "are mrow Pugs" "awesome"
-- "are mrow Pugs mrow awesome"
