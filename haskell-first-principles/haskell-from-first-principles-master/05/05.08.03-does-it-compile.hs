module DoesItCompile where

-- fixed functions

bigNum arg = (^) 5 $ arg
wahoo = bigNum $ 10

x arg = print arg
y = print "woohoo!"
z = x "hello world"

a = (+)
b = 5
c = a b 10
d = a c 200

a1 = 12 + b1
  where
    b1 = 10000 * c1
    c1 = 1
