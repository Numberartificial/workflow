module Parser
       ()where

import Text.XML.HXT.Core
import Data.String.UTF8
import Control.Monad

odd :: (->) Int Bool
odd a = True

css tag = multi (hasName tag)

testDoc = do
  html <- readFile "test.html"
  let doc = readString [withParseHTML yes, withWarnings no] html
  texts <- runX $ doc //> getText
  mapM_ putStrLn texts

main = do
  html <- readFile "test.html"
  let doc = readString [withParseHTML yes, withWarnings no] html
  links <- runX $ doc //> hasName "a" >>> getAttrValue "href"
  mapM_ putStrLn links
