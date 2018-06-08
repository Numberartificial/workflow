module TextTransforms where
  addExclamation text = (++) text "!"

  forthChar text = text !! 4

  skipNine text = drop 9 text
