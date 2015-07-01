-- Simple Markdown Text Parser

-- TODO
-- inline stuff, like bold/italic and links

module Markdown (parse) where

import Data.List (foldl')

parse :: String -> String
parse t = unlines $ map parseParagraph $ splitParagraphs t

parseParagraph :: String -> String
parseParagraph t |  len == 0 = ""
                 |  len == 1 = singleLineParse t
                 | otherwise = multiLineParse . lines $ t
  where
    len = length $ lines t

singleLineParse :: String -> String
singleLineParse ('-':'-':'-':_) = ruler
singleLineParse ('*':'*':'*':_) = ruler
singleLineParse ('#':t)         = heading 1 t
singleLineParse t               = paragraph t

multiLineParse :: [String] -> String
multiLineParse []                        = ""
multiLineParse t@(('*':' ':_):_)         = unorderedList t
multiLineParse t@(('-':' ':_):_)         = unorderedList t
multiLineParse t@((' ':' ':' ':' ':_):_) = code t
mutliLineParse t                         = paragraph . unlines $ t

heading :: Integer -> String -> String
heading n ('#':t) = heading (n+1) t
heading n (' ':t) = heading n t
heading _ ""      = ""
heading n t       = encapsulate ('h':show n) t

unorderedList :: [String] -> String
unorderedList [] = ""
unorderedList t  = encapsulate "ul" $ list t

orderedList :: [String] -> String
orderedList [] = ""
orderedList t  = encapsulate "ol" $ list t

list :: [String] -> String
list t  = (concat (foldl' buildList [] t)) ++ "</li>"
  where
    -- TODO
    -- handle ordred list indicators
    buildList :: [String] -> String -> [String]
    buildList r e | take 2 e == "- " || take 2 e == "* " = if r /= []
                    then init r ++ [last r ++ "</li>"] ++ ["<li>" ++ drop 2 e]
                    else r ++ ["<li>" ++ drop 2 e]
                  |   r /= [] = init r ++ [last r ++ e]
                  | otherwise = [e]

ruler :: String
ruler = "<hr />"

code :: [String] -> String
code [] = ""
code t  | all indented t = encapsulate "pre" $ encapsulate "code" $ unlines
                           $ map (drop 4) t
        |      otherwise = paragraph . unlines $ t
  where
    indented :: String -> Bool
    indented t = "    " == take 4 t

paragraph :: String -> String
paragraph "" = ""
paragraph t  = encapsulate "p" t

encapsulate :: String -> String -> String
encapsulate _ "" = ""
encapsulate c t  = "<" ++ c ++ ">" ++ t ++ "</" ++ c ++ ">"

splitParagraphs :: String -> [String]
splitParagraphs "" = []
splitParagraphs t  = foldl' (\r e -> if e == ""
                                       then r ++ [""]
                                     else if r /= []
                                       then (init r) ++ [last r ++ "\n" ++ e]
                                       else [e]) [] $ lines t

