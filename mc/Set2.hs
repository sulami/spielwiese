{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set2 where

import           MCPrelude

-- 201

data Maybe a = Just a
             | Nothing

instance Show a => Show (Maybe a) where
  show (Just x) = "Just " ++ show x
  show Nothing  = "Nothing"

-- 202

headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay []     = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ []         = Nothing
lookupMay k ((x,v):xs) | x == k    = Just v
                       | otherwise = lookupMay k xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just $ x / y

maximumMay :: Ord a => [a] -> Maybe a
maximumMay []     = Nothing
maximumMay (x:xs) = Just $ foldr max x xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay []     = Nothing
minimumMay (x:xs) = Just $ foldr min x xs

-- 203

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gk k = case lookupMay k gk of
                    Nothing -> Nothing
                    Just xs -> case tailMay xs of
                      Nothing -> Nothing
                      Just tl -> case maximumMay tl of
                        Nothing -> Nothing
                        Just tm -> case headMay xs of
                          Nothing -> Nothing
                          Just th -> divMay (fromInteger tm) (fromInteger th)

