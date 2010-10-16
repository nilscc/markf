
module MarkF.Utils where

headM, lastM :: [a] -> Maybe a

headM (a:_) = Just a
headM _     = Nothing

lastM [] = Nothing
lastM ls = Just $ last ls
