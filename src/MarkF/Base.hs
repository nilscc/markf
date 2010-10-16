module MarkF.Base where

import MarkF.Data
import MarkF.Utils

--------------------------------------------------------------------------------
-- * Selectors
--
-- Selectors are always of kind "MarkUp -> Bool"

type Selector a = a -> Bool

code :: Selector MarkUp
code Code {} = True
code _ = False

text :: Selector MarkUp
text Text {} = True
text _ = False

headline :: Selector MarkUp
headline Headline {} = True
headline _ = False

list :: Selector MarkUp
list List {} = True
list _ = False


--------------------------------------------------------------------------------
-- * Navigation

nextP :: Selector a -> Position a -> Maybe (Position a)
nextP f pos@(Position p c n)
    | f c       = Just pos
    | otherwise = do
        c' <- headM n
        nextP f (Position (p ++ [c]) c' (tail n))

prevP :: Selector a -> Position a -> Maybe (Position a)
prevP f pos@(Position p c n)
    | f c       = Just pos
    | otherwise = do
        c' <- lastM p
        prevP f (Position (init p) c' (c:n))


--------------------------------------------------------------------------------
-- * Function application

onP :: Selector a
    -> (a -> a)
    -> Position a
    -> Position a
onP s f (Position p c n) = Position (on p) (mf c) (on n)
  where
    on i = case i of (x:xs) -> conc $ walk (Position [] x xs)
                     _      -> []

    mf cur = if s cur then f cur else cur

    walk (Position p c (n:ns)) =
        walk $ Position (p ++ [mf c]) n ns

    walk (Position p c []) =
        Position p (mf c) []

    conc :: Position a -> [a]
    conc (Position a b c) = a ++ [b] ++ c
