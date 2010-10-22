module MarkF.Base
    (
      -- * Navigation
      onCurrent
    , onAll
    , onNext
    , onNextBlock
    , onPrev
    , onPrevBlock

      -- * Markup creation
    , text
    -- , html
    -- , code
    -- , list
    ) where

import Control.Monad.State
import Data.List

import qualified Data.Text as T

import MarkF.Data

--------------------------------------------------------------------------------
-- * Navigation

-- | Run a function on the currently selected node (usually the `Expressions'
-- node itself).
onCurrent :: Func -> LocatedFunc
onCurrent (Func f) = LFunc $ \es ->
    modify $ \st@EvalState { position = pos @ Position { posCur = c } } ->
        st { position = pos { posCur = f es c } }


-- | Run a function on all elements in the current position list which satisfy
-- the `Selector' predicate. This function does not go down/up the tree.
onAll :: Selector -> Func -> LocatedFunc
onAll s (Func f) = LFunc $ \es -> do
    let mf m = if s m then f es m else m
     in modify $ \ st@EvalState { position = Position u p c n } ->
           st { position = Position u (map mf p) (mf c) (map mf n) }

-- | Go to the next selection and turn `Func' into a `LocatedFunc'.
onNext :: Selector -> Func -> LocatedFunc
onNext s f = LFunc $ \es ->
    modify $ \st -> st { position = onNext' (position st) s f es }

onNext' :: Position SrcMarkUp -> Selector -> Func -> [Exp] -> Position SrcMarkUp
onNext' pos@Position { posNext = n } s (Func f) es =
    pos { posNext = fst $ foldl' next ([], True) n }
  where
    next (r, True) e | s e = (r ++ [f es e], False)
    next (r, b)    e       = (r ++ [e], b)


-- | Go to the next block element. Go up until you're at the top of the tree,
-- then go to the next block element.
onNextBlock :: Selector -> Func -> LocatedFunc
onNextBlock s f = LFunc $ \es -> do
    modify $ \st -> st { position = onTop' (position st) (\pos -> onNext' pos s f es) }


-- | Go to the previous selection and turn `Func' into a `LocatedFunc'. If Func
-- is at a top level (block) position, `previousBlock' is called, otherwise
-- `previousInline' is called.
onPrev :: Selector -> Func -> LocatedFunc
onPrev s f = LFunc $ \es -> do
    modify $ \st -> st { position = onPrev' (position st) s f es }

onPrev' :: Position SrcMarkUp -> Selector -> Func -> [Exp] -> Position SrcMarkUp
onPrev' pos@Position { posPrev = p } s (Func f) es =
    pos { posPrev = fst $ foldr prev ([], True) p }
  where
    prev e (r, True) | s e = (f es e : r, False)
    prev e (r, b)          = (e      : r, b)

onPrevBlock :: Selector -> Func -> LocatedFunc
onPrevBlock s f = LFunc $ \es ->
    modify $ \st -> st { position = onTop' (position st) (\pos -> onPrev' pos s f es) }


onTop' :: Position SrcMarkUp
       -> (Position SrcMarkUp -> Position SrcMarkUp)
       -> Position SrcMarkUp
onTop' pos@Position { posUp = Just u } f =
    pos { posUp = Just $ onTop' u f }
onTop' pos f = f pos


--------------------------------------------------------------------------------
-- Markup procession

text :: T.Text -> Func
text t = Func $ \_ _ -> SrcText t
