module MarkF.RunSrc
    ( runSrc
    , next
    ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Text.Printf

import qualified Data.Text as T
import qualified Data.Map  as M

import MarkF.Data

runSrc :: Lookup -> Source -> IO (Either String Document)
runSrc _ []    = return $ Right []
runSrc l (c:n) = do

    (a,s) <- runStateT (runErrorT $ run c)
                       (EvalState l (Position Nothing [] c n))

    -- Either monad:
    return $ do

        -- make sure "runStateT" didn't fail:
        a
        -- build the document from the "position"
        build $ position s


run :: SrcMarkUp -> EvalT ()
run c = do

    case c of
         Expressions es -> mapM_ runExp es
         _              -> return ()

    n <- next
    case n of
         Just n' -> run n'
         _       -> return ()


--------------------------------------------------------------------------------
-- Errors

notInScope :: String -> String -> String
notInScope d v = printf "Not in scope: %s `%s'" d v

unexpected :: String -> String -> String
unexpected d v = printf "Unexpected %s: `%s'" d v

invalidGoUp :: String
invalidGoUp = "Invalid `curPos' at `goUp'."

emptyTree :: String
emptyTree = "Empty position tree."

--------------------------------------------------------------------------------
-- Expressions

runExp :: Exp -> EvalT ()

runExp (LitE l) = fail . uncurry unexpected $
    case l of
         LitText  t -> ("Text",     T.unpack t)
         LitBool  b -> ("Bool",     show b)
         LitInt   i -> ("Integer",  show i)
         LitFloat f -> ("Float",    show f)

runExp (VarE i) = do
    l <- gets lkup
    case M.lookup i l of
         Just (LFunc f) -> f []
         Nothing        -> fail $ notInScope "Variable" (T.unpack i)

runExp (AppE i es) = do
    l <- gets lkup
    case M.lookup i l of
         Just (LFunc f) -> f es
         Nothing        -> fail $ notInScope "Function" (T.unpack i)


{- TODO

runExp (LetE les) =
    modify $ \s -> s { lkup = foldr addLet (lkup s) les }
  where
    addLet (LetExp i e) l = M.insert i 

-}


--------------------------------------------------------------------------------
-- Navigation

-- | Move the position to the next (nested) element of the markup tree
next :: EvalT (Maybe SrcMarkUp)
next = do
    pos <- gets position
    case posCur pos of
         SrcHeadline _ s -> goDown s
         SrcParagraph  s -> goDown s
         SrcStyle _    s -> goDown s
         SrcList _     s -> goDown s
         SrcQuote      s -> goDown s
         SrcLink _     s -> goDown s
         _               -> next' pos

-- | Go to the next element in the current position without going into nested
-- elements of the markup tree
next' :: Position SrcMarkUp -> EvalT (Maybe SrcMarkUp)
next' (Position up pre cur nex) =
    case nex of
         []    -> goUp
         (c:n) -> do modify $ \s -> s { position = Position up (pre ++ cur') c n }
                     return (Just c)
  where
    -- remove "Expressions"
    cur' = if isExp cur then [] else [cur]


goDown :: [SrcMarkUp] -> EvalT (Maybe SrcMarkUp)
goDown []         = gets position >>= next'
goDown (c:n) = do
    modify $ \s@EvalState { position = pos } ->
        s { position = Position (Just pos) [] c n }
    return $ Just c

goUp :: EvalT (Maybe SrcMarkUp)
goUp = do
    s <- get
    case position s of
         Position (Just (Position uu pu cu (cu':nu'))) p c [] -> do

             let s' = p ++ if isExp c then [] else [c]
             c' <- case cu of
                        SrcHeadline i _ -> return $ SrcHeadline i s'
                        SrcParagraph  _ -> return $ SrcParagraph  s'
                        SrcStyle sty  _ -> return $ SrcStyle sty  s'
                        SrcList num   _ -> return $ SrcList num   s'
                        SrcQuote      _ -> return $ SrcQuote      s'
                        SrcLink l     _ -> return $ SrcLink l     s'
                        _               -> fail invalidGoUp

             put s { position = Position uu (pu ++ [c']) cu' nu' }
             return $ Just cu'

         -- Remove a last "Expressions":
         Position Nothing p@(_:_) (Expressions _) [] -> do
             put s { position = Position Nothing (init p) (last p) [] }
             return Nothing

         Position Nothing [] (Expressions _) [] -> fail emptyTree

         Position _ _ _ _ -> return Nothing


--------------------------------------------------------------------------------
-- * Document building

build :: Position SrcMarkUp -> Either String Document

build (Position (Just u) _ _ _) =
    build u

build (Position Nothing (c':n') c n) =
    build $ Position Nothing [] c' (n' ++ [c] ++ n)

build (Position Nothing [] c n) = do

    c' <- case c of
               Expressions _    -> fail "Cannot `build' Expressions. Please run `runSrc' first."

               SrcHeadline i s  -> Headline i <$> build' s
               SrcParagraph  s  -> Paragraph  <$> build' s
               SrcStyle sty  s  -> Style sty  <$> build' s
               SrcList num   s  -> List num   <$> build' s
               SrcQuote      s  -> Quote      <$> build' s
               SrcLink l     s  -> Link l     <$> build' s

               SrcText t        -> Right $ Text t
               SrcHtml h        -> Right $ Html h
               SrcLine          -> Right $ Line
               SrcCode h code   -> Right $ Code h code
               SrcImage l s     -> Right $ Image l s

    (c' :) <$> rest

  where
    rest = case n of
                (c':n') -> build $ Position Nothing [] c' n'
                _       -> Right []

    build' :: [SrcMarkUp] -> Either String Document
    build' []    = Right []
    build' (c':n') = build $ Position Nothing [] c' n'


--------------------------------------------------------------------------------
-- * Helper

isExp :: SrcMarkUp -> Bool
isExp (Expressions _) = True
isExp _               = False
