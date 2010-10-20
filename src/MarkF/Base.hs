module MarkF.Base where

import Control.Monad.State
import Data.Maybe

import MarkF.Data

--------------------------------------------------------------------------------
-- * EvalT functions

withCurrent :: (SrcMarkUp -> Maybe SrcMarkUp) -> Func
withCurrent f = Func $ \_ -> do
    modify $ \s -> s { position = let p = position s
                                      c = posCur p
                                   in p { posCur = fromMaybe c (f c) }
                     }
