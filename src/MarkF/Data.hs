{-# LANGUAGE RankNTypes #-}

module MarkF.Data where

import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map   as M
import qualified Data.Text  as T
import qualified Text.Blaze as B

type Source   = [SrcMarkUp]

-- | The "markup" source language representation, including expressions.
data SrcMarkUp

    -- "primitives"
    = SrcText                      T.Text
    | SrcHtml                      B.Html

    -- mark up
    | SrcHeadline      Int         [SrcMarkUp]    -- Int: Level (1 = highest, 6 = lowest)
    | SrcParagraph                 [SrcMarkUp]
    | SrcStyle         Style       [SrcMarkUp]
    | SrcLine                                  -- -> <hr /> tag in HTML

    -- span elements
    | SrcList          LstNum      [SrcMarkUp]
    | SrcQuote                     [SrcMarkUp]
    | SrcCode          Hlight      T.Text
    | SrcLink          Lnk         [SrcMarkUp]
    | SrcImage         Lnk         T.Text
    | Expressions                  [Exp]


--------------------------------------------------------------------------------
-- * Markup
--

-- Two type synonyms - just to keep track of what we've done so far.
type Document = [MarkUp]

-- | This is exactly the same markup representation as SrcMarkUp without
-- expressions
data MarkUp

    -- "primitives"
    = Text                      T.Text
    | Html                      B.Html

    -- mark up
    | Headline      Int         [MarkUp]    -- Int: Level (1 = highest, 6 = lowest)
    | Paragraph                 [MarkUp]
    | Style         Style       [MarkUp]
    | Line                                  -- -> <hr /> tag in HTML

    -- span elements
    | List          LstNum      [MarkUp]
    | Quote                     [MarkUp]
    | Code          Hlight      T.Text
    | Link          Lnk         [MarkUp]
    | Image         Lnk         T.Text


data Style
    = StyleStrong
    | StyleEmph

data LstNum
    = LstUnsorted
    | LstSorted

type Hlight = Maybe T.Text

type Url    = T.Text

data Lnk
    = LnkUrl        Url
    -- | LnkName       T.Text

type Name = T.Text


--------------------------------------------------------------------------------
-- * Expressions
--

data Exp
    = VarE              Id
    | LitE              Literal
    | AppE          Id  [Exp]
    -- TODO:
    -- | LetE              [LetExp]

type Id = T.Text

data LetExp
    = LetExp        Id  Exp

data Literal
    = LitText           T.Text
    | LitBool           Bool
    | LitInt            Integer
    | LitFloat          Float

type Lookup = M.Map Id Func

newtype Func = Func ([Exp] -> EvalT ())


data EvalState = EvalState
    { lkup      :: Lookup
    , position  :: Position SrcMarkUp
    }

type EvalT a = ErrorT String (StateT EvalState IO) a

--------------------------------------------------------------------------------
-- * Positions
--

data Position a = Position
    { posUp             :: Maybe (Position a)
    , posPrev           :: [a]
    , posCur            :: a
    , posNext           :: [a]
    }
