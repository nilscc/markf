{-# LANGUAGE RankNTypes #-}

module MarkF.Data where

import qualified Data.Text  as T
import qualified Text.Blaze as B

type Source   = [MarkF]

data MarkF
    = MarkUp        MarkUp
    -- | Expression    Core

--------------------------------------------------------------------------------
-- * Markup
--

type Document = [MarkUp]

data MarkUp

    -- "primitives"
    = Text                          T.Text
    | Html                          B.Html

    -- mark up
    | Headline      Int             MarkUp      -- Int: Level (1 = highest, 6 = lowest)
    | Style         Style           MarkUp
    | Line                                      -- -> <hr /> tag in HTML

    -- span elements
    | List          LstNum          [MarkUp]
    | Quote                         [MarkUp]
    | Code          Hlight          T.Text
    | Link          Lnk             MarkUp
    | Image         Lnk     String  MarkUp

data Style
    = StyleStrong
    | StyleEmph

data LstNum
    = LstStar
    | LstSorted
    | LstChar       Char

type Hlight = Maybe T.Text

type Url    = T.Text

data Lnk
    = LnkUrl        Url
    | LnkName       T.Text

type Name = T.Text


--------------------------------------------------------------------------------
-- * Expressions
--

{- TODO:

data Core
    = VarC          Name
    | LitC          Literal
    | AppC          Func        [Core]
    | LetC          [LetCore]

data LetCore
    = LetCore       Name        Core

data Literal
    = NoLit
    | LitText       T.Text
    | LitList       [Literal]

data Func
    = FuncNoArg     Core
    | FuncArg       (Core -> Func)

-}

--------------------------------------------------------------------------------
-- * Positions
--

data Position a = Position
    { posPrev           :: [a]
    , posCur            :: a
    , posNext           :: [a]
    }
  deriving (Eq, Show)
