module MarkF.Data where

import qualified Data.Text  as T
import qualified Text.Blaze as B

data MarkF

    -- "primitives"
    = Text                  T.Text
    | Html                  B.Html

    -- mark ups
    | Headline      Int     MarkF       -- Int: Level (1 = highest, 6 = lowest)
    | Markup        MrkUp   MarkF
    | List          LstNum  [MarkF]
    | Quote                 MarkF
    | Code          Hlight  T.Text
    | Line                              -- -> <hr /> tag in HTML

    -- span elements
    | Link          Lnk     MarkF
    | LinkName      T.Text  Url

data MrkUp
    = MrkUpStrong
    | MrkUpEmph

data LstNum
    = LstStar
    | LstSorted
    | LstChar       Char

type Hlight = Maybe T.Text

type Url    = T.Text

data Lnk
    = LnkUrl        Url
    | LnkImg        Url     (Maybe T.Text)    -- alt text
    | LnkName       T.Text
