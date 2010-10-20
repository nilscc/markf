{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M

import MarkF.Base
import MarkF.Data
import MarkF.RunSrc

import MarkF.Render.Html
import Text.Blaze.Renderer.Pretty

test :: Func
test = withCurrent . const $ Just (SrcText "Test!")

lookup' :: Lookup
lookup' = M.fromList
    [ ("test", test)
    ]

source :: Source
source =
    [ SrcHeadline    1  [ SrcText "Dies ist ein "
                        , Expressions [ AppE "test" [] ]
                        , SrcText ""
                        ]
    , SrcParagraph      [ SrcText "More "
                        , SrcLink (LnkUrl "http://test") [SrcText "tests"]
                        , SrcText "."
                        ]
    , Expressions       [ AppE "test" []
                        ]
    ]

document :: IO (Either String Document)
document = runSrc lookup' source

printDocument :: IO ()
printDocument = do
    Right doc <- document
    putStrLn . renderHtml $ renderDoc doc
