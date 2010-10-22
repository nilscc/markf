{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M

import MarkF.Base
import MarkF.Data
import MarkF.RunSrc

import MarkF.Render.Html
import Text.Blaze.Renderer.Pretty

-- Some test functions:

-- | Print a "test!" to the current node
test :: LocatedFunc
test = onCurrent $ text "test!"

-- | Print a "Next!" on the next node
testNext :: LocatedFunc
testNext = onNext (const True) $ text "Next!"

-- | Print a "Previous!" on the previouse node
testPrev :: LocatedFunc
testPrev = onPrev (const True) $ text "Previous!"


-- | The lookup table for our functions
lookup' :: Lookup
lookup' = M.fromList
    [ ( "test"       , test     )
    , ( "testNext"   , testNext )
    , ( "testPrev"   , testPrev )
    ]


-- | The source code representation. The original code might have looked like
-- this:
--
-- > This is a $test :)
-- > ===
-- >
-- > $testNext
-- > <next>
-- > <prev>
-- > $testPrev
source :: Source
source =
    [ SrcHeadline    1  [ SrcText "This is a "
                        , Expressions [ AppE "test" [] ]    -- print "Test!" here
                        , SrcText " :)"
                        ]

    , Expressions       [ AppE "testNext" [] ]
    , SrcText "<next>"                                      -- replace this text with the function above
    , SrcText "<prev>"                                      -- replace this text with the function below
    , Expressions       [ AppE "testPrev" [] ]
    ]


-- | The source gets executed in the IO monad and might fail:
document :: IO (Either String Document)
document = runSrc lookup' source


-- | Render this document in HTML will (hopefully) result in:
--
-- > *Main> printDocument
-- > <h1>
-- >     This is a
-- >     test!
-- >      :)
-- > </h1>
-- > Next!
-- > Previous!
printDocument :: IO ()
printDocument = do
    doc <- document
    case doc of
         Right d -> putStrLn . renderHtml $ renderDoc d
         Left er -> putStrLn $ "Error: " ++ er
