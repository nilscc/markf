module MarkF.Render.Html
    ( renderDoc
    ) where

import Text.Blaze ((!))
import qualified Text.Blaze                     as B
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

import MarkF.Data

renderDoc :: Document -> B.Html
renderDoc = mapM_ renderEl

renderEl :: MarkUp -> B.Html

renderEl (Text t) = H.text t

renderEl (Html h) = h

renderEl (Headline 1        mu) = H.h1 $ renderDoc mu
renderEl (Headline 2        mu) = H.h2 $ renderDoc mu
renderEl (Headline 3        mu) = H.h3 $ renderDoc mu
renderEl (Headline 4        mu) = H.h4 $ renderDoc mu
renderEl (Headline 5        mu) = H.h5 $ renderDoc mu
renderEl (Headline _        mu) = H.h6 $ renderDoc mu

renderEl (Paragraph         mu) = H.p $ renderDoc mu

renderEl (Style StyleStrong mu) = H.strong $ renderDoc mu
renderEl (Style StyleEmph   mu) = H.em $ renderDoc mu

renderEl Line = H.hr

renderEl (List LstUnsorted  mu) = H.ul $ mapM_ (H.li . renderEl) mu
renderEl (List LstSorted    mu) = H.ol $ mapM_ (H.li . renderEl) mu

renderEl (Quote             mu) = H.blockquote $ renderDoc mu
renderEl (Code _            mu) = H.code $ H.text mu

renderEl (Link (LnkUrl u)   mu) = H.a ! A.href (B.textValue u) $ renderDoc mu
renderEl (Image (LnkUrl u) alt) = H.img ! A.alt (B.textValue alt)
                                        ! A.src (B.textValue u)
