-- | This module sketches tests
--   intended for adding streaming and XPath function to `xeno`.
module XPathSpec where

-- | Extract DOM for each <b/> element separately. Ignore the others.

xmlAB = "<document><b></b><a/><b></b><c/></document>"

-- | Template for `Process` override
defaultProcess :: Process
defaultProcess  = undefined -- process that ignores all elements

-- | Implement DOM parsing as Process
domProcess :: Process
domProcess  = undefined

processB = defaultProcess {
             startElement eltName | eltName == "b" =
               writeIORef domResult $ Just domResult
               continueWith domProcess
           , endElement eltName = do
               yield domResult
               writeIORef domResult Nothing
           }

-- TODO:
-- -- continueWith
-- yield
--


