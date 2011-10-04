{-# LANGUAGE OverloadedStrings #-}

{- | Echo Controller just does some echoing of values.

-}

module Controllers.Echo where

import           Control.Applicative

import           Data.Maybe (fromMaybe)

import           Snap.Types
import           Snap.Extension.Heist
import           Text.Templating.Heist


import           Data.Text.Encoding as TE


import           Application
import           Views.Echo


{-|

The problem with your first "<script><json/></script>" approach is
that the parsing library has been designed to interpret everything
inside the <script> tag as plain text, so you can't have splices
there.  If we didn't do this, the parser would interpret the less-than
operator in javascript as the start of a new tag and you would get
parse errors.  Your approach is correct.  Your splices have to add the
script tags for you.  Greg's suggestion should work.  I have a splice
that I use for this.  Since it's a single tag I didn't bother with
using blaze.

scriptSplice :: Text
             -> (Text -> Text)
             -> Splice Application
scriptSplice scriptType transform = do
    node <- getParamNode
    return $ [Element "script" [("type", scriptType)] $
                [TextNode $ transform $ catTextNodes $ childNodes node]]
-}


------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" (TE.decodeUtf8 message)) $ render "echo" 
  where
    decodedParam p = fromMaybe "" <$> getParam p
