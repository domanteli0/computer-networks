module Utils(splitURL) where

import Data.Bifunctor

-- removeProt :: Sting -> String
-- removeProt = 

splitURL :: String -> (String, String)
splitURL = second ensureSlash . span (/= '/')

ensureSlash :: String -> String
ensureSlash ('/':str) = '/':str
ensureSlash str = '/':str
