{-# LANGUAGE TemplateHaskell #-}
module Files where

import           Data.FileEmbed
import           Data.String

index :: IsString a => a
index = $(embedStringFile "index.html")
