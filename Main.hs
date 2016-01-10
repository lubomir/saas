{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Except
import           Data.ByteString.Lazy.UTF8 (toString)
import           Data.Maybe                (fromMaybe)
import           Data.String               (fromString)
import           Network.HTTP.Types        (badRequest400)
import           Network.Wai.Handler.Warp
import           System.Environment
import qualified Web.Scotty                as S

import           Check
import           Download
import qualified Files

runCheck :: String -> String -> S.ActionM ()
runCheck url contents =
  case checkFile url contents of
    Left err  -> S.status badRequest400 >> S.text err
    Right res -> S.text (formatResult res contents)

main :: IO ()
main = do
  host <- fromMaybe "*4" <$> lookupEnv "OPENSHIFT_DIY_IP"
  putStrLn $ "Listening on " ++ host
  let opts = setPort 8080 $ setHost (fromString host) defaultSettings
  S.scottyOpts (S.Options 1 opts) $ do
      S.get "/" $ do
        S.setHeader "Content-Type" "text/html"
        S.raw Files.index
      S.post "/" $ do
        contents <- S.body
        runCheck "" (toString contents)
      S.get (S.regex "^/(https?://.+)$") $ do
        url <- S.param "1"
        mcontents <- liftIO $ getFile url
        case mcontents of
          Left err -> S.status badRequest400 >> S.text err
          Right contents -> runCheck url contents
