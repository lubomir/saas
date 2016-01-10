{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Except
import qualified Data.ByteString.Lazy.UTF8   as L (toString)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid
import qualified Data.String                 as Str
import           Data.Text.Lazy              (Text)
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder      as TLB
import           Data.Text.Lazy.Builder.Int
import           GHC.Exts                    (groupWith)
import           Network.HTTP.Types          (badRequest400)
import           Network.Wai.Handler.Warp
import           ShellCheck.Checker
import           ShellCheck.Formatter.Format hiding (header)
import           ShellCheck.Interface
import           System.Environment
import qualified Web.Scotty                  as S

import           Download
import qualified Files

checkFile :: String -> String -> Either Text CheckResult
checkFile url contents = do
    let checkspec = emptyCheckSpec { csFilename = url
                                   , csScript = contents
                                   }
    checkScript dummyInterface checkspec
  where
    dummyInterface = SystemInterface { siReadFile = die }
    die = return $ Left "Error: script sources another script\n"

formatResult :: CheckResult -> String -> Text
formatResult CheckResult{crComments=[]} _ =
    "Looks fine here. But it can still do something nasty...\n"
formatResult result contents = toLazyText $
    mconcat $ map (\x -> do
        let lineNum = lineNo (head x)
        let line = if lineNum < 1 || lineNum > lineCount
                    then mempty
                    else fileLines !! fromIntegral (lineNum - 1)
        let filename = if null (crFilename result)
                        then mempty
                        else fromString (crFilename result) <> ": "
        filename <> "line " <> decimal lineNum <> ":\n"
            <> fromString line <> singleton '\n'
            <> mconcat (map (\c -> cuteIndent c <> "\n") x) <> "\n"
       ) groups
  where
    comments = crComments result
    fileLines = lines contents
    lineCount = fromIntegral $ length fileLines
    groups = groupWith lineNo comments

cuteIndent :: PositionedComment -> TLB.Builder
cuteIndent comment = fromString $
  replicate (fromIntegral $ colNo comment - 1) ' ' ++
      "^-- SC" ++ show (codeNo comment) ++ ": " ++ messageText comment

runCheck :: String -> String -> S.ActionM ()
runCheck url contents =
  case checkFile url contents of
    Left err  -> S.status badRequest400 >> S.text err
    Right res -> S.text (formatResult res contents)

main :: IO ()
main = do
  host <- fromMaybe "*4" <$> lookupEnv "OPENSHIFT_DIY_IP"
  putStrLn $ "Listening on " ++ host
  let opts = setPort 8080 $ setHost (Str.fromString host) defaultSettings
  S.scottyOpts (S.Options 1 opts) $ do
      S.get "/" $ do
        S.setHeader "Content-Type" "text/html"
        S.raw Files.index
      S.post "/" $ do
        contents <- S.body
        runCheck "" (L.toString contents)
      S.get (S.regex "^/(https?://.+)$") $ do
        url <- S.param "1"
        mcontents <- liftIO $ getFile url
        case mcontents of
          Left err -> S.status badRequest400 >> S.text err
          Right contents -> runCheck url contents
