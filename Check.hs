module Check (checkFile, formatResult) where

import           Data.Monoid
import           Data.Text.Lazy              (Text)
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int
import           GHC.Exts                    (groupWith)
import           ShellCheck.Checker
import           ShellCheck.Formatter.Format
import           ShellCheck.Interface

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

cuteIndent :: PositionedComment -> Builder
cuteIndent comment = fromString $
  replicate (fromIntegral $ colNo comment - 1) ' ' ++
      "^-- SC" ++ show (codeNo comment) ++ ": " ++ messageText comment

