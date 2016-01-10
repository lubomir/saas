module Download where

import qualified Control.Exception           as E
import           Control.Lens
import qualified Data.ByteString             as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.UTF8   as L
import qualified Data.ByteString.UTF8        as B
import           Data.Monoid
import           Data.Text.Lazy              (Text, pack)
import           Network.HTTP.Client         (HttpException (..))
import           Network.Wreq

data EnoughException = Enough Builder

instance Show EnoughException where
    show _ = "EnoughException"

instance E.Exception EnoughException

maxLen :: Integral a => a
maxLen = 10 * 1024

describeError :: HttpException -> Text
describeError (StatusCodeException s _ _) = pack (B.toString (s ^. statusMessage))
describeError (InvalidUrlException url err) = pack (url <> ": " <> err)
describeError (TooManyRedirects _) = "Too many redirects"
describeError (UnparseableRedirect _) = "Invalid redirect"
describeError ResponseTimeout = "Time out"
describeError (FailedConnectionException2 host _ _ _) = "Failed to connect to " <> pack host
describeError _ = "HTTP error when downloading"

getFile :: String -> IO (Either Text String)
getFile url = ((extract . fst) <$> foldGet process (mempty, 0) url) `E.catch` enough `E.catch` handler
  where
    extract :: Builder -> Either a String
    extract = Right . take maxLen . L.toString . toLazyByteString

    handler e = return $ Left $ mconcat [ "Failed to download script: "
                                        , describeError e
                                        , "\n"
                                        ]

    process :: (Builder, Int) -> BS.ByteString -> IO (Builder, Int)
    process (acc, cur) now = do
        let acc' = acc <> byteString now
        let cur' = cur + BS.length now
        if cur' > maxLen
            then E.throw (Enough acc')
            else return (acc', cur')

    enough (Enough d) = return (extract d)

