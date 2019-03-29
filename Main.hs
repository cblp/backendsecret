{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as Conduit
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           System.Directory (XdgDirectory (XdgConfig), getXdgDirectory)
import           System.FilePath ((</>))
import           Web.Twitter.Conduit (Credential (Credential),
                                      OAuth (oauthConsumerKey),
                                      TWInfo (twToken), def, homeTimeline,
                                      newManager, oauthConsumerSecret,
                                      sourceWithMaxId, tlsManagerSettings,
                                      twCredential, twOAuth, twitterOAuth)
import           Web.Twitter.Types.Lens (statusId, statusText, statusUser,
                                         userScreenName)

main :: IO ()
main = do
    oauthConsumerKey    <- readParam "consumer_key"
    oauthConsumerSecret <- readParam "consumer_secret"
    accessToken         <- readParam "access_token"
    accessTokenSecret   <- readParam "access_token_secret"
    let tokens = twitterOAuth{oauthConsumerKey, oauthConsumerSecret}
    let credential = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessTokenSecret)
            ]
    let twInfo = def{twToken = def{twOAuth = tokens, twCredential = credential}}
    mgr <- newManager tlsManagerSettings
    runConduit
        $   sourceWithMaxId twInfo mgr homeTimeline
        .|  Conduit.isolate 60
        .|  Conduit.mapM_ (\status ->
                liftIO $ Text.putStrLn $ mconcat
                    [ Text.pack . show $ status ^. statusId
                    , ": "
                    , status ^. statusUser . userScreenName
                    , ": "
                    , status ^. statusText
                    ])

readParam :: FilePath -> IO ByteString
readParam name = do
    configDir <- getXdgDirectory XdgConfig "backendsecret"
    fmap (Text.encodeUtf8 . Text.strip) . Text.readFile $ configDir </> name
