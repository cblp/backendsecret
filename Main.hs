{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as Conduit
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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

twInfo :: TWInfo
twInfo = def{twToken = def{twOAuth = tokens, twCredential = credential}}

-- To generate your access token and secret, you must have a Twitter app.
-- https://developer.twitter.com/en/apps
tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = "YOUR CONSUMER KEY"
    , oauthConsumerSecret = "YOUR CONSUMER SECRET"
    }

credential :: Credential
credential = Credential
    [ ("oauth_token", "YOUR ACCESS TOKEN")
    , ("oauth_token_secret", "YOUR ACCESS TOKEN SECRET")
    ]
