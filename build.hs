#!/usr/bin/env stack
-- stack --resolver=resolver.yaml script

{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Conduit
import qualified Data.Conduit.List      as CL
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Web.Twitter.Conduit
import           Web.Twitter.Types.Lens

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    runConduit
        $   sourceWithMaxId twInfo mgr homeTimeline
        .|  CL.isolate 60
        .|  CL.mapM_ (\status ->
                liftIO $ T.putStrLn $ T.concat
                    [ T.pack . show $ status ^. statusId
                    , ": "
                    , status ^. statusUser . userScreenName
                    , ": "
                    , status ^. statusText
                    ])

twInfo :: TWInfo
twInfo = def
    { twToken = def{twOAuth = tokens, twCredential = credential}
    , twProxy = Nothing
    }

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
