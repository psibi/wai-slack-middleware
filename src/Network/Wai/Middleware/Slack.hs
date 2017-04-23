{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.Slack
  (
   -- * Usage
   -- $usage

   -- * Integration with Yesod Scaffolding templates
   -- $yesod
   
   SlackConfig(..)
  , slack
  ) where

import Network.Wai
       (Middleware(..), Application(..), requestHeaders, Request,
        responseStatus)
import Network.HTTP.Types.Status (Status(..))
import Network.HTTP.Client hiding (responseStatus)
import Data.Aeson (encode, object, (.=))

-- $usage
-- Settings are controlled via the type 'SlackConfig':
--
-- @
-- import Network.HTTP.Types.Status
-- let slackConfig = SlackConfig {
--     webHookUrl = "https://hooks.slack.com/services/xxx/xxxxxxxx",
--     httpManager = appHttpManager foundation,
--     logStatus = \status -> status400 == status
-- }
-- @
--
-- The above configuration will send slack notification to all 400 http status code.
--
-- $yesod
--
-- Go to Application.hs and change the 'makeApplication' function to something like this:
--
-- @
-- makeApplication :: App -> IO Application
-- makeApplication foundation = do
--   logWare <- makeLogWare foundation
--   let slackConfig = SlackConfig {
--                                webHookUrl = "https://hooks.slack.com/services/xxxx/xxxxxxx",
--                                httpManager = appHttpManager foundation,
--                                logStatus = \_ -> True
--                              }
--   -- Create the WAI application and apply middlewares
--   appPlain <- toWaiAppPlain foundation
--   return $ slack slackConfig $ logWare $ defaultMiddlewaresNoLogging appPlain
-- @
--

-- | Slack configuration for the middleware
data SlackConfig = SlackConfig
  { webHookUrl :: String -- ^ Slack webhook URL
  , httpManager :: Manager
  , logStatus :: Status -> Bool -- ^ Kind of HTTP Status for which you want slack notification.
  }

slackCall :: SlackConfig -> String -> IO ()
slackCall sconfig payload = do
  let url = webHookUrl sconfig
  req <- parseRequest url
  let reqObj = object ["text" .= payload]
  let req' =
        req
        { method = "POST"
        , requestBody = RequestBodyLBS $ encode reqObj
        }
  httpLbs req' (httpManager sconfig)
  return ()

-- | Slack middleware for Wai. Use the `logStatus` to control on which
-- status you want to log the request information in Slack.
slack :: SlackConfig -> Middleware
slack sconfig (app :: Application) env sendResponse =
  app env $
  \res ->
     if (logStatus sconfig $ responseStatus res)
       then slackCall sconfig (show env) *> sendResponse res
       else sendResponse res
