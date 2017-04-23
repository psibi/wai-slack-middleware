# wai-slack-middleware

A Slack middleware for [wai](https://www.stackage.org/package/wai)

## Usage:

Settings are controlled via the type `SlackConfig`:

``` haskell
import Network.HTTP.Types.Status
let slackConfig = SlackConfig {
    webHookUrl = "https://hooks.slack.com/services/xxx/xxxxxxxx",
    httpManager = appHttpManager foundation,
    logStatus = \status -> status400 == status
}
```

The above configuration will send slack notification to all 400 htpp status code.

### Integrating with yesod scaffolding templates

Go to `Application.hs` and change the function `makeApplication` to something like this:

``` haskell
makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  let slackConfig = SlackConfig {
                               webHookUrl = "https://hooks.slack.com/services/xxxx/xxxxxxx",
                               httpManager = appHttpManager foundation,
                               logStatus = \_ -> True
                             }
  -- Create the WAI application and apply middlewares
  appPlain <- toWaiAppPlain foundation
  return $ slack slackConfig $ logWare $ defaultMiddlewaresNoLogging appPlain
```


