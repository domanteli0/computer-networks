module Args(
    RawArgs
  , followRedirectsRaw
  , host
  , port
  , outputRaw
  , getAppArgs
) where

import qualified System.Console.GetOpt as GO
import qualified Data.Maybe as M
import qualified Control.Monad.Trans.Except as ET
import qualified System.Environment as Env
import qualified Control.Error.Util as ErrUtil
import Control.Monad.Trans.Class (lift)

data RawArgs = RawArgs {
    followRedirectsRaw :: Bool
  , host :: String
  , port :: Maybe String
  , outputRaw :: Maybe String
} deriving (Show)

defaultRawArgs :: RawArgs
defaultRawArgs = RawArgs {
    followRedirectsRaw = False
  , host = ""
  , port = Nothing
  , outputRaw = Nothing
}

usageInfo :: String
usageInfo = "USAGE: stack run -- -h <URL> [-p PORT] [-L] [-o FILE]"

appArgs :: [GO.OptDescr (RawArgs -> RawArgs)]
appArgs = [
    GO.Option ['h'] ["host"] (GO.ReqArg (\h args -> args { host = h } ) "HOST" ) "host to connect to"
  , GO.Option ['p'] ["port"] (GO.OptArg (\p args -> args { port = p}) "PORT" ) "port to connect to"
  , GO.Option ['L'] []       (GO.NoArg (\args -> args { followRedirectsRaw = True }) ) "Tells to follow redirects"
  , GO.Option ['o'] ["output"] (GO.OptArg (\f args -> args { outputRaw = f}) "FILE") "Saves response body to FILE, otherwise it is printed to stdout"
  ]

getAppArgs :: ET.ExceptT String IO RawArgs
getAppArgs = do
  args <- lift Env.getArgs
  ErrUtil.hoistEither $ parseArgs args

parseArgs :: [String] -> Either String RawArgs
parseArgs argv =
  case GO.getOpt GO.Permute appArgs argv of
    ([], [], []) -> Left usageInfo
    (o,_,[]) -> Right $ foldl (flip id) defaultRawArgs o
    (_,_,errs) -> Left $ concat errs ++ GO.usageInfo usageInfo appArgs
