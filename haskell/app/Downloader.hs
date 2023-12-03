import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Network.HTTP.Simple
import Network.HTTP.Conduit (tlsManagerSettings, requestHeaders)
import Network.HTTP.Types.Header

import Data.ByteString.Internal (packChars)
import Data.ByteString.Lazy.Char8 (unpack)

import Data.List (isPrefixOf)

year = "2023"

main = do
    args <- getArgs
    let day = head args
        path = "inputs" </> day ++ ".txt"

    input <- getDayInput day
    writeFile path input
    print $ "Successfully written to '" ++ path ++ "'"

getDayInput :: String -> IO String
getDayInput day = do
    sessionFilePath <- (</> ".config/adventofcode.session") <$> getHomeDirectory
    session <- readFile sessionFilePath

    let sessionCookie = "session=" ++ session
        url = "https://adventofcode.com/" ++ year ++ "/day/" ++ day ++ "/input"
        initRequest = parseRequest_ url
        request = initRequest { requestHeaders = (hCookie, packChars sessionCookie) : requestHeaders initRequest }

    response <- httpLBS request

    let statusCode = getResponseStatusCode response
        body = unpack $ getResponseBody response

    if statusCode /= 200
        then error "An error occurred or the day is not unlocked yet"
        else if "Puzzle inputs differ by user" `isPrefixOf` body
            then error "Session cookie is not set correctly"
            else return body
