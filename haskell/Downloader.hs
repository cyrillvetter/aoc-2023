{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Simple
import Network.HTTP.Conduit (tlsManagerSettings)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

main = do
    sessionFilePath <- (</> ".config/adventofcode.session") <$> getHomeDirectory
    session <- readFile sessionFilePath
    print session

    let url = "https://adventofcode.com/2022/day/2/input"
        requestHeaders = [("Cookie", "session=")]
        request = setRequestHeaders requestHeaders $ parseRequest_ url

    request <- parseRequest "https://adventofcode.com/2022/day/2/input"

    response <- httpLBS request
    print $ getResponseBody response
    -- request <- parseRequest "http://adventofcode.com/2022/day/2/input"
    -- print session

    -- request <- parseRequest ""
    -- print "Downloader"
