module Main where

-- for work with network
import Network.Browser
import Network.HTTP
import Network.HTTP.Base

-- for parsing
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Control.Monad ( liftM )

import qualified Data.ByteString.Lazy as BS
import Network.URI    ( parseURI, URI )
import Data.Maybe ( fromJust, fromMaybe )
import Data.List ( find, elemIndex )

-- for string convertion
import Codec.Text.IConv ( convert )
import Codec.Binary.UTF8.String ( decode, encode )
import Data.Char ( isSpace )

import System.Environment ( getArgs )

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f

toUtf8 :: BS.ByteString -> BS.ByteString
toUtf8 = convert "CP1251" "UTF-8"

toWin :: BS.ByteString -> BS.ByteString
toWin = convert "UTF-8" "CP1251"

fromByteString :: BS.ByteString -> String
fromByteString = decode . BS.unpack

getRequestBS :: String -> Request BS.ByteString
getRequestBS = (mkRequest GET) . fromJust . parseURI

data FormData = FormData { fdUrl :: URI, fdVars :: [(String, String)] }

makeFormRequest :: FormData -> Request BS.ByteString
makeFormRequest d = Request {
                    rqURI = fdUrl d,
                    rqMethod = POST,
                    rqHeaders = [ 
                             Header HdrContentType "application/x-www-form-urlencoded",
                             Header HdrContentLength $ show $ BS.length body
                    ],
                    rqBody = body
                } 
    where body = toWin $ BS.pack $ encode $ urlEncodeVars $ fdVars d

getMainPage :: Action BS.ByteString
getMainPage =  getPage $ getRequestBS "http://vk.com"

type Action a = BrowserAction (HandleStream BS.ByteString) a

getPage :: Request BS.ByteString -> Action BS.ByteString
getPage req =  do
      setOutHandler $ const $ return ()
      res <- request req
      return . toUtf8 . rspBody . snd $ res

getFormData :: String -> IO FormData
getFormData html = do
    let doc = parseHtml html
        form = doc //> loginForm
    vars <- runX $ form //> getFormFields
    act <- runX $ form >>> (getAttrValue "action")
    return $ FormData { fdUrl = fromJust $ parseURI $ head act, fdVars = vars }

loginForm = hasAttrValue "id" ( == "quick_login_form" ) 

hidden = hasAttrValue "type" ( == "hidden" )

getValue = getAttrValue "value"

getFormFields = hidden >>> (getAttrValue "name" &&& getValue)

data Audio = Audio { title :: String, url :: String, author :: String } deriving Show

removeEmpty :: IO [String] -> IO [String]
removeEmpty = liftM $ filter ( /= " " )

getAudioList :: String -> IO [Audio]
getAudioList html = do
    let doc = parseHtml html
        audio = doc //> css ".audio .area" 
    vals <- runX $ audio //> hidden ! "value"
    titles <- removeEmpty $ runX $ audio //> css ".title" //> getText
    authors <- removeEmpty $ runX $ audio //> css "b" //> getText
    return $ zipWith3 (\v t a -> Audio { 
        url = takeWhile ( /= ',')  v, 
        title = trim t, 
        author = a 
    }) vals titles authors

loginTo :: InputArgs -> Action ()
loginTo i = do
      mainHtml <- getMainPage
      formData <- ioAction $ getFormData $ fromByteString mainHtml
      let auth = formData { fdVars = fdVars formData ++ [("email", vkLogin i), ("pass", vkPass i)] }
      getPage $ makeFormRequest auth
      updateCoockies

updateCoockies :: Action ()
updateCoockies = do
    cs <- getCookies
    let newCs = (fromJust $ find ((== "remixsid") . ckName ) cs) { ckDomain = "vk.com" } : cs
    setCookies newCs

data InputArgs = InputArgs { vkLogin :: String, vkPass :: String, outDir :: String } deriving Show

parseInputArgs :: [String] -> InputArgs
parseInputArgs i = InputArgs { 
        vkLogin = getLogin i,
        vkPass = getPassword i,
        outDir = getOutDir i
    }
    where getLogin = option "-u"
          getPassword = option "-p"
          getOutDir = optionDefault "-o" "."
          option n i = fromJust $ getArg n i
          optionDefault n d i = fromMaybe d $ getArg n i 
          getArg n i = elemIndex n i >>= (return . (i !! ) . (+1))

getInputArgs :: IO InputArgs
getInputArgs = do
    args <- getArgs
    return . parseInputArgs $ args

getAudioInfo :: InputArgs -> IO [Audio]
getAudioInfo inp = browse $ do 
    loginTo inp
    html <- getPage $ getRequestBS "http://vk.com/audio"
    ioAction $ getAudioList $ fromByteString html

makeTrackName :: Audio -> String
makeTrackName mp3 = (author mp3) ++ " - " ++ (title mp3) ++ ".mp3"

getTrack :: String -> Audio -> IO ()
getTrack dir mp3 = browse $ do 
    setOutHandler $ const $ return ()
    res <- request $ getRequestBS $ url mp3 
    let body = rspBody . snd $ res
    ioAction $ flip BS.writeFile body $ dir ++ "/" ++ (makeTrackName mp3)

main :: IO ()
main = do
     inp <- getInputArgs 
     mp3s <- getAudioInfo inp
     mapM_ (getTrack $ outDir inp) $ mp3s
     putStrLn "done"