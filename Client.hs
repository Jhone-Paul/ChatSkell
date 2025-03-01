import System.IO
import qualified Network.Socket as N
import qualified Data.ByteString as B

myServer = "localhost" :: String
myPort   = "6667" :: String  -- Use String to match server
myChan   = "#tutbot-testing" :: String
myNick   = "tutbot" :: String

main :: IO ()
main = do
    h <- connectTo myServer myPort
    write h "NICK" myNick
    write h "USER" (myNick ++ " 0 * :tutorial bot")
    write h "JOIN" myChan
    listen h

connectTo :: N.HostName -> N.ServiceName -> IO Handle
connectTo host port = do
    let hints = N.defaultHints { N.addrFamily = N.AF_INET }  -- Force IPv4
    addr : _ <- N.getAddrInfo (Just hints) (Just host) (Just port)
    sock <- N.socket (N.addrFamily addr) N.Stream N.defaultProtocol
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode

-- Send a message to a handle
write :: Handle -> String -> String -> IO ()
write h cmd args = do
    let msg = cmd ++ " " ++ args ++ "\r\n"
    hPutStr h msg          -- Send message on the wire
    putStr ("> " ++ msg)   -- Show sent message on the command line

-- Process each line from the server
listen :: Handle -> IO ()
listen h = forever $ do
    line <- hGetLine h
    putStrLn line
  where
    forever :: IO () -> IO ()
    forever a = do a; forever a

