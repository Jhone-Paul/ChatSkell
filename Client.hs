import System.IO
import qualified Network.Socket as N
import qualified Data.ByteString as B

myServer = "localhost" :: String
myPort   = "6667" :: String  -- Use String to match server

main :: IO ()
main = do
    h <- connectTo myServer myPort
    t <- B.hGetContents h
    hSetBuffering stdout NoBuffering
    print t

connectTo :: N.HostName -> N.ServiceName -> IO Handle
connectTo host port = do
    let hints = N.defaultHints { N.addrFamily = N.AF_INET }  -- Force IPv4
    addr : _ <- N.getAddrInfo (Just hints) (Just host) (Just port)
    sock <- N.socket (N.addrFamily addr) N.Stream N.defaultProtocol
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode

