import Network.Socket
import System.IO

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "6667"
    sock <- open addr
    putStrLn "Server is running on port 6667..."
    acceptLoop sock

resolve :: String -> IO AddrInfo
resolve port = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE] }
    addr : _ <- getAddrInfo (Just hints) Nothing (Just port)
    return addr

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1  -- Allows quick restart of the server
    bind sock (addrAddress addr)       -- Bind to the address
    listen sock 10                     -- Start listening (with a queue of 10)
    return sock


acceptLoop :: Socket -> IO ()
acceptLoop sock = do
    (conn, _) <- accept sock
    handleConnection conn
    acceptLoop sock

handleConnection :: Socket -> IO ()
handleConnection conn = do
    h <- socketToHandle conn ReadWriteMode
    hPutStrLn h "Welcome to your local IRC server!"
    hClose h
