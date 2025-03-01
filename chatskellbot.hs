{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- Third step: interpreting commands -}

import Data.List                      -- base
import System.Exit                    --
import System.IO (Handle, hPutStrLn, hPutStr, hGetLine, IOMode(ReadWriteMode))
import System.IO.Unsafe (unsafePerformIO) --
import qualified Network.Socket as N  -- network
import System.Random (randomRIO)
import GHC.Integer (Integer)
import Control.Monad (replicateM)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.IORef
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Text.Printf (printf)

-- Configuration options
myServer = "192.168.0.151" :: String
myPort   = 6667 :: N.PortNumber
myChan   = "#tutbot-testing" :: String
myNick   = "tutbot" :: String
type GameState = Map.Map String [Int]

-- Global IORef to store active games
gamesRef :: IORef GameState
{-# NOINLINE gamesRef #-}
gamesRef = unsafePerformIO (newIORef Map.empty)

{-# NOINLINE botStartTime #-}
botStartTime :: IORef POSIXTime
botStartTime = unsafePerformIO (getPOSIXTime >>= newIORef)

-- Toplevel program
main :: IO ()
main = do
    h <- connectTo myServer myPort
    getUptime >>= privmsg h
    write h "NICK" myNick
    write h "USER" (myNick ++ " 0 * :tutorial bot")
    waitForWelcome h  -- Wait until 001 message is received
    write h "JOIN" myChan
    listen h

-- Connect to a server given its name and port number
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
    addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode

-- Send a message to a handle
write :: Handle -> String -> String -> IO ()
write h cmd args = do
    let msg = cmd ++ " " ++ args ++ "\r\n"
    hPutStr h msg          -- Send message on the wire
    putStr ("> " ++ msg)   -- Show sent message on the command line

-- Wait for the server's welcome message before joining a channel
waitForWelcome :: Handle -> IO ()
waitForWelcome h = do
    line <- hGetLine h
    putStrLn line
    if " 001 " `isInfixOf` line  -- Check if it's the welcome message
        then return ()
        else waitForWelcome h  -- Keep waiting until 001 is received

-- Process each line from the server
listen :: Handle -> IO ()
listen h = forever $ do
    line <- hGetLine h
    putStrLn line
    let s = init line
    if isPing s then pong s else eval h (clean s)
  where
    forever :: IO () -> IO ()
    forever a = do a; forever a

    clean :: String -> String
    clean = drop 1 . dropWhile (/= ':') . drop 1

    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x

    pong :: String -> IO ()
    pong x = write h "PONG" (':' : drop 6 x)

-- Dispatch a command
eval :: Handle -> String -> IO ()
eval h "!quit" = write h "QUIT" ":Exiting" >> exitSuccess
eval h x | "!id " `isPrefixOf` x = privmsg h (drop 4 x)
eval h x | "!bj" `isPrefixOf` x = startBlackjack h (drop 4 x)
eval h x | "!hit" `isPrefixOf` x = playerHit h (drop 5 x)
eval h x | "!stand" `isPrefixOf` x = playerStand h (drop 7 x)
eval h "!uptime" = getUptime >>= privmsg h
eval _ _ = return ()  -- ignore everything else

-- Send a privmsg to the channel
privmsg :: Handle -> String -> IO ()
privmsg h msg = write h "PRIVMSG" (myChan ++ " :" ++ msg)

getRandomNumber :: IO Int
getRandomNumber = randomRIO (1, 11)

-- Start a new game
startBlackjack :: Handle -> String -> IO ()
startBlackjack h user = do
    hand <- replicateM 2 getRandomNumber  -- Get two random cards
    modifyIORef gamesRef (Map.insert user hand)
    let total = sum hand
    privmsg h ("you drew: " ++ show hand ++ ". Total: " ++ show total)
    if total == 21
        then privmsg h (user ++ "Blackjack! You win!") >> endGame user
        else privmsg h (user ++ "!hit or !stand?")

-- Player chooses to hit
playerHit :: Handle -> String -> IO ()
playerHit h user = do
    gameState <- readIORef gamesRef
    case Map.lookup user gameState of
        Nothing -> privmsg h "No active game. Start with !bj"
        Just hand -> do
            newCard <- getRandomNumber
            let newHand = hand ++ [newCard]
                total = sum newHand
            modifyIORef gamesRef (Map.insert user newHand)
            privmsg h ( "you drew: " ++ show newCard ++ ". Total: " ++ show total)
            if total > 21
                then privmsg h "Bust! You lose!" >> endGame user
                else privmsg h "!hit or !stand?"

-- Player chooses to stand, dealer plays
playerStand :: Handle -> String -> IO ()
playerStand h user = do
    gameState <- readIORef gamesRef
    case Map.lookup user gameState of
        Nothing -> privmsg h "No active game. Start with !bj"
        Just playerHand -> do
            dealerHand <- playDealer []
            let playerTotal = sum playerHand
                dealerTotal = sum dealerHand
            privmsg h ("Dealer drew: " ++ show dealerHand ++ ". Total: " ++ show dealerTotal)
            if dealerTotal > 21 || playerTotal > dealerTotal
                then privmsg h "You win!"
                else if dealerTotal == playerTotal
                    then privmsg h "It's a tie!"
                    else privmsg h "Dealer wins!"
            endGame user

-- Dealer AI: Hits on any total below 17
playDealer :: [Int] -> IO [Int]
playDealer hand = do
    let total = sum hand
    if total >= 17
        then return hand
        else do
            newCard <- getRandomNumber
            playDealer (hand ++ [newCard])

-- End game, remove player from active games
endGame :: String -> IO ()
endGame user = modifyIORef gamesRef (Map.delete user)

getUptime :: IO String
getUptime = do
    start <- readIORef botStartTime
    now <- getPOSIXTime
    let diff = now - start
        seconds = round diff :: Int
        (hours, minutes, secondsLeft) = (seconds `div` 3600, (seconds `mod` 3600) `div` 60, seconds `mod` 60)
    return $ printf "Uptime: %02d:%02d:%02d (hh:mm:ss)" hours minutes secondsLeft
