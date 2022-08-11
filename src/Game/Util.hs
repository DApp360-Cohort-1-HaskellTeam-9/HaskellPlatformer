module Game.Util where

-- import Control.Concurrent
-- import Control.Concurrent.STM
-- import Control.Exception
-- import Control.Monad

-- type Work        = IO ()
-- type SendWork    = Work -> STM ()
-- type ThreadCount = Int

-- newThreads :: ThreadCount -> IO (SendWork, IO ())
-- newThreads t
--     | t  <=  0  = error "Invalid number of thread"
--     | otherwise = do
--         workChan <- atomically newTChan
--         runCount <- atomically . newTVar $ t
--         let stop  = atomically $ do
--                 running <- readTVar runCount
--                 writeTVar runCount . pred $ running
--             die e = do
--                 id <- myThreadId
--                 print $ "Thread " ++ show id ++ " died with exception " ++ show (e :: ErrorCall)
--                 stop
--             work  = do
--                 mJob <- atomically (readTChan workChan)
--                 case mJob of
--                     Just jb -> do
--                         catch jb die
--                         work
--                     Nothing -> stop
--         replicateM_ t (forkIO work)
--         let stopCommand = do
--                 atomically . replicateM_ t . writeTChan workChan $ Nothing
--                 atomically $ do
--                     running <- readTVar runCount
--                     when (running > 0) retry
--         return (writeTChan workChan . Just, stopCommand)
    
