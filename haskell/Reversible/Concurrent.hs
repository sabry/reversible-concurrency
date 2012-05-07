module Reversible.Concurrent (
  runPar 
  ) where

  import CHD.Control.Concurrent  
  
  wrapProc :: Show r => IO [r] -> MVar () -> MVar [r] -> IO ()
  wrapProc p block mvar = do
    xs <- p
  --  putStrLn ("Returning: " ++ (show xs))
    modifyMVar_ mvar (\ls -> return $ xs ++ ls)
    putMVar block ()

  wrapProcPrint :: Show r => IO r -> IO ()
  wrapProcPrint p = do
    r <- p
    putStrLn $ show r

  wrapProcRes :: IO r -> MVar r -> IO ()
  wrapProcRes p res = do
    r <- p
    putMVar res r

  forkChild :: MVar [r] -> MVar [MVar ()] -> 
              (MVar () -> MVar [r] -> IO ()) -> IO ThreadId
  forkChild results children io = do
    block <- newEmptyMVar 
    modifyMVar_ children (\xs -> return $ block:xs)
    forkIO $ io block results

  waitOnChildren :: MVar [MVar ()] -> IO ()
  waitOnChildren children = do
    childs <- takeMVar children
    case childs of
      [] -> return ()
      (x:xs) -> do
      putMVar children xs
      takeMVar x
      waitOnChildren children

  runPar :: Show r => IO r -> IO r -> IO r
  runPar k1 k2 = do
    --children <- newMVar []
    --results <- newMVar []
    result <- newEmptyMVar
    -- We might cause some unintentional ordering by spawning one of these
    -- first, as it gets a head start. But.. I think that's unavoidable.
    --forkChild results children $ wrapProc k1
    --forkChild results children $ wrapProc k2
    forkIO $ wrapProcPrint k1
    forkIO $ wrapProcRes k2 result
    -- Block until all children have returned
    --waitOnChildren children
    --takeMVar results
    takeMVar result
