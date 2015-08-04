module Utils where


import           System.Directory
import           System.FilePath

import           Debug.Trace


walk :: FilePath -> IO [FilePath]
walk dirname = do
    (dirs, files) <-  partitionM doesDirectoryExist
                  .   map (dirname </>)
                  .   filter (not . hidden)
                  =<< getDirectoryContents dirname
    concat . (files :) <$> mapM walk dirs
    where
        hidden []      = True
        hidden ('.':_) = True
        hidden _       = False

partitionM :: Monad m => (x -> m Bool) -> [x] -> m ([x], [x])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
    result   <- f x
    (ts, fs) <- partitionM f xs
    return $ if result
                 then (x:ts, fs)
                 else (ts, x:fs)

info :: Show a => Bool -> String -> a -> a
info False _   x = x
info True  msg x = trace (msg ++ ": " ++ show x) x
