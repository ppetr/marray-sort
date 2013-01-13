module Main
where

import System.Environment(getArgs)
import System.Exit

import Data.List(sort)
import Data.ByteString.UTF8(ByteString, fromString, toString)
import qualified Data.ByteString.Char8 as C8

import Control.DeepSeq(NFData, deepseq)
import Control.Exception as CE

import System.CPUTime
import qualified System.IO as SysIO


import Control.Monad
--import qualified Data.Array.MArray.Heapsort as Heapsort
import Data.Array.IO

import Data.Array.MArray.Quicksort

import Debug.Trace


instance NFData ByteString

instance NFData (IOArray i e)


-- List of utf-8 byte strings.
type StringList = [ByteString]

-- Conversion between string lists and bytestring lists.
fromStrings :: [String] -> StringList
fromStrings = map Data.ByteString.UTF8.fromString

toStrings :: StringList -> [String]
toStrings = map Data.ByteString.UTF8.toString

-- Procedures for file IO.
readLines :: String -> IO (Maybe StringList)
readLines filename = do
                        r <- try (SysIO.readFile filename) :: IO (Either IOException String)
                        case r of
                          Left ex -> return Nothing
                          Right s -> return $ Just (fromStrings (lines s))

writeLines :: String -> StringList -> IO Int
writeLines filename ss = do
                            let strs = toStrings ss
                            let s    = unlines strs
                            r <- try (SysIO.writeFile filename s) :: IO (Either IOException ())
                            case r of
                              Left ex -> return (-1)
                              Right _ -> return (length strs)

writeLinesIO :: String -> IOArray Int ByteString -> IO Int
writeLinesIO filename a = SysIO.withFile filename SysIO.WriteMode $ \h -> do
                            (mn, mx) <- getBounds a
                            mapM_ (\i -> readArray a i >>= C8.hPutStrLn h) [mn..mx]
                            return $ mx - mn + 1

-- Timer type.
data Timer = Timer Integer Integer

pad :: String -> Int -> String
pad s n = if (n <= (length s))
            then s
            else pad ('0':s) n

create :: Timer
create = Timer 0 0

cpuTimeToMilliseconds :: Integer -> Integer
cpuTimeToMilliseconds x = div x 1000000000

-- Start timer (set first field to current CPU time).
start :: Timer -> IO Timer
start (Timer x y) = do
                      z <- getCPUTime
                      return $ Timer (cpuTimeToMilliseconds z) y


-- Stop timer (set second field to current CPU time).
stop :: Timer -> IO Timer
stop (Timer x y) = do
                     z <- getCPUTime
                     return $ Timer x (cpuTimeToMilliseconds z)


elapsedMilliseconds :: Timer -> Integer
elapsedMilliseconds (Timer x y) = y - x


formatMilliseconds :: Integer -> String
formatMilliseconds tms = if (tms < 0)
                          then
                            ""
                          else
                            let s  = tms `div` 1000
                                m  = tms `mod` 1000
                                ss = show s
                                ms = pad (show m) 3
                             in ss ++ "." ++ ms ++ " s"

format :: Timer -> String
format (Timer x y) = formatMilliseconds (y - x)


printUsage :: IO ()
printUsage = do
                putStrLn "Usage: sort <filename>"

processFile :: String -> IO ()
processFile filename = do
                          mInList <- readLines filename
                          let filenameBase = filename

                          case mInList of
                            Just inList ->
                              do
                                 let l = length inList
                                 putStrLn $ "File    " ++ filename
                                 putStrLn $ "Read    " ++ (show l) ++ " lines."

                                 putStrLn $ "Method  " ++ "qsort"
                                 putStrLn $ "Sorting"

                                 inArr <- newListArray (1, l) inList :: IO (IOArray Int ByteString)
                                 t0 <- deepseq inArr (start create)

                                 --Heapsort.sort inArr
                                 let maxdepth = 3 * ceiling (log (fromIntegral l))
                                 putStrLn $ "Max. depth: " ++ show maxdepth
                                 --qsort maxdepth Heapsort.sort l inArr
                                 --qsort maxdepth (\_ _ _ -> return ()) inArr
                                 qsort inArr

                                 -- Use deepseq to make sure the output list has been
                                 -- completely evaluated before stopping the timer.
                                 --t1 <- (deepseq outArr (stop t0))
                                 -- No need for deepseq, it's the same object.
                                 t1 <- stop t0
                                 b <- getBounds inArr
                                 putStrLn $ "Sorted  " ++ (show b) ++ " lines."
                                 putStrLn $ "Sorting took:           " ++ (format t1)

                                 let suffix   = "out"
                                 let filename = filenameBase ++ suffix

                                 putStrLn $ "Writing file " ++ filename
                                 len <- writeLinesIO filename inArr
                                 if (len /= -1)
                                   then putStrLn $ "Written " ++ (show len) ++ " lines."
                                   else putStrLn "Error: Cannot write file."

                            -- One of the two is Nothing.
                            _ ->
                              do
                                 putStrLn "Wrong file name specified."
                                 exitWith $ ExitFailure 1



processFile' :: String -> IO ()
processFile' filename = do
                          mInList <- readLines filename
                          let filenameBase = filename

                          case mInList of
                            Just inList ->
                              do
                                 let l = length inList
                                 putStrLn $ "File    " ++ filename
                                 putStrLn $ "Read    " ++ (show l) ++ " lines."

                                 putStrLn $ "Method  " ++ "Data.List.sort"
                                 putStrLn $ "Sorting"

                                 t0 <- deepseq inList (start create)

                                 let outList = sort inList

                                 -- Use deepseq to make sure the output list has been
                                 -- completely evaluated before stopping the timer.
                                 t1 <- (deepseq outList (stop t0))
                                 putStrLn $ "Sorted  " ++ show (length outList) ++ " lines."
                                 putStrLn $ "Sorting took:           " ++ (format t1)

                                 let suffix   = "out"
                                 let filename = filenameBase ++ suffix

                                 putStrLn $ "Writing file " ++ filename
                                 len <- writeLines filename outList
                                 if (len /= -1)
                                   then putStrLn $ "Written " ++ (show len) ++ " lines."
                                   else putStrLn "Error: Cannot write file."

                            -- One of the two is Nothing.
                            _ ->
                              do
                                 putStrLn "Wrong file name specified."
                                 exitWith $ ExitFailure 1


run :: [String] -> IO ()
run args = do
              putStrLn ""

              case (length args) of
                -- One argument: process.
                1 -> processFile (head args) 

                _ -> printUsage


main :: IO ()
main = do
          args <- getArgs
          run args

