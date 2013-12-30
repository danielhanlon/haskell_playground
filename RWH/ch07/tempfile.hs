import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception(finall,catch)

main :: IO ()
main = withTempFile "mytemp.txt" myAction

myAction tempname temph =
  do
    putStrLn "Welcome to tempfile.hs"
    putStrLn $ "I have a temporary file at " ++ tempname
    pos <- hTell temph
    putStrLn $ "My initial position is " ++ show pos

    let tempdata = show [1..10]
    putStrLn $ "Writing one line containing " ++ show (length tempdata) 
               ++ " bytes: " ++ tempdata
    hPutStrLn temph tempdata

    pos <- hTell temph
    putStrLn $ "After writing my new position is " ++ show pos

    putStrLn $ "The file content is: "
    hSeek temph AbsoluteSeek 0

    c <- hGetContents temph
    putStrLn c

    putStrLn $ "Which could be expressed as this Haskell literal:"
    print c

withTempFile pattern func =
  do
    tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
    (tempfile, temph) <- openTempFile tempdir pattern
    finally (func tempfile temph)
            (do hClose temph
                removeFile tempfile)
