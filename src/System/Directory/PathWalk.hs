-- | Provides path traversal functions much like Python's os.walk.

module System.Directory.PathWalk
    ( Callback
    , PureCallback
    , ForeverCallback
    , PureForeverCallback
    , SimpleCallback
    , SimpleForeverCallback
    , pathWalk
    , pathWalk_
    , pureCallback
    , foreverCallback
    , pureForeverCallback
    , simpleCallback
    , simpleForeverCallback
    , WalkStatus(..)
    ) where

import Control.Monad (filterM, foldM)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

type GenericCallback a = FilePath -> [FilePath] -> [FilePath] -> a
-- | Called with a directory, list of relative subdirectories, a list of
-- file names, and an accumulator.  If using 'pathWalk', the callback
-- always returns '()'.  If using 'pathWalkInterruptible', it returns
-- whether to continue, prevent recursing further, or stop traversal
-- entirely.
type Callback a = GenericCallback (a -> IO (WalkStatus, a))
type PureCallback a = GenericCallback (a -> (WalkStatus, a))
type ForeverCallback a = GenericCallback (a -> IO a)
type PureForeverCallback a = GenericCallback (a -> a)
type SimpleCallback = GenericCallback (IO WalkStatus)
type SimpleForeverCallback = GenericCallback (IO ())

-- | 'pathWalk' recursively enumerates the given root directory, calling
-- callback once per directory with the traversed directory name, a list
-- of subdirectories, a list of files, and an accumulator.
--
-- The subdirectories and file names are always relative to the root
-- given.
--
-- @
-- pathWalk "src" $ \\dir subdirs files -> do
--   forM_ files $ \\file -> do
--     when ("Test.hs" \`isSuffixOf\` file) $ do
--       registerTestFile $ dir \</\> file
-- @
pathWalk :: FilePath -> Callback a -> a -> IO a
pathWalk = pathWalkInternal

-- | Like 'pathWalk' but does not accumulate.
--
-- @
-- pathWalk_ "src" (simpleForeverCallback (\\dir subdirs files -> print files))
-- @
pathWalk_ :: FilePath -> Callback () -> IO ()
pathWalk_ dir c = pathWalk dir c ()

pureCallback :: PureCallback a -> Callback a
pureCallback c = \dir dirs files initial -> return (c dir dirs files initial)

foreverCallback :: ForeverCallback a -> Callback a
foreverCallback c = \dir dirs files initial -> c dir dirs files initial >>= \r -> return (Continue, r)

pureForeverCallback :: PureForeverCallback a -> Callback a
pureForeverCallback c = \dir dirs files initial -> return (Continue, c dir dirs files initial)

simpleCallback :: SimpleCallback -> Callback ()
simpleCallback c = \dir dirs files _ -> c dir dirs files >>= \s -> return (s, ())

simpleForeverCallback :: SimpleForeverCallback -> Callback ()
simpleForeverCallback c = \dir dirs files _ -> c dir dirs files >> return (Continue, ())

{--- | The callback given to 'pathWalkInterruptible' returns a WalkStatus-}
{--- which determines which subsequent directories are traversed.-}
data WalkStatus
  = Continue -- ^ Continue recursing all subdirectories.
  | Stop -- ^ Stop recursing.
  deriving (Show, Eq)

pathWalkInternal :: FilePath -> Callback a -> a -> IO a
pathWalkInternal root callback initial = do
  names <- getDirectoryContents root
  let properNames = filter (`notElem` [".", ".."]) names

  dirs <- filterM (\n -> doesDirectoryExist $ root </> n) properNames
  files <- filterM (\n -> doesFileExist $ root </> n) properNames

  result <- callback root dirs files initial
  case result of
    (Continue, a) -> foldM (\a' dir -> pathWalkInternal (root </> dir) callback a') a dirs
    (Stop, a) -> return a
