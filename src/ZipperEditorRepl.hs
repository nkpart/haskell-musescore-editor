{-# LANGUAGE BangPatterns #-}
module ZipperEditorRepl (
  module ZipperEditorRepl,
  module C
  ) where

import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe
import Control.Lens
import Data.IORef
import qualified Data.ByteString as BS
import Text.XML.Light.Cursor as C
import Save
import XMLLens
import Text.XML.Light as X


-- | TODO
-- * [x] Open file
-- * [ ] Go to note (skip over grace notes)
-- * [ ] color selected notes red
-- * [ ] add/remove grace notes
-- * [ ] split note
-- * [ ] insert/append note

-- |
-- load "blah"
-- modify (note 3 . bar 0)
-- modify (makeFlam)
-- save
-- saveAs
-- recover "blah" -- look for preview file and recover it

type Doc = Cursor

-- | Load and remember a file path
load :: FilePath -> IO ()
load fp = do
  Just !v <- parseXMLDoc <$> BS.readFile fp
  writeIORef myEditVar (fp, fromElement v)

-- | Run some modification function on the loaded document
modify :: (Doc -> Doc) -> IO ()
modify f = do
  (fp, x) <- readIORef myEditVar
  let v = f x
  writeIORef myEditVar (fp, v)
  Save.preview (SaveOpts undefined ("Preview" ++ fp)) v
  -- save (SaveOpts fp undefined) v

modifyM :: (Doc -> Maybe Doc) -> IO ()
modifyM f = modify (\z -> fromMaybe z (f z))

myEditVar :: IORef (FilePath, Doc)
myEditVar = unsafePerformIO (newIORef undefined)

describe :: Content -> String
describe c@(Text _) = show c
describe c@(CRef _) = show c
describe (Elem e) = show (elName e) `mappend` show (elAttribs e) `mappend` show (elContent e)

--------------------------------------------
---------- Modification Functions ----------
--------------------------------------------

advanceMeasure :: Cursor -> Cursor
advanceMeasure =
  advanceP (atElementNamed "Measure")

advanceNote :: Cursor -> Cursor
advanceNote c =
  -- tail here, otherwise we include the current position in our comparisons, and if
  -- its the top node, then it'll definitely have an embellishment somewhere beneath it
  let allChords = tail $ iterate (advanceP (atElementNamed "Chord")) c
   in headOr c $ filter (not . hasEmbellishment) allChords

-- atElementNamed :: String -> Cursor -> Bool
-- atElementNamed n x = anyOf (_Elem . elNameL . qNameL) (== n) (current x)

hasEmbellishment :: Cursor -> Bool
hasEmbellishment = isJust . C.findChild isEmbellishment

isEmbellishment :: Cursor -> Bool
isEmbellishment c =
  anyOf (_Elem . elNameL . qNameL)
        (\v -> v == "grace16" || v == "appoggiatura")
        (current c)


-- | Move (depth first) to the next position that passes the predicate
advanceP :: (Cursor -> Bool) -> Cursor -> Cursor
advanceP p c =
  -- nextDF steps us off from the current position
  nextDF c >>= C.findRec p & fromMaybe c

headOr :: t -> [t] -> t
headOr x [] = x
headOr _ (x:_) = x 
