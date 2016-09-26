module ZipperEditorRepl (
  module ZipperEditorRepl,
  module Text.XML.Light.Cursor
  ) where

import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe
import Data.IORef
import Text.XML.Light.Cursor
import Text.XML.Light

targetFile :: String
targetFile = "test.me"

type Doc = Cursor

renderer :: Doc -> String
renderer = ppContent . toTree . root

myEditVar :: IORef Doc
myEditVar = unsafePerformIO (newIORef undefined)

load :: FilePath -> IO ()
load fp =
  do
     Just v <- parseXMLDoc <$> readFile fp
     writeIORef myEditVar (fromElement v)

whereami :: IO ()
whereami = readIORef myEditVar >>= describe . current
  where describe c@(Text _) = print c
        describe c@(CRef _) = print c
        describe (Elem e) = print (elName e)

modify :: (Doc -> Doc) -> IO ()
modify f =
  do
    x <- readIORef myEditVar
    let v = f x
    writeIORef myEditVar v
    postModify (renderer v)

modifyM :: (Doc -> Maybe Doc) -> IO ()
modifyM f = modify (\z -> fromMaybe z (f z))

postModify :: String -> IO ()
postModify _ = whereami
