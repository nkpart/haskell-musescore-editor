module Save where

import Text.XML.Light.Cursor as C
import Data.List (unfoldr)
import Data.Maybe
import Text.XML.Light as X
import XMLLens
import Control.Lens
import Data.Data.Lens

data SaveOpts = SaveOpts
  { _saveOptsTo :: FilePath
  , _saveOptsPreviewTo :: FilePath
  } deriving (Eq, Show)

instance Plated Content where
  plate = uniplate

save :: SaveOpts -> Cursor -> IO ()
save opts c =
  writeFile (_saveOptsTo opts) (renderer c)

preview :: SaveOpts -> Cursor -> IO ()
preview opts c =
  do let highlighted = highlightNotesDown c -- TODO highlight notes from here down
     writeFile (_saveOptsPreviewTo opts) (renderer highlighted)

highlightNotesDown :: Cursor -> Cursor
highlightNotesDown c =
  let thisStuff = current c
   in c { current = transform (_Elem . filtered (atElementNamed' "Note") %~ setColorC) thisStuff }

setColorC :: Element -> Element
setColorC = (^?! _Elem) . toTree . setColor . fromElement

setColor :: Cursor -> Cursor
setColor c =
  maybe (addColor c) id . (>>= (parent . changeColor)) . C.findChild (atElementNamed "color") $ c
  where changeColor = modifyContent (_Elem . elAttribsL .~ rgba )
        addColor = modifyContent (_Elem . elContentL %~ (meColor:))

        rgba = [Attr (n "r") "92", Attr (n "g" )"186", Attr (n "b") "251", Attr ( n "a" ) "255"]
        meColor = Elem $ blank_element & elNameL .~ n "color" & elAttribsL .~ rgba
        n c = QName c Nothing Nothing


dup :: t -> (t, t)
dup x = (x, x)

renderer :: Cursor -> String
renderer = showContent . toTree . root

atElementNamed :: String -> Cursor -> Bool
atElementNamed n x = anyOf (_Elem . elNameL . qNameL) (== n) (current x)

atElementNamed' :: String -> Element -> Bool
atElementNamed' n x = anyOf (elNameL . qNameL) (== n) x

-- | Move (depth first) to the next position that passes the predicate
advance :: (Cursor -> Bool) -> Cursor -> Maybe Cursor
advance p c =
  -- nextDF steps us off from the current position
  nextDF c >>= C.findRec p
