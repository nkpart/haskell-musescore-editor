module XMLLens where

import Control.Lens
import Text.XML.Light as X

-- | Content
_Elem :: Prism' Content Element
_Elem = prism Elem g
  where g (Elem e) = Right e
        g v = Left v

_Text :: Prism' Content CData
_Text = prism Text g
  where g (Text e) = Right e
        g v = Left v

_CRef :: Prism' Content String
_CRef = prism CRef g
  where g (CRef e) = Right e
        g v = Left v

-- | Element

elNameL :: Lens' Element QName
elNameL = lens elName (\v a -> v { elName = a })

elAttribsL :: Lens' Element [Attr]
elAttribsL = lens elAttribs (\v a -> v { elAttribs = a })

elContentL :: Lens' Element [Content]
elContentL = lens elContent (\v a -> v { elContent = a })

elLineL :: Lens' Element (Maybe Line)
elLineL = lens elLine (\v a -> v { elLine = a })

-- | Attr

attrKeyL :: Lens' Attr QName
attrKeyL = lens attrKey (\v a -> v { attrKey = a })

attrValL :: Lens' Attr String
attrValL = lens attrVal (\v a -> v { attrVal = a })

-- | CData

cdVerbatimL :: Lens' CData CDataKind
cdVerbatimL = lens cdVerbatim (\v a -> v { cdVerbatim = a })

cdDataL :: Lens' CData String
cdDataL = lens cdData (\v a -> v { cdData = a })

cdLineL :: Lens' CData (Maybe Line)
cdLineL = lens cdLine (\v a -> v { cdLine = a })

-- | CDataKind

_CDataText :: Prism' CDataKind ()
_CDataText = prism' (const CDataText) g
  where g CDataText = Just ()
        g _ = Nothing

_CDataVerbatim :: Prism' CDataKind ()
_CDataVerbatim = prism' (const CDataVerbatim) g
  where g CDataVerbatim = Just ()
        g _ = Nothing

_CDataRaw :: Prism' CDataKind ()
_CDataRaw = prism' (const CDataRaw) g
  where g CDataRaw = Just ()
        g _ = Nothing


-- | QName

qNameL :: Lens' QName String
qNameL = lens qName (\v a -> v { qName = a })

qURIL :: Lens' QName (Maybe String)
qURIL = lens qURI (\v a -> v { qURI = a })

qPrefixL :: Lens' QName (Maybe String)
qPrefixL = lens qPrefix (\v a -> v { qPrefix = a })
