-- |
-- Module    :  Data.XCB.FromXML
-- Copyright :  (c) Antoine Latter 2008
-- License   :  BSD3
--
-- Maintainer:  Antoine Latter <aslatter@gmail.com>
-- Stability :  provisional
-- Portability: portable
--
-- Handls parsing the data structures from XML files.
--
-- In order to support copying events and errors across module
-- boundaries, all modules which may have cross-module event copies and
-- error copies must be parsed at once.
--
-- There is no provision for preserving the event copy and error copy
-- declarations - the copies are handled during parsing.
module Data.XCB.FromXML(fromFiles
                       ,fromStrings
                       ) where

import Data.XCB.Types
import Data.XCB.Utils

import Text.XML.Light

import Data.List as List
import qualified Data.Map as Map
import Data.Maybe

import Control.Monad
import Control.Monad.Reader

import System.IO (openFile, IOMode (ReadMode), hSetEncoding, utf8, hGetContents)

-- |Process the listed XML files.
-- Any files which fail to parse are silently dropped.
-- Any declaration in an XML file which fail to parse are
-- silently dropped.
fromFiles :: [FilePath] -> IO [XHeader]
fromFiles xs = do
  strings <- sequence $ map readFileUTF8 xs
  return $ fromStrings strings

-- | Like 'readFile', but forces the encoding
-- of the file to UTF8.
readFileUTF8 :: FilePath -> IO String
readFileUTF8 fp = do
  h <- openFile fp ReadMode
  hSetEncoding h utf8
  hGetContents h

-- |Process the strings as if they were XML files.
-- Any files which fail to parse are silently dropped.
-- Any declaration in an XML file which fail to parse are
-- silently dropped.
fromStrings :: [String] -> [XHeader]
fromStrings xs =
   let rs = mapAlt fromString xs
       Just headers = runReaderT rs headers
   in headers 

-- The 'Parse' monad.  Provides the name of the
-- current module, and a list of all of the modules.
type Parse = ReaderT ([XHeader],Name) Maybe

-- operations in the 'Parse' monad

localName :: Parse Name
localName = snd `liftM` ask

allModules :: Parse [XHeader]
allModules = fst `liftM` ask

-- Extract an Alignment from a list of Elements. This assumes that the
-- required_start_align is the first element if it exists at all.
extractAlignment :: (MonadPlus m, Functor m) => [Element] -> m (Maybe Alignment, [Element])
extractAlignment (el : xs) | el `named` "required_start_align" = do
                               align <- el `attr` "align" >>= readM
                               offset <- el `attr` "offset" >>= readM
                               return (Just (Alignment align offset), xs)
                           | otherwise = return (Nothing, el : xs)
extractAlignment xs = return (Nothing, xs)

-- a generic function for looking up something from
-- a named XHeader.
--
-- this implements searching both the current module and
-- the xproto module if the name is not specified.
lookupThingy :: ([XDecl] -> Maybe a)
             -> (Maybe Name)
             -> Parse (Maybe a)
lookupThingy f Nothing = do
  lname <- localName
  liftM2 mplus (lookupThingy f $ Just lname)
               (lookupThingy f $ Just "xproto") -- implicit xproto import
lookupThingy f (Just mname) = do
  xs <- allModules
  return $ do
    x <- findXHeader mname xs
    f $ xheader_decls x

-- lookup an event declaration by name.
lookupEvent :: Maybe Name -> Name -> Parse (Maybe EventDetails)
lookupEvent mname evname = flip lookupThingy mname $ \decls ->
                 findEvent evname decls

-- lookup an error declaration by name.
lookupError :: Maybe Name -> Name -> Parse (Maybe ErrorDetails)
lookupError mname ername = flip lookupThingy mname $ \decls ->
                 findError ername decls

findXHeader :: Name -> [XHeader] -> Maybe XHeader
findXHeader name = List.find $ \ x -> xheader_header x == name

findError :: Name -> [XDecl] -> Maybe ErrorDetails
findError pname xs =
      case List.find f xs of
        Nothing -> Nothing
        Just (XError name code alignment elems) -> Just $ ErrorDetails name code alignment elems
        _ -> error "impossible: fatal error in Data.XCB.FromXML.findError"
    where  f (XError name _ _ _) | name == pname = True
           f _ = False 
                                       
findEvent :: Name -> [XDecl] -> Maybe EventDetails
findEvent pname xs = 
      case List.find f xs of
        Nothing -> Nothing
        Just (XEvent name code alignment elems noseq) ->
            Just $ EventDetails name code alignment elems noseq
        _ -> error "impossible: fatal error in Data.XCB.FromXML.findEvent"
   where f (XEvent name _ _ _ _) | name == pname = True
         f _ = False 

data EventDetails = EventDetails Name Int (Maybe Alignment) [StructElem] (Maybe Bool)
data ErrorDetails = ErrorDetails Name Int (Maybe Alignment) [StructElem]

---

-- extract a single XHeader from a single XML document
fromString :: String -> ReaderT [XHeader] Maybe XHeader
fromString str = do
  el@(Element _qname _ats cnt _) <- lift $ parseXMLDoc str
  guard $ el `named` "xcb"
  header <- el `attr` "header"
  let name = el `attr` "extension-name"
      xname = el `attr` "extension-xname"
      maj_ver = el `attr` "major-version" >>= readM
      min_ver = el `attr` "minor-version" >>= readM
      multiword = el `attr` "extension-multiword" >>= readM . ensureUpper
  decls <- withReaderT (\r -> (r,header)) $ extractDecls cnt
  return $ XHeader {xheader_header = header
                   ,xheader_xname = xname
                   ,xheader_name = name
                   ,xheader_multiword = multiword
                   ,xheader_major_version = maj_ver
                   ,xheader_minor_version = min_ver
                   ,xheader_decls = decls
                   }

-- attempts to extract declarations from XML content, discarding failures.
extractDecls :: [Content] -> Parse [XDecl]
extractDecls = mapAlt declFromElem . onlyElems

-- attempt to extract a module declaration from an XML element
declFromElem :: Element -> Parse XDecl
declFromElem el
    | el `named` "request" = xrequest el
    | el `named` "event"   = xevent el
    | el `named` "eventcopy" = xevcopy el
    | el `named` "error" = xerror el
    | el `named` "errorcopy" = xercopy el
    | el `named` "struct" = xstruct el
    | el `named` "union" = xunion el
    | el `named` "xidtype" = xidtype el
    | el `named` "xidunion" = xidunion el
    | el `named` "typedef" = xtypedef el
    | el `named` "enum" = xenum el
    | el `named` "import" = ximport el
    | el `named` "eventstruct" = xeventstruct el
    | otherwise = mzero


ximport :: Element -> Parse XDecl
ximport = return . XImport . strContent

xenum :: Element -> Parse XDecl
xenum el = do
  nm <- el `attr` "name"
  fields <- mapAlt enumField $ elChildren el
  guard $ not $ null fields
  return $ XEnum nm fields

enumField :: Element -> Parse (EnumElem Type)
enumField el = do
  guard $ el `named` "item"
  name <- el `attr` "name"
  let expr = firstChild el >>= expression
  return $ EnumElem name expr

xrequest :: Element -> Parse XDecl
xrequest el = do
  nm <- el `attr` "name"
  code <- el `attr` "opcode" >>= readM
  -- TODO - I don't think I like 'mapAlt' here.
  -- I don't want to be silently dropping fields
  (alignment, xs) <- extractAlignment $ elChildren el
  fields <- mapAlt structField $ xs
  let reply = getReply el
  return $ XRequest nm code alignment fields reply

getReply :: Element -> Maybe XReply
getReply el = do
  childElem <- unqual "reply" `findChild` el
  (alignment, xs) <- extractAlignment $ elChildren childElem
  fields <- mapM structField xs
  guard $ not $ null fields
  return $ GenXReply alignment fields

xevent :: Element -> Parse XDecl
xevent el = do
  name <- el `attr` "name"
  number <- el `attr` "number" >>= readM
  let noseq = ensureUpper `liftM` (el `attr` "no-sequence-number") >>= readM
  (alignment, xs) <- extractAlignment (elChildren el)
  fields <- mapM structField $ xs
  guard $ not $ null fields
  return $ XEvent name number alignment fields noseq

xevcopy :: Element -> Parse XDecl
xevcopy el = do
  name <- el `attr` "name"
  number <- el `attr` "number" >>= readM
  ref <- el `attr` "ref"
  -- do we have a qualified ref?
  let (mname,evname) = splitRef ref
  details <- lookupEvent mname evname
  return $ let EventDetails _ _ alignment fields noseq =
                 case details of
                   Nothing ->
                       error $ "Unresolved event: " ++ show mname ++ " " ++ ref
                   Just x -> x  
           in XEvent name number alignment fields noseq

-- we need to do string processing to distinguish qualified from
-- unqualified types.
mkType :: String -> Type
mkType str =
    let (mname, name) = splitRef str
    in case mname of
         Just modifier -> QualType modifier name
         Nothing  -> UnQualType name

splitRef :: Name -> (Maybe Name, Name)
splitRef ref = case split ':' ref of
                 (x,"") -> (Nothing, x)
                 (a, b) -> (Just a, b)

-- |Neither returned string contains the first occurance of the
-- supplied Char.
split :: Char -> String -> (String, String)
split c = go
    where go [] = ([],[])
          go (x:xs) | x == c = ([],xs)
                    | otherwise = 
                        let (lefts, rights) = go xs
                        in (x:lefts,rights)
                 

xerror :: Element -> Parse XDecl
xerror el = do
  name <- el `attr` "name"
  number <- el `attr` "number" >>= readM
  (alignment, xs) <- extractAlignment $ elChildren el
  fields <- mapM structField $ xs
  return $ XError name number alignment fields


xercopy :: Element -> Parse XDecl
xercopy el = do
  name <- el `attr` "name"
  number <- el `attr` "number" >>= readM
  ref <- el `attr` "ref"
  let (mname, ername) = splitRef ref
  details <- lookupError mname ername
  return $ uncurry (XError name number) $ case details of
               Nothing -> error $ "Unresolved error: " ++ show mname ++ " " ++ ref
               Just (ErrorDetails _ _ alignment elems) -> (alignment, elems)

xstruct :: Element -> Parse XDecl
xstruct el = do
  name <- el `attr` "name"
  (alignment, xs) <- extractAlignment $ elChildren el
  fields <- mapAlt structField $ xs
  guard $ not $ null fields
  return $ XStruct name alignment fields

xunion :: Element -> Parse XDecl
xunion el = do
  name <- el `attr` "name"
  (alignment, xs) <- extractAlignment $ elChildren el
  fields <- mapAlt structField $ xs
  guard $ not $ null fields
  return $ XUnion name alignment fields

xidtype :: Element -> Parse XDecl
xidtype el = liftM XidType $ el `attr` "name"

xidunion :: Element -> Parse XDecl
xidunion el = do
  name <- el `attr` "name"
  let types = mapMaybe xidUnionElem $ elChildren el
  guard $ not $ null types
  return $ XidUnion name types

xidUnionElem :: Element -> Maybe XidUnionElem
xidUnionElem el = do
  guard $ el `named` "type"
  return $ XidUnionElem $ mkType $ strContent el

xtypedef :: Element -> Parse XDecl
xtypedef el = do
  oldtyp <- liftM mkType $ el `attr` "oldname"
  newname <- el `attr` "newname"
  return $ XTypeDef newname oldtyp

xeventstruct :: Element -> Parse XDecl
xeventstruct el = do
  name <- el `attr` "name"
  allowed <- mapAlt allowedEvent $ elChildren el
  return $ XEventStruct name allowed

allowedEvent :: (MonadPlus m, Functor m) => Element -> m AllowedEvent
allowedEvent el = do
  extension <- el `attr` "name"
  xge <- el `attr` "xge" >>= readM
  opMin <- el `attr` "opcode-min" >>= readM
  opMax <- el `attr` "opcode-max" >>= readM
  return $ AllowedEvent extension xge opMin opMax

structField :: (MonadFail m, MonadPlus m, Functor m) => Element -> m StructElem
structField el
    | el `named` "field" = do
        typ <- liftM mkType $ el `attr` "type"
        let enum = liftM mkType $ el `attr` "enum"
        let mask = liftM mkType $ el `attr` "mask"
        name <- el `attr` "name"
        return $ SField name typ enum mask

    | el `named` "pad" = do
        bytes <- el `attr` "bytes" >>= readM
        return $ Pad bytes

    | el `named` "list" = do
        typ <- liftM mkType $ el `attr` "type"
        name <- el `attr` "name"
        let enum = liftM mkType $ el `attr` "enum"
        let expr = firstChild el >>= expression
        return $ List name typ expr enum

    | el `named` "valueparam" = do
        mask_typ <- liftM mkType $ el `attr` "value-mask-type"
        mask_name <- el `attr` "value-mask-name"
        let mask_pad = el `attr` "value-mask-pad" >>= readM
        list_name <- el `attr` "value-list-name"
        return $ ValueParam mask_typ mask_name mask_pad list_name

    | el `named` "switch" = do
        nm <- el `attr` "name"
        (exprEl,caseEls) <- unconsChildren el
        expr <- expression exprEl
        (alignment, xs) <- extractAlignment $ caseEls
        cases <- mapM bitCase xs
        return $ Switch nm expr alignment cases

    | el `named` "exprfield" = do
        typ <- liftM mkType $ el `attr` "type"
        name <- el `attr` "name"
        expr <- firstChild el >>= expression
        return $ ExprField name typ expr

    | el `named` "reply" = fail "" -- handled separate

    | el `named` "doc" = do
        fields <- el `children` "field"
        let mkField = \x -> fmap (\y -> (y, strContent x)) $ x `attr` "name"
            fields' = Map.fromList $ catMaybes $ map mkField fields
            sees = findChildren (unqual "see") el
            sees' = catMaybes $ flip map sees $ \s -> do typ <- s `attr` "type"
                                                         name <- s `attr` "name"
                                                         return (typ, name)
            brief = fmap strContent $ findChild (unqual "brief") el
        return $ Doc brief fields' sees'

    | el `named` "fd" = do
        name <- el `attr` "name"
        return $ Fd name

    | el `named` "length" = do
        expr <- firstChild el >>= expression
        let typ = mkType "CARD32"
        return $ Length typ expr

    | otherwise = let name = elName el
                  in error $ "I don't know what to do with structelem "
 ++ show name

bitCase :: (MonadFail m, MonadPlus m, Functor m) => Element -> m BitCase
bitCase el | el `named` "bitcase" || el `named` "case" = do
              let mName = el `attr` "name"
              (exprEl, fieldEls) <- unconsChildren el
              expr <- expression exprEl
              (alignment, xs) <- extractAlignment $ fieldEls
              fields <- mapM structField xs
              return $ BitCase mName expr alignment fields
           | otherwise =
              let name = elName el
              in error $ "Invalid bitCase: " ++ show name

expression :: (MonadFail m, MonadPlus m, Functor m) => Element -> m XExpression
expression el | el `named` "fieldref"
                    = return $ FieldRef $ strContent el
              | el `named` "enumref" = do
                   enumTy <- mkType <$> el `attr` "ref"
                   let enumVal = strContent el
                   guard $ enumVal /= ""
                   return $ EnumRef enumTy enumVal
              | el `named` "value"
                    = Value `liftM` readM (strContent el)
              | el `named` "bit"
                    = Bit `liftM` do
                        n <- readM (strContent el)
                        guard $ n >= 0
                        return n
              | el `named` "op" = do
                    binop <- el `attr` "op" >>= toBinop
                    [exprLhs,exprRhs] <- mapM expression $ elChildren el
                    return $ Op binop exprLhs exprRhs
              | el `named` "unop" = do
                    op <- el `attr` "op" >>= toUnop
                    expr <- firstChild el >>= expression
                    return $ Unop op expr
              | el `named` "popcount" = do
                    expr <- firstChild el >>= expression
                    return $ PopCount expr
              | el `named` "sumof" = do
                    ref <- el `attr` "ref"
                    return $ SumOf ref
              | el `named` "paramref"
                    =  return $ ParamRef $ strContent el
              | otherwise =
                  let nm = elName el
                  in error $ "Unknown epression " ++ show nm ++ " in Data.XCB.FromXML.expression"


toBinop :: MonadPlus m => String -> m Binop
toBinop "+"  = return Add
toBinop "-"  = return Sub
toBinop "*"  = return Mult
toBinop "/"  = return Div
toBinop "&"  = return And
toBinop "&amp;" = return And
toBinop ">>" = return RShift
toBinop _ = mzero

toUnop :: MonadPlus m => String -> m Unop
toUnop "~" = return Complement
toUnop _ = mzero


----
----
-- Utility functions
----
----

firstChild :: MonadPlus m => Element -> m Element
firstChild = listToM . elChildren

unconsChildren :: MonadPlus m => Element -> m (Element, [Element])
unconsChildren el
    = case elChildren el of
        (x:xs) -> return (x,xs)
        _ -> mzero

listToM :: MonadPlus m => [a] -> m a
listToM [] = mzero
listToM (x:_) = return x

named :: Element -> String -> Bool
named (Element qname _ _ _) name | qname == unqual name = True
named _ _ = False

attr :: MonadPlus m => Element -> String -> m String
(Element _ xs _ _) `attr` name = case List.find p xs of
      Just (Attr _ res) -> return res
      _ -> mzero
    where p (Attr qname _) | qname == unqual name = True
          p _ = False

children :: MonadPlus m => Element -> String -> m [Element]
(Element _ _ xs _) `children` name = case List.filter p xs of
      [] -> mzero
      some -> return $ onlyElems some
    where p (Elem (Element n _ _ _)) | n == unqual name = True
          p _ = False

-- adapted from Network.CGI.Protocol
readM :: (MonadPlus m, Read a) => String -> m a
readM = liftM fst . listToM . reads

