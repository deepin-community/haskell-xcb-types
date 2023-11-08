{-# LANGUAGE
     RecordWildCards,
     DeriveFunctor
     #-}

-- |
-- Module    :  Data.XCB.Types
-- Copyright :  (c) Antoine Latter 2008
-- License   :  BSD3
--
-- Maintainer:  Antoine Latter <aslatter@gmail.com>
-- Stability :  provisional
-- Portability: portable
--
-- Defines types inteneded to be equivalent to the schema used by
-- the XCB project in their XML protocol description.
--


module Data.XCB.Types
    ( XHeader
    , XDecl
    , StructElem
    , XEnumElem
    , BitCase
    , XidUnionElem
    , XReply
    , XExpression
    , GenXHeader ( .. )
    , GenXDecl ( .. )
    , GenStructElem ( .. )
    , GenBitCase ( .. )
    , GenXReply ( .. )
    , GenXidUnionElem ( .. )
    , EnumElem ( .. )
    , Expression ( .. )
    , Binop ( .. )
    , Unop ( .. )
    , Type ( .. )
    , EnumVals
    , MaskVals
    , Name
    , Ref
    , MaskName
    , ListName
    , MaskPadding
    , Alignment ( .. )
    , AllowedEvent ( .. )
    ) where

import Data.Map

-- 'xheader_header' is the name gauranteed to exist, and is used in
-- imports and in type qualifiers.
--
-- 'xheader_name' is the InterCaps name, and should be prefered in the naming
-- of types, functions and haskell modules when available.
-- |This is what a single XML file maps to.  It contains some meta-data
-- then declarations.
data GenXHeader typ = XHeader
    {xheader_header :: Name -- ^Name of module.  Used in the other modules as a reference.
    ,xheader_xname :: Maybe Name  -- ^Name used to indentify extensions between the X client and server.
    ,xheader_name :: Maybe Name -- ^InterCaps name.
    ,xheader_multiword :: Maybe Bool
    ,xheader_major_version :: Maybe Int
    ,xheader_minor_version :: Maybe Int
    ,xheader_decls :: [GenXDecl typ]  -- ^Declarations contained in this module.
    }
 deriving (Show, Functor)

type XHeader = GenXHeader Type
type XDecl = GenXDecl Type
type StructElem = GenStructElem Type
type BitCase = GenBitCase Type
type XidUnionElem = GenXidUnionElem Type
type XReply = GenXReply Type
type XExpression = Expression Type
type XEnumElem = EnumElem Type

-- |The different types of declarations which can be made in one of the
-- XML files.
data GenXDecl typ
    = XStruct  Name (Maybe Alignment) [GenStructElem typ]
    | XTypeDef Name typ
    | XEvent Name Int (Maybe Alignment) [GenStructElem typ] (Maybe Bool)  -- ^ The boolean indicates if the event includes a sequence number.
    | XRequest Name Int (Maybe Alignment) [GenStructElem typ] (Maybe (GenXReply typ))
    | XidType  Name
    | XidUnion  Name [GenXidUnionElem typ]
    | XEnum Name [EnumElem typ]
    | XUnion Name (Maybe Alignment) [GenStructElem typ]
    | XImport Name
    | XError Name Int (Maybe Alignment) [GenStructElem typ]
    | XEventStruct Name [AllowedEvent]
 deriving (Show, Functor)

data GenStructElem typ
    = Pad Int
    | List Name typ (Maybe (Expression typ)) (Maybe (EnumVals typ))
    | SField Name typ (Maybe (EnumVals typ)) (Maybe (MaskVals typ))
    | ExprField Name typ (Expression typ)
    | ValueParam typ Name (Maybe MaskPadding) ListName
    | Switch Name (Expression typ) (Maybe Alignment) [GenBitCase typ]
    | Doc (Maybe String) (Map Name String) [(String, String)]
    | Fd String
    | Length typ (Expression typ)
 deriving (Show, Functor)

data GenBitCase typ
    = BitCase (Maybe Name) (Expression typ) (Maybe Alignment) [GenStructElem typ]
 deriving (Show, Functor)

type EnumVals typ = typ
type MaskVals typ = typ

type Name = String
data GenXReply typ = GenXReply (Maybe Alignment) [GenStructElem typ]
 deriving (Show, Functor)
type Ref = String
type MaskName = Name
type ListName = Name
type MaskPadding = Int

-- |Types may include a reference to the containing module.
data Type = UnQualType Name
          | QualType Name Name
 deriving (Show, Eq, Ord)

data GenXidUnionElem typ = XidUnionElem typ
 deriving (Show, Functor)

-- Should only ever have expressions of type 'Value' or 'Bit'.
data EnumElem typ = EnumElem Name (Maybe (Expression typ))
 deriving (Show, Functor)

-- |Declarations may contain expressions from this small language
data Expression typ
    = Value Int  -- ^A literal value
    | Bit Int    -- ^A log-base-2 literal value
    | FieldRef Name -- ^A reference to a field in the same declaration
    | EnumRef typ Name -- ^A reference to a member of an enum.
    | PopCount (Expression typ) -- ^Calculate the number of set bits in the argument
    | SumOf Name -- ^Note sure. The argument should be a reference to a list
    | Op Binop (Expression typ) (Expression typ) -- ^A binary opeation
    | Unop Unop (Expression typ) -- ^A unary operation
    | ParamRef Name -- ^I think this is the name of an argument passed to the request. See fffbd04d63 in xcb-proto.
 deriving (Show, Functor)

-- |Supported Binary operations.
data Binop = Add
           | Sub
           | Mult
           | Div
           | And
           | RShift
 deriving (Show)

data Unop = Complement
 deriving (Show)

data Alignment = Alignment Int Int deriving (Show)

data AllowedEvent = AllowedEvent Name Bool Int Int deriving (Show)
