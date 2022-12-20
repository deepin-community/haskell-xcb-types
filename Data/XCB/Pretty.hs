{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- |
-- Module    :  Data.XCB.Pretty
-- Copyright :  (c) Antoine Latter 2008
-- License   :  BSD3
--
-- Maintainer:  Antoine Latter <aslatter@gmail.com>
-- Stability :  provisional
-- Portability: portable - requires TypeSynonymInstances
--
-- Pretty-printers for the tyes declared in this package.
-- This does NOT ouput XML - it produces human-readable information
-- intended to aid in debugging.
module Data.XCB.Pretty where

import Prelude hiding ((<>))

import Data.XCB.Types

import Text.PrettyPrint.HughesPJ

import qualified Data.Map as Map
import Data.Maybe

-- |Minimal complete definition:
--
-- One of 'pretty' or 'toDoc'.
class Pretty a where
    toDoc :: a -> Doc
    pretty :: a -> String

    pretty = show . toDoc
    toDoc = text . pretty

-- Builtin types

instance Pretty String where
    pretty = show

instance Pretty Int where
    pretty = show

instance Pretty Bool where
    pretty = show

instance Pretty a => Pretty (Maybe a) where
    toDoc Nothing = empty
    toDoc (Just a) = toDoc a

    pretty Nothing = ""
    pretty (Just a) = pretty a

-- Simple stuff

instance Pretty a => Pretty (GenXidUnionElem a) where
    toDoc (XidUnionElem t) = toDoc t

instance Pretty Binop where
    pretty Add  = "+"
    pretty Sub  = "-"
    pretty Mult = "*"
    pretty Div  = "/"
    pretty RShift = ">>"
    pretty And = "&"

instance Pretty Unop where
    pretty Complement = "~"

instance Pretty a => Pretty (EnumElem a) where
    toDoc (EnumElem name expr)
        = text name <> char ':' <+> toDoc expr

instance Pretty Type where
    toDoc (UnQualType name) = text name
    toDoc (QualType modifier name)
        = text modifier <> char '.' <> text name

-- More complex stuff

instance Pretty a => Pretty (Expression a) where
    toDoc (Value n) = toDoc n
    toDoc (Bit n) = text "2^" <> toDoc n
    toDoc (FieldRef ref) = char '$' <> text ref
    toDoc (EnumRef typ child)
        = toDoc typ <> char '.' <> text child
    toDoc (PopCount expr)
        = text "popcount" <> parens (toDoc expr)
    toDoc (SumOf ref)
        = text "sumof" <> (parens $ char '$' <> text ref)
    toDoc (Op binop exprL exprR)
        = parens $ hsep [toDoc exprL
                        ,toDoc binop
                        ,toDoc exprR
                        ]
    toDoc (Unop op expr)
        = parens $ toDoc op <> toDoc expr
    toDoc (ParamRef n) = toDoc n

instance Pretty a => Pretty (GenStructElem a) where
    toDoc (Pad n) = braces $ toDoc n <+> text "bytes"
    toDoc (List nm typ len enums)
        = text nm <+> text "::" <+> brackets (toDoc typ <+> toDoc enums) <+> toDoc len
    toDoc (SField nm typ enums mask) = hsep [text nm
                                            ,text "::"
                                            ,toDoc typ
                                            ,toDoc enums
                                            ,toDoc mask
                                            ]
    toDoc (ExprField nm typ expr)
        = parens (text nm <+> text "::" <+> toDoc typ)
          <+> toDoc expr
    toDoc (Switch name expr alignment cases)
        = vcat
           [ text "switch" <> parens (toDoc expr) <> toDoc alignment <> brackets (text name)
           , braces (vcat (map toDoc cases))
           ]
    toDoc (Doc brief fields see)
        = text "Doc" <+>
          text "::" <+>
          text "brief=" <+> text (fromMaybe "" brief) <+>
          text "fields=" <+>
          hsep (punctuate (char ',') $ joinWith ":" $ Map.toList fields) <+>
          text ";" <+>
          text "see=" <+>
          hsep (punctuate (char ',') $ joinWith "." see)

        where
          joinWith c = map $ \(x,y) -> text $ x ++ c ++ y

    toDoc (Fd fd)
        = text "Fd" <+>
          text "::" <+>
          text fd
    toDoc (ValueParam typ mname mpad lname)
        = text "Valueparam" <+>
          text "::" <+>
          hsep (punctuate (char ',') details)

        where details
                  | isJust mpad =
                      [toDoc typ
                      ,text "mask padding:" <+> toDoc mpad
                      ,text mname
                      ,text lname
                      ]
                  | otherwise =
                      [toDoc typ
                      ,text mname
                      ,text lname
                      ]


instance Pretty a => Pretty (GenBitCase a) where
    toDoc (BitCase name expr alignment fields)
        = vcat
           [ bitCaseHeader name expr
           , toDoc alignment
           , braces (vcat (map toDoc fields))
           ]

bitCaseHeader :: Pretty a => Maybe Name -> Expression a -> Doc
bitCaseHeader Nothing expr =
    text "bitcase" <> parens (toDoc expr)
bitCaseHeader (Just name) expr =
    text "bitcase" <> parens (toDoc expr) <> brackets (text name)

instance Pretty Alignment where
    toDoc (Alignment align offset) = text "alignment" <+>
                                       text "align=" <+> toDoc align <+>
                                       text "offset=" <+> toDoc offset

instance Pretty AllowedEvent where
    toDoc (AllowedEvent extension xge opMin opMax) = text "allowed" <+>
                                                       text "extension=" <+> text extension <+>
                                                       text "xge=" <> toDoc xge <>
                                                       text "opcode-min" <> toDoc opMin <>
                                                       text "opcode-max" <> toDoc opMax

instance Pretty a => Pretty (GenXDecl a) where
    toDoc (XStruct nm alignment elems) =
        hang (text "Struct:" <+> text nm <+> toDoc alignment) 2 $ vcat $ map toDoc elems
    toDoc (XTypeDef nm typ) = hsep [text "TypeDef:"
                                    ,text nm
                                    ,text "as"
                                    ,toDoc typ
                                    ]
    toDoc (XEvent nm n alignment elems (Just True)) =
        hang (text "Event:" <+> text nm <> char ',' <> toDoc n <+> toDoc alignment <+>
             parens (text "No sequence number")) 2 $
             vcat $ map toDoc elems
    toDoc (XEvent nm n alignment elems _) =
        hang (text "Event:" <+> text nm <> char ',' <> toDoc n <+> toDoc alignment) 2 $
             vcat $ map toDoc elems
    toDoc (XRequest nm n alignment elems mrep) = 
        (hang (text "Request:" <+> text nm <> char ',' <> toDoc n <+> toDoc alignment) 2 $
             vcat $ map toDoc elems)
         $$ case mrep of
             Nothing -> empty
             Just (GenXReply repAlignment reply) ->
                 hang (text "Reply:" <+> text nm <> char ',' <> toDoc n <+> toDoc repAlignment) 2 $
                      vcat $ map toDoc reply
    toDoc (XidType nm) = text "XID:" <+> text nm
    toDoc (XidUnion nm elems) = 
        hang (text "XID" <+> text "Union:" <+> text nm) 2 $
             vcat $ map toDoc elems
    toDoc (XEnum nm elems) =
        hang (text "Enum:" <+> text nm) 2 $ vcat $ map toDoc elems
    toDoc (XUnion nm alignment elems) = 
        hang (text "Union:" <+> text nm <+> toDoc alignment) 2 $ vcat $ map toDoc elems
    toDoc (XImport nm) = text "Import:" <+> text nm
    toDoc (XError nm _n alignment elems) =
        hang (text "Error:" <+> text nm <+> toDoc alignment) 2 $ vcat $ map toDoc elems
    toDoc (XEventStruct name allowed) =
        hang (text "Event struct:" <+> text name) 2 $ vcat $ map toDoc allowed

instance Pretty a => Pretty (GenXHeader a) where
    toDoc xhd = text (xheader_header xhd) $$
                (vcat $ map toDoc (xheader_decls xhd))
