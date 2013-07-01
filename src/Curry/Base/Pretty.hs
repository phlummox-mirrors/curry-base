{- |
    Module      :  $Header$
    Description :  Pretty printing
    Copyright   :  (c) 2013 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  stable
    Portability :  portable

    This module re-exports the well known pretty printing combinators
    from Hughes and Peyton-Jones. In addition, it re-exports the type class
    'Pretty' for pretty printing arbitrary types.
-}
module Curry.Base.Pretty
  ( module Curry.Base.Pretty
  , module Text.PrettyPrint
  ) where

import Text.PrettyPrint

-- | Pretty printing class.
-- The precedence level is used in a similar way as in the 'Show' class.
-- Minimal complete definition is either 'pPrintPrec' or 'pPrint'.
class Pretty a where
  -- | Pretty-print something in isolation.
  pPrint :: a -> Doc
  pPrint = pPrintPrec 0

  -- | Pretty-print something in a precedence context.
  pPrintPrec :: Int -> a -> Doc
  pPrintPrec _ = pPrint

  -- |Pretty-print a list.
  pPrintList :: [a] -> Doc
  pPrintList = brackets . fsep . punctuate comma . map (pPrintPrec 0)

-- | Pretty print a value to a 'String'.
prettyShow :: Pretty a => a -> String
prettyShow = render . pPrint

-- | Parenthesize an value if the boolean is true.
parenIf :: Bool -> Doc -> Doc
parenIf False = id
parenIf True  = parens

-- |A '.' character.
dot :: Doc
dot = char '.'

-- |Precedence of function application
appPrec :: Int
appPrec = 10

instance Pretty Int      where pPrint = int
instance Pretty Integer  where pPrint = integer
instance Pretty Float    where pPrint = float
instance Pretty Double   where pPrint = double
instance Pretty ()       where pPrint _ = text "()"
instance Pretty Bool     where pPrint = text . show
instance Pretty Ordering where pPrint = text . show

instance Pretty Char where
  pPrint     = char
  pPrintList = text . show

instance (Pretty a) => Pretty (Maybe a) where
  pPrintPrec _ Nothing  = text "Nothing"
  pPrintPrec p (Just x) = parenIf (p > appPrec)
                        $ text "Just" <+> pPrintPrec (appPrec + 1) x

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pPrintPrec p (Left  x) = parenIf (p > appPrec)
                         $ text "Left" <+> pPrintPrec (appPrec + 1) x
  pPrintPrec p (Right x) = parenIf (p > appPrec)
                         $ text "Right" <+> pPrintPrec (appPrec + 1) x

instance (Pretty a) => Pretty [a] where
  pPrintPrec _ xs = pPrintList xs

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pPrintPrec _ (a, b) = parens $ fsep $ punctuate comma [pPrint a, pPrint b]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pPrintPrec _ (a, b, c) = parens $ fsep $ punctuate comma
    [pPrint a, pPrint b, pPrint c]

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  pPrintPrec _ (a, b, c, d) = parens $ fsep $ punctuate comma
    [pPrint a, pPrint b, pPrint c, pPrint d]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e)
  => Pretty (a, b, c, d, e) where
  pPrintPrec _ (a, b, c, d, e) = parens $ fsep $ punctuate comma
    [pPrint a, pPrint b, pPrint c, pPrint d, pPrint e]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f)
  => Pretty (a, b, c, d, e, f) where
  pPrintPrec _ (a, b, c, d, e, f) = parens $ fsep $ punctuate comma
    [pPrint a, pPrint b, pPrint c, pPrint d, pPrint e, pPrint f]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g)
  => Pretty (a, b, c, d, e, f, g) where
  pPrintPrec _ (a, b, c, d, e, f, g) = parens $ fsep $ punctuate comma
    [pPrint a, pPrint b, pPrint c, pPrint d, pPrint e, pPrint f, pPrint g]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h)
  => Pretty (a, b, c, d, e, f, g, h) where
  pPrintPrec _ (a, b, c, d, e, f, g, h) = parens $ fsep $ punctuate comma
    [pPrint a, pPrint b, pPrint c, pPrint d, pPrint e, pPrint f, pPrint g, pPrint h]
