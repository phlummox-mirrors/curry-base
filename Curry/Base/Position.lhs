> {-# LANGUAGE DeriveDataTypeable #-}

% -*- LaTeX -*-
% $Id: Position.lhs,v 1.2 2000/10/08 09:55:43 lux Exp $
%
% $Log: Position.lhs,v $
% Revision 1.2  2000/10/08 09:55:43  lux
% Column numbers now start at 1. If the column number is less than 1 it
% will not be shown.
%
% Revision 1.1  2000/07/23 11:03:37  lux
% Positions now implemented in a separate module.
%
%
\nwfilename{Position.lhs}
\section{Positions}
A source file position consists of a filename, a line number, and a
column number. A tab stop is assumed at every eighth column.
\begin{verbatim}

> module Curry.Base.Position where
> import Data.Generics

> newtype SrcRef = SrcRef [Int] deriving (Typeable,Data) -- a pointer to the origin

-- the instances for standard classes or such that SrcRefs are invisible

> instance Show SrcRef where show _ = ""
> instance Read SrcRef where readsPrec _ s = [(noRef,s)]
> instance Eq SrcRef   where _ == _ = True
> instance Ord SrcRef  where compare _ _ = EQ

> noRef :: SrcRef
> noRef = SrcRef []
>
> incSrcRef :: SrcRef -> Int -> SrcRef
> incSrcRef (SrcRef [i]) j = SrcRef [i+j]
> incSrcRef is  _ = error $ "internal error; increment source ref: " ++ show is

> data Position 
>   = Position{ file :: FilePath, line :: Int, column :: Int, astRef :: SrcRef }
>   | AST { astRef :: SrcRef }
>   | NoPos
>     deriving (Eq, Ord,Data,Typeable)

> incPosition :: Position -> Int -> Position
> incPosition NoPos _ = NoPos
> incPosition p j = p{astRef=incSrcRef (astRef p) j}

> instance Read Position where
>   readsPrec p s = 
>     [ (Position{file="",line=i,column=j,astRef=noRef},s')  | ((i,j),s') <- readsPrec p s]

> instance Show Position where
>   showsPrec _ Position{file=fn,line=l,column=c} =
>     (if null fn then id else shows fn . showString ", ") .
>     showString "line " . shows l .
>     (if c > 0 then showChar '.' . shows c else id)
>   showsPrec _ AST{} = id
>   showsPrec _ NoPos = id

> tabWidth :: Int
> tabWidth = 8

> first :: FilePath -> Position
> first fn = Position fn 1 1 noRef

> incr :: Position -> Int -> Position
> incr p@Position{column=c} n = p{column=c + n}
> incr p _ = p

> next :: Position -> Position
> next = flip incr 1

> tab :: Position -> Position
> tab p@Position{column=c} = p{column=c + tabWidth - (c - 1) `mod` tabWidth}
> tab p = p

> nl :: Position -> Position
> nl p@Position{line=l} = p{line=l + 1, column=1}
> nl p = p

> showLine :: Position -> String
> showLine NoPos = ""
> showLine AST{} = ""
> showLine Position{line=l,column=c} 
>     = "(line " ++ show l ++ "." ++ show c ++ ") "

\end{verbatim}

> class SrcRefOf a where
>   srcRefsOf :: a -> [SrcRef]
>   srcRefsOf = (:[]) . srcRefOf
>   srcRefOf :: a -> SrcRef
>   srcRefOf = head . srcRefsOf

> instance SrcRefOf Position where
>     srcRefOf NoPos = noRef
>     srcRefOf x = astRef x
