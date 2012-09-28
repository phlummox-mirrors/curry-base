Module overview of package `curry-base`
=======================================

  * `Currry.AbstractCurry`: Definition of AbstractCurry
  * `Curry.Base`
      * `.Ident`       : Identifier (unqualified, qualified, module identifier)
      * `.LexComb`     : CPS lexer combinators
      * `.LLParseComb` : CPS parser combinators
      * `.Message`     : Error/Warning monad
      * `.Position`    : source code position
  * `Curry.ExtendedFlat`
      * `.CurryArithmetics` :
      * `.EraseTypes`       :
      * `.Goodies`          :
      * `.InterfaceEquality`:
      * `.LiftLetrec`       :
      * `.MonadicGoodies`   :
      * `.Type`             : Definition of ExtendedFlatCurry
      * `.TypeInference`    :
      * `.UnMutual`         :
  * `Curry.Files`
      * `.Filenames`: Curry file extensions and file name manipulation
      * `.PathUtils`: lookup/read/write of Curry files-Dateien
      * `.Unlit`    : unliteration of literate Curry
  * `Curry.FlatCurry`
      * `.Goodies`: Auxiliary functions for working with FlatCurry
      * `.Pretty` : Pretty printer for FlatCurry
      * `.Type`   : Definition of FlatCurry
  * `Curry.Syntax`: Curry AST and related functions
      * `.Lexer`     : Lexer for Curry
      * `.Parser`    : Parser for Curry
      * `.Pretty`    : Pretty-Printer for Curry
      * `.ShowModule`: artificial Show instance
      * `.Type`      : Definition of the abstract syntax tree
      * `.Utils`     : Auxiliary functions
