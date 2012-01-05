Modulübersicht curry-base
=========================

  * `Currry.AbstractCurry`: Definition von AbstractCurry
  * `Curry.Base`
      * `.Ident`       : Identifier (normal, qualifiziert, Modulidentifier)
      * `.LexComb`     : CPS-Lexerkombinatoren
      * `.LLParseComb` : CPS-Parserkombinatoren
      * `.MessageMonad`: Error/Warning-Monade für Compiler
      * `.Position`    : Sourcecodeposition
  * `Curry.ExtendedFlat`
      * `.CurryArithmetics` :
      * `.EraseTypes`       :
      * `.Goodies`          :
      * `.InterfaceEquality`:
      * `.LiftLetrec`       :
      * `.MonadicGoodies`   :
      * `.Type`             : Definition von ExtendedFlatCurry
      * `.TypeInference`    :
      * `.UnMutual`         :
  * `Curry.Files`
      * `.Filenames`: Dateiendungen
      * `.PathUtils`: Suchen/Lesen/Schreiben von Curry-Dateien
      * `.Unlit`    : Umformung von LiterateCurry nach Curry
  * `Curry.FlatCurry`
      * `.Goodies`: Hilfsfunktionen
      * `.Type`   : Definition von FlatCurry
  * `Curry.Syntax`: Modul zum Arbeiten mit Curry-Modulen as AST
      * `.Lexer`     : Lexer für Curry
      * `.Parser`    : Parser für Curry
      * `.Pretty`    : Pretty-Printer für Curry
      * `.ShowModule`: künstliche Show-Instanz
      * `.Type`      : Definition der abstrakten Syntax von Curry
      * `.Utils`     : Hilfsfunktionen
