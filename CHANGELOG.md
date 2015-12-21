Change log for curry-base
=========================

Under Development (0.4.1)
=========================

  * Derive `Show` and `Read` instances also for identifiers to facilitate
    debugging and reading/writing from/to files
  * Emitted FlatCurry files now contain newlines to improve readability
    for humans
  * Implemented pretty printer for extended FlatCurry

Version 0.4.0
=============

  * Introduced new representation of AbstractCurry

    - AbstractCurry files now contain version information
    - support for new record syntax
    - support for newtype declarations
    - evaluation annotations removed
    - arity of constructor declarations removed
    - simplified representation of function rules
    - String literals added

  * Removed support for Curry's record syntax and introduced Haskell's record
    syntax instead

  * Lexer is now capable of lexing binary integer literals, for instance
    `0b101010` or `0B101010` can now be lexed and are converted to `42`.

  * Removed record type extensions

  * Moved `CYT` monads to `curry-base` (`Curry.Base.Monad`)
    and removed `MessageM` monad

  * Adapted Curry syntax and parser: Now declaration of operator precendence
    in declarations of infix operators is optional

  * Moved module `InterfaceEquivalence` (curry-frontend) to
    `Curry.Syntax.InterfaceEquivalence` (curry-base)

  * Removed module `Curry.Base.Equiv`

  * Replaced module `Curry.ExtendedFlat.Interface.Equality` by
    `Curry.ExtendedFlat.InterfaceEquivalence` using a type class to
    implement equivalence of FlatCurry interfaces

  * Removed file name extensions for FlatCurry XML files.

  * Added syntax extension `NegativeLiterals` to translate negated literals
    into negative literals instead of a call to `Prelude.negate` and
    `Prelude.negateFloat`, respectively.

  * Added `CYMAKE` to the list of recognized tools when parsing an options
    pragma (`{-# OPTIONS_CYMAKE opt1 opt2 ... optN #-}`).

Version 0.3.10
==============

  * Updated internal structure of `Curry.Base.Filenames` and
    `Curry.Base.PathUtils`.

  * Fixed bug in parser which complained `:-> expected` when it really
    looked for `:>`.

  * Make library compile under GHC 7.8 without warnings.

  * Unliterating and lexing/parsing of source files are now decoupled
    to support custom preprocessors.

  * Split `Curry.AbstractCurry` and `Curry.FlatCurry` into two modules
    `.Type` and `.Files`, where `.Type` now only contains the type definition
    while `.Files` contains read/write functions.
    Both are subsumed by the parent modules `Curry.AbstractCurry`
    and `Curry.FlatCurry` for convenience.

Version 0.3.9
=============

  * Implementation of module pragmas added. Module pragmas of the following
    types are now parsed and represented in the abstract syntax tree:

    ~~~ {.curry}
    {-# LANGUAGE LANG_EXT+ #-}
    {-# OPTIONS "string" #-}
    {-# OPTIONS_TOOL "string" #-}
    module Main where
    ~~~

    where

      - `LANGEXT+` is a non-empty, comma-separated list of the following
        language extensions: `AnonFreeVars`, `FunctionalPatterns`,
        `NoImplicitPrelude`, `Records`
      - `TOOL` is either `KICS2`, `PAKCS`, or some other tool, represented
        as `Unknown String`.

    Note that, naturally, the curry-base library only recognizes the above
    mentioned pragmas, while the processing is up to the respective tool.

    All other texts given in the pragma braces is ignored and treated as
    a nested comment.

  * Reactivation of Curry interface files.
    During adaption of the MCC frontend to FlatCurry the Curry interface
    files have been deactivated and replaced by FlatCurry's interface
    files. To allow the later addition of type classes to Curry,
    they have now been reactivated.

Version 0.3.8
=============

  * The parser now takes the layout into respect when parsing the import
    list. This fixes issue #494 where a module with imports without
    restrictions, directly followed by an operator definition,
    could not be parsed.

  * Various internal improvements.

Version 0.3.7
=============

  * Support for typed FlatCurry expressions added. Now additional type
    information given by the programmer as in

    ~~~ {.curry}
    null (unknown :: [()])
    ~~~

    is represented in FlatCurry and cann therefore be processed by other
    programs like PAKCS or KICS2.

Version 0.3.6
=============

  * Fixed a bug where character constants not contained in the ASCII alphabet
    were translated incorrectly.

Version 0.3.5
=============

  * Fixed a bug w.r.t. pretty-printing of records.

Version 0.3.4
=============

  * Made compiler messages comparable to allow later sorting of compiler
    errors and warnings to present them in the order of their occurence.

Version 0.3.3
=============

  * Improved pretty printing of Curry modules.

Version 0.3.2
=============

  * Improved pretty-printing of warnings and errors.

  * Improved error message for missing precendence after fixity declaration.

  * Changed syntax of records to allow disambiguation of record selection
    and case branches.

  * Various improvements.

Version 0.3.1
=============

  * Improved support for anonymous identifiers (test predicate, parser
    also returns source code position).

Version 0.3.0
=============

  * Massive refactoring of the previous version.

  * All compiler warnings removed.

  * Fixed various implementation bugs.
