Change log for curry-base
=========================

Version 0.3.10 (under development)
==================================

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
    list. This fixes issue #494 where a module with imports, but without
    other declarations could not be parsed.

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
