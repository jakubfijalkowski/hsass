-- | Module    : Text.Sass
-- Stability   : experimental
--
-- This module provides support for <http://sass-lang.com/ Sass>, a CSS
-- extension language. It supports basic compilation, functions, importers and
-- headers, so it should suffice for most of the work.
--
-- Code used in this document is testable - see test/Text/Sass/TutorialSpec.hs.
module Text.Sass
  (
    -- * Compilation
    -- $compilation
    module Text.Sass.Compilation
    -- * Options
    -- $options
  , module Text.Sass.Options
    -- * Values
    -- $values
  , module Text.Sass.Values
    -- * Functions, headers, importers
    -- $functions_basic

    -- ** Functions
    -- $functions

    -- ** Headers and importers
    -- $headers_and_importers

  , module Text.Sass.Functions
  , module Data.Default.Class
  ) where

import           Data.Default.Class    (def)
import           Text.Sass.Compilation
import           Text.Sass.Functions
import           Text.Sass.Options
import           Text.Sass.Values

-- $compilation
-- Compilation of Sass source is very easy - you only have to use 'compileFile'
-- or 'compileString' and leave the work to hsass. 'compileFile' takes path to
-- the file as first parameter, while 'compileString' takes source code.
-- Both functions take 'SassOptions' as second parameter, so if you want to
-- customize compilation behaviour, you may use it.
--
-- The result of both functions is 'Either' @Left 'SassError'@, indicating that
-- something went wrong, or @Right 'String'@ with resulting code.
-- You can examine 'SassError' to gain more knowledge about the error.
--
-- 'SassOptions' is instance of 'Default' class, so you may use 'def' function
-- to get defaults.
--
-- For example, this code:
--
-- > compileString "foo { margin: 2 * 14px; }" def
--
-- Will result in something like
--
-- > "foo { margin: 28px; }"
--
-- When you want to compile source code instead of file, consider setting
-- 'sassIncludePaths' - it will allow to resolve @includes without custom
-- importers.

-- $options
-- 'SassOptions' wraps <http://libsass.org libsass> context options. It does not
-- try to be smarter than libsass, so it is mostly 1-1 mapping. See
-- "Text.Sass.Options" documentation for more info.

-- $values
-- "Text.Sass.Values" module wraps native values that libsass uses. It provides
-- easy way to manage them in pure Haskell code.

-- $functions_basic
-- This is the most advanced stuff in the library (even though it is quite
-- simple). It allows you to define functions in Haskell and use them from Sass
-- source, provide custom resolution for @import statements and include custom
-- headers in files.

-- $functions
-- Let's start with explanation of functions. For example, assume that we want to
-- use function @max3@, that takes three numbers and returns the largest. In
-- Haskell, we would write it like this:
--
-- > max3 :: Int -> Int -> Int -> Int
-- > max3 a b c = max a $ max b c
--
-- Unfortunately, sass would not be able to use that. We must rewrite this
-- function so that it operates on 'SassValue's. If we want to use function
-- in sass, it has to have following signature:
--
-- > func :: SassValue -> IO SassValue
--
-- It takes an argument that is 'SassList' with all of arguments passed in sass
-- code and returns computed 'SassValue'. With this in mind, we may write
-- following code:
--
-- > max3 (SassList (SassNumber a _:SassNumber b _:SassNumber c _:_) _) =
-- >     return $ SassNumber (max a $ max b c) "px"
-- > max3 _ = SassError "invalid arguments"
--
-- Having this function, we may proceed and define its signature:
--
-- > max3sig = SassFunction "max3($a, $b, $c)" max3
--
-- This description allows compiler to map Haskell function (@max3@) to a form
-- that may be used in sass. Signature consists of function name (@max3@),
-- opening parenthesis, list of arguments (dollar sign and its name) separated
-- by comma and closing parenthesis. It is the same as function definition in
-- sass code.
--
-- With this code we may tell compiler to use this function. In order to do
-- this, we must replace 'sassFunctions' field in 'SassOptions':
--
-- > opts = def { sassFunctions = Just [max3sig] }
--
-- Now, we may compile code that uses function max3:
--
-- > compileString "foo { margin: max3(1px, 2px, 3px); }" opts
--
-- And we will get
--
-- > "foo { margin: 3px; }"
--
-- There exist several functions that are special:
--
-- * @*@ - fallback implementation
-- * @@warn@ - overload warn statement
-- * @@error@ - overload error statement
-- * @@debug@ - overload debug statement
--
-- See <https://github.com/sass/libsass/wiki/API-Sass-Function libsass>
-- documentation for more information.

-- $headers_and_importers
-- Importers are functions that override default behaviour of @import statement
-- behaviour. For example, you may implement rewrite rules or even download
-- stylesheets from remote server.
--
-- Headers leverage the same infrastructure as importers - they are just used
-- not for @imports, but for every file being loaded. They allow you to inject
-- arbitrary sass source in the file.
--
-- Let's say that we want to inject path to currently compiled file. We may write
-- following header:
--
-- > header src = return [makeSourceImport $ "$file: " ++ src ++ ";"]
--
-- It simply returns sass code that defines @$file@ variable set to path to
-- current file (first argument of function).
-- Then, we must define header signature and options:
--
-- > headerSig = SassImporter 1 header
-- > opts = def { sassHeaders = Just [headerSig], sassInputPath = Just "path" }
--
-- We set 'sassInputPath', because we will be compiling string and it won't be
-- set automatically.
-- Now, executing
--
-- > compileString "foo { prop: $file; }" opts
--
-- Will produce following result:
--
-- > "foo { prop: path; }"
--
-- Importers are defined and act similarly - they just take path to file being
-- imported instead of file being processed and are injected using
-- 'sassImporters'.
--
-- Additionally importers support priorities - if two importers return source
-- for some file, the one with higher priority wins. For example
--
-- > importer1 src = return [makeSourceImport $ "$file: " ++ src ++ "1;"]
-- > importer2 src = return [makeSourceImport $ "$file: " ++ src ++ "2;"]
-- > importerSigs = [SassImporter 0.5 importer1, SassImporter 1 importer2]
-- > opts = def { sassImporters = Just importerSigs }
-- > compileString "@import \"file\";\nfoo { prop: $file; }" opts
--
-- Will result in
--
-- > "foo { prop: file2; }"
--
-- Instead of providing source code for imports, you may provide path to file
-- and leave loading to library. This is done by settings only 'importPath' and
-- 'importBase' in 'SassImport' or using 'makePathImport' instead of
-- 'makeSourceImport'. This may be useful to implement just rewrite rules and
-- not full loading.
