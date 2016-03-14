--------------------------------------------------------------------------------
-- Test Suite for Curry Base
--------------------------------------------------------------------------------
-- 
-- This Test Suite supports three kinds of tests:
-- 
-- 1) tests which should pass
-- 2) tests which should pass with a specific warning
-- 3) tests which should fail yielding a specific error message
-- 
-- In order to add a test to this suite, proceed as follows:
-- 
-- 1) Store your test code in a file (please use descriptive names) and put it
--    in the corresponding subfolder (i.e. test/pass for passing tests,
--    test/fail for failing tests and test/warning for passing tests producing
--    warnings)
-- 2) Extend the corresponding test information list (there is one for each test
--    group at the end of this file) with the required information (i.e. name of
--    the Curry module to be tested and expected warning/failure message(s))
-- 3) Run 'cabal test'

{-# LANGUAGE CPP #-}
module TestBase (tests) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative    ((<$>))
#endif

import Control.Monad.Trans    (lift)
import Data.List              (isInfixOf, sort)
import Distribution.TestSuite
import System.FilePath        (FilePath, (</>), (<.>))

import Curry.Base.Message     (Message, ppMessages, ppError)
import Curry.Base.Monad       (CYIO, runCYIO, liftCYM)
import Curry.Files.PathUtils  (readModule)
import Curry.Syntax           (parseModule, unlit)

tests :: IO [Test]
tests = return [passingTests, warningTests, failingTests]

-- Call the Curry parser
parseCurry :: FilePath -> CYIO ()
parseCurry file = do
  msrc <- lift (readModule file)
  case msrc of Nothing  -> error $ "Missing file " ++ file
               Just src -> liftCYM $ do ul <- unlit file src
                                        parseModule file ul
                                        return ()

-- Execute a test by calling cymake
runTest :: String -> [String] -> IO Progress
runTest test [] = runCYIO (parseCurry test) >>= passOrFail
 where
  passOrFail = (Finished <$>) . either fail pass
  fail msgs
    | null msgs = return Pass
    | otherwise = return $ Fail $ "An unexpected failure occurred"
  pass _     = return Pass
runTest test errorMsgs = runCYIO (parseCurry test) >>= catchE
 where
   catchE    = (Finished <$>) . either pass fail
   pass msgs = let errorStr = showMessages msgs
               in if all (`isInfixOf` errorStr) errorMsgs
                    then return Pass
                    else return $ Fail $ "Expected warning/failure did not occur: " ++ errorStr
   fail _    = return $ Fail "Expected warning/failure did not occur"

showMessages :: [Message] -> String
showMessages = show . ppMessages ppError . sort

-- group of tests which should pass
passingTests :: Test
passingTests = Group { groupName    = "Passing Tests"
                     , concurrently = False
                     , groupTests   = map (mkTest "test/pass/") passInfos
                     }

-- group of test which should fail yielding a specific error message
failingTests :: Test
failingTests = Group { groupName    = "Failing Tests"
                     , concurrently = False
                     , groupTests   = map (mkTest "test/fail/") failInfos
                     }

-- group of tests which should pass producing a specific warning message
warningTests :: Test
warningTests = Group { groupName    = "Warning Tests"
                     , concurrently = False
                     , groupTests   = map (mkTest "test/warning/") warnInfos
                     }

-- create a new test
mkTest :: FilePath -> TestInfo -> Test
mkTest path (testName, testTags, testOpts, mSetOpts, errorMsgs) =
  let file = path </> testName <.> "curry"
      test = TestInstance
        { run       = runTest file errorMsgs
        , name      = testName
        , tags      = testTags
        , options   = testOpts
        , setOption = maybe (\_ _ -> Right test) id mSetOpts
        }
  in Test test

-- Information for a test instance:
-- * name of test
-- * tags to classify a test
-- * options
-- * function to set options
-- * optional warning/error message which should be thrown on execution of test
type TestInfo = (String, [String], [OptionDescr], Maybe SetOption, [String])

type SetOption = String -> String -> Either String TestInstance

--------------------------------------------------------------------------------
-- Definition of passing tests
--------------------------------------------------------------------------------

-- generate a simple passing test
mkPassTest :: String -> TestInfo
mkPassTest name = (name, [], [], Nothing, [])

-- To add a passing test to the test suite simply add the module name of the
-- test code to the following list
-- TODO: add test cases
passInfos :: [TestInfo]
passInfos = map mkPassTest []

--------------------------------------------------------------------------------
-- Definition of failing tests
--------------------------------------------------------------------------------

-- generate a simple failing test
mkFailTest :: String -> [String] -> TestInfo
mkFailTest name errorMsgs = (name, [], [], Nothing, errorMsgs)

-- To add a failing test to the test suite simply add the module name of the
-- test code and the expected error message(s) to the following list
-- TODO: add test cases
failInfos :: [TestInfo]
failInfos = map (uncurry mkFailTest) []

--------------------------------------------------------------------------------
-- Definition of warning tests
--------------------------------------------------------------------------------

-- To add a warning test to the test suite simply add the module name of the
-- test code and the expected warning message(s) to the following list
-- TODO: add test cases
warnInfos :: [TestInfo]
warnInfos = map (uncurry mkFailTest) []
