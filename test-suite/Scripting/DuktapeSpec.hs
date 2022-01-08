{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}

module Scripting.DuktapeSpec (spec) where

import           Test.Hspec hiding (shouldBe)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           TestCommon
import           Data.Maybe
import           Data.Aeson hiding (json)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Clock
import           Scripting.Duktape
import           Scripting.Duktape.Raw (createGovernedHeap)
import           Foreign (nullFunPtr)

spec ∷ Spec
spec = do
  describe "evalDuktape" $ do
    it "evaluates ECMAScript expressions" $ do
      ctx ← createDuktapeCtx
      r0 ← evalDuktape (fromJust ctx) ""
      r0 `shouldBe` Right Nothing
      rE ← evalDuktape (fromJust ctx) "print('hello"
      rE `shouldBe` Left "SyntaxError: unterminated string (line 1)"
      rT ← evalDuktape (fromJust ctx) "1 == 1"
      rT `shouldBe` (Right $ Just $ Bool True)
      rF ← evalDuktape (fromJust ctx) "1 == 2"
      rF `shouldBe` (Right $ Just $ Bool False)
      rN ← evalDuktape (fromJust ctx) "12345"
      rN `shouldBe` (Right $ Just $ Number 12345)
      rS ← evalDuktape (fromJust ctx) "'string'"
      rS `shouldBe` (Right $ Just $ String "string")
      rA ← evalDuktape (fromJust ctx) "[1,2,'string',{what:'ever'}]"
      rA `shouldBe` (Right $ Just [json|[1,2,"string",{"what":"ever"}]|])
      rO ← evalDuktape (fromJust ctx) "({what: ['ever', 1, 2, 3]})"
      rO `shouldBe` (Right $ Just [json|{"what":["ever", 1, 2, 3]}|])

  describe "callDuktape" $ do
    it "calls ECMAScript functions" $ do
      ctx ← createDuktapeCtx
      _ ← evalDuktape (fromJust ctx) [r|
function returnsObj () { return {hello: {from: [1, 2, 3]}} }
function throwsErr () { throw new Error('hi') }
|]
      rE ← callDuktape (fromJust ctx) Nothing "n0thing!" []
      rE `shouldBe` (Left "TypeError: undefined not callable (property 'n0thing!' of [object global])")
      rO ← callDuktape (fromJust ctx) Nothing "returnsObj" []
      rO `shouldBe` (Right $ Just [json|{"hello": {"from": [1, 2, 3]}}|])
      rT ← callDuktape (fromJust ctx) Nothing "throwsErr" []
      rT `shouldBe` (Left "Error: hi")

    it "calls ECMAScript functions on objects" $ do
      ctx ← createDuktapeCtx
      _ ← evalDuktape (fromJust ctx) [r|
var Stuff = { x: 'testFromThis' }
Stuff.fun = function () { return this.x }
|]
      rS ← callDuktape (fromJust ctx) (Just "Stuff") "fun" []
      rS `shouldBe` (Right $ Just $ String "testFromThis")
      rE ← callDuktape (fromJust ctx) (Just "failNonExistent") "fun" []
      rE `shouldBe` (Left "Nonexistent property of global object: \"failNonExistent\"")

    it "calls ECMAScript functions with args" $ do
      ctx ← createDuktapeCtx
      _ ← evalDuktape (fromJust ctx) [r|
function double (x, y) { return x*2 + y*2 }
function awesomeString (x) { return '-= ' + x + ' =-' }
function boolTest (x, y, z) { return typeof(x) === 'boolean' && x && (y && !z) }
function arrTest (a) { return a.map(function (el) { return typeof(el) }).join('') }
function objTest (obj) { return obj.name + obj.stuff.filter(function (x) { return x != null && (typeof(x) == 'string' || typeof(x) == 'object') })
                                                    .map(function(x) { return typeof(x) == 'object' ? x.value : x } ).join('') }
|]
      rN ← callDuktape (fromJust ctx) Nothing "double" [Number 7, Number 8]
      rN `shouldBe` (Right $ Just $ Number 30)
      rS ← callDuktape (fromJust ctx) Nothing "awesomeString" [String "hello"]
      rS `shouldBe` (Right $ Just $ String "-= hello =-")
      rB ← callDuktape (fromJust ctx) Nothing "boolTest" [Bool True, Bool True, Bool False]
      rB `shouldBe` (Right $ Just $ Bool True)
      rA ← callDuktape (fromJust ctx) Nothing "arrTest" [[json|[1, 2, "test", null]|]]
      rA `shouldBe` (Right $ Just $ String "numbernumberstringobject")
      rO ← callDuktape (fromJust ctx) Nothing "objTest" [[json|{"stuff": [1, 2, "hello", null, {"value": "world"}], "name": "test"}|]]
      rO `shouldBe` (Right $ Just $ String "testhelloworld")

  describe "exposeFnDuktape" $ do
    it "exposes Haskell functions to ECMAScript" $ do
      ctx ← createDuktapeCtx
      _ ← evalDuktape (fromJust ctx) "var X = {}"
      let dbl x = return $ Number $ x * 2 ∷ IO Value
          add x y = return $ x + y ∷ IO Integer
          cnst = return $ 123 ∷ IO Integer
      _ ← exposeFnDuktape (fromJust ctx) Nothing "double" dbl
      _ ← exposeFnDuktape (fromJust ctx) (Just "X") "cnst" cnst
      _ ← exposeFnDuktape (fromJust ctx) (Just "X") "add" add
      rd ← evalDuktape (fromJust ctx) "double(7) + X.cnst() + X.add(3,5)"
      rd `shouldBe` (Right $ Just $ Number 145)
      rD ← callDuktape (fromJust ctx) Nothing "double" [Number 7]
      rD `shouldBe` (Right $ Just $ Number 14)
      rE ← callDuktape (fromJust ctx) Nothing "double" []
      rE `shouldBe` (Left "TypeError: error (rc -6)")
      rF ← evalDuktape (fromJust ctx) "try { X.add(undefined, \"wtf\") } catch (e) { 0; }"
      rF `shouldBe` (Right $ Just $ Number 0)

  describe "createGovernedHeap" $ do
    it "applies CheckTimeout to signal the runtime to terminate with RangeError" $ do
      guvnor ← allowQuarterSecond
      ctx ← createGovernedHeap nullFunPtr nullFunPtr nullFunPtr guvnor nullFunPtr
      rE ← evalDuktape (fromJust ctx) "while(true) {};"
      rE `shouldBe` Left "RangeError: execution timeout"

  describe "modules" $ do
    it "is supported" $ do
      Just ctx <- createDuktapeCtx
      -- expose the shim client code will use to implement 'require':
      let hs_modsearch :: T.Text -> IO T.Text
          hs_modsearch = \case
            "some_module" -> return "module.exports = {a: 1}"
            "some_other_module" -> return "exports.a = 2"
            -- NOTE: duktape only supports commonjs-style modules, not ES modules like:
            -- "some_module" -> return "export const a = 41"
            _ -> error "unimplemented"
      exposeFnDuktape ctx Nothing "hs_modsearch" hs_modsearch >>= \case
        Right () -> return ()
        Left err -> error err

      defsOut ← evalDuktape ctx [r|
Duktape.modSearch = hs_modsearch;
const m1 = require('some_module');
const m2 = require('some_other_module');
function test_module (x) { return (m1.a + m2.a + x) }
// TODO: hmmm, it looks like NaN is corrupted? 
// function test_module (x) { return (m1.doesnotexist + x) }
|]
      defsOut `shouldBe` (Right Nothing)

      out ← callDuktape ctx Nothing "test_module" [Number 39]
      out `shouldBe` (Right $ Just $ Number 42)
--------------------------------
{- TODO we might actually add typescript.js and do this
    it "ts" $ do
      Just ctx <- createDuktapeCtx
      -- expose the shim client code will use to implement 'require':
      let hs_modsearch :: T.Text -> IO T.Text
          hs_modsearch = \case
            "ts" -> do
                typescript_js <- T.readFile "../duktape-typescript-hs/typescript_vendored/typescript.js"
                return $ typescript_js 
                       -- TODO is this the proper way to turn typescript.js into a module?:
                       <> ";\n module.exports = ts;"
            _ -> error "unimplemented"
      exposeFnDuktape ctx Nothing "hs_modsearch" hs_modsearch >>= \case
        Right () -> return ()
        Left err -> error err

      defsOut ← evalDuktape ctx [r|
Duktape.modSearch = hs_modsearch;
const ts = require('ts');
function test_module () { return ts.versionMajorMinor }
// TODO: hmmm, it looks like NaN is corrupted? 
// function test_module (x) { return (m.doesnotexist + x) }
|]
      defsOut `shouldBe` (Right Nothing)

      out ← callDuktape ctx Nothing "test_module" []
      out `shouldBe` (Right $ Just $ String "4.4")
-}

-- TODO
--   tests: interrupted
--   tests: warning: too many hs_exit()s
--

-- FIXME:
-- https://github.com/GavinRay97/haskell-scripted-wai/issues/1#issuecomment-1009128345 


allowQuarterSecond ∷ IO (IO Bool)
allowQuarterSecond = do
  initializedAt ← getCurrentTime
  return $ do
    now ← getCurrentTime
    let diff = diffUTCTime now initializedAt
    return $ diff > 0.25
