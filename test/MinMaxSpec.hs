{-# language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module MinMaxSpec where

import Test.Hspec (Spec, describe, it, xit)
import Control.OutputCapable.Blocks (LangM)
import Test.QuickCheck (Gen, chooseAny, forAll)
import qualified LogicTasks.Semantics.Max as Max (verifyQuiz, verifyStatic, genMaxInst, description, partialGrade', completeGrade')
import qualified LogicTasks.Semantics.Min as Min (verifyQuiz, verifyStatic, genMinInst, description, partialGrade', completeGrade')
import Config (
  MinMaxConfig(..),
  dMinMaxConf,
  MaxInst (cnf),
  MinInst (dnf),
  )
import FillSpec (validBoundsNormalFormConfig)
import TestHelpers (doesNotRefuse)



validBoundsMinMaxConfig :: Gen MinMaxConfig
validBoundsMinMaxConfig = do
  normalFormConf <- validBoundsNormalFormConfig
  offerUnicodeInput <- chooseAny
  printSolution <- chooseAny
  pure $ MinMaxConfig
    { normalFormConf = normalFormConf
    -- Restrictions on this lead to infinite loops.
    -- A satisfying formula is frequently not found, even with large intervals.
    , percentTrueEntries = Nothing
    , printSolution
    , extraText = Nothing
    , offerUnicodeInput
    }


spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (Max.verifyQuiz dMinMaxConf :: LangM Maybe) &&
      doesNotRefuse (Min.verifyQuiz dMinMaxConf :: LangM Maybe)
    it "validBoundsMinMaxConfig should generate a valid config" $
      forAll validBoundsMinMaxConfig $ \minMaxConfigConfig ->
        doesNotRefuse (Max.verifyQuiz minMaxConfigConfig :: LangM Maybe) &&
        doesNotRefuse (Min.verifyQuiz minMaxConfigConfig :: LangM Maybe)
  describe "description" $ do
    it "should not reject - Max" $
      forAll validBoundsMinMaxConfig $ \config ->
        forAll (Max.genMaxInst config) $ \inst ->
          doesNotRefuse (Max.description inst :: LangM Maybe)
    it "should not reject - Min" $
      forAll validBoundsMinMaxConfig $ \config ->
        forAll (Min.genMinInst config) $ \inst ->
          doesNotRefuse (Min.description inst :: LangM Maybe)
  describe "generateInst" $ do
    it "should pass verifyStatic - Max" $
      forAll validBoundsMinMaxConfig $ \config ->
        forAll (Max.genMaxInst config) $ \inst ->
          doesNotRefuse
            (Max.verifyStatic inst :: LangM Maybe)
    it "should pass verifyStatic - Min" $
      forAll validBoundsMinMaxConfig $ \config ->
        forAll (Min.genMinInst config) $ \inst ->
          doesNotRefuse
            (Min.verifyStatic inst :: LangM Maybe)
    xit "possible solution passes partialGrade - Max" $
      forAll (Max.genMaxInst dMinMaxConf) $ \inst ->
        doesNotRefuse
          (Max.partialGrade' inst $ cnf inst :: LangM Maybe)
    xit "possible solution passes partialGrade - Min" $
      forAll (Min.genMinInst dMinMaxConf) $ \inst ->
        doesNotRefuse
          (Min.partialGrade' inst $ dnf inst :: LangM Maybe)
    xit "possible solution passes completeGrade - Max" $
      forAll (Max.genMaxInst dMinMaxConf) $ \inst ->
        doesNotRefuse
          (Max.completeGrade' inst $ cnf inst :: LangM Maybe)
    xit "possible solution passes completeGrade - Min" $
      forAll (Min.genMinInst dMinMaxConf) $ \inst ->
        doesNotRefuse
          (Min.completeGrade' inst $ dnf inst :: LangM Maybe)
