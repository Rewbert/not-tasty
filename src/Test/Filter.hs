{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Test.Filter (mark, sweep) where

import Language.Haskell.TH.Syntax hiding (nothingName, justName)

-- * the exposed API

{- 

example generation:

mark [1]
====>
{-# RULES ""sweep/1"" [~2]
              forall ioa. sweep 1 ioa
                = fmap Just ioa #-}
{-# RULES ""sweep/prune"" [2]
              forall n ioa. sweep n ioa
                = return Nothing #-}

-}

-- | sweep n ioa associates the action ioa with the group n
{-# NOINLINE sweep #-}
sweep :: Monad m => Int -> m a -> m (Maybe a)
sweep n ioa = fmap Just ioa

-- | generate rewrite rules that only retains the actions of the group
-- identified by n
mark :: [Int] -> Q [Dec]
mark ns = return $ ruleForPruning : map ruleForRunning ns

-- * template haskell the generates the rules

pattern AppE3 :: Exp -> Exp -> Exp -> Exp
pattern AppE3 e1 e2 e3 = AppE (AppE e1 e2) e3

-- | The rule that will actually keep the computation around
ruleForRunning :: Int -> Dec
ruleForRunning n = PragmaD $ RuleP name Nothing [RuleVar ioaName] lhs rhs (BeforePhase 2)
  where
    name :: String
    name = concat ["\"sweep/", show n, "\""]

    lhs :: Exp
    lhs = AppE3 (VarE sweepName) (LitE (IntegerL $ toInteger n)) (VarE ioaName)

    rhs :: Exp
    rhs = AppE3 (VarE fmapName) justE (VarE ioaName)

ruleForPruning :: Dec
ruleForPruning = PragmaD $ RuleP name Nothing [RuleVar nName, RuleVar ioaName] lhs rhs (FromPhase 2)
  where
    name :: String
    name = concat ["\"sweep/prune\""]

    lhs :: Exp
    lhs = AppE3 (VarE sweepName) (VarE nName) (VarE ioaName)

    rhs :: Exp
    rhs = AppE (VarE returnName) nothingE

-- * names

sweepName :: Name
sweepName = mkName "sweep"

ioaName :: Name
ioaName = mkName "ioa"

nName :: Name
nName = mkName "n"

fmapName :: Name
fmapName = mkName "fmap"

justName :: Name
justName = mkName "Just"

nothingName :: Name
nothingName = mkName "Nothing"

returnName :: Name
returnName = mkName "return"

-- * expressions

nothingE :: Exp
nothingE = ConE nothingName

justE :: Exp
justE = ConE justName