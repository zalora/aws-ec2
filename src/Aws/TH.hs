{-# LANGUAGE TemplateHaskell
           , CPP
           #-}

module Aws.TH where

import Control.Applicative ((<$>))
import Data.Monoid (mconcat)
import Language.Haskell.TH

derivePatchedShowRead :: Name -> (String -> String) -> Q [Dec]
derivePatchedShowRead name patch = do
#if MIN_VERSION_template_haskell(2,11,0)
    TyConI (DataD _ _ _ _ cons _) <- reify name
#else
    TyConI (DataD _ _ _ cons _) <- reify name
#endif
    let names = (\(NormalC name []) -> name) <$> cons
    Just show <- lookupValueName "show"
    showD <- instanceD (cxt []) (appT (conT ''Show) (conT name)) [fun show names]

    Just read <- lookupValueName "readsPrec"
    Just ret <- lookupValueName "return"
    Just err <- lookupValueName "error"
    Just concat <- lookupValueName "mconcat"
    readD <- instanceD (cxt []) (appT (conT ''Read) (conT name)) [funr read (varE ret) (varE err) (varE concat) names]

    return [showD, readD]
  where
    fun show names = funD show $ (\n -> clause [conP n []] (normalB $ litE $ stringL $ patch $ nameBase n) []) <$> names
    funr read ret err concat names =
      funD read $ mconcat [ (\n -> clause [wildP, litP $ stringL $ patch $ nameBase n]
                                          (normalB $ appE ret $ tupE [conE n, litE (StringL "")]) []
                            ) <$> names
                          , [let any = mkName "any"
                              in clause [wildP, varP any]
                                        (normalB $ appE err $ appE concat $ listE [ litE (StringL "unknown ")
                                                                                  , litE (StringL $ nameBase name)
                                                                                  , litE (StringL ": ")
                                                                                  , varE any
                                                                                  ]) []]
                          ]

patchPer s = go s False
  where
    go [] started = []
    go ('P':'e':'r':xs) started@False = 'P' : 'e' : 'r' : go xs started
    go ('P':'e':'r':xs) started@True = '/' : go xs started
    go (x:xs) _ = x : go xs True

