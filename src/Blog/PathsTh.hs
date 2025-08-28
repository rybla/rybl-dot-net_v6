{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.PathsTh where

import Control.Lens ((&))
import Data.Char (toLower, toUpper)
import Data.Functor ((<&>))
import Data.Tree (Tree (..))
import Language.Haskell.TH
import System.FilePath

data MakeRootParams = MakeRootParams
  { valName :: String,
    nodeType :: TypeQ,
    toNode :: ExpQ -- String -> $nodeType
  }

makeRoot :: [MakeRootParams] -> [Tree String] -> DecsQ
makeRoot list_params = makeAllDecs
  where
    capitalize s = (toUpper <$> take 1 s) ++ drop 1 s
    uncapitalize s = (toLower <$> take 1 s) ++ drop 1 s

    rootLabel :: String
    rootLabel = "root"

    makeDataName :: String -> Name
    makeDataName s = mkName (capitalize s)

    makeFieldName :: String -> Name
    makeFieldName s = mkName (uncapitalize s)

    makeValName :: MakeRootParams -> Name
    makeValName prms = mkName prms.valName

    makeAllDecs :: [Tree String] -> DecsQ
    makeAllDecs ts =
      fmap concat . sequence $
        [ makeDataDecs rootLabel ts,
          [makeValDec prms ts | prms <- list_params] & sequence <&> concat
        ]

    dataVarName :: Name
    dataVarName = mkName "x"

    makeDataDecs :: String -> [Tree String] -> DecsQ
    makeDataDecs s0 ts0 = do
      dec <- go s0 ts0
      decs <-
        ts0
          <&> (\t -> (t.rootLabel, t.subForest))
          & traverse (uncurry makeDataDecs)
          <&> concat
      return (dec : decs)
      where
        go :: String -> [Tree String] -> DecQ
        go s ts =
          dataD
            mempty
            (makeDataName s)
            [plainTV dataVarName]
            Nothing
            [ recC
                (makeDataName s)
                ( varBangType
                    (mkName "here")
                    (bangType (bang sourceNoUnpack sourceStrict) (varT dataVarName))
                    : ( ts <&> \t' ->
                          varBangType
                            (makeFieldName t'.rootLabel)
                            (bangType (bang sourceNoUnpack sourceStrict) (conT (makeDataName t'.rootLabel) `appT` varT dataVarName))
                      )
                )
            ]
            [derivClause Nothing [[t|Show|]]]

    makeValDec :: MakeRootParams -> [Tree String] -> DecsQ
    makeValDec prms ts0 = do
      sequence
        [ sigD
            (makeValName prms)
            (conT (makeDataName rootLabel) `appT` prms.nodeType),
          valD
            (varP (makeValName prms))
            (normalB (go "" (rootLabel, "") ts0))
            []
        ]
      where
        go :: String -> (String, String) -> [Tree String] -> ExpQ
        go prefix (k, v) ts =
          appsE
            ( conE (makeDataName k)
                : (appE prms.toNode (stringE (prefix </> v)))
                : ( uncurry (go (prefix </> v))
                      <$> (ts <&> \t' -> ((t'.rootLabel, t'.rootLabel), t'.subForest))
                  )
            )
