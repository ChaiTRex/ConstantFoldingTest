{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE Strict, TemplateHaskell #-}

module ThisGHC ( environment
               , thisGHC, thisGHCLibdirArg, prependThisGHC
               , runHaskellFile
               ) where

import Control.Arrow              (first, second)
import GHC.Environment            (getFullArgs)
import Language.Haskell.TH.Syntax ( Q, lift, returnQ
                                  , mkName
                                  , Exp( AppE, ConE, InfixE, LamE, ListE, LitE
                                       , RecUpdE, TupE, VarE
                                       )
                                  , Lit(StringL)
                                  , Pat(ConP, VarP)
                                  )
import System.Environment         (getEnvironment)
import System.Exit                (ExitCode)
import System.IO.Unsafe           (unsafePerformIO)
import System.Process             ( CreateProcess(env, std_in, std_out, std_err)
                                  , StdStream(Inherit, NoStream)
                                  , proc, waitForProcess, withCreateProcess
                                  )

environment' :: [(String, String)]
{-# NOINLINE environment' #-}
environment' = unsafePerformIO getEnvironment

environment :: Q Exp
{-# INLINE environment #-}
environment = lift environment'

compilationCommand :: [String]
{-# NOINLINE compilationCommand #-}
compilationCommand = unsafePerformIO getFullArgs

thisGHC' :: String
{-# NOINLINE thisGHC' #-}
thisGHC' = head compilationCommand

thisGHC :: Q Exp
{-# INLINE thisGHC #-}
thisGHC = lift thisGHC'

thisGHCLibdirArg' :: Maybe String
{-# NOINLINE thisGHCLibdirArg' #-}
thisGHCLibdirArg' = lastLibdir Nothing $ tail compilationCommand
  where
    lastLibdir :: Maybe String -> [String] -> Maybe String
    {-# INLINABLE lastLibdir #-}
    lastLibdir r []                  = r
    lastLibdir r (xs@('-':'B':_):ys) = lastLibdir (Just xs) ys
    lastLibdir r (    _         :ys) = lastLibdir r         ys

thisGHCLibdirArg :: Q Exp
{-# INLINE thisGHCLibdirArg #-}
thisGHCLibdirArg = lift thisGHCLibdirArg'

prependThisGHC' :: [String] -> [String]
{-# NOINLINE prependThisGHC' #-}
prependThisGHC' args = case thisGHCLibdirArg' of
                            Just libdir -> (thisGHC' :) . (libdir :) $ args
                            _           -> (thisGHC' :)              $ args

prependThisGHC :: [String] -> Q Exp
{-# INLINE prependThisGHC #-}
prependThisGHC = lift . prependThisGHC'

runHaskellFile :: Q Exp
-- $( runHaskellFile ) :: FilePath -> IO ExitCode
{-# INLINE runHaskellFile #-}
runHaskellFile =
  let args' = [ "-no-keep-hi-files"
              , "-no-keep-o-files"
              , "-fobject-code"
              , "-ignore-dot-ghci"
              , "-e"
              , "main"
              ]
      args  = map (LitE . StringL) $ case thisGHCLibdirArg' of
                                          Just libdir -> libdir:args'
                                          _           ->        args'
      envir = map (\ (x, y) -> TupE [LitE (StringL x), LitE (StringL y)])
                 environment'
  in  returnQ $
        LamE [VarP (mkName "path")]
             (AppE (AppE (VarE 'withCreateProcess)
                         (RecUpdE (AppE (AppE (VarE 'proc)
                                              (LitE (StringL thisGHC'))
                                        )
                                        (InfixE (Just (VarE (mkName "path")))
                                                (ConE '(:))
                                                (Just (ListE args))
                                        )
                                  )
                                  [ ( 'env
                                    , AppE (ConE 'Just)
                                           (ListE envir)
                                    )
                                  , ( 'std_in
                                    , ConE 'Inherit
                                    )
                                  , ( 'std_out
                                    , ConE 'Inherit
                                    )
                                  , ( 'std_err
                                    , ConE 'Inherit
                                    )
                                  ]
                         )
                   )
                   (LamE [ ConP 'Nothing []
                         , ConP 'Nothing []
                         , ConP 'Nothing []
                         , VarP (mkName "p")
                         ]
                         (AppE (VarE 'waitForProcess)
                               (VarE (mkName "p"))
                         )
                   )
             )

