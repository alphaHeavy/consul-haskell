{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

data ConsulClient = ConsulClient { ccHostname :: String
                                 , ccPort :: Int
                                 }

modFunc :: String -> Int -> Int
modFunc str x = x * (length str)

main = do
  -- dec <- runQ [d| myFunc _consul@ConsulClient{..} = modFunc ccHostname ccPort |]
  dec <- runQ [d| myFunc _consul = modFunc (ccHostname _consul) (ccPort _consul) |]
  print dec

-- Output with as-pattern: [FunD myFunc_0 [Clause [AsP _consul_1 (RecP Main.ConsulClient [(Main.ccHostname,VarP ccHostname_2),(Main.ccPort,VarP ccPort_3)])] (NormalB (AppE (AppE (VarE Main.modFunc) (VarE ccHostname_2)) (VarE ccPort_3))) []]]
-- Output without as-pattern: [FunD myFunc_0 [Clause [VarP _consul_1] (NormalB (AppE (AppE (VarE Main.modFunc) (AppE (VarE Main.ccHostname) (VarE _consul_1))) (AppE (VarE Main.ccPort) (VarE _consul_1)))) []]]
