{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

data ConsulClient = ConsulClient { ccHostname :: String
                                 , ccPort :: Int
                                 }

modFunc :: String -> Int -> Int
modFunc str x = x * (length str)

main = do
  dec <- runQ [d| myFunc _consul@ConsulClient{..} = modFunc ccHostname ccPort |]
  print dec

-- Output: [FunD myFunc_0 [Clause [AsP _consul_1 (RecP Main.ConsulClient [(Main.ccHostname,VarP ccHostname_2),(Main.ccPort,VarP ccPort_3)])] (NormalB (AppE (AppE (VarE Main.modFunc) (VarE ccHostname_2)) (VarE ccPort_3))) []]]
