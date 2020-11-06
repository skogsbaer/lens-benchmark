{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

import Types
import Control.Lens
import qualified Data.Text as T
import Criterion
import Criterion.Main

colorOfPickup :: Lens' BassElectronics Pickup -> Lens' AllStores T.Text
colorOfPickup pickupL =
    as_musicStoreL . ms_gitAndBassDepartmentL . gb_bassSubDepartmentL . bd_4stringL . b_bodyL .
    bb_electronicsL . pickupL . p_coverL . pc_colorL

colorOfPickupDirect :: (BassElectronics -> Pickup) -> AllStores -> T.Text
colorOfPickupDirect pickup =
    pc_color . p_cover . pickup . bb_electronics . b_body . bd_4string .
    gb_bassSubDepartment . ms_gitAndBassDepartment . as_musicStore

sampleStores :: AllStores
sampleStores =
    AllStores
    { as_grocery = Grocery
    , as_musicStore =
        MusicStore
        { ms_name = "Thomann"
        , ms_gitAndBassDepartment =
            GitAndBassDepartment
            { gb_name = "Git & Bass"
            , gb_gitSubDepartment = GitDepartment
            , gb_bassSubDepartment =
                BassDepartment
                { bd_5string = Bass5
                , bd_4string =
                    Bass
                    { b_model = "Musicman Big Al"
                    , b_body =
                        BassBody
                        { bb_color = "red"
                        , bb_electronics =
                            BassElectronics
                            { be_inputJack = InputJack
                            , be_frontPickup = mkPickup "black"
                            , be_middlePickup = mkPickup "creme"
                            , be_rearPickup = mkPickup "white"
                            }
                        }
                    }
                }
            }
        }
    }
    where
      mkPickup color =
          Pickup
          { p_cover =
                PickupCover
                { pc_color = color }
          , p_windings = 10000
          }

loadStores :: IO AllStores
loadStores = do
  str <- readFile "data.txt"
  let !x = read str
  return x

benchmarkGetLens :: AllStores -> [T.Text]
benchmarkGetLens app =
    [ app ^. (colorOfPickup be_frontPickupL)
    , app ^. (colorOfPickup be_rearPickupL)
    , app ^. (colorOfPickup be_middlePickupL)
    ]

benchmarkSetLens :: AllStores -> ()
benchmarkSetLens app =
    (colorOfPickup be_frontPickupL .~ "value01") app `seq`
    (colorOfPickup be_rearPickupL .~ "value02") app `seq`
    (colorOfPickup be_middlePickupL .~ "value03") app `seq`
    ()

benchmarkGetDirect :: AllStores -> [T.Text]
benchmarkGetDirect app =
    [ colorOfPickupDirect be_frontPickup app
    , colorOfPickupDirect be_rearPickup app
    , colorOfPickupDirect be_middlePickup app
    ]

setPickupColor ::
    AllStores
    -> (BassElectronics -> Pickup)
    -> (BassElectronics -> Pickup -> BassElectronics)
    -> T.Text
    -> AllStores
setPickupColor as getPickup setPickup color =
    as { as_musicStore = f0 (as_musicStore as) }
  where
    f0 x = x { ms_gitAndBassDepartment = f1 (ms_gitAndBassDepartment x) }
    f1 x = x { gb_bassSubDepartment = f2 (gb_bassSubDepartment x) }
    f2 x = x { bd_4string = f3 (bd_4string x) }
    f3 x = x { b_body = f4 (b_body x) }
    f4 x = x { bb_electronics = f5 (bb_electronics x) }
    f5 e = setPickup e (f6 (getPickup e))
    f6 p = p { p_cover = f7 (p_cover p) }
    f7 pc = pc { pc_color = color }

benchmarkSetDirect :: AllStores -> ()
benchmarkSetDirect x =
    setPickupColor x be_frontPickup (\e p -> e { be_frontPickup = p }) "value01" `seq`
    setPickupColor x be_rearPickup (\e p -> e { be_rearPickup = p }) "value02" `seq`
    setPickupColor x be_middlePickup (\e p -> e { be_middlePickup = p }) "value03" `seq`
    ()

main :: IO ()
main = do
  writeFile "data.txt" (show sampleStores)
  defaultMain
      [
       env loadStores $ \x ->
           bgroup "main"
               [
                 bench "getLens" (nf benchmarkGetLens x)
               , bench "setLens" (nf benchmarkSetLens x)
               , bench "getDirect" (nf benchmarkGetDirect x)
               , bench "setDirect" (nf benchmarkSetDirect x)
               ]
      ]
