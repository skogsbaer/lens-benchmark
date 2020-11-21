{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

-- import TypesLazy
import TypesStrict

import Control.Lens
import qualified Data.Text as T
import Criterion
import Criterion.Main
import Control.DeepSeq
import System.Random

{-# INLINE colorOfPickup #-}
colorOfPickup :: Lens' BassElectronics Pickup -> Lens' AllStores T.Text
colorOfPickup pickupL =
    as_musicStoreL . ms_gitAndBassDepartmentL . gb_bassSubDepartmentL . bd_4stringL . b_bodyL .
    bb_electronicsL . pickupL . p_coverL . pc_colorL

-- {-# INLINE colorOfPickupDirect #-}
colorOfPickupDirect :: (BassElectronics -> Pickup) -> AllStores -> T.Text
colorOfPickupDirect pickup =
    pc_color . p_cover . pickup . bb_electronics . b_body . bd_4string .
    gb_bassSubDepartment . ms_gitAndBassDepartment . as_musicStore

colorOfPickup' :: Lens' BassElectronics Pickup -> Lens' MusicStore T.Text
colorOfPickup' pickupL =
    ms_gitAndBassDepartmentL . gb_bassSubDepartmentL . bd_4stringL . b_bodyL .
    bb_electronicsL . pickupL . p_coverL . pc_colorL

colorOfPickupDirect' :: (BassElectronics -> Pickup) -> MusicStore -> T.Text
colorOfPickupDirect' pickup =
    pc_color . p_cover . pickup . bb_electronics . b_body . bd_4string .
    gb_bassSubDepartment . ms_gitAndBassDepartment

sampleStores :: AllStores
sampleStores =
    AllStores
    { as_grocery = "Grocery"
    , as_musicStore =
        MusicStore
        { ms_name = "Thomann"
        , ms_gitAndBassDepartment =
            GitAndBassDepartment
            { gb_name = "Git & Bass"
            , gb_gitSubDepartment = GitDepartment
            , gb_bassSubDepartment =
                BassDepartment
                { bd_name = "private reserve"
                , bd_5string = Bass5
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

loadStores :: IO (AllStores, AllArgs)
loadStores = do
  str <- readFile "data.txt"
  let x = read str
  args <- randomArgs
  x `deepseq` return (x, args)

newtype LensArg = LensArg (Lens' BassElectronics Pickup)

instance NFData LensArg where
    rnf (LensArg _) = ()

type AllLensArgs = (LensArg, LensArg, LensArg)
type DirectGetArgs = BassElectronics -> Pickup
type AllDirectGetArgs = (DirectGetArgs, DirectGetArgs, DirectGetArgs)
type DirectSetArgs = (BassElectronics -> Pickup -> BassElectronics)
type AllDirectSetArgs = (DirectSetArgs, DirectSetArgs, DirectSetArgs)
type AllArgs = (AllLensArgs, AllDirectGetArgs, AllDirectSetArgs)

randomArgs :: IO AllArgs
randomArgs = do
  (lens1, getter1, setter1) <- getRandom
  (lens2, getter2, setter2) <- getRandom
  (lens3, getter3, setter3) <- getRandom
  return ( (lens1, lens2, lens3)
         , (getter1, getter2, getter3)
         , (setter1, setter2, setter3))
  where
    getRandom = do
      x <- randomRIO (1::Int, 3)
      case x of
        1 -> return (LensArg be_frontPickupL, be_frontPickup, (\e p -> e { be_frontPickup = p }))
        2 -> return (LensArg be_middlePickupL, be_middlePickup, (\e p -> e { be_middlePickup = p }))
        3 -> return (LensArg be_rearPickupL, be_rearPickup, (\e p -> e { be_rearPickup = p }))
        _ -> error ("unexpected random number: " ++ show x)

benchmarkGetLens :: LensArg -> LensArg -> LensArg -> AllStores -> ()
benchmarkGetLens (LensArg l1) (LensArg l2) (LensArg l3) x =
    (x ^. (colorOfPickup l1)) `seq`
    (x ^. (colorOfPickup l2)) `seq`
    (x ^. (colorOfPickup l3)) `seq`
    ()

benchmarkGetLensSmall :: LensArg -> LensArg -> LensArg -> MusicStore -> ()
benchmarkGetLensSmall (LensArg l1) (LensArg l2) (LensArg l3) x =
    (x ^. (colorOfPickup' l1)) `seq`
    (x ^. (colorOfPickup' l2)) `seq`
    (x ^. (colorOfPickup' l3)) `seq`
    ()

benchmarkSetLens :: LensArg -> LensArg -> LensArg -> AllStores -> ()
benchmarkSetLens (LensArg l1) (LensArg l2) (LensArg l3) x =
    (((colorOfPickup l1 .~ "value01") x) ^. colorOfPickup l2) `seq`
    (((colorOfPickup l2 .~ "value02") x) ^. colorOfPickup l3) `seq`
    (((colorOfPickup l3 .~ "value03") x) ^. colorOfPickup l1) `seq` ()

benchmarkSetLensSmall :: LensArg -> LensArg -> LensArg -> MusicStore -> ()
benchmarkSetLensSmall (LensArg l1) (LensArg l2) (LensArg l3) x =
    (((colorOfPickup' l1 .~ "value01") x) ^. colorOfPickup' l2) `seq`
    (((colorOfPickup' l2 .~ "value02") x) ^. colorOfPickup' l3) `seq`
    (((colorOfPickup' l3 .~ "value03") x) ^. colorOfPickup' l1) `seq` ()

benchmarkGetDirect :: DirectGetArgs -> DirectGetArgs -> DirectGetArgs -> AllStores -> ()
benchmarkGetDirect g1 g2 g3 x =
    colorOfPickupDirect g1 x `seq`
    colorOfPickupDirect g2 x `seq`
    colorOfPickupDirect g3 x `seq`
    ()

benchmarkGetDirectSmall :: DirectGetArgs -> DirectGetArgs -> DirectGetArgs -> MusicStore -> ()
benchmarkGetDirectSmall g1 g2 g3 x =
    colorOfPickupDirect' g1 x `seq`
    colorOfPickupDirect' g2 x `seq`
    colorOfPickupDirect' g3 x `seq`
    ()

{-# INLINE setPickupColor #-}
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

{-# INLINE setPickupColor' #-}
setPickupColor' ::
    MusicStore
    -> (BassElectronics -> Pickup)
    -> (BassElectronics -> Pickup -> BassElectronics)
    -> T.Text
    -> MusicStore
setPickupColor' as getPickup setPickup color = f0 as
  where
    f0 x = x { ms_gitAndBassDepartment = f1 (ms_gitAndBassDepartment x) }
    f1 x = x { gb_bassSubDepartment = f2 (gb_bassSubDepartment x) }
    f2 x = x { bd_4string = f3 (bd_4string x) }
    f3 x = x { b_body = f4 (b_body x) }
    f4 x = x { bb_electronics = f5 (bb_electronics x) }
    f5 e = setPickup e (f6 (getPickup e))
    f6 p = p { p_cover = f7 (p_cover p) }
    f7 pc = pc { pc_color = color }

benchmarkSetDirect ::
    DirectGetArgs -> DirectSetArgs
    -> DirectGetArgs -> DirectSetArgs
    -> DirectGetArgs -> DirectSetArgs
    -> AllStores -> ()
benchmarkSetDirect g1 s1 g2 s2 g3 s3 x =
    (colorOfPickupDirect g2 $
              (setPickupColor x g1 s1 "value01")) `seq`
    (colorOfPickupDirect g3 $
              (setPickupColor x g2 s2 "value02")) `seq`
    (colorOfPickupDirect g1 $
              (setPickupColor x g3 s3 "value03")) `seq` ()

benchmarkSetDirectSmall ::
    DirectGetArgs -> DirectSetArgs
    -> DirectGetArgs -> DirectSetArgs
    -> DirectGetArgs -> DirectSetArgs
    -> MusicStore -> ()
benchmarkSetDirectSmall g1 s1 g2 s2 g3 s3 x =
    (colorOfPickupDirect' g2 $
              (setPickupColor' x g1 s1 "value01")) `seq`
    (colorOfPickupDirect' g3 $
              (setPickupColor' x g2 s2 "value02")) `seq`
    (colorOfPickupDirect' g1 $
              (setPickupColor' x g3 s3 "value03")) `seq` ()

main :: IO ()
main = do
  writeFile "data.txt" (show sampleStores)
  ((l1, l2, l3), (g1, g2, g3), (s1, s2, s3)) <- randomArgs
  defaultMain
      [
       env loadStores $ \arg -> case arg of
         ~(x, _) -> -- ((l1, l2, l3), (g1, g2, g3), (s1, s2, s3))) ->
           bgroup "main"
               [
                bench "getLens" (nf (benchmarkGetLens l1 l2 l3) x)
               , bench "getDirect" (nf (benchmarkGetDirect g1 g2 g3) x)
               , bench "getLensSmall" (nf (benchmarkGetLensSmall l1 l2 l3) (as_musicStore x))
               , bench "getDirectSmall"
                     (nf (benchmarkGetDirectSmall g1 g2 g3) (as_musicStore x))
               , bench "setLens" (nf (benchmarkSetLens l1 l2 l3) x)
               , bench "setDirect" (nf (benchmarkSetDirect g1 s1 g2 s2 g3 s3) x)
               , bench "setLensSmall" (nf (benchmarkSetLensSmall l1 l2 l3) (as_musicStore x))
               , bench "setDirectSmall"
                     (nf (benchmarkSetDirectSmall g1 s1 g2 s2 g3 s3) (as_musicStore x))
               ]
      ]
