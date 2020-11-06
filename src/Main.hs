{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

import Types
import Control.Lens
import qualified Data.Text as T
import Criterion
import Criterion.Main

colorOfPickup :: Lens' GuitarElectronics Pickup -> Lens' Notification T.Text
colorOfPickup pickupL =
    not_deliveredToL . md_ownerL . p_guitarL . g_bodyL .
    gb_electronicsL . pickupL . p_coverL . pc_colorL

colorOfPickupDirect :: (GuitarElectronics -> Pickup) -> Notification -> T.Text
colorOfPickupDirect pickup =
    pc_color . p_cover . pickup . gb_electronics . g_body . p_guitar . md_owner .
    not_deliveredTo

sampleAppointment :: Appointment
sampleAppointment =
    Appointment
    { app_title = "FOO"
    , app_notification =
        Notification
        { not_title = "BAR"
        , not_deliveredTo =
            MobileDevice
            { md_brand = "Apple"
            , md_owner =
                Person
                { p_name = "Stefan"
                , p_guitar =
                    Guitar
                    { g_model = "Les Paul"
                    , g_body =
                        GuitarBody
                        { gb_color = "red"
                        , gb_electronics =
                            GuitarElectronics
                            { ge_inputJack = InputJack
                            , ge_frontPickup = mkPickup "black"
                            , ge_middlePickup = mkPickup "creme"
                            , ge_rearPickup = mkPickup "white"
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

loadAppointment :: IO Appointment
loadAppointment = do
  appStr <- readFile "data.txt"
  let !app = read appStr
  return app

benchmarkGetLens :: Notification -> [T.Text]
benchmarkGetLens app =
    [ app ^. (colorOfPickup ge_frontPickupL)
    , app ^. (colorOfPickup ge_rearPickupL)
    , app ^. (colorOfPickup ge_middlePickupL)
    ]

benchmarkSetLens :: Notification -> ()
benchmarkSetLens app =
    (colorOfPickup ge_frontPickupL .~ "value01") app `seq`
    (colorOfPickup ge_rearPickupL .~ "value02") app `seq`
    (colorOfPickup ge_middlePickupL .~ "value03") app `seq`
    ()

benchmarkGetDirect :: Notification -> [T.Text]
benchmarkGetDirect app =
    [ colorOfPickupDirect ge_frontPickup app
    , colorOfPickupDirect ge_rearPickup app
    , colorOfPickupDirect ge_middlePickup app
    ]

setPickupColor ::
    Notification
    -> (GuitarElectronics -> Pickup)
    -> (GuitarElectronics -> Pickup -> GuitarElectronics)
    -> T.Text
    -> Notification
setPickupColor not getPickup setPickup color =
    f1 not
  where
    f1 not =
        not { not_deliveredTo = f2 (not_deliveredTo not) }
    f2 md =
        md { md_owner = f3 (md_owner md) }
    f3 p =
        p { p_guitar = f4 (p_guitar p) }
    f4 g =
        g { g_body = f5 (g_body g) }
    f5 b =
        b { gb_electronics = f6 (gb_electronics b) }
    f6 e =
        setPickup e (f7 (getPickup e))
    f7 p =
       p { p_cover = f8 (p_cover p) }
    f8 pc =
        pc { pc_color = color }

benchmarkSetDirect :: Notification -> ()
benchmarkSetDirect app =
    setPickupColor app ge_frontPickup (\e p -> e { ge_frontPickup = p }) "value01" `seq`
    setPickupColor app ge_rearPickup (\e p -> e { ge_rearPickup = p }) "value02" `seq`
    setPickupColor app ge_middlePickup (\e p -> e { ge_middlePickup = p }) "value03" `seq`
    ()

main :: IO ()
main = do
  writeFile "data.txt" (show sampleAppointment)
  defaultMain
      [
       env loadAppointment $ \(app_notification -> not) ->
           bgroup "main"
               [
                 bench "getLens" (nf benchmarkGetLens not)
               , bench "setLens" (nf benchmarkSetLens not)
               , bench "getDirect" (nf benchmarkGetDirect not)
               , bench "setDirect" (nf benchmarkSetDirect not)
               ]
      ]
