import Data.List
import Control.Monad (forM_)

{- on day 1, these should be the values output
(defendingForces)DGT = 200,000
(attackingForces)AGT = 330,000
(attackersGroundProsecutionRate)aGT = 2.000
(attackersAttrition)aat = 4.531
(defendersWithdrawalRate)Wt = 0.0
(defendersAttrition)adt = 3.375
(frontDisplacement)sum w* = 0.0
(defendersAirSupport)DaT = 300
(attackersAirSupport)AaT = 250
g2gCasualityExchange)p = 1.50
defendersMaxWithdrawalRate) Wmax = 20.00
defendersThreshold (adT) 
attackersThreshold (aaT)
defendersAircraftAttritionRate (ada)
defendersCASSortieRate (Sd)
attackersAFVsKilled (Kd)
attackersAircraftAttritionRate (aaa)
defendersAFVsKilled (Ka)
attackersCASSortieRate (Sa)
afvsPerDivision (V)
afvLethalityPoints (L)
-}


memo :: (Int -> a) -> (Int -> a)
memo f = \n -> table !! n
    where
    table = [ f n | n <- [0..] ]



type Days = Int
type R = Double

{- Defines the default values for day 1 -}

g2gCasualityExchange :: R
g2gCasualityExchange = 1.50

defendersMaxWithdrawalRate :: R
defendersMaxWithdrawalRate = 20.00

defendersThreshold :: R
defendersThreshold = 0.050

attackersThreshold :: R
attackersThreshold = 0.075

defendersAircraftAttritionRate :: R
defendersAircraftAttritionRate = 0.05

defendersCASSortieRate :: R
defendersCASSortieRate = 1.50

attackersAFVsKilled :: R
attackersAFVsKilled = 0.50

attackersAircraftAttritionRate :: R
attackersAircraftAttritionRate = 0.05

defendersAFVsKilled :: R
defendersAFVsKilled = 0.25

attackersCASSortieRate :: R
attackersCASSortieRate = 1.00

afvsPerDivision :: R
afvsPerDivision = 1200

afvLethalityPoints :: R
afvLethalityPoints = 47490

{- end of default values -}

attackingForces :: Days -> R
attackingForces = memo f     {- every "force" has this memoization to deal with the intense recursive nature of the program -}
    where
    f 1 = 330000
    f t = attackingForces (t - 1) * (1 - attackerG2GAttrition (t - 1)) - attackersCasualtiesByAirSupport (t - 1)

defendingForces :: Days -> R
defendingForces = memo f
    where
    f 1 = 200000
    f t = defendingForces (t - 1) - ((attackerG2GAttrition (t - 1) / g2gCasualityExchange) * attackingForces (t - 1)) - defendersCasualtiesByAirSupport (t - 1) {- These formulas were taken from Professor Epstein's book "The calculus of conventional war. -}

attackerG2GAttrition :: Days -> R
attackerG2GAttrition = memo f
    where
    f t = attackersGroundProsecutionRate t * (1 - (defendersWithdrawalRate t / defendersMaxWithdrawalRate))


defendersWithdrawalRate :: Days -> R
defendersWithdrawalRate = memo f
    where
    f 1 = 0
    f t
        | defendersAttrition (t - 1) <= defendersThreshold = 0
        | defendersAttrition (t - 1) > defendersThreshold = defendersWithdrawalRate (t - 1) + ((defendersMaxWithdrawalRate - defendersWithdrawalRate (t-1)) / (1 - defendersThreshold)) * (defendersAttrition (t - 1) - defendersThreshold)

defendersAttrition :: Days -> R
defendersAttrition = memo f
    where
    f t = ((defendingForces (t) - defendingForces (t + 1)) / defendingForces (t))

attackersGroundProsecutionRate :: Days -> R
attackersGroundProsecutionRate = memo f
    where
    f 1 = 0.020
    f t = attackersGroundProsecutionRate (t - 1) - ((attackersThreshold - attackersGroundProsecutionRate (t - 1)) / attackersThreshold) * (attackersAttrition (t - 1) - attackersThreshold)

attackersAttrition :: Days -> R
attackersAttrition = memo f
    where
    f t = ((attackingForces (t) - attackingForces (t + 1)) / attackingForces t)

attackersCasualtiesByAirSupport :: Days -> R
attackersCasualtiesByAirSupport = memo f
    where
    f t = ((afvLethalityPoints / afvsPerDivision) * (defendersAirSupport 1) * ((1 - defendersAircraftAttritionRate) ** (fromIntegral (t - 1) * defendersCASSortieRate)) * attackersAFVsKilled * ((1 - ((1 - defendersAircraftAttritionRate) ** (defendersCASSortieRate + 1)) - defendersAircraftAttritionRate) / defendersAircraftAttritionRate))

defendersCasualtiesByAirSupport :: Days -> R
defendersCasualtiesByAirSupport = memo f
    where
    f t = ((afvLethalityPoints / afvsPerDivision) * (attackersAirSupport 1) * ((1 - attackersAircraftAttritionRate) ** (fromIntegral (t - 1) * attackersCASSortieRate)) * defendersAFVsKilled * ((1 - ((1 - attackersAircraftAttritionRate) ** (attackersCASSortieRate + 1)) - attackersAircraftAttritionRate) / attackersAircraftAttritionRate))

defendersAirSupport :: Days -> R
defendersAirSupport = memo f
    where
    f 1 = 300
    f t = defendersAirSupport (1) * ((1 - defendersAircraftAttritionRate) ** (fromIntegral (t - 1) * defendersCASSortieRate))

attackersAirSupport :: Days -> R
attackersAirSupport = memo f
    where
    f 1 = 250
    f t = attackersAirSupport (1) * ((1 - attackersAircraftAttritionRate) ** (fromIntegral (t - 1) * attackersCASSortieRate))

displacementOfFront :: Days -> R
displacementOfFront t = sum [defendersWithdrawalRate x | x <- [1..t]]


getFunction :: String -> (Days -> R)
getFunction "attackingForces" = attackingForces
getFunction "defendingForces" = defendingForces
getFunction "defendersAirSupport" = defendersAirSupport
getFunction "attackersAirSupport" = attackersAirSupport

getUnroundedFunction :: String -> (Days -> R)
getUnroundedFunction "defendersAttrition" = defendersAttrition
getUnroundedFunction "attackersAttrition" = attackersAttrition
getUnroundedFunction "attackersGroundProsecutionRate" = attackersGroundProsecutionRate
getUnroundedFunction "defendersWithdrawalRate" = defendersWithdrawalRate

forces :: IO ()
{- input and output functions -}
forces = do
    putStr "What values would you like to see?"
    x <- getLine
    let function = getFunction x

    putStr "Enter day: "
    n <- getLine
    forM_ [1..read n] $ \day -> do
        putStr ("Day: " <> show day <> " ")
        print (round (function day))
        
rates :: IO ()
rates = do
    putStr "What values would you like to see?"
    x <- getLine
    let function = getUnroundedFunction x

    putStr "Enter day: "
    n <- getLine
    forM_ [1..read n] $ \day -> do
        putStr ("Day: " <> show day <> " ")
        print (function day)
    
{- end IO's -}
