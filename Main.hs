data HP = HP Int
     deriving Show

data Firepower = Firepower { runFp :: Int }
     deriving Show

data Unit = Unit { remainingHp :: HP, firepower :: Firepower }
     deriving Show

damage :: Unit -> Int -> Maybe Unit
damage u @ Unit { remainingHp = HP hp } dmg =
       cond (hp > dmg) $ u { remainingHp = HP $ hp - dmg }

shoot :: Unit -> Unit -> Maybe Unit
shoot x y = damage y (runFp $ firepower x) 

--
-- fight has a bias, the left unit gets to shoot first
--
fight :: Unit -> Unit -> Either Unit Unit
fight x y = 
  let damagedY = x `shoot` y
  in maybe (Left x) (\y' -> swap $ fight y' x) damagedY
  

--
-- Helpers
--
swap :: Either a b -> Either b a
swap (Left x)  = Right x 
swap (Right x) = Left x

cond :: Bool -> a -> Maybe a
cond True a = Just a
cond False a = Nothing

repeatBind :: (Monad m) => Int -> m a -> (a -> m a) -> m a
repeatBind n ma f = foldl (>>=) ma (replicate n f)

--
-- Main
--
main :: IO ()
main = do
     let unit1 = Unit (HP 100) (Firepower  5)
     let unit2 = Unit (HP  50) (Firepower 12)
     let winner = unit1 `fight` unit2
     putStrLn $ show unit1 ++ " vs " ++ show unit2 ++ "  --> " ++ show winner 