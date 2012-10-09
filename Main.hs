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
shoot a b = damage b (runFp $ firepower a) 

--
-- may Hoogle for it
--
cond :: Bool -> a -> Maybe a
cond True a = Just a
cond False a = Nothing

--
-- Binds repeatedly
--
repeatBind :: (Monad m) => Int -> m a -> (a -> m a) -> m a
repeatBind n ma f = foldl (>>=) ma (replicate n f)

main :: IO ()
main = do
     let unit1 = Unit (HP 100) (Firepower  5)
     let unit2 = Unit (HP  50) (Firepower 12)
     let unit2' = repeatBind 5 (Just unit2) (shoot unit1) 
     putStrLn $ show (remainingHp unit2) ++ " --> " ++ show (fmap remainingHp unit2')