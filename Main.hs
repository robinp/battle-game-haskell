data HP = HP Int
     deriving Show

data Firepower = Firepower Int
     deriving Show

data Unit = Unit { remainingHp :: HP, firepower :: Firepower }
     deriving Show

damage :: Unit -> Int -> Maybe Unit
damage u @ Unit { remainingHp = HP hp } dmg =
       cond (hp > dmg) $ u { remainingHp = HP $ hp - dmg }

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
     let unit = Unit (HP 100) (Firepower 5)
     let unit' = repeatBind 5 (Just unit) (flip damage 5) 
     putStrLn $ show (remainingHp unit) ++ " --> " ++ show (fmap remainingHp unit')