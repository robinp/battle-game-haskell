data HP = HP Int
     deriving Show

data Firepower = Firepower Int
     deriving Show

data Unit = Unit { remainingHp :: HP, firepower :: Firepower }
     deriving Show

damage :: Unit -> Int -> Maybe Unit
damage u @ (Unit (HP hp) _) dmg =
       if hp > dmg
       	  then Just $ u { remainingHp = HP $ hp - dmg }
	  else Nothing

repeatM :: (Monad m) => Int -> m a -> (a -> m a) -> m a
repeatM n ma f = foldl (>>=) ma (replicate n f)

main :: IO ()
main = do
     let unit = Unit (HP 100) (Firepower 5)
     let unit' = repeatM 5 (Just unit) (flip damage 5) 
     putStrLn $ show (remainingHp unit) ++ " --> " ++ show (fmap remainingHp unit')