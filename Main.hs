data HP = HP Int
     deriving Show

damage (HP hp) dmg =
       if hp > dmg
       	  then Just $ HP (hp - dmg)
	  else Nothing

repeatM :: (Monad m) => Int -> m a -> (a -> m a) -> m a
repeatM n ma f = foldl (>>=) ma (replicate n f)

main :: IO ()
main = do
     let hp = HP 100
     let hp' = repeatM 5 (Just hp) (flip damage 5) 
     putStrLn $ show hp ++ " --> " ++ show hp'