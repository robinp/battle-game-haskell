import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Trans.Error
import Control.Monad.Trans.Maybe

data HP = HP Int
     deriving Show

data Firepower = Firepower { runFp :: Int }
     deriving Show

data Unit = Unit { name :: String, remainingHp :: HP, firepower :: Firepower }

instance Show Unit where
  show u =
    let HP hp        = remainingHp u
        Firepower fp = firepower u
    in
      "Unit '" ++ name u ++ "' [HP:" ++ show hp ++ " FP:" ++ show fp ++ "]"

type Logged = Writer String

--
-- Battling
--

damage :: Unit -> Int -> Maybe Unit
damage u @ Unit { remainingHp = HP hp } dmg =
       cond (hp > dmg) $ u { remainingHp = HP $ hp - dmg }

shoot :: Unit -> Unit -> MaybeT Logged Unit
shoot x y = MaybeT $ damage y (runFp $ firepower x) `logged` (name x ++ " shoots " ++ name y)

--
-- fight has a bias, the left unit gets to shoot first
--
fight :: Unit -> Unit -> ErrorT Unit Logged Unit
fight x y = 
  let 
    damagedY = x `shoot` y
    xWon = Left x `logged` (name x ++ " survived")
  in ErrorT $ do 
    dy <- runMaybeT damagedY
    maybe xWon (\y' -> runErrorT $ swapT $ fight y' x) dy
    
--
-- Helpers
--
maybeT :: (Functor m) => b -> (a -> b) -> MaybeT m a -> m b
maybeT fallback f ma = fmap (maybe fallback f) $ runMaybeT ma 

logged :: a -> String -> Logged a 
logged x s = writer (x, s) 

swap :: Either a b -> Either b a
swap (Left x)  = Right x 
swap (Right x) = Left x

swapT :: (Monad m) => ErrorT a m b -> ErrorT b m a
swapT e = mapErrorT (liftM swap) e

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
     let unit1 = Unit "Fizz" (HP 100) (Firepower  5)
     let unit2 = Unit "Buzz" (HP  50) (Firepower 12)
     let winner = unit1 `fight` unit2
     let winnerText = show $ runIdentity $ runWriterT $ runErrorT $ winner
     putStrLn $ show unit1 ++ " vs " ++ show unit2 ++ "  --> " ++ winnerText
