{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


class Eval df t | df -> t where
  eval :: df -> t

newtype Fst a b = Fst (a, b)

instance Eval (Fst a b) a where
  eval (Fst (a, _)) = a


data MapList dfb a = MapList (a -> dfb) [a]


instance Eval dfb dft => Eval (MapList dfb a) [dft] where
  eval (MapList f []) = []
  eval (MapList f (a : as)) =
    eval (f a) : eval (MapList f as)


main :: IO ()
main = do
  putStrLn "Testing eval..."
  putStrLn $ eval (Fst ("Hello", 27))
  print $ eval (Fst (28, "World"))
  print $ eval (MapList Fst [('a', 2), ('3', 4)])
