{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}

module SBool where

{-

  compile time ==> runtime

  Terms ==> runtime

  Types ==> compile time

  x :: Int
  x = 10 + 10
  x = "Foo"


  Dependent Types (DT) ==> runtime


  # Pyton
  >>> x = 10
  >>> type(x)
  >>> <class 'int'>
  >>> x = "Foo"
  >>> type(x)
  >>> <class 'str'>


  Richard Eisenberg researching on a first-class dependent types to Haskell
  https://richarde.dev/papers/2016/thesis/eisenberg-thesis.pdf

  Until then, we're going to depend on a few language features:
   - rank-n types, GADTs, type families, data kinds, etc.


  * DT approximation in Haskell via 'singletons'

  * 'singletons' => isomorphismos between terms and values

  KIND => Type | Constraint | Arrow Kinds
  TYPE => Bool, 'False, 'True, etc.
  TERMS => False, True

  data () = () ## () => Unit Type

   UT has one value ()
   UT has one term ()

   > for every inhabitant of a type, we create a
   > singleton type capable of bridging the term–type divide
-}


-- STrue and SFalse are the bridge between the term and type levels
data SBool (b :: Bool) where
  STrue :: SBool 'True
  SFalse :: SBool 'False

-- Type 2 Term
-- STrue ~ True
-- SFalse ~ False

fromSBool :: SBool b -> Bool
fromSBool STrue  = True
fromSBool SFalse = False

-- Term 2 Type
-- STrue 'True /= SBool 'False
-- we cannot write the other side of the isomorphism directly

data SomeSBool where
  SomeSBool :: SBool b -> SomeSBool

withSomeSBool
  :: SomeSBool
  -> (forall (b :: Bool). SBool b -> r)
  -> r
withSomeSBool (SomeSBool s) f = f s

toSBool :: Bool -> SomeSBool
toSBool True  = SomeSBool STrue
toSBool False = SomeSBool SFalse


run :: IO ()
run = do
  print $ withSomeSBool (toSBool True) fromSBool
  print $ withSomeSBool (toSBool False) fromSBool
