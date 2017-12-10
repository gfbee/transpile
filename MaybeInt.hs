data MaybeInt = Success Integer | Failure deriving Show

blowUpTen :: Integer -> MaybeInt
blowUpTen 10 = Failure
blowUpTen  x = Success x

safeDiv :: Integer -> Integer -> MaybeInt
safeDiv _ 0 = Failure
safeDiv x y = Success (div x y)

biggerThanThree :: Integer -> MaybeInt
biggerThanThree x = if x > 3
                    then Success x
                    else Failure

computeInt :: Integer -> MaybeInt
computeInt n =
  let x = safeDiv 100 n
  in case x of
       Failure -> Failure
       Success y -> blowUpTen y

computeInt2 :: Integer -> MaybeInt
computeInt2 n =
    let x = safeDiv 100 n
    in case x of
       Failure -> Failure
       Success y ->
         let z = blowUpTen y
         in case z of
           Failure -> Failure
           Success w -> Success (w + 3)

add3Maybe :: MaybeInt -> MaybeInt
add3Maybe Failure = Failure
add3Maybe (Success x) = Success (x + 3)

lift :: (Integer -> Integer) -> MaybeInt -> MaybeInt
lift f Failure = Failure
lift f (Success x) = Success (f x)

(~>) :: MaybeInt -> (Integer -> Integer) -> MaybeInt
Failure ~> _ = Failure
(Success x) ~> f = Success (f x)

computeInt3 :: Integer -> MaybeInt
computeInt3 n =
  let x = safeDiv 100 n
  in case x of
       Failure -> Failure
       Success y ->
         blowUpTen y ~> (+3)

(>~>) :: MaybeInt -> (Integer -> MaybeInt) -> MaybeInt
Failure >~> _ = Failure
(Success x) >~> f = f x

computeIntFinal :: Integer -> MaybeInt
computeIntFinal n =
  safeDiv 100 n >~>
  blowUpTen ~>
  (+3)

data MMaybe a = JJust a | NNothing

(~~>) :: MMaybe a -> (a -> b) -> MMaybe b
NNothing ~~> _ = NNothing
(JJust x) ~~> f = JJust (f x)


(>~~>) :: MMaybe a -> (a -> MMaybe b) -> MMaybe b
NNothing >~~> _ = NNothing
(JJust x) >~~> f = f x
