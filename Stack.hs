type Stack = [Integer]

pop  :: Stack      -> (Integer, Stack)
pop     (top:rest) =  (top, rest)

push :: Integer -> Stack -> Stack
push    x          stack =  x:stack


switchTopTwo :: Stack -> Stack
switchTopTwo s =
    let (x, s1) = pop s
        (y, s2) = pop s1
        s3      = push x s2
        s4      = push y s3
    in s4

type StackOp a = Stack -> (a, Stack)

popTwo :: StackOp Integer
popTwo s = let (_, s1) = pop s
           in pop s1

(>>>) :: StackOp a -> StackOp b -> StackOp b
op1 >>> op2 = \s ->
  let (_, s1) = op1 s
  in op2 s1
