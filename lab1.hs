
id' x = x

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

myMax :: Int -> Int -> Int
myMax x y = if x <= y 
				then y else x

mySum :: Int -> Int
mySum x = if x <= 0
			 then 0
		   else x + mySum (x - 1)

fibo :: Int -> Int 
fibo x = if x == 0
			 then 0
	     else if x == 1
	     	 then 1 
	     else 
	     	 fibo (x - 1) + fibo (x - 2) 

myMax3 :: Int -> Int -> Int -> Int
myMax3 x y z = (x `max` y) `max` z 