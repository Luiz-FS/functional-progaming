{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor a b = ((not a) && b) || (a && (not b))
impl a b = (not a) || b
equiv a b = (impl a b) && (impl b a)

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x y = x ** y


{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial x = product [1..x]

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
isPrime x = prime [2..(x-1)] x
    where prime [] x = True
          prime (a:as) x | x `mod` a == 0 = False
                         | otherwise = prime as x

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc x y | x `mod` y == 0 = y
        | otherwise = mdc y (x `mod` y)

{-
- Calcula um MMC de dois numeros. 
-}
mmc x y = (multples x (x*y)) `match` (multples y (x*y))
    where multples x max = [x*y | y <- [1..(max/x)], x*y <= max]
          match (a:as) (b:bs) | a  == b = a
                              | a > b = match ([a] ++ as) bs
                              | otherwise = match as ([b] ++ bs) 

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y | x `mdc` y == 1 = True
            | otherwise = False

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x = getSum (getPrimes x) x
        where getPrimes x = [y | y <- [2..x], isPrime y]
              getSum [] x = []
              getSum (a:as) x | a + (last as) == x = [a, last as]
                              | a + (last as) > x = getSum ([a] ++ (take ((length as)-1) as)) x
                              | otherwise = getSum as x
