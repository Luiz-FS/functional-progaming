--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Triple a b c deriving (Eq,Show)

tripleFst (Triple a _ _) = a
tripleSnd (Triple _ b _) = b
tripleThr (Triple _ _ c) = c

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quadruple a a b b deriving (Eq,Show)

firstTwo (Quadruple a b _ _) = (a, b)
secondTwo (Quadruple _ _ c d) = (c, d)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving(Eq, Show)

tuple1 (Tuple1 a) = a
tuple1 (Tuple2 a b) = a
tuple1 (Tuple3 a b c) = a
tuple1 (Tuple4 a b c d) = a

tuple2 (Tuple2 a b) = b
tuple2 (Tuple3 a b c) = b
tuple2 (Tuple4 a b c d) = b
tuple2 _ = Nothing

tuple3 (Tuple3 a b c) = c
tuple3 (Tuple4 a b c d) = c
tuple3 _ = Nothing

tuple4 (Tuple4 a b c d) = d
tuple4 _ = Nothing

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = Nil
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isBST NIL = True
isBST (Node x left right) = (verify (<x) left) && (verify (>x) right) && isBST left && isBST right
    where verify f NIL = True
          verify f (Node x left right) | f x = (verify f left) && (verify f right)
                                       | otherwise = False

--insere uma nova chave na BST retornando a BST modificada
insert x NIL = Node x NIL NIL 
insert x (Node a left right) | x > a = (Node a left (insert x right))
                             | otherwise = (Node a (insert x left) right) 

--retorna o Node da BST contendo o dado procurado ou entao NIL
search _ NIL = NIL
search x (Node a left right) | x == a = Node a left right
                             | x > a = search x right
                             | otherwise = search x left

--retorna o elmento maximo da BST
maximo NIL = error "Foda-se"
maximo (Node a left NIL)= a
maximo (Node a left right) =  maximo right

--retorna o elemento minimo da BST
minimo NIL = error "Foda-se"
minimo (Node a NIL right) = a
minimo (Node a left right) = minimo left

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor (Node a left right) = maximo left

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successor (Node a left right) = minimo right 

-- Node 4 (Node 2 (Node 1 NIL NIL) (Node 3 NIL NIL)) (Node 6 (Node 5 NIL NIL) (Node 7 NIL NIL))
--remove ume lemento da BST
remove _ NIL = error "Fudeu!"
remove n (Node x left right) | n > x = Node x left (remove n right)
                             | n < x = Node x (remove n left) right
                             | n == x = removeAux (Node x left right)
        where removeAux (Node n NIL NIL) = NIL
              removeAux (Node n left NIL) = left
              removeAux (Node n NIL right) = right
              removeAux (Node n left right) = Node (successor (Node n left right)) left (remove (successor (Node n left right)) right)

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder NIL = []
preOrder (Node a left right) = [a] ++ (preOrder left) ++ (preOrder right)
order NIL = []
order (Node a left right) = (order left) ++ [a] ++ (order right)
postOrder NIL = []
postOrder (Node a left right) = (postOrder left) ++ (postOrder right) ++ [a]