import System.Random

import qualified Data.Text as T
import Data.Char
import Data.String
import Data.List
import Data.Sequence (Seq, Seq(..), (><))
import qualified Data.Sequence as Seq

bind :: (a -> StdGen -> (b,StdGen)) -> (StdGen -> (a,StdGen)) -> (StdGen -> (b,StdGen))
bind f x seed = let (x',seed') = x seed in f x' seed'
unit x g = (x,g)
lift f = unit . f

global_seed = 345123213
beaker_seed = mkStdGen 1223243

synthesis :: String->Int->[(Char,Char)]--3 = 3'-5' , 5 = 5'-3' , 3 has top filled
synthesis [c] 3 = [(c,'X')]
synthesis [c] 5 = [('X',c)]
synthesis (x:xs) 3  = [(x,'X')] ++ (synthesis xs 3)
synthesis (x:xs) 5  = [('X',x)] ++ (synthesis xs 5)

after_k_iterations :: [[(Char,Char)]] -> Int-> [[(Char,Char)]]
after_k_iterations l 0 = l
after_k_iterations l k 
 | (((reaction)!!(k-1)) == []) = (after_k_iterations l (k-1)) 
 | otherwise = (((reaction)!!(k-1)))
   where 
    reaction = iterate (beaker_anneal seed) l
     where seed = mkStdGen global_seed

--beaker_anneal :: [[(Char,Char)]] -> [[(Char,Char)]]
--beaker_anneal [] = []
--beaker_anneal beaker = [each_anneal | x<-beaker,y<-beaker,each_anneal <- (full_anneal x y)]
--------------------------------------

measure_value :: [(Char,Char)]->[(Char,Char)]->Int
measure_value [] [] = 0
measure_value [] _ = 0
measure_value _ [] = 0
measure_value l1 l2 = max (length (full_anneal l1 l2)) (length (full_anneal l2 l1))

find_best_match :: [(Char,Char)]->[[(Char,Char)]]->(Int,Int,Int,[(Char,Char)])->(Int,Int,Int,[(Char,Char)])
find_best_match strand [] tuple = tuple
find_best_match strand (l:ls) (index,best_index,score,cur_best_item) 
 | cur_score>score = find_best_match strand ls (index+1,index,cur_score,l)
 | otherwise = find_best_match strand ls (index+1,best_index,score,cur_best_item)
 where
  cur_score = measure_value strand l

removenth :: Int->[[(Char,Char)]]->[[(Char,Char)]]
removenth _ [] = []
removenth n xs = (take (n-1) xs) ++ (drop n xs)

sum_till_n :: Int->Int
sum_till_n n = div (n*(n+1)) 2

prob_pick_index :: Int->Int->Int->Int
prob_pick_index start cur_sum n 
 | (n<=cur_sum) = start
 | otherwise = prob_pick_index (start+1) (cur_sum+(start+1)) n 

beaker_anneal :: StdGen->[[(Char,Char)]] -> [[(Char,Char)]]
beaker_anneal _ [] = []
beaker_anneal seed (b:bs) 
 | (len>0) = (beaker_anneal newseed after_removal)++[(anneal_list!!(picked-1))] 
 | otherwise = beaker_anneal newseed after_removal
 where
  (dummy,best_index,dummy2,best_match) = find_best_match b bs (0,0,0,[('X','X')])
  after_removal = removenth best_index bs
  (rand_num,newseed) = random seed 
  anneal_list = (full_anneal b best_match)++(full_anneal best_match b)
  mod_num = (rem rand_num (sum_till_n len))+1
  len = length anneal_list
  picked = prob_pick_index 1 1 mod_num 
----------------------------------------
full_anneal :: [(Char,Char)]->[(Char,Char)]-> [[(Char,Char)]]
full_anneal [] _ = []
full_anneal _ [] = [] 
full_anneal l1 l2 = make_anneals (suf) (pref) l1 l2 
 where
  suf = rev (suffix_list1 l1 'X')
  pref = rev (prefix_list1 l2 'X')
  --suf = (rev_suffix_list l1)
  --pref = (prefix_list l2 [])


contained :: String->[(Char,Char)]->Bool 
contained s l = (isInfixOf s (upstring l))||(isInfixOf s (downstring l))


complement_of :: Char -> Char 
complement_of 'A' = 'T'
complement_of 'T' = 'A'
complement_of 'G' = 'C'
complement_of 'C' = 'G'
complement_of 'X' = '#'


make_anneals :: [String]->[String]->[(Char,Char)]->[(Char,Char)]->[[(Char,Char)]]
make_anneals [] _ _ _ = []
make_anneals _ [] _ _ = []
make_anneals (x:xs) (y:ys) l1 l2 
 | ((map complement_of x) == y) = [annealed_strand] ++ (make_anneals xs ys l1 l2)
 | otherwise = make_anneals xs ys l1 l2
 where 
  annealed_strand = anneal y (length y) l1 l2 

anneal :: String-> Int-> [(Char,Char)]->[(Char,Char)]->[(Char,Char)]
anneal s k l1 l2 = (take (len1-k) l1) ++ (make_middle s)++ (drop k l2)
 where 
  len1 = (length l1)

make_middle :: String->[(Char,Char)]
make_middle [] = [] 
make_middle (x:xs) = (complement_of x,x):(make_middle xs)

isMember:: (Eq a)=>a -> [(a,a)] -> Bool
isMember _ [] = False
isMember x ((u,v):xs) = (u==x)||(v==x)||(isMember x xs)

prefix::[a] ->[[a]]
prefix [] =[]
prefix l = [l]++ (prefix (take (n-1) l))
           where n=length l

allx :: (Eq a)=>[a] -> a -> Bool 
allx [] _ = True
allx (x:xs) y = (x==y)&& (allx xs y)

allx_downstring ::(Eq a)=> a-> [(a,a)]  -> Bool
allx_downstring y l = allx (downstring l) y

allx_upstring ::(Eq a)=> a-> [(a,a)]  -> Bool
allx_upstring y l = allx (upstring l) y

remove_x_suffix::(Eq a)=>[[(a,a)]]-> a ->[[(a,a)]]
remove_x_suffix l y= filter (allx_downstring y) l

remove_x_prefix::(Eq a)=>[[(a,a)]]-> a ->[[(a,a)]]
remove_x_prefix l y = filter (allx_upstring y) l

prefix_list1:: (Eq a)=>[(a,a)] ->a -> [[a]]
prefix_list1 [] _=[]
prefix_list1 l y= map downstring (new_prefix)
 where 
  new_prefix = remove_x_prefix (prefix l) y

prefix_list :: [(Char,Char)]-> String-> [String]
prefix_list [] _ = [] 
prefix_list ((y,x):xs) s = ([s++[x]] ++ (prefix_list xs (s++[x])))

suffix:: (Eq a)=>[a] -> [[a]]
suffix [] =[]
suffix (x:xs)= [x:xs]++ (suffix xs)

suffix_list1:: (Eq a,Show a)=>[(a,a)]->a -> [[a]]
suffix_list1 [] _ =[]
suffix_list1 l y=  map upstring (new_suffix)
 where 
  new_suffix = remove_x_suffix (suffix l) y

suffix_list :: [(Char,Char)]-> [String]
suffix_list [] = []
suffix_list (x:xs) = ( [upstring (x:xs)] ++ (suffix_list xs))

rev_suffix_list :: [(Char,Char)]-> [String]
rev_suffix_list l = rev (suffix_list l)

upstring :: [(a,a)]->[a] 
upstring [] = []
upstring ((x,y):xs) = [x] ++ (upstring xs)

downstring :: [(a,a)]->[a] 
downstring [] = []
downstring ((x,y):xs) = [y] ++ (downstring xs)

even :: Int->Bool
even x 
 | ((mod x 2) ==0) = True
 | otherwise = False

rev :: [a]->[a]
rev [] = [] 
rev (x:xs) = (rev xs) ++ [x]


--------------------------------------SAT SOLVER 

strand_len = 5

isEmpty :: [a]->Bool
isEmpty [] = True
isEmpty _ = False

numtoletter :: (Eq a,Num a)=>a -> Char
numtoletter 1 = 'A'
numtoletter 2 = 'T'
numtoletter 3 = 'C'
numtoletter 4 = 'G'
numtoletter _ = 'X'

one = 1
four = 4



--random_list_gen :: Int->Int->[Int] -- seed, size, list of ints
--random_list_gen s 0 = []
--random_list_gen s l  = 




magic_fun :: StdGen->(Int,StdGen)
magic_fun x = random x 
--magic_fun x = let (a,b) = random global_seed in a

seed_for_a = 546
seed_for_x = 123
seed_for_x' = 112

dna_strand_generator :: StdGen->Int->Int->[(Char,Char)]
dna_strand_generator s 0 _ = []
dna_strand_generator s l t
 | (t==1) = [(rand_char,'X')]++(dna_strand_generator seed (l-1) 1)
 | otherwise = [('X',rand_char)]++(dna_strand_generator seed (l-1) 0)
 where
  (rand_num,seed) = magic_fun s
  rand_char = numtoletter ((mod rand_num 4) + 1)

num_of_var :: [[Int]] -> Int
num_of_var [] = 0
num_of_var (x:xs) = (length x)

a_beaker :: StdGen->Int->Int->Int->([[(Char,Char)]],StdGen)
a_beaker s 0 _ _ = ([],newseed)
 where
  (num,newseed) = magic_fun s
a_beaker s c l t = ([(dna_strand_generator s l t)] ++ nextiter,newseed)
 where 
  (num,newseed) = magic_fun s
  (nextiter,dummy) = (a_beaker newseed (c-1) l t)

x_beaker :: StdGen->Int->Int->Int->([[(Char,Char)]],StdGen)
x_beaker s 0 _ _ = ([],newseed)
 where
  (num,newseed) = magic_fun s
x_beaker s c l t = ([(dna_strand_generator s l t)] ++ nextiter,newseed)
 where 
  (num,newseed) = magic_fun s
  (nextiter,dummy) = (x_beaker newseed (c-1) l t)


x'_beaker :: StdGen->Int->Int->Int->([[(Char,Char)]],StdGen)
x'_beaker s 0 _ _ = ([],newseed)
 where
  (num,newseed) = magic_fun s
x'_beaker s c l t = ([(dna_strand_generator s l t)] ++ nextiter,newseed)
 where 
  (num,newseed) = magic_fun s
  (nextiter,dummy) = (x'_beaker newseed (c-1) l t)

--x'_beaker :: Int->Int->Int->[[(Char,Char)]]
--x'_beaker 0 _ _ = []
--x'_beaker c l t = [(dna_strand_generator (seed) l t)] ++ (x'_beaker (c-1) l t)
-- where 
--  seed = mkStdGen (seed_for_x'+l+c)

make_qpcomp :: [(Char,Char)]->[(Char,Char)]->[(Char,Char)]--0 type assume 1 type input
make_qpcomp p q = (flip_comp p_second_half) ++ (flip_comp q_first_half)
 where 
  p_second_half = drop (div lp 2) p
  q_first_half = take (div lq 2) q
  lp = length p
  lq = length q



flip_comp :: [(Char,Char)]->[(Char,Char)]
flip_comp [] = [] 
flip_comp ((a,b):xs) = (complement1_of b,complement1_of a):(flip_comp xs)

a_x_beaker :: [[(Char,Char)]]->[[(Char,Char)]]->[[(Char,Char)]]->[[(Char,Char)]]
a_x_beaker [] [] []= [] 
a_x_beaker [] _  _ = []
a_x_beaker _ [] _ = []
a_x_beaker _ _ [] = []
a_x_beaker (a:as) (x:xs) (x':x's)  = (make_qpcomp a x):(make_qpcomp a x'):(a_x_beaker as xs x's)

-- a should be drop 1
x_a_beaker :: [[(Char,Char)]]->[[(Char,Char)]]->[[(Char,Char)]]->[[(Char,Char)]]
x_a_beaker [] [] []= [] 
x_a_beaker [] _  _ = []
x_a_beaker _ [] _ = []
x_a_beaker _ _ [] = []
x_a_beaker (a:as) (x:xs) (x':x's)  = (make_qpcomp x a):(make_qpcomp x' a):(x_a_beaker as xs x's)

first_half_a1 :: [(Char,Char)]-> [(Char,Char)]
first_half_a1 l = flip_comp ((take (div len 2) l))
 where 
  len = length l

last_half_an :: [(Char,Char)]-> [(Char,Char)]
last_half_an l = flip_comp ((drop (div len 2) l))
 where 
  len = length l

complement1_of :: Char -> Char 
complement1_of 'A' = 'T'
complement1_of 'T' = 'A'

complement1_of 'G' = 'C'
complement1_of 'C' = 'G'
complement1_of 'X' = 'X'

temp_seed = mkStdGen 324

make_sat_beaker :: Int->[[(Char,Char)]]-- number of variables
make_sat_beaker 0 = []
make_sat_beaker n = a_stuff++x_stuff++x'_stuff++ax_stuff++xa_stuff++[a1]++[an]
 where 
  (a_stuff,seed1) = a_beaker beaker_seed (n+1) strand_len 1
  (x_stuff,seed2) = x_beaker temp_seed (n) strand_len 1
  (x'_stuff,seed3) = x'_beaker seed2 (n) strand_len 1
  ax_stuff = a_x_beaker a_stuff x_stuff x'_stuff
  xa_stuff = x_a_beaker a_stuff x_stuff x'_stuff
  a1 = first_half_a1 ((take 1 a_stuff)!!0)
  an = last_half_an ((take n a_stuff)!!0)

extract :: [[(Char,Char)]] -> String -> [[(Char,Char)]]
extract [] _ = []
extract l s = filter (contained s) l


remaining :: [[(Char,Char)]] -> String -> [[(Char,Char)]]
remaining [] _ = []
remaining l s = (l \\ (extract l s))


--formula,beaker,xname,x'name,resultant_beaker
algo ::  (Eq a,Num a)=>[[a]]->[[(Char,Char)]]->[[(Char,Char)]]->[[(Char,Char)]]->[[(Char,Char)]]
algo [] b x x'= b
algo _ [] x x'= []
algo (vl:vls) b x x'= algo vls (satisfy vl x x' [] b) x x'

--clause,xname,x'name,beaker,buffer
satisfy :: (Eq a,Num a)=>[a]->[[(Char,Char)]]->[[(Char,Char)]]->[[(Char,Char)]]->[[(Char,Char)]]->[[(Char,Char)]]
satisfy [] _ _ bk bf = bk
satisfy (v:vs) (x:xs) (x':x's) bk bf 
 |(v==1) = satisfy vs xs x's (bk++on) (bf\\on)
 |otherwise = satisfy vs xs x's (bk++off) (bf\\on)
 where 
  on = extract bf (upstring x)
  off = extract bf (upstring x')

sat_solver ::[[Int]]->Bool 
sat_solver l 
 | (result == []) = False
 | otherwise = True
 where 
  result = algo l b x_stuff x'_stuff
  (x_stuff,seed1) = x_beaker temp_seed (n) strand_len 1
  (x'_stuff,seed2) = x'_beaker seed1 (n) strand_len 1
  b = after_k_iterations (make_sat_beaker n) 10
  n = num_of_var l



--myPureFunction :: Int -> Int
--myPureFunction x = (mod x 4) + 1

--callrand :: Int
--callrand = myPureFunction main

--main :: IO ()
--main = do
--    -- num :: Float
--    num <- randomIO :: IO Int
--    -- This "extracts" the float from IO Float and binds it to the name num
--    return (myPureFunction num)

