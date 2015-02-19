
-- we're introducing different language features in this file.
module Intro2 where


-- we can define simple functions.

-- f just does some mathematical calculations.
f a b = a*b+2

-- g just reimplements if-then-else logic by actually piggybacking on
-- the if then else syntax. It also showcases that Haskell uses lazy
-- evaluation.  Calling (g False undefined) won't crash the program,
-- even though every time we 'touch' the value undefined, it throws an
-- exception.
g x y = if x then y else 5

-- we can ascribe type signatures to functions if we'd like. It either
-- just serves as documentation, or it can actually narrow the type
-- (remove some genericity so that it's only callable on specific
-- types).  The parentheses in the type ascription here are not
-- necessary, but are shown once for clarity.
h :: Int -> (Int -> (Int -> Int))
h a b c = a+b*c


-- a definition can have no arguments - basically a constant.
v = 1

-- we can define functions with multiple parts. Each fib definition
-- "pattern matches" on different values (in this case just integers
-- or the "match-anything" behavior of an identifier (n, here)).
fib 0 = 1
fib 1 = 1
fib n =  (fib (n-v))   + (fib (n-2))

fibIter :: Int -> Int
fibIter 0 = 0
fibIter 1 = 1
fibIter n = fibHelp n 0 1

fibHelp :: Int -> Int -> Int -> Int
fibHelp n a b = if n>0 then (fibHelp (n-1) (a+b) (a)) else a


plusFunc = uncurry (+)

         
-- once we understand zipWith, this is a fun way to calculate
-- fibonacci numbers quickly.
mfib = 0 : 1 : (zipWith (+) mfib (drop 1 mfib))


-- this'll look so much better after pattern matching and other
-- features.
getGrade n = if n>=90
             then "A"
             else 
                 if n>=80
                 then "B"
                 else
                     if n>=60 then "C" else "F"

-- at least a little better using "pattern guards": we first find the
-- first pattern to match (in this case, v will match anything). Then,
-- we ensure the expression between | and = is True, skipping until we
-- find one that is True.  If we don't find one, then this whole
-- pattern doesn't match, and we'd look at the next getGrade2
-- definition to see if it matched the pattern (and a guard, if
-- present). We don't have a second definition because the first one
-- matched all.  "otherwise" is just another name for True.
getGrade2 v | v>=90     = "A"
            | v>=80     = "B"
            | v>=70     = "C"
            | otherwise = "F"

        
-- infinite list
ones :: [Int]
ones = 1 : ones

-- pattern-matching over a list to (recursively) create 
double [x]    = [x*2]  -- unnecessary line, but shows that we need to put it before next line to differentiate them
double (x:xs) = (x*2) : (double xs)
double [ ]    = [ ]
             

-- focus on the let-expression.  We can have multiple definitions in
-- the single let. Also, notice how we use ab before it gets defined!
-- Again, we're not running separate assignment statements: we're
-- giving a set of (possibly recursive) definitions. So that's cool.
slowMax a b c =
    let abc = max ab c
        ab  = max a b
    in abc+0

-- -------------------------------------------------------------------

{-

-- We ended up making a different color datatype; since the
-- constructor names clashed, we're commenting these out rather than
-- just deleting them.

data Color = Red | Green | Blue | Black | White | Yellow | Plaid
             deriving (Show,Eq)


-- simple pattern matching on the Color type above.
rgb (Red)    = (255,0,0)
rgb (Green)  = (0,255,0)
rgb (Blue)   = (0,0,255)
rgb (Black)  = (0,0,0)
rgb (White)  = (255,255,255)
rgb _        = (100,100,100)

{- by the way, multi-line comments actually nest in Haskell! -}

scaled_rgb 0 Red = (255,0,0)
scaled_rgb n Red = (n,0,0)
scaled_rgb n Green = (0,n,0)
-}

-- -------------------------------------------------------------------


-- making the new type StopLight. It has three constructors (R, G,
-- Amber).
data StopLight = R | G | Amber deriving (Show, Eq)

-- -------------------------------------------------------------------

-- a type to help us represent colors; notice that our four
-- constructors can have different amounts of (and types of)
-- parameters.
data Color2 = RGB Int Int Int | HSV Int Int Int | Grey Int | Black
           deriving (Show, Eq)

-- we can pattern-match over our Color2 type.

-- we choose to write a type ascription here, but didn't have to.
is_red :: Color2 -> Bool

is_red (RGB r g b) = r>g && r > b
is_red (HSV _ _ _) = False -- Mark doesn't know what
                           -- he's doing here but
                           -- it's valid code!
is_red (Grey _) = False
is_red Black    = False

-- -------------------------------------------------------------------

-- the Bool type is just a plain old datatype. let's make our own
-- (misspelled) version!
data Boole = Tru | Fls deriving (Show,Eq)
           
-- we can write an if-expression via a function. Also, note the type
-- parameter 'a'. This is an example of parametric polymorphism.
iff :: Boole -> a -> a -> a
iff Tru x _ = x
iff Fls _ y = y

-- -------------------------------------------------------------------

-- more pattern-matching, this time over lists. Uses a let expression
-- and an if-expression as well!
maxx [ ] = error "can't maxx an []!"
maxx [v] = v
maxx (v:vs) = let vs_max = maxx vs
              in if vs_max > v 
                 then vs_max
                 else v
                      
-- we are missing the maxx[] definition... what happens when we try to
-- run such a function call? (This means we have defined a partial
-- function).

-- -------------------------------------------------------------------

secondToLast [] = error "garbage in, garbage out! empty."
secondToLast (x:(y:[])) = x
secondToLast (x:xs) = secondToLast xs

{-
--we could add this following x:[] version.  In the above,it 'falls
--through' to the (x:xs) version of secondToLast. But we don't get as
--specific a message if we let that happen.

secondToLast (x:[]) = error "garbage in, garbage out! length one."
-}


-- we can write our own version of (and::[Bool]->Bool).
andd :: [Boole] -> Boole
andd [] = Tru
andd (Tru:vs) = andd vs
andd (Fls:vs) = Fls

-- another definition, this time relying upon a case statement. It
-- lets us pattern-match anywhere, not just at a top-level function
-- definition.
andd' :: [Boole] -> Boole
andd' [] = Tru
andd' (b:vs) = case b of
                Tru -> andd' vs
                Fls -> Fls



data IntList = Cons Int IntList | Nil deriving(Show,Eq)


doubleIL :: IntList -> IntList
doubleIL Nil = Nil
doubleIL (x `Cons` xs) = Cons (x*2) (doubleIL xs)
--doubleIL (Cons x ( Cons y (Cons z Nil))) = Cons (x*2) (Cons (y*3) (Cons (z*2) Nil))

{-
double  []    = []
double (x:xs) = (x*2) : (double xs)
-}


mapIL :: (Int->Int) -> IntList -> IntList
mapIL f (Nil) = Nil
mapIL f (Cons x xs) = Cons (f x)  (mapIL f xs)

il1 = Cons 1 Nil
il5 = Cons 2 (Cons 3 ( Cons 4 ( Cons 5 ( Cons 6 Nil))))




doubleEach :: [Int] -> [Int]
doubleEach xs = map (*2) xs

inc :: Int -> Int 
inc x = x+1

mul2 x = x*2

isEven x = mod x 2 ==0




addTup (a,b) = a+b

addTwo a b = a+b


-- our own version of zip.
zipp :: [a] -> [b] -> [(a,b)]
zipp (a:as) (b:bs) = (a,b) : (zipp as bs)
zipp _      _      = []
{-
--The last case _ _ replaced these three:
zipp []     (b:bs) = []
zipp (a:as) []     = []
zipp []     []     = []
-}

-- our own version of zipWith.
zippWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zippWith f (a:as) (b:bs) = (f a b) : (zippWith f as bs)
zippWith f _ _ = []
-- here's a one-liner version
zippWith1L f as bs = map (\(a,b)-> f a b) $ zip as bs


-- keep a longer tail of values. This is not the usual notion of zip;
-- in fact, it's only possible when the two lists' element types and
-- the function's result type are all the same, as with Int here. As a
-- result, we have more patterns to match.
zipWithInts :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
zipWithInts f (a:as) (b:bs) = (f a b) : (zipWithInts f as bs)
zipWithInts f [] [] = []
zipWithInts f [] bs = bs
zipWithInts f as [] = as


-- an almost-true definition of quicksort. (The pivot is not truly
-- random; choosing the middle element would be better, but since
-- we're using lists that would also be a bit more expensive
-- regardless of the language. If the list is already mostly ordered,
-- we'll get some worst-case behaviors.)
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = let lefts  = [ v | v <- xs , v<=x ]
                   rights = [ v | v <- xs , v> x ]
               in (qsort lefts) ++[x]++ (qsort rights)
