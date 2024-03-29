











> module GenUtils (


>       partition', tack,
>       assocMaybeErr,
>       arrElem,
>       memoise,
>       returnMaybe,handleMaybe, findJust,
>       MaybeErr(..),
>       mapMaybe,
>       maybeMap,
>       joinMaybe,
>       mkClosure,
>       foldb,
>       listArray',
>       cjustify,
>       ljustify,
>       rjustify,
>       space,
>       copy,
>       combinePairs,
>       --trace,                -- re-export it
>       fst3,
>       snd3,
>       thd3,
>       mapDollarDollar,
>       str, char, nl, brack, brack',
>       interleave, interleave',
>       strspace, maybestr
>        ) where


> import Data.Char  (isAlphaNum)
> import Data.List
> import Data.Ix    ( Ix(..) )
> import Data.Array ( Array, listArray, array, (!) )












> mapMaybe :: (a -> Maybe b) -> [a] -> [b]
> mapMaybe _ [] = []
> mapMaybe f (a:r) = case f a of
>                       Nothing -> mapMaybe f r
>                       Just b  -> b : mapMaybe f r


> maybeMap :: (a -> b) -> Maybe a -> Maybe b
> maybeMap f (Just a) = Just (f a)
> maybeMap _ Nothing  = Nothing


> joinMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
> joinMaybe _ Nothing  Nothing  = Nothing
> joinMaybe _ (Just g) Nothing  = Just g
> joinMaybe _ Nothing  (Just g) = Just g
> joinMaybe f (Just g) (Just h) = Just (f g h)


> data MaybeErr a err = Succeeded a | Failed err deriving (Eq,Show)










> mkClosure :: (a -> a -> Bool) -> (a -> a) -> a -> a
> mkClosure eq f = match . iterate f
>   where
>       match (a:b:_) | a `eq` b = a
>       match (_:c)              = match c
>       match [] = error "Can't happen: match []"


> foldb :: (a -> a -> a) -> [a] -> a
> foldb _ [] = error "can't reduce an empty list using foldb"
> foldb _ [x] = x
> foldb f l  = foldb f (foldb' l)
>    where
>       foldb' (x:y:x':y':xs) = f (f x y) (f x' y') : foldb' xs
>       foldb' (x:y:xs) = f x y : foldb' xs
>       foldb' xs = xs


> returnMaybe :: a -> Maybe a
> returnMaybe = Just


> handleMaybe :: Maybe a -> Maybe a -> Maybe a
> handleMaybe m k = case m of
>                Nothing -> k
>                _ -> m


> findJust :: (a -> Maybe b) -> [a] -> Maybe b
> findJust f = foldr handleMaybe Nothing . map f








> fst3 :: (a, b, c) -> a
> fst3 (a,_,_) = a
> snd3 :: (a, b, c) -> b
> snd3 (_,a,_) = a
> thd3 :: (a, b, c) -> c
> thd3 (_,_,a) = a


> cjustify, ljustify, rjustify :: Int -> String -> String
> cjustify n s = space halfm ++ s ++ space (m - halfm)
>                where m     = n - length s
>                      halfm = m `div` 2
> ljustify n s = s ++ space (max 0 (n - length s))
> rjustify n s = space (n - length s) ++ s


> space       :: Int -> String
> space n      = copy n ' '


> copy  :: Int -> a -> [a]      -- make list of n copies of x
> copy n x = take n xs where xs = x:xs


> partition' :: (Eq b) => (a -> b) -> [a] -> [[a]]
> partition' _ [] = []
> partition' _ [x] = [[x]]
> partition' f (x:x':xs) | f x == f x'
>    = tack x (partition' f (x':xs))
>                       | otherwise
>    = [x] : partition' f (x':xs)


> tack :: a -> [[a]] -> [[a]]
> tack x xss = (x : head xss) : tail xss


> combinePairs :: (Ord a) => [(a,b)] -> [(a,[b])]
> combinePairs xs =
>       combine [ (a,[b]) | (a,b) <- sortBy (\ (a,_) (b,_) -> compare a b) xs]
>  where
>       combine [] = []
>       combine ((a,b):(c,d):r) | a == c = combine ((a,b++d) : r)
>       combine (a:r) = a : combine r
>


> assocMaybeErr :: (Eq a) => [(a,b)] -> a -> MaybeErr b String
> assocMaybeErr env k = case [ val | (key,val) <- env, k == key] of
>                        [] -> Failed "assoc: "
>                        (val:_) -> Succeeded val
>








> arrElem :: (Ix a, Ord a) => [a] -> a -> Bool
> arrElem obj = \x -> inRange size x && arr ! x
>   where
>       obj' = sort obj
>       size = (head obj',last obj')
>       arr = listArray size [ i `elem` obj | i <- range size ]








      > fib = memoise (0,100) fib'
      >   where
      >       fib' 0 = 0
      >       fib' 1 = 0
      >       fib' n = fib (n-1) + fib (n-2)








> memoise :: (Ix a) => (a,a) -> (a -> b) -> a -> b
> memoise bds f = (!) arr
>   where arr = array bds [ (t, f t) | t <- range bds ]


> listArray' :: (Int,Int) -> [a] -> Array Int a
> listArray' (low,up) elems =
>       if length elems /= up-low+1 then error "wibble" else
>       listArray (low,up) elems










> mapDollarDollar :: String -> Maybe (String -> String)
> mapDollarDollar code0 = go code0 ""
>   where go code acc =
>           case code of
>               [] -> Nothing
>
>               '"'  :r    -> case reads code :: [(String,String)] of
>                                []       -> go r ('"':acc)
>                                (s,r'):_ -> go r' (reverse (show s) ++ acc)
>               a:'\'' :r | isAlphaNum a -> go r ('\'':a:acc)
>               '\'' :r    -> case reads code :: [(Char,String)] of
>                                []       -> go r ('\'':acc)
>                                (c,r'):_ -> go r' (reverse (show c) ++ acc)
>               '\\':'$':r -> go r ('$':acc)
>               '$':'$':r  -> Just (\repl -> reverse acc ++ repl ++ r)
>               c:r  -> go r (c:acc)










> str :: String -> String -> String
> str = showString
> char :: Char -> String -> String
> char c = (c :)
> interleave :: String -> [String -> String] -> String -> String
> interleave s = foldr (\a b -> a . str s . b) id
> interleave' :: String -> [String -> String] -> String -> String
> interleave' s = foldr1 (\a b -> a . str s . b)


> strspace :: String -> String
> strspace = char ' '
> nl :: String -> String
> nl = char '\n'


> maybestr :: Maybe String -> String -> String
> maybestr (Just s)     = str s
> maybestr _            = id


> brack :: String -> String -> String
> brack s = str ('(' : s) . char ')'
> brack' :: (String -> String) -> String -> String
> brack' s = char '(' . s . char ')'

