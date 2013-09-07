module Text.PrettyPrint.Box (Box,char,uniform,line,($$),(<>),(<+>),(<-+->),frame,connect) where

import Prelude hiding (lines)
import Data.List as List

newtype Box = Box [String]

instance Show Box where
    show (Box xs) = unlines xs

width :: Box -> Int
width (Box (x:_)) = length x
width (Box []) = 0

height :: Box -> Int
height (Box xs) = length xs

char :: Char -> Box
char x = Box [[x]]

uniform :: Char -> Int -> Int -> Box
uniform x w h = Box (replicate h (replicate w x))

line :: [Char] -> Box
line str = Box (zipWith f xs ns)
  where
    xs = List.lines str
    ns = map length xs
    maxLen = maximum ns
    f x len = replicate (div (maxLen - len) 2) ' ' ++
              x ++
              replicate (maxLen - len - (div (maxLen - len) 2)) ' '

lines :: [String] -> Box
lines xs = Box xs

-- above
infixl 5 $$
($$) :: Box -> Box -> Box
box1 $$ box2 = Box (xs1 ++ xs2)
    where
      Box xs1 = widen box1 (width box2)
      Box xs2 = widen box2 (width box1)

sepBy :: Box -> [Box] -> Box
sepBy sep boxes = foldl1 (\b k -> b <> sep <> k) boxes

connect :: Box -> [Box] -> Box
connect box [] = box
connect box [b] = box $$ char '│' $$ b
-- connect box boxes = box $$
--                     char '│' $$ horiz $$
--                     foldl1 (\b1 b2 -> b1 <-> sep <-> b2) boxes'
--     where
--       boxes' = map (char '│' $$) boxes
--       widths = map ((width sep +) . width) boxes'
--       a = head widths
--       b = last widths
--       horiz = uniform ' ' (div a 2 - 1) 1 <>
--               uniform '─' (sum widths - (div a 2) - (div b 2)) 1 <>
--               uniform ' ' (div b 2) 1
--       sep = line "  "
connect box (b:bs) = box $$ line buffer' $$ bs2
    where
      go1 [b] = let w = width b
                in replicate (div w 2) '─' ++ "┐" ++ replicate (w - div w 2 + 1) ' '
      go1 (b:bs) = let w = width b
                   in replicate (div w 2) '─' ++ "┬" ++ replicate (w - div w 2 + 1) '─' ++ go1 bs
      w = width b
      buffer = replicate (div w 2 - 1) ' ' ++ "┌" ++ replicate (w - div w 2) '─' ++ go1 bs
      q = div (length buffer - 1) 2
      buffer' = case splitAt q buffer of
                  (xs,c:ys) | c == '─' -> xs ++ '┴':ys
                            | otherwise -> xs ++ '┼':ys
                  _ -> buffer
      bs2 = foldr1 (\b1 b2 -> b1 <-> char ' ' <-> b2) (b:bs)

-- beside
infixl 5 <>
(<>) :: Box -> Box -> Box
box1 <> box2 = Box (zipWith (++) xs1 xs2)
    where
      Box xs1 = heighten box1 (height box2)
      Box xs2 = heighten box2 (height box1)

infixl 5 <->
(<->) :: Box -> Box -> Box
box1 <-> box2 = Box (zipWith (++) xs1 xs2)
    where
      Box xs1 = heighten2 box1 (height box2)
      Box xs2 = heighten2 box2 (height box1)

infixl 5 <-+->
(<-+->) :: Box -> Box -> Box
box1 <-+-> box2 = box1 <-> char ' ' <-> box2

infixl 5 <+>
(<+>) :: Box -> Box -> Box
box1 <+> box2 = box1 <> char ' ' <> box2

widen b@(Box xs) w | w <= wid  = b
                   | otherwise = left <> b <> right
    where
      wid   = width b
      hei   = height b
      left  = uniform ' ' ((w - wid) `div` 2) hei
      right = uniform ' ' (w - wid - width left) hei


heighten b@(Box xs) h | h <= hei  = b
                      | otherwise = top $$ b $$ bottom
    where
      wid    = width b
      hei    = height b
      top    = uniform ' ' wid ((h - hei) `div` 2)
      bottom = uniform ' ' wid (h - hei - height top)

heighten2 :: Box -> Int -> Box
heighten2 b@(Box xs) h | h <= hei  = b
                       | otherwise = b $$ bottom
    where
      wid    = width b
      hei    = height b
      bottom = uniform ' ' wid (h - hei)

frame :: Box -> Box
frame b = (char '┌' <> horiz <> char '┐') $$
          (vert <> b <> vert) $$
          (char '└' <> horiz <> char '┘')
  where
    horiz = line (replicate (width b) '─')
    vert = uniform '│' 1 (height b)

spiral :: (Eq a, Integral a1, Num a) => a -> a1 -> Box
spiral 1 direction = char '+'
spiral n direction =
    case direction of
      0 -> (corner <> horizontalBar) $$ (sp <> space)
      1 -> (sp $$ space) <> (corner $$ verticalBar)
      2 -> (space <> sp) $$ (horizontalBar <> corner)
      _ -> (verticalBar $$ corner) <> (space $$ sp)
    where
      space         = char ' '
      corner        = char '+'
      sp            = spiral (n - 1) ((direction + 3) `mod` 4)
      verticalBar   = uniform '|' 1 (height sp)
      horizontalBar = uniform '-' (width sp) 1


main = putStrLn (show (spiral 17 0))
