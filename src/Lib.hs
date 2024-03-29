module Lib where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.DummyList.Examples
import qualified Data.MyString.Examples

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPES!
data MaritalStatus = Single | Married | Widowed
                   deriving (Show, Read, Eq)

data Gender = Male | Female
            deriving (Show, Read, Eq)

data AcademicTitle = DiS | Bc | Mgr | Ing | PhDr | MUDr | PhD | Doc | Prof
                   deriving (Show, Read, Ord, Eq, Bounded, Enum)

data Person = Person { pFirstname     :: String
                     , pLastname      :: String
                     , pGender        :: Gender
                     , pMaritalStatus :: MaritalStatus
                     , pAge           :: Int
                     , pATitles       :: [AcademicTitle]
                     }

-- | Full czech salutation (in nominative - i.e. první pád)
-- |
-- | "pan doktor Pavel Novák", "paní inženýrka Karolína Šťastná"
-- | "slečna Petra Králová", "Jan" (kid)
-- | if younger than 15 -> kid -> just firstname
-- | if female younger than 25 without academic title and single -> "slečna"
-- | otherwise "pan/paní" depending on the gender
-- | if academic titles, pick the most important (nothing for DiS and Bc)
-- |
-- | https://www.muni.cz/o-univerzite/uredni-deska/oslovovani-akademickych-pracovniku
-- | http://www.etiketavse.estranky.cz/clanky/etiketa/4.-oslovovani-a-spolecenska-vyznamnost.html
-- | http://www.studenta.cz/vysokoskolske-tituly-jak-oslovovat-na-akademicke-pude/magazin/article/587
czechSalutation :: Person -> String
czechSalutation p = unwords $ filter unEmpty $ map salute [personGender, personTitle, personFullName]
    where salute f = f p
          unEmpty = not . List.null

personGender :: Person -> String
personGender (Person _ _ g s a t)
    | a < 15 = ""
    | g == Male = "pan"
    | isMs = "slečna"
    | otherwise = "paní"
    where isMs = s == Single && a < 25 && t == []

personTitle :: Person -> String
personTitle (Person _ _ g _ _ t) = title g t
    where title _ [] = ""
          title g t
            | g == Male = manTitle bestTitle
            | otherwise = womanTitle bestTitle
          bestTitle = maximum t 

manTitle :: AcademicTitle -> String
manTitle t
    | t == Mgr = "magistr"
    | t == Ing = "inženýr"
    | t == Doc = "docent"
    | t == Prof = "profesor"
    | t `elem` [PhDr, MUDr, PhD] = "doktor"
    | otherwise = ""

womanTitle :: AcademicTitle -> String
womanTitle t
    | t == Mgr = "magistra"
    | t == Ing = "inženýrka"
    | t == Doc = "docentka"
    | t == Prof = "profesorka"
    | t `elem` [PhDr, MUDr, PhD] = "doktorka"
    | otherwise = ""

personFullName (Person f l _ _ a _)
    | a < 15 = f
    | otherwise = unwords [f, l]

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPE!
-- https://en.wikipedia.org/wiki/Allen%27s_interval_algebra
-- Notice that even DATA CONSTRUCTOR can be written in infix by using ` `
-- - it is normal, because data constructor is actually function!
--
--                                 X                Y
data AllensIAlgebraRelation a = (a, a) `Equals`   (a, a) -- X = Y
                              | (a, a) `Before`   (a, a) -- X < Y
                              | (a, a) `Meets`    (a, a) -- X m Y
                              | (a, a) `Overlaps` (a, a) -- X o Y
                              | (a, a) `Starts`   (a, a) -- X s Y
                              | (a, a) `During`   (a, a) -- X d Y
                              | (a, a) `Finishes` (a, a) -- X f Y
                             deriving (Show, Read, Eq)

-- | Compare two intervals given as tuples and return appropriate
-- | Allen's Interval Algebra relation between them
-- | It assumes that for (x, y) is always x <= y
allensComparison :: Ord a => (a, a) -> (a, a) -> AllensIAlgebraRelation a
allensComparison l@(a, b) r@(x, y)
    | shouldSwitch     = allensComparison r l
    | a == x && b == y = l `Equals` r
    | b < x            = l `Before` r
    | b == x           = l `Meets` r
    | a < x && b < y   = l `Overlaps` r
    | a == x           = l `Starts` r
    | a > x && b < y   = l `During` r
    | b == y           = l `Finishes` r
    where shouldSwitch = y < b || (y <= b && x > a)

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPE!
data Shape2D = Circle { ciRadius :: Double }
             | Square { sqSide :: Double }
             | Rectangle { reWidth :: Double, reHeight :: Double }
             | Triangle { triSideA :: Double, triSideB :: Double, triSideC :: Double }
             deriving (Show, Read, Eq)

shapeCircumference :: Shape2D -> Double
shapeCircumference (Circle radius) = 2 * pi * radius
shapeCircumference (Square edge) = 4 * edge
shapeCircumference (Rectangle width height) = 2 * (width + height)
shapeCircumference (Triangle a b c) = a + b + c

shapeArea :: Shape2D -> Double
shapeArea (Circle radius) = pi * radius ^ 2
shapeArea (Square edge) = edge ^ 2
shapeArea (Rectangle width height) = width * height
shapeArea triangle@(Triangle a b c) = sqrt $ s * (s - a) * (s - b) * (s - c)
    where s = (shapeCircumference triangle) / 2

-------------------------------------------------------------------------------
-- | Geometric sequence as infinite list
-- | https://en.wikipedia.org/wiki/Geometric_progression
geometricSequence :: Num b => b -> b -> [b]
geometricSequence a r = sequence a 1 r
    where sequence :: Num b => b -> b -> b -> [b]
          sequence a exp ratio = a * exp : sequence a (exp * ratio) ratio

primes :: [Integer]
primes = filter isPrime [2..]
    where isPrime n = length (take 1 $ factors n) == 0
          factors n = [x | x <- [2..n-1], n `mod` x == 0]

factorization :: Integer -> [Integer]
factorization n
    | n < 2 = []
    | otherwise = lowestFactor : (factorization $ n `div` lowestFactor)
        where lowestFactor = head $ primeFactors n
              primeFactors n = [x | x <- primes, n `mod` x == 0] 

-- | Euler's totient function
-- | https://en.wikipedia.org/wiki/Euler%27s_totient_function
phi :: Integer -> Integer
phi n = round $ fromIntegral num * productFormula num
    where num = abs n
          productFormula n = product $ map productMember $ unique $ factorization n
          productMember p = 1 - 1 / (fromIntegral p)
          unique = Set.toList . Set.fromList

-------------------------------------------------------------------------------
-- !!! DO NOT COPY, JUST IMPORT (avoid conflicts, pick the best option for you)
-- iii visit the content of modules
dummyListExample1 = Data.DummyList.Examples.example1

stringExample2 = Data.MyString.Examples.example2

stringExample3 = Data.MyString.Examples.example3
