import Test.Hspec

import Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "czechSalutation" $ do
    it "salutates to kids just with firstname" $ do
      czechSalutation (Person "Tom" "Sawyer" Male Single 12 []) `shouldBe` "Tom"
      czechSalutation (Person "Alice" "Wonderland" Female Single 7 []) `shouldBe` "Alice"
    it "salutates men without titles" $ do
      czechSalutation (Person "Prokop" "Buben" Male Single 15 []) `shouldBe` "pan Prokop Buben"
      czechSalutation (Person "Aleš" "Novák" Male Married 20 []) `shouldBe` "pan Aleš Novák"
      czechSalutation (Person "Jan" "Nerdský" Male Single 35 []) `shouldBe` "pan Jan Nerdský"
      czechSalutation (Person "Alfons" "Příbramský" Male Widowed 100 []) `shouldBe` "pan Alfons Příbramský"
    it "salutates women without titles" $ do
      czechSalutation (Person "Petra" "Novotná" Female Single 17 []) `shouldBe` "slečna Petra Novotná"
      czechSalutation (Person "Lucie" "Smutná" Female Widowed 19 []) `shouldBe` "paní Lucie Smutná"
      czechSalutation (Person "Emma" "Mléko" Female Married 20 []) `shouldBe` "paní Emma Mléko"
      czechSalutation (Person "Dita" "Zelená" Female Single 35 []) `shouldBe` "paní Dita Zelená"
      czechSalutation (Person "Aloisie" "Příbramská" Female Widowed 100 []) `shouldBe` "paní Aloisie Příbramská"
    it "salutates men with titles" $ do
      czechSalutation (Person "Prokop" "Buben" Male Single 20 [Mgr]) `shouldBe` "pan magistr Prokop Buben"
      czechSalutation (Person "Sheldon" "Cooper" Male Single 30 [PhD]) `shouldBe` "pan doktor Sheldon Cooper"
      czechSalutation (Person "Ivo" "Zlý" Male Widowed 35 [DiS, Bc]) `shouldBe` "pan Ivo Zlý"
      czechSalutation (Person "Tomáš" "Marný" Male Married 35 [DiS, Bc, Ing, PhD]) `shouldBe` "pan doktor Tomáš Marný"
      czechSalutation (Person "Josef" "Šteng" Male Single 38 [Mgr, PhDr]) `shouldBe` "pan doktor Josef Šteng"
      czechSalutation (Person "Bruno" "Marek" Male Single 55 [MUDr]) `shouldBe` "pan doktor Bruno Marek"
      czechSalutation (Person "Mojmír" "Čepek" Male Married 69 [Mgr, Ing, Doc]) `shouldBe` "pan docent Mojmír Čepek"
      czechSalutation (Person "Miroslav" "Král" Male Widowed 83 [Ing, Prof]) `shouldBe` "pan profesor Miroslav Král"
    it "salutates women with titles" $ do
      czechSalutation (Person "Klára" "Jílková" Female Single 20 [Mgr]) `shouldBe` "paní magistra Klára Jílková"
      czechSalutation (Person "Amy Farrah" "Fowler" Female Single 30 [PhD]) `shouldBe` "paní doktorka Amy Farrah Fowler"
      czechSalutation (Person "Andrea" "Zlá" Female Widowed 35 [DiS, Bc]) `shouldBe` "paní Andrea Zlá"
      czechSalutation (Person "Zdislava" "Zděšená" Female Married 35 [DiS, Bc, Ing, PhD]) `shouldBe` "paní doktorka Zdislava Zděšená"
      czechSalutation (Person "Zuzana" "Zimová" Female Single 42 [Mgr, PhDr]) `shouldBe` "paní doktorka Zuzana Zimová"
      czechSalutation (Person "Marie" "Rusová" Female Single 57 [MUDr]) `shouldBe` "paní doktorka Marie Rusová"
      czechSalutation (Person "Anastazie" "Rudá" Female Married 66 [Mgr, Ing, Doc]) `shouldBe` "paní docentka Anastazie Rudá"
      czechSalutation (Person "Denisa" "Šídlová" Female Widowed 74 [Ing, PhD, Prof]) `shouldBe` "paní profesorka Denisa Šídlová"

  describe "allensComparison" $ do
    it "returns Equals for equal internals" $ do
      allensComparison (1, 2) (1, 2) `shouldBe` Equals (1, 2) (1, 2)
      allensComparison (-2, 2) (-2, 2) `shouldBe` Equals (-2, 2) (-2, 2)
      allensComparison ('a', 'z') ('a', 'z') `shouldBe` Equals ('a', 'z') ('a', 'z')
    it "returns Before when end of one is before start of other" $ do
      allensComparison (1, 2) (4, 5) `shouldBe` Before (1, 2) (4, 5)
      allensComparison (-2, 0) (1, 2) `shouldBe` Before (-2, 0) (1, 2)
      allensComparison ('a', 'c') ('x', 'z') `shouldBe` Before ('a', 'c') ('x', 'z')
    it "returns Before when end of one is before start of other (inversed)" $ do
      allensComparison (4, 5) (1, 2) `shouldBe` Before (1, 2) (4, 5)
      allensComparison (1, 2) (-2, 0) `shouldBe` Before (-2, 0) (1, 2)
      allensComparison ('x', 'z') ('a', 'c') `shouldBe` Before ('a', 'c') ('x', 'z')
    it "returns Meets when end of one is equal to start of other" $ do
      allensComparison (1, 2) (2, 5) `shouldBe` Meets (1, 2) (2, 5)
      allensComparison (-2, 0) (0, 2) `shouldBe` Meets (-2, 0) (0, 2)
      allensComparison ('a', 'c') ('c', 'z') `shouldBe` Meets ('a', 'c') ('c', 'z')
    it "returns Meets when end of one is equal to start of other (inversed)" $ do
      allensComparison (2, 5) (1, 2) `shouldBe` Meets (1, 2) (2, 5)
      allensComparison (0, 2) (-2, 0) `shouldBe` Meets (-2, 0) (0, 2)
      allensComparison ('c', 'z') ('a', 'c') `shouldBe` Meets ('a', 'c') ('c', 'z')
    it "returns Overlaps when there is common part but first starts sooner and second end later" $ do
      allensComparison (1, 4) (2, 5) `shouldBe` Overlaps (1, 4) (2, 5)
      allensComparison (-2, 1) (-1, 2) `shouldBe` Overlaps (-2, 1) (-1, 2)
      allensComparison ('a', 'm') ('i', 'z') `shouldBe` Overlaps ('a', 'm') ('i', 'z')
    it "returns Overlaps when there is common part but first starts sooner and second end later (inversed)" $ do
      allensComparison (2, 5) (1, 4) `shouldBe` Overlaps (1, 4) (2, 5)
      allensComparison (-1, 2) (-2, 1) `shouldBe` Overlaps (-2, 1) (-1, 2)
      allensComparison ('i', 'z') ('a', 'm') `shouldBe` Overlaps ('a', 'm') ('i', 'z')
    it "returns Starts if one starts with the other but ends sooner" $ do
      allensComparison (1, 5) (1, 10) `shouldBe` Starts (1, 5) (1, 10)
      allensComparison (-1, 2) (-1, 7) `shouldBe` Starts (-1, 2) (-1, 7)
      allensComparison ('b', 'e') ('b', 'm') `shouldBe` Starts ('b', 'e') ('b', 'm')
    it "returns Starts if one starts with the other but ends sooner (inversed)" $ do
      allensComparison (1, 10) (1, 5) `shouldBe` Starts (1, 5) (1, 10)
      allensComparison (-1, 7) (-1, 2) `shouldBe` Starts (-1, 2) (-1, 7)
      allensComparison ('b', 'm') ('b', 'e') `shouldBe` Starts ('b', 'e') ('b', 'm')
    it "returns Finishes if one ends with the other but starts sooner" $ do
      allensComparison (5, 10) (1, 10) `shouldBe` Finishes (5, 10) (1, 10)
      allensComparison (2, 7) (-1, 7) `shouldBe` Finishes (2, 7) (-1, 7)
      allensComparison ('e', 'm') ('b', 'm') `shouldBe` Finishes ('e', 'm') ('b', 'm')
    it "returns Finishes if one ends with the other but starts sooner (inversed)" $ do
      allensComparison (1, 10) (5, 10) `shouldBe` Finishes (5, 10) (1, 10)
      allensComparison (-1, 7) (2, 7) `shouldBe` Finishes (2, 7) (-1, 7)
      allensComparison ('b', 'm') ('e', 'm') `shouldBe` Finishes ('e', 'm') ('b', 'm')
    it "returns During if one starts after the other and also ends before the other" $ do
      allensComparison (5, 10) (1, 20) `shouldBe` During (5, 10) (1, 20)
      allensComparison (2, 7) (-1, 10) `shouldBe` During (2, 7) (-1, 10)
      allensComparison ('e', 'm') ('a', 'z') `shouldBe` During ('e', 'm') ('a', 'z')
    it "returns During if one starts after the other and also ends before the other (inversed)" $ do
      allensComparison (1, 20) (5, 10) `shouldBe` During (5, 10) (1, 20)
      allensComparison (-1, 10) (2, 7) `shouldBe` During (2, 7) (-1, 10)
      allensComparison ('a', 'z') ('e', 'm') `shouldBe` During ('e', 'm') ('a', 'z')

  describe "shapeCircumference" $ do
    it "computes circumference of circle" $ do
      shapeCircumference (Circle 5) `shouldBe` 10.0 * pi
      shapeCircumference (Circle 1) `shouldBe` 2.0 * pi
      shapeCircumference (Circle 10) `shouldBe` 20.0 * pi
    it "computes circumference of square" $ do
      shapeCircumference (Square 5)  `shouldBe` 20
      shapeCircumference (Square 1) `shouldBe` 4
      shapeCircumference (Square 10) `shouldBe` 40
    it "computes circumference of rectangle" $ do
      shapeCircumference (Rectangle 2 5) `shouldBe` 14
      shapeCircumference (Rectangle 5 5) `shouldBe` 20
      shapeCircumference (Rectangle 3 1) `shouldBe` 8
    it "computes circumference of triangle" $ do
      shapeCircumference (Triangle 2 5 4) `shouldBe` 11
      shapeCircumference (Triangle 5 5 5) `shouldBe` 15
      shapeCircumference (Triangle 3 2 4) `shouldBe` 9

  describe "shapeArea" $ do
    it "computes area of circle" $ do
      shapeArea (Circle 5) `shouldBe` 25 * pi
      shapeArea (Circle 1) `shouldBe` pi
      shapeArea (Circle 10) `shouldBe` 100 * pi
    it "computes area of square" $ do
      shapeArea (Square 5) `shouldBe` 25
      shapeArea (Square 1) `shouldBe` 1
      shapeArea (Square 10) `shouldBe` 100
    it "computes circumference of rectangle" $ do
      shapeArea (Rectangle 2 5) `shouldBe` 10
      shapeArea (Rectangle 5 5) `shouldBe` 25
      shapeArea (Rectangle 3 1) `shouldBe` 3
    it "computes area of triangle" $ do
      shapeArea (Triangle 3 5 4) `shouldBe` 6
      shapeArea (Triangle 5 5 5) `shouldBe` sqrt 117.1875
      shapeArea (Triangle 3 2 4) `shouldBe` sqrt 8.4375

  describe "geometricSequence" $ do
    it "has head always equal to a" $ do
      head (geometricSequence 5 0) `shouldBe` 5
      head (geometricSequence 5 10) `shouldBe` 5
      head (geometricSequence 5 0.5) `shouldBe` 5
      head (geometricSequence 5 (-2)) `shouldBe` 5
    it "works with  =1 as repeat" $ do
      take 20 (geometricSequence 5 1) `shouldBe` replicate 20 5
      take 20 (geometricSequence (-5) 1) `shouldBe` replicate 20 (-5)
      take 20 (geometricSequence 0 1) `shouldBe` replicate 20 0
    it "has just zeroes in tail if r=0" $ do
      take 20 (tail (geometricSequence 5 0)) `shouldBe` replicate 20 0
      take 20 (tail (geometricSequence (-5) 0)) `shouldBe` replicate 20 0
      take 20 (tail (geometricSequence 0 0)) `shouldBe` replicate 20 0
    it "works with integers r>1" $ do
      take 10 (geometricSequence 2 2) `shouldBe` [2,4,8,16,32,64,128,256,512,1024]
      take 10 (geometricSequence 5 2) `shouldBe` [5,10,20,40,80,160,320,640,1280,2560]
      take 10 (geometricSequence 2 3) `shouldBe` [2,6,18,54,162,486,1458,4374,13122,39366]
    it "work with integers r<-1" $ do
      take 10 (geometricSequence 2 (-1)) `shouldBe` [2,-2,2,-2,2,-2,2,-2,2,-2]
      take 10 (geometricSequence (-1) 2) `shouldBe` [-1,-2,-4,-8,-16,-32,-64,-128,-256,-512]
      take 10 (geometricSequence 1 (-2)) `shouldBe` [1,-2,4,-8,16,-32,64,-128,256,-512]
      take 10 (geometricSequence (-1) (-2)) `shouldBe` [-1,2,-4,8,-16,32,-64,128,-256,512]
    it "work with integers -1>r>1" $ do
      take 10 (geometricSequence 128 0.5) `shouldBe` [128.0,64.0,32.0,16.0,8.0,4.0,2.0,1.0,0.5,0.25]
      take 10 (geometricSequence 1024 (-0.5)) `shouldBe` [1024.0,-512.0,256.0,-128.0,64.0,-32.0,16.0,-8.0,4.0,-2.0]
      take 10 (geometricSequence 1048576 0.25) `shouldBe` [1048576.0,262144.0,65536.0,16384.0,4096.0,1024.0,256.0,64.0,16.0,4.0]
    it "looks like infinite" $ do
      geometricSequence 1 2 `shouldContain` [2^100]
      geometricSequence (-18) 2 `shouldContain` [(-18) * 2^129]
      geometricSequence 1 5 `shouldContain` [5^1000]

  describe "primes" $ do
    it "contains basic primes" $ do
      primes `shouldContain` [2]
      primes `shouldContain` [3]
      primes `shouldContain` [5]
      primes `shouldContain` [7]
      primes `shouldContain` [11]
    it "contains various big primes" $ do
      primes `shouldContain` [9931]
      primes `shouldContain` [12941]
      primes `shouldContain` [35323]
      primes `shouldContain` [56767]
    it "does not contain composite number" $ do
      take 3000 primes `shouldNotContain` [4]
      take 3000 primes `shouldNotContain` [27]
      take 3000 primes `shouldNotContain` [99]
      take 3000 primes `shouldNotContain` [666]
      take 3000 primes `shouldNotContain` [2942]
    it "does not contain non primes" $ do
      take 3000 primes `shouldNotContain` [1]
      take 3000 primes `shouldNotContain` [0]
      take 3000 primes `shouldNotContain` [-1]
      take 3000 primes `shouldNotContain` [-2]
      take 3000 primes `shouldNotContain` [-7]
    it "has primes on correct positions" $ do
      head primes `shouldBe` 2
      primes !! 4 `shouldBe` 11
      primes !! 141 `shouldBe` 821
      primes !! 666 `shouldBe` 4987
      primes !! 2653 `shouldBe` 23869

  describe "factorization" $ do
    it "returns empty list for zero and one(s)" $ do
      factorization 0 `shouldBe` []
      factorization 1 `shouldBe` []
      factorization (-1) `shouldBe` []
    it "give single factor (itself) for primes" $ do
      factorization 2 `shouldBe` [2]
      factorization 5 `shouldBe` [5]
      factorization 13 `shouldBe` [13]
      factorization 9931 `shouldBe` [9931]
    it "give prime factors of compound (single) in ascending order" $ do
      factorization 15 `shouldBe` [3,5]
      factorization 77 `shouldBe` [7,11]
      factorization 210 `shouldBe` [2,3,5,7]
    it "give prime factors of compound (repeating) in ascending order" $ do
      factorization 8 `shouldBe` [2,2,2]
      factorization 45 `shouldBe` [3,3,5]
      factorization 180 `shouldBe` [2,2,3,3,5]

  describe "phi" $ do
    it "works for trivial cases" $ do
      phi 0 `shouldBe` 0
      phi 1 `shouldBe` 1
      phi (-1) `shouldBe` 1
    it "works for primes" $ do
      phi 2 `shouldBe` 1
      phi 3 `shouldBe` 2
      phi 167 `shouldBe` 166
      phi (-4973) `shouldBe` 4972
      phi 56767 `shouldBe` 56766
    it "works for composite numbers" $ do
      phi 10 `shouldBe` 4
      phi 16 `shouldBe` 8
      phi 666 `shouldBe` 216
      phi (-15684) `shouldBe` 5224
      phi 56766 `shouldBe` 18920

  describe "imports" $ do
    it "has correct example 1" $
      take 100 dummyListExample1 `shouldBe` replicate 100 25
    it "has correct example 2" $
      stringExample2 `shouldBe` "Example 2"
    it "has correct example 3" $
      stringExample3 `shouldBe` "Example 3"
