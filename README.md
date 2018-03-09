# MI-AFP homework #03

Homework to practice different ways of branching in Haskell and modularization

## Task

Open `src/Lib.hs` and implement all the TODOs there. You can also check the specification in `test/Spec.hs` and run the tests with `stack test`.

To complete you will need to work also with `src/Examples.hs` and `src/Data/DummyList/Examples.hs`, but you are **not allowed** to change those nor data types in `src/Lib.hs` and it is also forbidden to add new dependencies.

1. `czechSalutation` should return salutation (in nominative, 1st case = "1. pád") in the Czech language for given person (gender, age, marital status and academic titles must be used appropriately). For details read the comment in code and check test specification, you can also read [(1)](https://www.muni.cz/o-univerzite/uredni-deska/oslovovani-akademickych-pracovniku), [(2)](http://www.etiketavse.estranky.cz/clanky/etiketa/4.-oslovovani-a-spolecenska-vyznamnost.html), and [(3)](http://www.studenta.cz/vysokoskolske-tituly-jak-oslovovat-na-akademicke-pude/magazin/article/587) if interested (Czech only). *Do not edit prepared data types, just implement the function.*
2. `allensComparison` should return relation in [Allen's Interval Algebra](https://en.wikipedia.org/wiki/Allen%27s_interval_algebra) of two given intervals as tuples. Notice that you can use data constructor in infix notation. *Do not edit prepared data type, just implement the function.*
3. `shapeCircumference` and `shapeArea` should return circumference and area of given shape (can be Circle, Square, Rectangle, or Triangle). *Do not edit prepared data type, just implement the function.*
4. `geometricSequence` should return for given `a` (first parameter) and `r` (second parameter) a [geometric sequence](https://en.wikipedia.org/wiki/Geometric_progression) as endless list.
5. `primes` should return an endless list of [primes](https://en.wikipedia.org/wiki/List_of_prime_numbers). Try to find a solution on your own, don't think too much about effectiveness and enjoy laziness with Haskell. You can use the definition of prime which leads to [generating primes](https://en.wikipedia.org/wiki/Generating_primes).
6. `factorization` should return for a given number a sorted list of its [prime factors](https://en.wikipedia.org/wiki/Prime_factor) in ascending order (e.g. for `50` the result should be `[2,5,5]`, because of `2*5*5 = 50`). Using `primes` is recommended.
7. `phi` should work as [Euler's totient function](https://en.wikipedia.org/wiki/Euler%27s_totient_function) known also as &#966; or &#981; (Greek letter [Phi](https://en.wikipedia.org/wiki/Phi). You may use `primes` and `factorization` to make your life easier.
8. `dummyListExample1`, `stringExample2`, `stringExample2` should be assigned with values from `Data.DummyList.Examples` (`example1`) and `Data.MyString.Examples` (`example2` and `example3`). It is **not allowed** to copy or "implement" them, working import must be used.

Hints & general requirements: 

* Being [DRY](https://cs.wikipedia.org/wiki/Don%27t_repeat_yourself) is essential, do not repeat code (for example, in inversed comparison of intervals).
* Local names (via `where` or `let-in`) in functions should be introduced to make the code more readable. Creating helper functions in module scope is **awful**.
* Avoid using `if-then-else` with patterns and guards (and/or combination of those two) if possible and better for readability.
* Look up functions that can help you (`Prelude`, `Data.List`) with general tasks like finding a maximum in list, converting `Integers` to generic numbers, or getting unique values from a list. *Do not re-invent the wheel!*
* You must understand your code completely!

## Notes 

 * In case of uncertainty, check the [dummy homework](https://github.com/MI-AFP/hw00) to recall what is the homework workflow for this course.
 * If you encounter some trouble, create an issue in your repository.
 * In case you find a bug or have an idea how to improve assignment project, create an issue or PR in this repository.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE)
file for more details.
