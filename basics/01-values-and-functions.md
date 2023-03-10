# Values and functions (& basic types)

- [Values and functions (& basic types)](#values-and-functions--basic-types)
  - [Values](#values)
    - [Exercises (Values)](#exercises-values)
  - [Functions](#functions)
    - [Boolean & arithmetic operations](#boolean--arithmetic-operations)
    - [Exercises (Functions)](#exercises-functions)
      - [Exercise notes (Functions)](#exercise-notes-functions)
  - [Asking questions about values](#asking-questions-about-values)
    - [`if` expressions](#if-expressions)
    - [Guards](#guards)
    - [Exercises (Asking questions about values)](#exercises-asking-questions-about-values)
      - [Exercise notes (Asking questions about values)](#exercise-notes-asking-questions-about-values)
  - [`$` for function application](#-for-function-application)
  - [Partial application](#partial-application)
    - [Interlude: Lambdas / Anonymous functions](#interlude-lambdas--anonymous-functions)
    - [Common design patterns with partial application](#common-design-patterns-with-partial-application)
    - [Exercises (Partial application)](#exercises-partial-application)
      - [Exercise notes (Partial application)](#exercise-notes-partial-application)
  - [Pipelines using partial application](#pipelines-using-partial-application)
    - [A "Project Euler" example](#a-project-euler-example)
    - [Exercises (Pipelines using partial application)](#exercises-pipelines-using-partial-application)
      - [Exercise notes (Pipelines using partial application)](#exercise-notes-pipelines-using-partial-application)
  - [A note on functions, their parameter order and partial application](#a-note-on-functions-their-parameter-order-and-partial-application)
    - [Don't worry about mis-designing this](#dont-worry-about-mis-designing-this)

All programs are made up out of values and functions that operate on those values. Here we'll look
at those basic building blocks.

## A note about dependencies

PureScript has comparatively great dependency management, but it also follows a philosophy when it
comes to dependencies that may catch you off guard. Many functions you might expect to be in
`Prelude` are not. When a module requires a dependency we will note the import statement at the
top of the code snippet with a comment that says how to install it. For example:

```purescript
import Prelude

import Data.String.Utils as StringUtils -- run `spago install stringutils && spago build`

hasCorrectScope :: String -> Boolean
hasCorrectScope string = StringUtils.startsWith ":scope:" string
```

## Values

Before a value or a function definition comes the type of the definition. This is by convention to a
large degree, we don't actually **have to** specify types for a lot of things depending on whether
or not the compiler can figure them out itself. If you don't have a type for a definition in your
module (at the "top-level", meaning not inside an inner scope of a function) you'll get a warning
that says as much. Type signatures are both a specification and a very basic documentation tool and
adding them to your code is a generally good idea.

A type signature is written with the name of the thing it's for, followed by `::` and then the type:

```purescript
myValue :: Int
```

The actual definition for it comes immediately after and is written with `=` to signify that the
name on the left is equal to the expression on the right:

```purescript
myValue :: Int
myValue = 42
```

The basic types in PureScript:

```purescript
module BasicTypes where

import Prelude

myAnswerToEverything :: Int
myAnswerToEverything = 42

myChar :: Char -- A single character
myChar = '$'

myNumber :: Number -- Always written with a decimal point in PureScript
myNumber = 1337.0

myBool :: Boolean -- `true` or `false`
myBool = false

myString :: String
myString = "This is not the best string in the world, it's just a tribute"

-- `String`s also handle "exotic" script
helloThere :: String
helloThere = "?? ???????? ???????????? ???????????? ???????????????? ?? ??????????????"
```

### Exercises (Values)

1. Define a value with the name `myOwnValue` with the type `Number` and the value `42`. What
   happens?

2. Define a value with the type `String` and the value `'hello'`. What happens and why do you think
   that is?

## Functions

Functions are written much like values, but have arrows in their type. The types, starting at the
leftmost, represent the arguments/parameters passed to the function, with the final (rightmost) one
representing the return value.

```purescript
import Prelude

--          a      b     result
addInts :: Int -> Int -> Int
addInts a b = a + b
```

Calling functions, as we can see above, can be done via infix notation when they are operators. We
could also call the operator in a prefix position:

```purescript
import Prelude

--          a      b     result
addInts :: Int -> Int -> Int
addInts a b = (+) a b
```

The application of a function is done by just writing the function name followed by a space and then
spaces between the parameters, making it as lightweight an operation as possible.

```purescript
import Prelude

import Data.Int as Int

--                x    divisor result
isDivisibleBy :: Int -> Int -> Boolean
isDivisibleBy x divisor =
  -- When we want to divide our problem into smaller parts we can use `let`.
  -- `rem` here is a function that takes an integer and returns the remainder of dividing it by the
  -- second argument. It is the same as the `%` operator (modulo) from C, JavaScript, and so on.
  let remainderOfDivision = Int.rem x divisor
  -- `let` has to be followed by `in` and then the final expression that is to be executed.
   in remainderOfDivision == 0
```

`rem` in the above code snippet is a function that takes an integer-like number and returns the
remainder of dividing it by the second argument. Here we check whether or not that division returns
0 to determine if it was divisible by it.

We could also write the division as follows:

```purescript
import Prelude

import Data.Int as Int

--                x    divisor result
isDivisibleBy :: Int -> Int -> Boolean
isDivisibleBy x divisor =
  -- Note how we surround the function in backticks (`) to be able to put it in the infix position.
  let remainderOfDivision = x `Int.rem` divisor
   in remainderOfDivision == 0
```

This can be used to make code read more intuitively and is very common in the case of checking if
something is a member of a map, list or the like:

```purescript
import Data.Map as Map

key `Map.member` ourMap -- Is the key defined in the map?
```

```purescript
import Data.List as List

element `List.elem` ourList -- Is the element present in the list?
```

We can use this with any function, even functions with more than two arguments. It's important to
recognize that it's not always a great idea to use this feature and each case should be examined on
an individual basis in terms of whether or not it makes the code more or less easy to understand.

### Boolean & arithmetic operations

|   Math  |  PureScript                   |                Notes                    |
| :-----: |  :--------------------------: | :-------------------------------------: |
|    +    |       +                       |                                         |
|    -    |       -                       |                                         |
|    *    |       *                       |                                         |
|    /    |       /                       |                                         |
|    >    |       >                       |                                         |
|    <    |       <                       |                                         |
|    ???    |      >=                       |                                         |
|    ???    |      <=                       |                                         |
|    =    |      ==                       |                                         |
|    ???    |      /=                       |                                         |
|    x???   |    x `Int.pow` n              | x is numeric, n is integral             |
|    x???   |    x `Number.pow` n           | x & n are both floating point values    |
| x mod n |  x `rem` n                    | x and n are integral [0]                |
| x mod n |  x `mod` n                    | x and n are integral [0]                |

| Math |  C   | PureScript |        Notes        |
| :--: | :-:  |  :-----:   | :-----------------: |
|  ??   |  !   |    not     |                     |
|  ???   |  &&  |    &&      |                     |
|  ???   |  ????  |    ????      | Two pipe characters |

[0]:

```purescript
-- Note: Use `rem` if you know your parameters are both positive

5 `mod` 3 == 2
5 `rem` 3 == 2

5 `mod` (-3) == -1
5 `rem` (-3) == 2

(-5) `mod` 3 == 1
(-5) `rem` 3 == -2

(-5) `mod` (-3) == -2
(-5) `rem` (-3) == -2
```

Arithmetic can only be done with numbers of the same type, so what happens when we want to divide an
integer number with a floating point number? Let's see:

```purescript
import Prelude

import Data.Int as Int -- run `spago install integers && spago build`

--              integer  number   result
divideInteger :: Int -> Number -> Number
-- This is the same as `(Int.toNumber integer) / number` because `/` divides everything on the left by
-- everything on the right.
divideInteger :: Int -> Number -> Number
divideInteger integer number = Int.toNumber integer / number
```

Likewise we can also take a number and turn it into an integer, though this can obviously lead to a
loss of precision in our calculations:

```purescript
import Prelude

import Data.Int as Int -- run `spago install integers && spago build`

subtractRoundedNumber :: Int -> Number -> Int
subtractRoundedNumber int number = int - Int.round number
```

If we were to run this we'd see:

```purescript
> subtractRoundedNumber 5 5.5
-1
> subtractRoundedNumber 5 5.4
0
```

### Exercises (Functions)

1. Define a function that returns whether or not an `Int` is zero.

```purescript
> isZero 1
false
> isZero 0
true
```

2. Define a function that returns whether or not a `Number` is greater than zero.

```purescript
> isGreaterThanZero 1.2
true
> isGreaterThanZero 0.0
false
```

3. Define a function that adds 1/10th of a Double to itself.

```purescript
> addOneTenth 1.0
1.1
> addOneTenth 0.1
0.11000000000000001
```

4. Define a function that takes 2 `Number`s `length'` & `width` and returns the area of the rectangle
   they make up.

```purescript
> rectangleArea 2.5 3.0
7.5
> rectangleArea 2.0 2.0
4.0
```

5. Define a function that takes a radius of type `Number` and returns the area of a circle[0].

```purescript
> circleArea 3.5
38.484512
> circleArea 2.1
13.854422
```

6. Define a function `calculateBMI` that takes a `Number` representing weight in kilograms and an
   `Int` representing height in centimeters and returns the person's BMI.

```purescript
> bmi 90.0 185 
26.3
> bmi 60.0 165
22.0
```

#### Exercise notes (Functions)

0. `pi` can be imported from `Data.Number` from the package `numbers`. Install it by running
   `spago install numbers && spago build`.

## Asking questions about values

Quite regularly we will have to pose some kind of question about the structure of a value, or
whether or not it matches some kind of predicate.

### `if` expressions

`if` in PureScript is a lot like `if` in other languages, with the one slight difference to some of
them that both the `true` and `false` branch need to be present, and both branches need to return
the same type of value. This is because `if`, like most other things in PureScript, is an expression
and the result can be bound to a name.

```purescript
value :: Int -> Int
value x = if x < 10 then x else 10

value' :: Int -> Int
value' x =
  if x < 10
    then x
    else 10
```

Let's look at a few ways to ask questions about values in the case of "clamping" a number.

### Guards

```purescript
import Prelude

-- | Limits a given integer to be within the range @lowerBound <= value <= upperBound@.
clamp :: Int -> Int -> Int -> Int
clamp lowerBound upperBound value
  | value < lowerBound = lowerBound
  | value > upperBound = upperBound
  | otherwise = value
```

The above is likely the most natural way of doing this in this very example, because we have several
questions to ask about the value and we can do so immediately in what are known as "guards".

Note that we do not have an immediate `=` after our parameters but instead each `|` introduces a new
question that we pose, a new **guard**. If the boolean expression that follows the pipe (`|`)
evaluates to `true` the expression to the right of `=` is what will be returned.

The word `otherwise` is an always matching case and we can use this case as an "for all other cases"
clause.

### Exercises (Asking questions about values)

1. Define a function that takes two `Int`s and returns the biggest of the two. Implement it both
   with function guards as well as `if`.

```purescript
> biggest 2 1
2
> biggest 1 2
2
> biggest 1 (-2)
1
```

2. Define a function that takes two `Int`s and returns the smallest of the two. Implement it both
   with function guards as well as `if`.

```purescript
> smallest 2 1
1
> smallest 1 2
1
> smallest 1 (-2)
-2
```

3. Define a function that subtracts an integer from another, but if the result is less than zero,
   instead return `0`.

```purescript
> subtractPositive 3 1
2
> subtractPositive 1 3
0
```

4. Define a function that takes an `Int` and if it's smaller than zero returns `0`, if it's bigger
   than 255 returns `255`. Otherwise it returns the integer itself.

```purescript
> clampByteValue 365
255
> clampByteValue (-255)
0
> clampByteValue 128
128
```

#### Exercise notes (Asking questions about values)

## `$` for function application

Sometimes you will see a dollar sign operator (`$`) in code. This is actually a utility operator
meant for function application:

```purescript
f :: Int -> String
f x = show (deriveFlorbFactorFromMerkleNumber (castToMerkleNumber x))

f' :: Int -> String
f' x = show $ deriveFlorbFactorFromMerkleNumber $ castToMerkleNumber x
```

As you can see from the above snippet, using `$` allows us to just say "Apply the function on the
left to the expression on the right". `$` binds tightly to the right, so the parentheses we see
above is what will be by default, hence we have no need for parentheses like this when we use `$`.

## Partial application

When you apply a function, you can choose to **not** pass all the arguments it's expecting. This
will result in a function that expects the remaining arguments and that will have the same return
value:

```purescript
import Prelude

addInts :: Int -> Int -> Int
addInts a b = a + b

add42 ::          Int -> Int
add42 = addInts 42
```

Passing only one arguments to `addInts` in the example above results in a function that expects one
more argument instead of the original two.

It's quite common to use this fact by putting important arguments in the last parameter position of
a function in order to let people use this pattern.

### Interlude: Lambdas / Anonymous functions

In the code snippet above we are using what is called an "anonymous function". This is a function
defined in-place and passed to another function. These are also commonly called "lambdas". A lambda
is written beginning with `\` (because it is supposed to resemble ??, an actual lambda), followed by
the arguments that the function takes. After the arguments we have an arrow and what follows is the
expression of the function, i.e. what usually comes after `=` in a normal function definition.

### Common design patterns with partial application

```purescript
import Prelude

-- | Adds 42 to every item in a list
add42ToAll :: Array Int -> Array Int
add42ToAll = map (\x -> x + 42)

add42ToAll' :: Array Int -> Array Int
add42ToAll' array = map (\x -> x + 42) array
```

In this example we are creating a lambda that takes one argument and adds 42 to it. This is passed
to a partially applied `map` that takes an array of `Int` as the second argument. When we don't give
this argument, what we get back is exactly the type signature that `add42ToAll` has:
`Array Int -> Array Int`. The second version makes this argument visible. As a rule, when you have
arguments both on the left side in the same order as you have them on the right side of a
definition you can remove them on both sides. `list` appears here as the last argument both in the
arguments and the implementation and so we can get rid of it, to talk only about the essence of the
function; "mapping" over a list and adding 42 to each item in the list.

We can also create a function that does the same thing as `(\x -> x + 42)` without naming the
parameter:

```purescript
import Prelude

-- | Adds 42 to every item in a list
add42ToAll :: Array Int -> Array Int
add42ToAll = map (_ + 42) -- could also be `(42 + _)`
```

The underscore here represents a placeholder where the argument we are expecting will go. In the
case of `+` it doesn't really matter which side it goes on, but in the case of `-`, for example, it
would:

```purescript
import Prelude

-- | Subtracts 42 from every item in a list
subtract42FromAll :: Array Int -> Array Int
subtract42FromAll = map (_ - 42)

-- | Subtracts every item in a list from 42
subtractAllFrom42 :: Array Int -> Array Int
subtractAllFrom42 = map (42 - _)
```

### Exercises (Partial application)

1. Define a function that takes a list of `Int`s and multiplies each with `2`. Remember `map`[0].

```purescript
> multiplyAllByTwo [1, 2, 3]
[2, 4, 6]
> multiplyAllByTwo []
[]
```

2. Define a function that takes a list of `Int`s and squares each. Remember `map`[0].

```purescript
> squareAll [1, 2, 3]
[1, 4, 9]
> squareAll []
[]
> squareAll [-1, -2, -3]
[1, 4, 9]
```

3. `filter`[1] is a function that takes all elements that match a predicate from an array. This
   predicate is always a function that takes an element in the array and returns a boolean value.
   Define a function that takes all elements that are equal to `0` from an array. Use partial
   application when using `filter`.

```purescript
> import Data.Array ((..))
> allZeros [0, 1, 0, 2, 0, 3]
[0, 0, 0]
> allZeros (1..9)
[]
```

4. Define a function that returns all of the `Int`s in a list over `0`, use a placeholder for
   constructing the predicate function and partial application when using `filter`.

```purescript
> numbersAboveZero [0, 1, 0, 2, 0, 3]
[1, 2, 3]
> numbersAboveZero (1..9)
[1, 2, 3, 4, 5, 6, 7, 8, 9]
```

5. Define a function that takes numbers from a list of `Int`s, stopping when it reaches a number
   that is above 10. `takeWhile`[2] can be useful for this.

```purescript
> takeBelow10 (5..15)
[5, 6, 7, 8, 9]
> takeBelow10 (1..9)
[1, 2, 3, 4, 5, 6, 7, 8, 9]
> takeBelow10 []
[]
```

6. Define a function that checks whether or not all `Int`s in an array are even. Use the exercise
   notes below to figure out how to construct your solution and use partial application for your
   definition.

```purescript
> areAllEven [2, 4, 6, 8, 10]
true
> areAllEven (1..9)
false
> areAllEven []
true
```

#### Exercise notes (Partial application)

1. [filter](https://pursuit.purescript.org/packages/purescript-arrays/7.1.0/docs/Data.Array#v:filter)
2. [takeWhile](https://pursuit.purescript.org/packages/purescript-arrays/7.1.0/docs/Data.Array#v:takeWhile)
3. [all](https://pursuit.purescript.org/packages/purescript-arrays/7.1.0/docs/Data.Array#v:all)
4. [even](https://pursuit.purescript.org/packages/purescript-integers/6.0.0/docs/Data.Int#v:even)

## Pipelines using partial application

Partial application is especially useful in pipelines, since we can construct new functions by
partially applying other functions, easily creating a function that takes the result of a previous
operation and returning a new one, possibly passing it along to yet another function:

```purescript
-- `/=` is the "not equal" operator in PureScript, analogous to `!=` in many
-- other languages.
-- Note how we're again using an operator with only one named argument which
-- gives us a function expecting one argument, which is exactly what the
-- predicate we pass to `takeWhile` here expects: `CodePoint -> Boolean`

-- Types for this example:
--
-- Array.reverse :: String -> String
-- Array.takeWhile :: (CodePoint -> Boolean) -> Array CodePoint -> Array CodePoint
-- Array.length :: Array CodePoint -> Int

import Prelude

import Data.Array as Array
import Data.String.CodePoints as CodePoints

dataPartLength' :: String -> Int
dataPartLength' =
  CodePoints.toCodePointArray
    >>> Array.reverse
    >>> Array.takeWhile (_ /= CodePoints.codePointFromChar '1')
    >>> Array.length

dataPartLength'' :: String -> Int
dataPartLength'' string =
  string
    # CodePoints.toCodePointArray
    # Array.reverse
    # Array.takeWhile (_ /= CodePoints.codePointFromChar '1')
    # Array.length
```

The examples here show the same function written in two different ways. In the first one we are
composing functions to create new functions. When `toCodePointArray` is composed with `reverse` we
get a function that expects a `String` and returns an `Array CodePoint`. When that function is then
composed further with `takeWhile` we have a function from `String` to `Array CodePoint`. Ultimately
we've composed that together with `length` as well and end up with a function from `String` to `Int`.

This kind of composition is very useful because it rarely talks about any redundant information in
a series of transformations. We can't always express things like this, but when we can it makes for
a very concise and easy-to-modify way of expressing these kinds of things.

The second example, using the `#` operator is the same as commonly used pipeline operators like `|>`
from F#, Elm & Elixir, and might be more familiar to some. It works by taking whatever value we have
on the left side of it and passing it to the function on the right. Like F# and Elm the value is
passed as the last argument to the function on the right, as opposed to Elixir where it is passed
as the first.

### A "Project Euler" example

Let's look at the very first [Project Euler](https://projecteuler.net/problem=1) as an example of
using function composition and partial application to get the answer to a mildly complex question:

> If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
> The sum of these multiples is 23.
>
> Find the sum of all the multiples of 3 or 5 below 1000.

Let's first look at the given example numbers in action:

```purescript
import Prelude

import Data.Int as Int

isDivisibleBy :: Int -> Int -> Boolean
isDivisibleBy x divisor =
  -- Note how we surround the function in backticks (`) to be able to put it in the infix position.
  let remainderOfDivision = x `Int.rem` divisor
   in remainderOfDivision == 0

solution :: Int
solution =
  [1,2,3,4,5,6,7,8,9]
    # Array.filter (\x -> x `isDivisibleBy` 3 || x `isDivisibleBy` 5)
    # sum -- 23
```

If we now set an upper bound as a parameter we can get the solution to the actual problem:

```purescript
import Prelude

import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int

isDivisibleBy :: Int -> Int -> Boolean
isDivisibleBy x divisor =
  -- Note how we surround the function in backticks (`) to be able to put it in the infix position.
  let remainderOfDivision = x `Int.rem` divisor
   in remainderOfDivision == 0

solution :: Int -> Int
solution upperBound =
  Array.range 1 (upperBound - 1)
    # Array.filter (\x -> x `isDivisibleBy` 3 || x `isDivisibleBy` 5)
    # sum

-- `solution 1000` will give us the value 233168
```

If we wanted to support using different divisors we could also do the following:

```purescript
import Prelude

import Data.Array as Array
import Data.Foldable (sum)
import Data.Int as Int

isDivisibleBy :: Int -> Int -> Boolean
isDivisibleBy x divisor =
  -- Note how we surround the function in backticks (`) to be able to put it in the infix position.
  let
    remainderOfDivision = x `Int.rem` divisor
  in
    remainderOfDivision == 0

solution :: Int -> Array Int -> Int
solution upperBound divisors =
  -- `any` takes a predicate/question and a list of inputs and answers the question:
  -- "Are any of these true?"/"Do any of these return `true`?"
  -- Here we are asking "Is X divisble by any of the passed in divisors?"
  -- We could also write `isDivisibleBy x` only, but it can be useful to use backticks to emphasize
  -- that `x` is the first argument, and to make it read as a question about `x`.
  Array.range 1 (upperBound - 1)
    # Array.filter (\x -> Array.any (x `isDivisibleBy` _) divisors)
    # sum

-- `solution 1000 [3, 5]` will give us the value 233168
```

### Exercises (Pipelines using partial application)

1. Define a function using a pipeline that takes a list of `Int`, gets all the even numbers in it,
   multiplies them all by two and returns the sum[1]. Define versions that use:
   - a named argument; with `#`
   - an unnamed argument; with `>>>`

```purescript
> import Data.Array ((..))
> multipliedEvenSum [2, 4, 6, 8, 10]
60
> multipliedEvenSum (1..9)
40
> multipliedEvenSum []
0
> multipliedEvenSum [1, 3, 6, 9]
12
```

2. Define a function using a pipeline that takes a list of `Int`, takes numbers until it finds one
   that is not even[2], multiplies them all by two, sums them up and returns whether or not the
   sum is even. Define versions that use:
   - a named argument; with `#`
   - an unnamed argument; with `>>>`

```purescript
> isSumOfMultipliedLeadingEvensEven [2, 4, 6, 8, 10]
true
> isSumOfMultipliedLeadingEvensEven [2, 4, 5, 6]
true
> isSumOfMultipliedLeadingEvensEven []
true
```

3. Define a function that takes the last 3 elements of a `Array Int` and gets the average of them.
   Use notes as inspiration.

```purescript
> averageOfLast3 [2, 4, 6, 8, 10]
8.0
> averageOfLast3 [2, 4, 5, 6]
5.0
> averageOfLast3 []
0.0
```

4. Define a function using a pipeline that takes a list of strings, takes all the strings that begin
   with "#", then discards all leading "#" or " " from the resulting strings. Take note of what kind
   of structure you are working with and what we need to do to work with the structures inside. You
   may need to start out with an anonymous function[9] somewhere in order to see how you could go
   from that to a partially applied function.

```purescript
> headingNames ["# Functions", "## Pipelines using partial application", "Random text"]
[ "Functions"
, "Pipelines using partial application"
]
> headingNames []
[]
```

#### Exercise notes (Pipelines using partial application)

0. [sum](https://pursuit.purescript.org/packages/purescript-foldable-traversable/6.0.0/docs/Data.Foldable#v:sum)
1. [takeWhile](https://pursuit.purescript.org/packages/purescript-arrays/7.1.0/docs/Data.Array#v:takeWhile)
2. [dropWhile](https://pursuit.purescript.org/packages/purescript-arrays/7.1.0/docs/Data.Array#v:dropWhile)
3. [startsWith](https://pursuit.purescript.org/packages/purescript-stringutils/0.0.10/docs/Data.String.Utils#v:startsWith)
4. [any](https://pursuit.purescript.org/packages/purescript-arrays/7.1.0/docs/Data.Array#v:any)
5. [elem](https://pursuit.purescript.org/packages/purescript-arrays/7.1.0/docs/Data.Array#v:elem)
6. [take](https://pursuit.purescript.org/packages/purescript-arrays/7.1.0/docs/Data.Array#v:take)
7. [reverse](https://pursuit.purescript.org/packages/purescript-arrays/7.1.0/docs/Data.Array#v:reverse)
8. Lambdas are written like this: `\argumentOne argumentTwo -> bodyOfFunction`

## A note on functions, their parameter order and partial application

Because we have partial application it's very common to design our APIs in such a way where the
"subject" of a function comes last. When we design with partial application in mind we can get
functions where the arguments act almost like configuration and we create specialized versions of
these on-demand just by partially applying them.

If we take our `clamp` function from before as an example:

```purescript
-- Limits a value to be within the range `lowerBound <= value <= upperBound`
clamp :: Int -> Int -> Int -> Int
clamp lowerBound upperBound value
  | value < lowerBound = lowerBound
  | value > upperBound = upperBound
  | otherwise = value

clampAllToByteRange :: Array Int -> Array Int
clampAllToByteRange = List.map (clamp 0 255)

clampAllToHundreds :: Array Int -> Array Int
clampAllToHundreds = List.map (clamp (-100) 100)
```

Because `List.map` takes the list it is working with as the last argument we can partially apply
`List.map` and because `clamp` does the same with the value it is clamping we can partially apply
that as well. This leads to very easy code-reuse for different concerns.

### Don't worry about mis-designing this

If you do happen to mis-design an API with regards to partial application you will definitely feel
it. Seeing the issue is trivial and in most cases you'll just swap the order to more effectively
make use of partial application. In the absolute worst case you'll simply not use the function with
partial application and that's fine too.

It is absolutely worth thinking about this when designing your APIs, but at the end of the day it's
not important enough for you to do somersaults in the code base to get it to where it should be.
