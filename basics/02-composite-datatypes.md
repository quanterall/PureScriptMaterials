# Composite datatypes

- [Composite datatypes](#composite-datatypes)
  - [Bool](#bool)
    - [`if` and `Bool`](#if-and-bool)
    - [`case` for pattern matching on datatypes](#case-for-pattern-matching-on-datatypes)
    - [Top-level pattern matching](#top-level-pattern-matching)
    - [Exercises (Bool)](#exercises-bool)
  - [Interlude: `deriving (Eq, Show)`](#interlude-deriving-eq-show)
  - [Newtypes](#newtypes)
    - [Exercises (Newtypes)](#exercises-newtypes)
  - [Record types](#record-types)
    - [Exercises (Record types)](#exercises-record-types)
      - [Exercise notes (Record types)](#exercise-notes-record-types)
  - [Union types](#union-types)
    - [Exercises (Union types)](#exercises-union-types)
      - [Exercise notes (Union types)](#exercise-notes-union-types)
  - [Combining records and unions](#combining-records-and-unions)
    - [Exercises (Combining records and unions)](#exercises-combining-records-and-unions)
      - [Exercise notes (Combining records and unions)](#exercise-notes-combining-records-and-unions)
  - [Generic datatypes](#generic-datatypes)
    - [Exercises (Generic datatypes)](#exercises-generic-datatypes)
  - [Commonly used composite datatypes](#commonly-used-composite-datatypes)
    - [Maybe](#maybe)
      - [Exercises (Maybe)](#exercises-maybe)
        - [Exercise notes (Maybe)](#exercise-notes-maybe)
    - [Either](#either)
      - [Exercises (Either)](#exercises-either)
        - [Exercise notes (Either)](#exercise-notes-either)
    - [Tuples](#tuples)
    - [List / []](#list--)
      - [Exercises (Lists)](#exercises-lists)
        - [Exercise notes (Lists)](#exercise-notes-lists)
  - [Strictness annotations](#strictness-annotations)
    - [Lists and lazyness](#lists-and-lazyness)
    - [More tools for strictness](#more-tools-for-strictness)
    - [More extensive material on lazyness](#more-extensive-material-on-lazyness)

Not everything is just a primitive, of course, and we've actually already seen an example of a more
complex datatype in the previous chapter about "Values and functions": `Bool`.

## Bool

`Bool` is actually defined exactly as we would define one of our own types:

```haskell
-- After the `data` keyword comes the name of the type. This is how we would refer to it in a type
-- signature.
data Bool = True | False
-- ^ These are the valid ways of constructing a `Bool`, either with `True` or `False`.
-- Constructors are always, like types, written with an initial uppercase letter.
```

It can be either `True` or `False` and working with it is fairly instructive in terms of how one can
work with these kinds of data declarations.

### `if` and `Bool`

```haskell
import Prelude

add42or1337 :: Bool -> Int -> Int
-- Note that `if` is an expression and both branches need to return the same type. We also always
-- need the `else` branch for this reason.
add42or1337 shouldAdd42 x = x + if shouldAdd42 then 42 else 1337
```

### `case` for pattern matching on datatypes

We could also use pattern-matching via the `case` keyword here to inspect the value of the bool.
This isn't something that only works for `Bool`, but rather something we can use for all datatype
definitions. When we pattern-match on them we can use the constructors of the datatypes to
deconstruct the information. Matches are tried from top to bottom, so if the boolean value in the
below example was `True`, we would add `42` to our integer value and if it was `False`, we would
add `1337`. If we didn't handle both cases here we would get a warning from the compiler:

```haskell
import Prelude

add42or1337 :: Bool -> Int -> Int
add42or1337 shouldAdd42 x = x + case shouldAdd42 of
  True -> 42
  False -> 1337
```

Each `case` branch can deconstruct the different constructors of a union type (which `Bool` is),
even if they have associated data in them. We'll see this later.

### Top-level pattern matching

We could also pattern-match "in the top-level", meaning on the left of the `=`. The same rules as
for `case` apply here; we need to cover both cases or the compiler will tell us the pattern-match is
not exhaustive. The cases are tried in order and here we are saying that if the first value (`Bool`)
is `True` we (again) add 42, etc.:

```haskell
import Prelude

add42or1337 :: Bool -> Int -> Int
add42or1337 True x = x + 42
add42or1337 False x = x + 1337
```

If the logic for a function differs a lot between the cases I would personally prefer the last
version as it also allows you to have some of the arguments be bound only for certain cases, etc.,
and generally keeps each case separate. In this case the first version makes the most sense because
only the amount added depends on the boolean and we have special syntax for boolean values with
`if`. `Bool` values also work naturally with `if`.

### Exercises (Bool)

1. Create a function `and` that takes two `Bool`s and returns `True` if both are true, `False`
   otherwise. Define versions using:
   - `case` (`case (firstBool, secondBool)` will allow you to match on both at the same time)
   - Top-level pattern-matching

```haskell
Q> and True True
True
Q> and False True
False
Q> and True False
False
```

2. Create a function `or` that takes two `Bool`s and returns `True` if either is true, `False`
   otherwise. Define versions using:
   - `case` (`case (firstBool, secondBool)` will allow you to match on both at the same time)
   - Top-level pattern-matching

```haskell
Q> or False True
True
Q> or True False
True
Q> or True True
True
Q> or False False
False
```

3. Create a function `exclusiveOr` that takes two `Bool`s and returns true if exactly one of them
   is true. Define versions using:
   - `case` (`case (firstBool, secondBool)` will allow you to match on both at the same time)
   - Top-level pattern-matching

```haskell
Q> exclusiveOr False True
True
Q> exclusiveOr True False
True
Q> exclusiveOr True True
False
Q> exclusiveOr False False
False
```

4. Create a function `bool` that takes two parameters of the type `Int` as well as a `Bool` and
   returns the first one if the `Bool` is `True` and the other if it's `False`.
   Define versions using:
   - `case`
   - `if`
   - Top-level pattern-matching

```haskell
Q> bool 42 1337 True
42
Q> bool 42 1337 False
1337
```

## Interlude: `deriving (Eq, Show)`

In these examples you'll often find that there is a line under a lot of data definitions reading
`deriving (Eq, Show)`. We'll look at what `deriving` and the other components to this mean in the
4th chapter, but what you need to know right now is that this line will automatically generate the
capability for these types to be displayed the terminal, as well as be compared to eachother value
for value.

## Newtypes

Before we move into advanced composite types, let's look at a very basic tool in the Haskell
toolbelt: `newtype`.

Newtypes are types that wrap other types in order to make distinct versions of them. Let's look at
what that means in practice.

Let's imagine a function `filteredCopy` that takes a source filename, destination filename as well
as a string to filter by such that we only copy lines that contain that string:

```haskell
filteredCopy :: String -> String -> String -> IO ()
filteredCopy source destination copyPattern = ...
```

(**Note**: The `IO ()` for the purposes of this example means that we are doing something effectful
and there is no useful return value. We will go over this in the next chapter.)

Since the arguments to this function are all strings, what happens if we by mistake use this
function with `filteredCopy destination source copyPattern` or any other incorrect order for the
parameters? The type system doesn't know anything about what these strings represent.

The solution to this issue is fairly simple:

```haskell
--       type    constructor
newtype Source = Source String
-- ^ This constructor, `Source`, takes one argument, a `String`.
-- When that argument is supplied to it, we get a value of type `Source`.
  deriving (Eq, Show)

--       type         constructor
newtype Destination = Destination String
  deriving (Eq, Show)

--       type         constructor
newtype CopyPattern = CopyPattern String
  deriving (Eq, Show)

filteredCopy :: Source -> Destination -> CopyPattern -> IO ()
filteredCopy (Source source) (Destination destination) (CopyPattern copyPattern) = ...
-- ^ Note how we can deconstruct these wrappers just like with other forms of data definitions. This
-- is a very useful thing to do when we effectively want to be working with the strings that these
-- types contain. It means that while we cannot blindly pass strings **to** this function, we still
-- have the ease of working with the wrapped types inside of it.
```

When we use `filteredCopy` now we will have to wrap our strings:

```haskell
filteredCopy (Source source) (Destination destination) (CopyPattern copyPattern)
```

We can still make the mistake of wrapping our `source` in a `Destination` wrapper, to be clear, but
it's much easier to spot this mistake and if a value is produced in one place in a program as a
`Destination` it simply cannot be passed blindly to a place where a `Source` is required.

### Exercises (Newtypes)

1. Define a newtype that wraps `Float`, called `Meters`. Define a function `addMeters`that takes two
   `Meters` and adds them together to return a new `Meters`.

```haskell
Q> addMeters (Meters 2.5) (Meters 3.0)
Meters 5.5
```

2. Define two newtypes wrapping `Float`, called `Meters` and `Kilometers`. Define a function that
   takes `Meters` and correctly converts them into `Kilometers`.

```haskell
Q> metersToKilometers (Meters 1050.0)
Kilometers 1.05
```

3. Define a newtype wrapping `String` that is called `Username`, then a function that takes a
   `Username` and returns its length.

```haskell
Q> usernameLength (Username "pesho")
5
```

## Record types

Records are useful when we want to store multiple values together in a named structure. The
individual parts, or "fields", are named as well. We begin a record definition with the keyword
`data` after which we give the name of the type. Following an `=` we then give the constructor for
the type; a function that takes the record data and constructs the type.

```haskell
--      type       constructor
data UserProfile = UserProfile
  { username :: String,
    age :: Int,
    active :: Bool,
    interests :: [String]
  }
  deriving (Eq, Show)
```

The constructor name can be different than the type name, but this is comparatively rare.

One thing to note about record definitions in Haskell is that each field will have an associated
function that takes the type and returns the field. Let's use the `:type` command in our interactive
session to find out the type of each of these generated functions:

```haskell
Q> :type username
username :: UserProfile -> String
Q> :type age
age :: UserProfile -> Int
Q> :type active
active :: UserProfile -> Bool
Q> :type interests
interests :: UserProfile -> [String]
Q> rickard = UserProfile {
     username = "rickard",
     age = 34,
     active = True,
     interests = ["Programming", "Problem Solving", "Teaching"]
   }
Q> rickard
UserProfile
    { username = "rickard"
    , age = 34
    , active = True
    , interests =
        [ "Programming"
        , "Problem Solving"
        , "Teaching"
        ]
    }
Q> username rickard
"rickard"
Q> age rickard
34
Q> active rickard
True
Q> interests rickard
[ "Programming"
, "Problem Solving"
, "Teaching"
]
```

We can see this in action in this snippet where we turn a profile into a string:

```haskell
profileToString :: UserProfile -> String
profileToString profile =
  let ageString = show $ age profile
      activeString = if active profile then "active" else "not active"
      interestsString = intercalate ", " (interests profile)
   -- `mconcat` here concatenates a list of strings into a string
   in mconcat
        [ username profile,
          " (",
          ageString,
          "y, ",
          activeString,
          ") is interested in: ",
          interestsString
        ]

-- | Inserts a given string between every entry in the list of strings
intercalate :: String -> [String] -> String
intercalate between strings =
  mconcat $ List.intersperse between strings
```

Running this on our previously defined profile we get:

```haskell
Q> profileToString rickard
"rickard (34y, active) is interested in: Programming, Problem Solving, Teaching"
```

We could also pattern match on our `UserProfile` type:

```haskell
profileToString' :: UserProfile -> String
profileToString'
  UserProfile
    { username = username,
      age = age,
      active = active,
      interests = interests
    } =
  let ageString = show age
      activeString = if active then "active" else "not active"
      interestsString = intercalate ", " interests
   in mconcat
        [ username,
          " (",
          ageString,
          "y, ",
          activeString,
          ") is interested in: ",
          interestsString
        ]

-- | Inserts a given string between every entry in the list of strings
intercalate :: String -> [String] -> String
intercalate between strings =
  mconcat $ List.intersperse between strings
```

We can see that we've now bound the values we care about in our function definition "head" and so
the logic inside of the function is somewhat less busy, with values already being pulled out of our
profile value for us. There is one issue, however; our function head is so wide that we've now been
forced to spread it out over several lines, and we are repeating the field and variable names
unnecessarily. When we are using the same name for a field we are matching as the name we are
binding it to, we can use this nice shorthand:

```haskell
profileToString :: UserProfile -> String
profileToString UserProfile {username, age, active, interests} =
  let ageString = show age
      activeString = if active then "active" else "not active"
      interestsString = intercalate ", " interests
   in mconcat
        [ username,
          " (",
          ageString,
          "y, ",
          activeString,
          ") is interested in: ",
          interestsString
        ]

-- | Inserts a given string between every entry in the list of strings
intercalate :: String -> [String] -> String
intercalate between strings =
  mconcat $ List.intersperse between strings
```

If we omit the `=` in our bindings Haskell will assume we are binding the fields into a name equal
to the field's name. This mirrors the behavior you can find in, for example, JavaScript and other
languages and also applies when we construct records:

```haskell
let userProfile =
      -- Note how we don't have to pass all of these without `=`
      UserProfile {username = "rickard", age, active, interests}
    age = 34
    active = True
    interests = ["Programming" , "Problem Solving" , "Teaching"]
```

The `userProfile` value above is a valid way to construct a `UserProfile`. When we don't use `=` for
a field and we have a field with the same name as our value, Haskell again assumes that we mean to
set the corresponding field to that value. Passing a field that doesn't exist in the type is still
an error, so this is completely safe.

(**Note**: This behavior with the shorthand deconstruction and construction is available through a
language extension called `NamedFieldPuns`, that we by default enable in our Quanterall templates,
so you should see this work without issue when using them in projects you created via `stack new
project-name quanterall/{basic,application,web-postgres}`.)

### Exercises (Record types)

1. Define a function that takes a `String` and returns a datatype that stores both the length of the
   string and the string itself.

```haskell
Q> stringAndLength "hello"
StringAndLength
    { lengthOfString = 5
    , string = "hello"
    }
```

2. Define a data type that represents a product that has a name, a price and a taxation rate
   (`Double` between 0 and 1). Define a function taking this type that calculates the total price of
   a product.

```haskell
Q> totalPrice Product {name = "Bio Cucumber", price = 5, taxationRate = 0.2}
6.0
Q> totalPrice Product {name = "Bio Cucumber", price = 5, taxationRate = 0.1}
5.5
Q> totalPrice Product {name = "Normal Cucumber", price = 1, taxationRate = 0.1}
1.1
```

3. Define a HTTP request datatype that has a `url`, a list of query parameters (a query parameter
   has a key and a value), a HTTP method[0] and a body.

4. Add `newtype`s to the definition you made for exercise 3 where you think they are appropriate.

#### Exercise notes (Record types)

0. This is fine as a string for now.

## Union types

While a record represents a collection of values that make up a whole, all of them present, a
**union type** represents a set of alternatives that are all valid, but only one at a time. The
built-in `Bool` type is a union type; we can only have either `True` **or** `False`.

We define a union type with the `data` keyword followed by the type name and `=`. Then we list the
**constructors** of the type with `|` between them:

```haskell
-- This requires you to add `time` as a dependency in `package.yaml`
import Data.Time (Day)
import Prelude

data RelationshipStatus
  = MarriedTo MarriageInfo -- This could also be `MarriedTo String Day`
  | EngagedTo UserProfile
  | ItsComplicated
  | Single
  deriving (Eq, Show)

data MarriageInfo = MarriageInfo {spouse :: String, date :: Day}
  deriving (Eq, Show)
```

The different constructors all represent different cases and contain different data. In the case of
`MarriedTo` the constructor holds a record. If we wanted to we could take several arguments to the
constructor, but have elected to name the components because it can sometimes be clearer to take a
record. In the case of `EngagedTo` it's perfectly clear that the user is engaged to another user
profile. For the subsequent cases the constructors don't carry any additional data.

We can inspect and act on this data in several ways:

```haskell
-- Note how we can put an underscore alone or before some text here to say that we do not care what
-- the contents actually are, but we are saying that yes, there is a value there.
isSingle :: RelationshipStatus -> Bool
isSingle (MarriedTo _) = False
isSingle (EngagedTo _userProfile) = False
isSingle ItsComplicated = True
isSingle Single = True

isSingle' :: RelationshipStatus -> Bool
isSingle' status = case status of
  MarriedTo _marriageInfo -> False
  EngagedTo _ -> False
  ItsComplicated -> True
  Single -> True
```

The above functions are of course very course grained; it's a very binary thing. To accurately
represent what is actually the case we sometimes need to introduce more choices. Let's define a type
that is perhaps more accurate:

```haskell
data IsSingle
  = DefinitelySingle
  | MaybeSingle
  | DefinitelyNotSingle
  deriving (Eq, Show)

isSingle :: RelationshipStatus -> IsSingle
isSingle (MarriedTo _) = DefinitelyNotSingle
isSingle (EngagedTo _userProfile) = DefinitelyNotSingle
isSingle ItsComplicated = MaybeSingle
isSingle Single = DefinitelySingle

isSingle' :: RelationshipStatus -> IsSingle
isSingle' status = case status of
  MarriedTo _marriageInfo -> DefinitelyNotSingle
  EngagedTo _ -> DefinitelyNotSingle
  ItsComplicated -> MaybeSingle
  Single -> DefinitelySingle
```

When we introduce new types like this we enable our programs to more accurately model and act on the
information that flows through our systems. In the case above we've now enabled a choice for a
"maybe single" profile that previously had to be discerned from the `ItsComplicated` constructor. We
now allow the user of `isSingle` to be aware only of these three states that `IsSingle` encompasses,
and not have to derive what to do based on the bigger, more information-dense `RelationshipStatus`
type.

### Exercises (Union types)

1. Define a function that takes a default `Float` value as well as a `DivisionResult` and if
   the division result is a division by zero, returns the default. Otherwise it returns the result.
   Create a solution with top-level pattern matching as well as one with `case`.

   Remember that a `DivisionResult` looks as follows:

```haskell
data DivisionResult
  = DivideSuccess Float
  | DivisionByZero
  deriving (Show)
```

```haskell
Q> divisionOrDefault 42 DivisionByZero 
42.0
Q> divisionOrDefault 42 (DivideSuccess 1337) 
1337.0
```

2. Define a function `spouseName` that takes a `RelationshipStatus` and returns a `String`. Choose
   either top-level pattern matching or using `case`. What do we have to do when a case does not
   have a spouse?

   Remember that `RelationshipStatus` looks as follows:

```haskell
-- This requires you to add `time` as a dependency in `package.yaml`
import Data.Time (Day)
import Prelude

data RelationshipStatus
  = MarriedTo MarriageInfo
  | EngagedTo UserProfile
  | ItsComplicated
  | Single
  deriving (Eq, Show)

data MarriageInfo = MarriageInfo {spouse :: String, date :: Day}
  deriving (Eq, Show)
```

3. Define a data type that more accurately reflects the having or not of a spouse and modify the
   function you defined in exercise 2 to return this data type. What happened to the cases where we
   do not have a spouse name to take from the relationship status?

4. Return to the HTTP request type we defined in the "Record types" exercise and more accurately
   model the "method" field[0]. For the purposes of this exercise, let's say that the following
   methods exist and they expect the following payloads:
   - GET: Nothing
   - HEAD: Nothing
   - POST: String or Nothing
   - PUT: String or Nothing
   - DELETE: Nothing
   - CONNECT: Nothing
   - OPTIONS: Nothing
   - TRACE: Nothing
   - PATCH: String or Nothing

5. Define a `TradeOrder` type that can be either a `SellOrder` or a `BuyOrder`, both taking a
   `TickerSymbol` (a `newtype` around a `String`) and an `Int`. Define a function
   `correspondingOrderType` that takes a `TradeOrder` and returns a `SellOrder` if a `BuyOrder`
   has been passed to it and vice versa.

```haskell
Q> correspondingOrderType (BuyOrder (TickerSymbol "MSFT") 1000)
SellOrder ( TickerSymbol "MSFT" ) 1000
Q> correspondingOrderType (SellOrder (TickerSymbol "MSFT") 1000)
BuyOrder ( TickerSymbol "MSFT" ) 1000
```

6. Define a function `matchOrder` that takes a `TradeOrder` and a `[TradeOrder]` and returns
   whether or not we matched a sell/trade to an existing opposite trade/sell in the list of orders.
   If there is a match, return the matching entry as well as the list of trade orders **without**
   the matched order[1]. If there is no match, indicate this in the return value.

```haskell
Q> orders = [BuyOrder (TickerSymbol "AAPL") 1050, SellOrder (TickerSymbol "MSFT") 1000]
Q> matchOrder (BuyOrder (TickerSymbol "MSFT") 1000) orders
MatchedOrder
    ( SellOrder ( TickerSymbol "MSFT" ) 1000 )
    [ BuyOrder ( TickerSymbol "AAPL" ) 1050 ]
Q> matchOrder (SellOrder (TickerSymbol "MSFT") 1000) orders
NoMatchedOrder
Q> matchOrder (SellOrder (TickerSymbol "AAPL") 1050) orders
MatchedOrder
    ( BuyOrder ( TickerSymbol "AAPL" ) 1050 )
    [ SellOrder ( TickerSymbol "MSFT" ) 1000 ]
```

#### Exercise notes (Union types)

0. Remember that constructors can take payloads or not, so a method that has a body associated with
   it could take one and a method that doesn't could be designed to not take one.
1. [`delete`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Data-List.html#v:delete)

## Combining records and unions

As we saw in the previous section it's trivial to combine records and unions; our `MarriageInfo`
type is already embedded in the `MarriedTo` constructor. So let's take that one step further and
enrich our `UserProfile` data type by adding our `RelationshipStatus` to `UserProfile`:

```haskell
import qualified Data.List as List
import Data.Time (Day)
import qualified Data.Time as Time
import Prelude

data UserProfile = UserProfile
  { username :: String,
    age :: Int,
    active :: Bool,
    interests :: [String],
    relationshipStatus :: RelationshipStatus
  }
  deriving (Eq, Show)

data RelationshipStatus
  = MarriedTo MarriageInfo
  | EngagedTo UserProfile
  | ItsComplicated
  | Single
  deriving (Eq, Show)

data MarriageInfo = MarriageInfo {spouse :: String, date :: Day}
  deriving (Eq, Show)

profileToString :: UserProfile -> String
profileToString UserProfile {age, active, interests, relationshipStatus, username} =
  let ageString = show age
      activeString = if active then "active" else "not active"
      interestsString = intercalate ", " interests
      relationshipStatusString = case relationshipStatus of
        MarriedTo MarriageInfo {spouse, date} ->
          let dateString = Time.showGregorian date
           in -- `unwords` takes a `[String]` and joins them into a string with spaces inbetween
              unwords ["Married to:", spouse, "on", dateString]
        EngagedTo UserProfile {username = spouseUsername} ->
          unwords ["Engaged to:", spouseUsername]
        ItsComplicated -> "It's complicated"
        Single -> "Single"
   in mconcat
        [ username,
          " (",
          ageString,
          "y, ",
          activeString,
          ", ",
          relationshipStatusString,
          ") is interested in: ",
          interestsString
        ]

-- | Inserts a given string between every entry in the list of strings
intercalate :: String -> [String] -> String
intercalate between strings =
  mconcat $ List.intersperse between strings
```

If we now construct our `rickard` profile with this in mind we get the following:

```haskell
Q> rickard = UserProfile {
     username = "rickard",
     age = 34,
     active = True,
     interests = ["Programming", "Problem Solving", "Teaching"],
     relationshipStatus = MarriedTo MarriageInfo {
       spouse = "Ivana",
       date = Time.fromGregorian 2016 06 04
     }
   }
Q> profileToString rickard
"rickard (34y, active, Married to: Ivana on 2016-06-04) is interested in: Programming, Problem
Solving, Teaching"
```

Combining record types and union types is the basis for domain modelling in Haskell and allows us to
construct very rich datatypes that we can work safely with. Armed with more knowledge in the future
it will be easy for you to look at the above example and modify it according to hypothetical
business logic and having those changes be reflected in our functions.

As an example, if we decided that our marriage information should hold a `UserProfile`, that would
require users on our site to only be able to set their "married" status if their spouse is on the
site. However, if we instead make the `spouse` field take a type that allows us to have a name
**or** a userprofile, we can express this possibility clearly:

```haskell
-- Our `MarriageInfo` record now takes a `Spouse` type, which itself is a more
-- expressive type allowing for either a string or a user profile
data MarriageInfo = MarriageInfo {spouse :: Spouse, date :: Day}
  deriving (Eq, Show)

data Spouse
  = SpouseProfile UserProfile
  | SpouseName String
  deriving (Eq, Show)
```

This will give us the same capability as before, because we still support spouse names with strings:

```haskell
Q> rickard = UserProfile {
     username = "rickard",
     age = 34,
     active = True,
     interests = ["Programming", "Problem Solving", "Teaching"],
     relationshipStatus = MarriedTo MarriageInfo {
       spouse = SpouseName "Ivana",
       date = Time.fromGregorian 2016 06 04
     }
   }
Q> profileToString rickard
"rickard (34y, active, Married to: Ivana on 2016-06-04) is interested in: Programming, Problem
Solving, Teaching"
```

But we can now also use a user profile in our `spouse` field:

```haskell
Q> ivana = UserProfile {
     username = "ivana",
     age = 31,
     active = True,
     interests = ["Web Design", "Cats", "Beer"],
     relationshipStatus = MarriedTo MarriageInfo {
       spouse = SpouseProfile rickard,
       date = Time.fromGregorian 2016 06 04
     }
   }
Q> profileToString ivana
"ivana (31y, active, Married to: rickard on 2016-06-04) is interested in: Web Design, Cats, Beer"
```

Since we are now using the `SpouseProfile` constructor for `spouse` we can pass our previously
defined value `rickard` of type `UserProfile` and it still works. If we were building this site we
could use the fact that we are guaranteed to have another `UserProfile` in the `SpouseProfile` case
to insert a link to the other profile in this case and only output a string with the name in the
other.

For another example of modelling (part of) a domain with types, see
[this file](./02b-person-printing.md).

### Exercises (Combining records and unions)

1. Modify the `profileToString` function[0] to take into account that our `spouse` name in
   `MarriageInfo` is now a `Spouse`; this means we have to look closer at the data with the
   `Spouse` type in mind to get a string value out of it.

2. Modify the `EngagedTo` constructor in `RelationshipStatus` to also take a `Spouse` and then
   modify the latest version of `profileToString` accordingly.

#### Exercise notes (Combining records and unions)

0. The code for `profileToString` and its associated types:

```haskell
import qualified Data.List as List
import Data.Time (Day)
import qualified Data.Time as Time
import Prelude

data UserProfile = UserProfile
  { username :: String,
    age :: Int,
    active :: Bool,
    interests :: [String],
    relationshipStatus :: RelationshipStatus
  }
  deriving (Eq, Show)

data RelationshipStatus
  = MarriedTo MarriageInfo
  | EngagedTo UserProfile
  | ItsComplicated
  | Single
  deriving (Eq, Show)

data MarriageInfo = MarriageInfo {spouse :: Spouse, date :: Day}
  deriving (Eq, Show)

data Spouse
  = SpouseProfile UserProfile
  | SpouseName String
  deriving (Eq, Show)

profileToString :: UserProfile -> String
profileToString UserProfile {age, active, interests, relationshipStatus, username} =
  let ageString = show age
      activeString = if active then "active" else "not active"
      interestsString = intercalate ", " interests
      relationshipStatusString = case relationshipStatus of
        MarriedTo MarriageInfo {spouse, date} ->
          let dateString = Time.showGregorian date
           in -- `unwords` takes a `[String]` and joins them into a string with spaces inbetween
              unwords ["Married to:", spouse, "on", dateString]
        EngagedTo UserProfile {username = spouseUsername} ->
          unwords ["Engaged to:", spouseUsername]
        ItsComplicated -> "It's complicated"
        Single -> "Single"
   in mconcat
        [ username,
          " (",
          ageString,
          "y, ",
          activeString,
          ", ",
          relationshipStatusString,
          ") is interested in: ",
          interestsString
        ]

-- | Inserts a given string between every entry in the list of strings
intercalate :: String -> [String] -> String
intercalate between strings =
  mconcat $ List.intersperse between strings
```

## Generic datatypes

With datatypes we usually want structures that can hold other types and where it doesn't make sense
to specialize these beforehand. If we have a map from `Int` to `String` and another from `Char` to
`Int` we don't want to define a separate data type for each of these. Instead we want to have a
general `Map` structure where we can say that one usage of it has keys of type `Int` and values of
type `String`, as well as another usage has `Char` and `Int` instead. For this purpose we have
"generic" datatypes.

The most basic generic datatype is a type that can hold anything and that has only one constructor:

```haskell
data Holder a = Holder a
  deriving (Eq, Show)
```

Note how we now have a type variable on the left side of `=` which means that when we refer to the
type it will take a type name. If we were holding a `Int`, for example, the type is `Holder Int`.
The corresponding constructor call (or pattern match) is `Holder value`, where `value` is of type
`Int`.

This basic type doesn't have much going for it in terms of functionality, but it's useful to show
how we express type variables in data types. Fortunately for us we only need to extend the record
and union definitions with the same parts as we are using in this basic one in order to get generic
versions of them.

If we were to define a generic record, for example, we could just do the following:

```haskell
data HttpResponse a = HttpResponse
  { status :: HttpStatus,
    headers :: [HttpHeader],
    body :: a
  }
  deriving (Eq, Show)

data HttpStatus
  = Ok Int
  | ClientError Int
  | ServerError Int
  deriving (Eq, Show)

data HttpHeader = HttpHeader
  { headerName :: String,
    headerValue :: String 
  }
  deriving (Eq, Show)
```

We can see here that `HttpResponse` is generic over different types of body types. This means we can
construct it with different types and still get the expected structure for the rest of the response.
Since only the body will differ here we are free to say that it could be a JSON value, bytestring or
maybe UTF8 text. Much like our `Holder` example this would mean that when we refer to the type we
would have, for example, `HttpResponse String`, `HttpResponse JSONValue`, `HttpResponse ByteString`
or `HttpResponse UTF8Text`.

If we had these different applications of  `HttpResponse`, they would look as follows:

```haskell
-- `HttpResponse String`
HttpResponse
  { status :: HttpStatus,
    headers :: [HttpHeader],
    body :: String
  }

-- `HttpResponse JSONValue`
HttpResponse
  { status :: HttpStatus,
    headers :: [HttpHeader],
    body :: JSONValue
  }

-- `HttpResponse ByteString`
HttpResponse
  { status :: HttpStatus,
    headers :: [HttpHeader],
    body :: ByteString
  }

-- `HttpResponse UTF8Text`
HttpResponse
  { status :: HttpStatus,
    headers :: [HttpHeader],
    body :: UTF8Text
  }
```

With unions we predictably have the same format for generic unions as we do for basic ones:

```haskell
data SomeAmountOf a
  = None
  | One a
  | CoupleOf a a
  | BunchOf a a [a]
  deriving (Eq, Show)

none :: SomeAmountOf Int
none = None

one :: SomeAmountOf Int
one = One 42

coupleOf :: SomeAmountOf Int
coupleOf = CoupleOf 42 1337

bunchOf :: SomeAmountOf Int
bunchOf = BunchOf 42 1337 [1, 2, 3]
```

### Exercises (Generic datatypes)

1. Define a value of type `Holder Int` as well as a value of type `Holder String`. Remember that the
   definition of `Holder` looks as follows:

```haskell
data Holder a = Holder a
  deriving (Eq, Show)
```

2. Define a function `pureHolder :: a -> Holder a`. Knowing what you know about partial
   application, what is the most concise and direct definition you can come up with?

```haskell
Q> pureHolder 42
Holder 42
Q> pureHolder "hello"
Holder "hello"
```

3. Define a function `foldHolder :: (a -> b) -> Holder a -> b`. What is the most natural way to
   implement this function?

```haskell
Q> foldHolder (+ 1) (Holder 42)
43
Q> foldHolder length (Holder "hello")
5
```

4. Define a function `mapHolder :: (a -> b) -> Holder a -> Holder b` that applies the passed in
   function to the value inside the `Holder` and wraps it up again. After implementing it, try
   creating different concrete types like `Holder Int` and passing matching arguments to the
   function you wrote. As an example, try passing `length` and a `Holder [Int]` to the function and
   see what comes out.

```haskell
Q> mapHolder (+ 1) (Holder 42)
Holder 43
Q> mapHolder length (Holder "hello")
Holder 5
```

5. Define a function `applyHolder :: Holder (a -> b) -> Holder a -> Holder b`. Note how a `Holder`
   is completely flexible in what it will hold.

```haskell
Q> applyHolder (Holder (+ 1)) (Holder 42)
Holder 43
Q> applyHolder (Holder length) (Holder "hello")
Holder 5
```

6. Define a function `bindHolder :: (a -> Holder b) -> Holder a -> Holder b`.

```haskell
Q> bindHolder (\v -> Holder $ v + 1) (Holder 42)
Holder 43
Q> bindHolder (\v -> Holder $ length v) (Holder "hello")
Holder 5
```

7. Add a constructor to the `Holder` type that has no arguments and is named `NoValue`.  Modify the
   type signature of `foldHolder` to be as follows:

     `foldHolder :: (a -> b) -> b -> Holder a -> b`

   Why did we need to add a second argument of type `b` here?

   Don't modify the other functions' type signatures, but rather consider what we can return in
   this new case for each of them.

```haskell
Q> foldHolder (+ 1) 1337 (Holder 42)
43
Q> foldHolder (+ 1) 1337 NoValue
1337
Q> mapHolder (+ 1) NoValue
NoValue
Q> mapHolder length NoValue
NoValue
Q> applyHolder NoValue (Holder 42)
NoValue
Q> applyHolder (Holder (+ 1)) NoValue
NoValue
Q> bindHolder (\v -> Holder $ v + 1) NoValue
NoValue
Q> bindHolder (\v -> NoValue) (Holder 42)
NoValue
```

## Commonly used composite datatypes

It's usually very instructive to look at some of the supplied composite datatypes that one can find
in most Haskell code, so here is an introduction to their definitions and some of their use cases.

### Maybe

`Maybe a` is a type that represents the existence or non-existence of a value of type `a`. It can be
seen as a `null` type that retains its type even in the presence of nesting. It's defined as
follows:

```haskell
data Maybe a
  = Nothing
  | Just a
```

If the above looks vaguely familiar, it's because it is what our `Holder a` type turned into when we
added the `NoValue` constructor. `Just a` here is the `Holder a` case and `Nothing` is `NoValue`.

`Maybe` is useful generally speaking wherever you would have the type `null | SomeType`, which means
that we generally would like to use it for success/failure when we don't have any interesting error
information. It's also useful for when certain options aren't supplied to a call, or information is
not available and that's fine.

You may want to replace it with what is effectively the same type but with more descriptive names,
but this should be done on a case-by-case basis.

We could for example imagine that a resource is not loaded yet in some data, and this could be
represented with a `Maybe Resource`, but we could also create the following datatype:

```haskell
data ResourceLoadStatus
  = NotYetLoaded
  | Loaded Resource
```

This is all a matter of what makes things clear. Do you want to be able to use the functions that
already exist for `Maybe`, perhaps? In that case it's reasonable to keep it as a `Maybe`. If a
custom type makes things much clearer when looking at your data structures, then go for it. It's not
a complicated endevour to define the functions you need for this type and in the case of a resource
load status it's likely the case that you're pattern matching to see if you need to load the
resource, or just want to display "N/A" when it's not loaded.

If we wanted to convert a `ResourceLoadStatus` to a `Maybe Resource`, we could do the following:

```haskell
data ResourceLoadStatus
  = NotYetLoaded
  | Loaded Resource
  deriving (Eq, Show)

newtype Resource = Resource String
  deriving (Eq, Show)

resourceLoadStatusToMaybe :: ResourceLoadStatus -> Maybe Resource
resourceLoadStatusToMaybe NotYetLoaded = Nothing
resourceLoadStatusToMaybe (Loaded resource) = Just resource
```

#### Exercises (Maybe)

1. Define a type called `User` that has a username, e-mail address, **maybe** has a full name, and
   **maybe** has a telephone number. Make newtypes for each of these.

2. Define a function that gets a telephone number string from a `User`. If the `User` has no
   telephone number, return `"N/A"`. Use pattern matching in the top-level to accomplish this.

<!-- markdownlint-disable MD013 -->
```haskell
Q> user = User {username = Username "gonz", email = Email "rickard.andersson@quanterall.com", fullName = Just (FullName "Rickard Andersson"), phone = Just (PhoneNumber "555 363 22 34")}
Q> showUserPhoneNumber user
"555 363 22 34"
Q> userWithoutPhone = user {phone = Nothing}
Q> showUserPhoneNumber userWithoutPhone 
"N/A"
```
<!-- markdownlint-enable MD013 -->

3. Define a function `pureMaybe :: a -> Maybe a`.

```haskell
Q> pureMaybe 5
Just 5
Q> pureMaybe "hello"
Just "hello"
```

4. Define a function `foldMaybe :: b -> (a -> b) -> Maybe a -> b`. If the `Maybe` has a value, apply
   the function to it. If it doesn't, return the `b` that you take in as an argument.

```haskell
Q> foldMaybe 1337 (+ 1) (Just 42)
43
Q> foldMaybe 1337 (+ 1) Nothing
1337
```

5. Reimplement the previous function for getting a telephone number string from a `User`, but use
   the `foldMaybe` function. If the `User` has no telephone number, return `"N/A"`.

<!-- markdownlint-disable MD013 -->
```haskell
Q> user = User {username = Username "gonz", email = Email "rickard.andersson@quanterall.com", fullName = Just (Fullname "Rickard Andersson"), phone = Just (PhoneNumber "555 363 22 34")}
Q> showUserPhoneNumber user
"555 363 22 34"
Q> userWithoutPhone = user {phone = Nothing}
Q> showUserPhoneNumber userWithoutPhone 
"N/A"
```
<!-- markdownlint-enable MD013 -->

6. Define a function `mapMaybe :: (a -> b) -> Maybe a -> Maybe b`. Consider what the only things you
   can reasonably do in the cases of `Just x` and `Nothing` are.

```haskell
Q> mapMaybe (+ 1) (Just 42) 
Just 43
Q> mapMaybe length (Just "hello")
Just 5
Q> mapMaybe (+ 1) Nothing 
Nothing
Q> mapMaybe length Nothing
Nothing
```

7. Define a function `applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b`.

```haskell
Q> applyMaybe (Just (+ 1)) (Just 42) 
Just 43
Q> applyMaybe (Just length) (Just "hello")
Just 5
Q> applyMaybe (+ 1) Nothing 
Nothing
Q> applyMaybe length Nothing
Nothing
Q> applyMaybe Nothing (Just 42) 
Nothing
Q> applyMaybe Nothing (Just "hello")
Nothing
```

8. Define a function `bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b`.

```haskell
Q> bindMaybe (\v -> Just $ v + 1) (Just 42)
Just 43
Q> bindMaybe (\v -> Just $ length v) (Just "hello")
Just 5
Q> bindMaybe (\v -> Nothing) (Just 42)
Nothing
Q> bindMaybe (\v -> Nothing) (Just "hello")
Nothing
Q> bindMaybe (\v -> Just $ v + 1) Nothing
Nothing
Q> bindMaybe (\v -> Just $ length v) Nothing
Nothing
```

##### Exercise notes (Maybe)

### Either

`Either l r` is a type that represents either the error case `Left` with information attached to it
or the success case `Right` with success data attached to it. The reason `Left` is always the error
case is because of how `Either` works in a monadic context. When returning `Left` it is considered
an error. It's also because `Left` is not right, meaning it's "wrong".

```haskell
data Either l r
  = Left l
  | Right r
```

`Either` is useful when we want something that can either succeed or fail but we also want to bundle
some information into our error case. It's common to return a custom error type for the `Left` case
that can be inspected to see what kind of error that was hit and any extra information that is
attached.

This, again, can be specialized down to something custom but still retain the same meaning:

```haskell
data ResourceLoadResult
  = LoadFailure ResourceLoadError
  | LoadSuccess Resource
  deriving (Eq, Show)

newtype Resource = Resource String
  deriving (Eq, Show)

data ResourceLoadError
  = ResourceBusy
  | ResourceAccessDenied
  | ResourceHasBadData
  | UnknownResourceError String
  deriving (Eq, Show)
```

The above definition holds the same information as a `Either ResourceLoadError Resource`, but can be
more descriptive in certain contexts. Again, it's important to conisder whether or not we want the
functions that work with `Either` available to us or not. If we don't need them, a specialized type
of our own can be very descriptive.

#### Exercises (Either)

1. Define a function `foldEither :: (l -> a) -> (r -> a) -> Either l r -> a`.

```haskell
Q> foldEither length (+ 1) $ Right 42
43
Q> foldEither length (+ 1) $ Left "error"
5
```

2. Define a function `pureEither :: a -> Either l a`.

```haskell
Q> pureEither 42
Right 42
```

3. Define a function `mapEither :: (r -> a) -> Either l r -> Either l a`. Consider what we will have
   to do if we have a `Left`.

```haskell
Q> mapEither (+ 1) $ Right 42
Right 43
Q> mapEither (+ 1) $ Left "error"
Left "error"
```

4. Define a function `applyEither :: Either l (r -> a) -> Either l r -> Either l a`.

```haskell
Q> applyEither (Right (+ 1)) $ Right 42
Right 43
Q> applyEither (Right (+ 1)) $ Left "error"
Left "error"
Q> applyEither (Left "other error") $ Right 42
Left "other error"
```

5. Define a function `bindEither :: (r -> Either l a) -> Either l r -> Either l a`.

```haskell
Q> bindEither (\v -> Right (v + 1)) $ Right 42
Right 43
Q> bindEither (\v -> Right (v + 1)) $ Left "error"
Left "error"
Q> bindEither (\v -> Left "error from the function we ran") $ Right 42
Left "error from the function we ran"
```

##### Exercise notes (Either)

### Tuples

A tuple is an ad-hoc collection of values that can be of different types. Tuples are a staple of
many so called functional languages and Haskell is no exception:

```haskell
tuple :: (Int, String)
tuple = (42, "Forty-Two")

tuple' :: (Int, String, Bool)
tuple' = (42, "Forty-Two", False)

tuple'' :: (Int, String, Bool, Float)
tuple'' = (42, "Forty-Two", False, 1337.0)
```

In many ways a tuple is the same as a record, except we are not naming the different components or
the constructor. Consequently it's useful when names for the components or the constructor aren't
needed because the scope of the created value is small or the names themselves would not be deemed
useful. The utility of tuples should be examined on a case-by-case basis to ensure that they don't
make code harder to understand because of their lack of information/context. A name for both a
constructor and the individual fields/components can in many cases be very illuminating.

### List / []

`List` / `[]` is interesting because it's defined in terms of operators:

```haskell
data [] a
  = []
  | a : [a]
```

So we have a type, called `[]` that takes an `a`. The constructors are `[]` itself, which is the
empty list, and `:` which as the left argument takes an `a` and as the right argument takes another
list, `[a]`. This means that a list is effectively the `:` operator applied over and over until it
is connecting to a `[]`, which marks the end of the list:

```haskell
Q> 1 : 2 : 3 : 4 : []
[ 1, 2, 3, 4 ]
```

Defined another way we have the following:

```haskell
data List a
  = EmptyList -- This is commonly called `Nil`
  | Prepend a (List a) -- This is commonly called `Cons`
  -- ^ `a` here is commonly called the "head" of the list, and the list it is connected to is
  -- commonly called the "tail".
```

Lists are useful any time you need to have zero or more of something. It's important to note that
Haskell lists are pointers to pointers to pointers, which can be quite inefficient in many
situations.

We've seen many functions so far that have been operating on lists, but we have yet to work with
them with pattern matching. If we want to examine a list in similar ways to our other data we can do
so using the same tools we would otherwise:

```haskell
maybeFirstElement :: [a] -> Maybe a
maybeFirstElement (a : _) = Just a
maybeFirstElement [] = Nothing

maybeFirstElement' :: [a] -> Maybe a
maybeFirstElement' list = case list of
  a : _ -> Just a
  [] -> Nothing

maybeFirstTwoElements :: [a] -> Maybe (a, a)
maybeFirstTwoElements (a : b : _) = Just (a, b)
maybeFirstTwoElements [] = Nothing

maybeFirstTwoElements' :: [a] -> Maybe (a, a)
maybeFirstTwoElements' list = case list of
  a : b : _ -> Just (a, b)
  [] -> Nothing

maybeFirstAndRest :: [a] -> Maybe (a, [a])
maybeFirstAndRest (a : rest) = Just (a, rest)
maybeFirstAndRest _anyOtherCase = Nothing

-- We can also match to an exact structure of a list
maybeExactlyTwoElements :: [a] -> Maybe (a, a)
maybeExactlyTwoElements [a, b] = Just (a, b)
maybeExactlyTwoElements _anyOtherCase = Nothing
```

#### Exercises (Lists)

1. Define a function that takes a `[Int]` and divides the first element by the sum[0] of the rest of
   the list. If the sum of the "tail" (rest) is 0 or there are no elements in the list, return
   `Nothing`.

```haskell
Q> divideBySumOfRestOfList [1, 2, 3]
Just 0.2
Q> divideBySumOfRestOfList [1,2,3,4]
Just 0.11111111
Q> divideBySumOfRestOfList [1]
Nothing
Q> divideBySumOfRestOfList []
Nothing
```

2. Define a function that takes a `[a]` and returns a `Maybe [a]` where the returned list is the
   tail of the list. Consider what to return if the list is empty.

```haskell
Q> maybeTail [1, 2, 3]
Just [2, 3]
Q> maybeTail []
Nothing
Q> maybeTail [1]
Just []
```

3. Define an `average` function that takes a `[Int]` and returns `Maybe Float` where the return
   value is the average value. When and why might we need to return `Nothing`?

```haskell
Q> average [1, 2, 3]
Just 2.0
Q> average [1, 1, 3]
Just 1.6666666
Q> average []
Nothing
```

4. Define a function `maybeMaximumInt :: [Int] -> Maybe Int` function that takes a list of integers
   and finds the maximum integer of the list.

```haskell
Q> maybeMaximumInt [1, 3, 2]
Just 3
Q> maybeMaximumInt []
Nothing
Q> maybeMaximumInt [1, 3, 2, -5, 42, 8, 9, 15]
Just 42
```

5. Define a function `maximumInt :: Int -> [Int] -> Int` function that takes a default value and a
   list of integers, then either returns the default value or the found maximum value. Use the
   function you defined in exercise 4 together with `maybe`.

```haskell
Q> maximumInt 42 [1, 2, 3]
3
Q> maximumInt 42 []
42
```

6. Define a function `firstMatch :: (a -> Bool) -> [a] -> Maybe a` that returns the first element in
   a list that matches a given predicate, or `Nothing` otherwise.

```haskell
Q> firstMatch (== 3) [1, 2, 3]
Just 3
Q> firstMatch (== 3) []
Nothing
Q> firstMatch even [1, 2, 3]
Just 2
```

7. Define a function `firstMatchOr :: (a -> Bool) -> a -> [a] -> a` that uses the `firstMatch`
   function together with `foldMaybe` to provide a default value unless we find a matching element.

```haskell
Q> firstMatchOr (== 3) 42 [1, 2, 3]
3
Q> firstMatchOr (== 3) 42 []
42
Q> firstMatchOr even 42 [1, 2, 3]
2
Q> firstMatchOr even 42 [1, 3, 5, 7]
42
```

8. Define a function `filterList :: (a -> Bool) -> [a] -> [a]` that takes a predicate and a list,
   and returns all the elements matching the predicate.

```haskell
Q> filterList even [1..9]
[2, 4, 6, 8]
Q> filterList even [1, 3..9]
[]
```

9. Define a function `lengthOfList :: [a] -> Int` that returns the length of a list.

```haskell
Q> lengthOfList [1..9]
9
Q> lengthOfList []
0
```

10. Define a function `takeFromList :: Int -> [a] -> [a]` that returns the first N elements from a
    list, or as many as possible if N is greater than the length of the list.

```haskell
Q> takeFromList 3 [1..5]
[1, 2, 3]
Q> takeFromList 6 [1..5]
[1, 2, 3, 4, 5]
```

11. Define a function `takeWhileFromList :: (a -> Bool) -> [a] -> [a]` that takes elements from the
    list until it finds one that does not match the predicate passed to the function.

```haskell
Q> takeWhileFromList odd [1, 3, 5, 6, 7, 8, 9]
[1, 3, 5]
Q> takeWhileFromList even [1, 3, 5, 6, 7, 8, 9]
[]
```

12. Define a function `takeUntilFromList :: (a -> Bool) -> [a] -> [a]` that takes elements from the
    list until it finds one that matches the predicate passed to the function.

```haskell
Q> takeUntilFromList even [1, 3, 5, 6, 7, 8, 9]
[1, 3, 5]
Q> takeUntilFromList odd [1, 3, 5, 6, 7, 8, 9]
[]
```

13. Define a function `dropWhileFromList :: (a -> Bool) -> [a] -> [a]` that drops elements from the
    list until it finds one that does not match the predicate passed to the function.

```haskell
Q> dropWhileFromList odd [1, 3, 5, 6, 7, 8, 9]
[6, 7, 8, 9]
Q> dropWhileFromList even [1, 3, 5, 6, 7, 8, 9]
[1, 3, 5, 6, 7, 8, 9]
```

14. Define a function `dropUntilFromList :: (a -> Bool) -> [a] -> [a]` that drops elements from the
    list until it finds one that matches the predicate passed to the function.

```haskell
Q> dropUntilFromList odd [1, 3, 5, 6, 7, 8, 9]
[1, 3, 5, 6, 7, 8, 9]
Q> dropUntilFromList even [1, 3, 5, 6, 7, 8, 9]
[6, 7, 8, 9]
```

15. Define a function `zipList :: [a] -> [b] -> [(a, b)]`.

```haskell
Q> zipList [1..9] [5..10]
[(1, 5), (2, 6), (3, 7), (4, 8), (5, 9), (6, 10)]
Q> zipList [1..100] [42, 1337]
[(1, 42), (2, 1337)]
Q> zipList [1..100] []
[]
Q> zipList [] [1, 2, 3]
[]
```

16. Define a function `foldRight :: b -> (a -> b -> b) -> [a] -> b`.

```haskell
Q> foldRight 0 max [1, 2, 3]
3
Q> foldRight 0 max []
3
Q> foldRight 0 (+) [1, 2, 3]
6
Q> foldRight 1 (*) [1, 2, 3]
6
```

17. Define a function `pureList :: a -> [a]`.

```haskell
Q> pureList 42
[42]
```

18. Define a function `mapList :: (a -> b) -> [a] -> [b]`.

```haskell
Q> mapList (+ 1) [1, 2, 3]
[2, 3, 4]
Q> mapList (+ 1) []
[]
```

19[1]. Define a function `applyList :: [(a -> b)] -> [a] -> [b]`.

```haskell
Q> applyList [(+ 1), (* 2)] [1, 2, 3]
[2, 3, 4, 2, 4, 6]
```

20[1]. Define a function `bindList :: (a -> [b]) -> [a] -> [b]`.

```haskell
Q> bindList (\n -> replicate n n) [1, 2, 3, 4]
[1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
```

##### Exercise notes (Lists)

0. [sum](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Prelude.html#v:sum)
1. These exercises can be skipped in the interest of time.

## Strictness annotations

When reading (and subsequently writing) Haskell code you will likely stumble upon type definitions
that have exclamation marks (`!`) right before type names:

```haskell
data Tuple = Tuple !Int String
  deriving (Eq, Show)

-- This takes the string from our tuples
stringFromTuple :: Tuple -> String
stringFromTuple (Tuple _ string) = string
```

The exclamation mark before the `Int` here is a strictness annotation. Let's look at how this
affects the behavior of our constructor first and then see why that is. First let's evaluate an
error to see what an crash looks like in our REPL:

```haskell
Q> error "CRASH"
*** Exception: CRASH
CallStack (from HasCallStack):
  error, called at <interactive>:24:1 in interactive:Ghci2
Q> crash = error "CRASH"
Q> crash
*** Exception: CRASH
CallStack (from HasCallStack):
  error, called at <interactive>:25:5 in interactive:Ghci2
```

We first evaluate the error, then set `crash` to be that expression. Evaluating `crash` now reliably
causes the crash to happen.

Let's see how this strictness annotation seems to work out for our `stringFromTuple` function:

```haskell
Q> tuple = Tuple crash "hello"
Q> stringFromTuple tuple
*** Exception: CRASH
CallStack (from HasCallStack):
  error, called at <interactive>:45:9 in interactive:Ghci1
```

So, we first bind `tuple` to the expression that creates a `Tuple` out of our crash value and
"hello". It might seem surprising to some, but consider that if you defined a function to crash
you'd also have to execute it in order to have it crash. After we've defined that we then use our
function that takes the string (which is just a normal value, no crash) out of the `Tuple` and we
indeed see a crash happen.

Let's take a look at what happens when we remove our strictness annotation:

```haskell
data Tuple = Tuple Int String
  deriving (Eq, Show)
```

```haskell
Q> tuple = Tuple crash "hello"
Q> stringFromTuple tuple
"hello"
```

We don't see a crash, even though the crashing value is plainly in the `Tuple`. This happens because
Haskell defaults to lazy/non-strict evaluation, meaning it will only actually evaluate expressions
when they are needed by something else. Printing our `Tuple` to the terminal evaluates our crash
immediately so we might not notice this distinction in that case, but creating a function that only
uses parts of a structure can find these distinctions much more easily.

What we are doing when we add the strictness annotation to `Int` in our crashing example is say that
we want this piece of data to be fully evaluated when the structure itself is evaluated, even in the
case where the default behavior would not evaluate it.

Since this behavior applies to all expressions in Haskell (we only evaluate what is needed), what
does that mean for the actual execution of a program? In short, it means that every expression you
define is actually really just a recipe/formula for whatever value it should be producing,
represented as a function. If that function is never called, neither the expressions in it nor the
value it should be returning will materialize at all.

What it also means is that we have to be conscious that we may be building up massive amounts of
these functions, called "thunks", when we stitch together expressions. This is called a "space leak"
and is a common theme in Haskell users' frustrations when debugging the behavior of their program.

### Lists and lazyness

```haskell
Q> take 3 [1 :: Int, 2, 3, crash]
[ 1
, 2
, 3
]
Q> take 4 [1 :: Int, 2, 3, crash]
*** Exception: CRASH
CallStack (from HasCallStack):
  error, called at <interactive>:59:9 in interactive:Ghci1
```

Here we can see that the list type in Haskell is lazy by default, and the values we get from it
will only ever be evaluated if a function actually uses them. This also means that if I were to
bind the second expression to a name, we will be holding on to an expression that will crash,
unless we also happen to not use the 4th item of the list.

Lazyness is in Haskell for a reason, however, so it's important to also consider what we are
getting out of this feature. The easiest thing to show is how infinite lists are easy to work with:

```haskell
Q> take 5 [1..]
[ 1
, 2
, 3
, 4
, 5
]
```

What we are doing here is creating an infinite list of natural numbers (`[1..]`) and then passing
that infinite list to `take`, with which we are only asking for 5 numbers. The end result is that
we indeed only get 5 numbers, even though this infinite list expression is supposed to create an
infinite list... But since it's really only a description of how to create this infinite list, we
successively ask for more and more elements and eventually just stop asking, so at no point in time
does an infinite (or close to it) list exist.

This applies to many other similar contexts as well and is (in my personal opinion) a very nice
feature to have, so much so that I'm convinced that the benefits outweigh the cost. I think it's a
fair characterization that many things that in strict languages would be stack overflows
transparently work the way you want them to in Haskell, so much so that we don't even notice the
wins. Code in PureScript, a Haskell-like language that compiles to JavaScript, always has to
consider whether or not something is stack safe and these considerations permeate the language. In
a language based entirely on the composition of functions this can be quite a hassle sometimes.

### More tools for strictness

We can also annotate expressions with exclamation marks if we enable the `BangPatterns` extension,
which enables us to say that an expression we are binding to a name should be evaluated. We can also
use `seq` and `deepseq` to force evaluation of an expression, one level deep or completely,
respectively.

### More extensive material on lazyness

Michael Snoyman from FP Complete has written
[a good article](https://www.fpcomplete.com/blog/2017/09/all-about-strictness/) on lazyness and
strictness annotations that is worth reading.
