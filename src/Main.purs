module Main where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Number (fromString)
-- import Data.Profunctor (class Profunctor, dimap)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- What's a profunctor?
--   - It has input and output.
--   - You can bolt on something to the "output" end to modify the output.
--   - You can bolt on something to the "input" end to convert input.

-- Profunctors are not just functions. Profunctors are a general way to conceive of IO.

-- dimap map-input map-output profunctor == new_profunctor

-- The dimap function can be used to map functions over both arguments simultaneously.

class Profunctor p where
  -- dimap ∷ ∀ a b s t. (s → a) → (b → t) → p a b → p s t -- a profunctor optic has typically these type variables
  dimap ∷ ∀ a b c d. (a → b) → (c → d) → p b c → p a d

-- A function is a profunctor.
instance Profunctor (→) where
  dimap a2b c2d b2c = a2b >>> b2c >>> c2d

--                     |          |               |
--                  changes    applies         changes
--                   input     function        output
--                           (= profunctor)
-- b2c (b → c) is the profunctor.
-- dimap creates a new profunctor (a → d)

tst1 ∷ Number → Number
tst1 = dimap identity identity ((*) 3.0)

tst2 ∷ Number → Number
tst2 = dimap identity (_ - 5.0) ((*) 3.0)

tst3 ∷ Number → Number
tst3 = dimap ((+) 1.0) identity ((*) 3.0)

tst4 ∷ Number → Number
tst4 = dimap ((+) 1.0) (_ - 5.0) ((*) 3.0)

readFloat ∷ String → Number
readFloat = fromString >>> fromMaybe 0.0

-- Our toy profunctor: the Plus One Server!
-- `dimap` creates a profunctor (= Function) from a profunctor (= Function).
original ∷ Number → Number
original = dimap identity identity (1.0 + _) -- the same: original = ((+) 1.0)

oneServer = original

modified ∷ String → String
modified = dimap readFloat show original

-- Boss: "Hey, coder person, your plus-one server needs to accept UTF8 byte strings and return byte strings."

-- dummy function
-- Strings in PureScript are already UTF16 code units.
fromUTF8 ∷ String → String
fromUTF8 = toCharArray >>> fromCharArray

-- dummy function
-- Strings in PureScript are already UTF16 code units.
toUTF8 ∷ String → String
toUTF8 = toCharArray >>> fromCharArray

bossMadeMe1 = dimap fromUTF8 toUTF8 modified

-- Let's now imagine that the boss decides to keep the output format as String.
bossMadeMe2 = dimap fromUTF8 identity modified

-- dimap ∷ ∀ a b s t. (s → a) → (b → t) → p a b → p s t
-- If you look at the signature of dimap, you can read it a few different ways.
-- One way is that it returns a profunctor `p s t`. (It creates a profunctor from a profunctor.)
-- But another way - and one that is closer to that of map - is that it returns a function between profunctors `p a b` → `p s t`.

----------------------------------
-- First Profunctor Optic - Iso --
----------------------------------

-- When s == t and a == b, another way to look at `dimap (s → a) (a → s)` is that it has the potential to create an isomorphism between s and a,
-- e.g. converting a map to a list of tuples and back again.
-- In this case, and if there is no funny business (ie deleting entries), the two can be used interchangeably.

iso ∷ ∀ p s t a b. Profunctor p => (s → a) → (b → t) → p a b → p s t
iso = dimap

-- Using a type alias:
type Iso s t a b = ∀ p. Profunctor p => p a b → p s t

iso' ∷ ∀ s t a b. (s → a) → (b → t) → Iso s t a b
iso' s2a b2t = dimap s2a b2t -- Meaning that if you give me (s → a) → (b → t), I'll give you back a `p a b → p s t`.

-- In general, a profunctor optic is a function with the signature `p a b → p s t`.

---------------------------
-- A Stronger Profunctor --
---------------------------

-- Now, imagine that "The Plus One Server" is getting used more and, as is natural with servers, we want to hook it up to other services.
-- While other "more sophisticated" services have some sort of logging, our mighty ((+) 1.0) lacks logging capabilities.
-- So how can "The Plus One Server" take logs from upstream services and pass them to downstream services?

-- Does not work. We have to make adjustements.
-- pflog = dimap
--   (\(Tuple s myLog) → Tuple (readFloat s) myLog)
--   (\(Tuple b myLog) → Tuple (show b) myLog)
--   ((+) 1.0)

-- Adjustment. Making our Plus One Server stronger.
-- `p` is a Strong Profunctor.
class Profunctor p <= Strong p where
  first ∷ ∀ a b c. p a b → p (Tuple a c) (Tuple b c)
  second ∷ ∀ a b c. p a b → p (Tuple c a) (Tuple c b)

-- For a Function profunctor this means:
-- class Profunctor Function <= Strong Function where
--   first  ∷ ∀ a b c. Function a b → Function (Tuple a c) (Tuple b c) 
--   second ∷ ∀ a b c. Function a b → Function (Tuple c a) (Tuple c b)

-- the same:
-- class Profunctor (→) <= Strong (→) where
--   first  ∷ ∀ a b c. (a → b) → (Tuple a c) → (Tuple b c) 
--   second ∷ ∀ a b c. (a → b) → (Tuple c a) → (Tuple c b)

instance Strong (→) where
  first a2b (Tuple a c) = Tuple (a2b a) c
  second a2b (Tuple c a) = Tuple c (a2b a) -- the same: second = (<$>)

-- let's make The Plus One Server strong.
strongOneServer ∷ ∀ log. Tuple String log → Tuple String log
strongOneServer = dimap
  (\(Tuple s myLog) → Tuple (readFloat s) myLog)
  (\(Tuple b myLog) → Tuple (show b) myLog)
  (first ((+) 1.0))

-- What if we want to carry a map back to the incoming object?
--            Tuple      forwards         backwards
--            Tuple (String → Number) (Number → String)
-- → String → Tuple           Number  (Number → String) 
-- mapBackHome ∷ String → Tuple Number (Number → String)
mapBackHome ∷ ∀ a. Show a ⇒ String → Tuple Number (a → String)
mapBackHome s = Tuple (readFloat s) show -- `show` is our "map back" from `readFloat`

-- lens ∷ ∀ p s t a b. Strong p => (s → Tuple a (b → t)) → p a b → p s t
-- Applied to Profunctor Function:
-- lens creates a function which takes an `s` and creates a `t`:       (s → t)
-- lens ∷ ∀ p s t a b. Strong p => (s → Tuple a (b → t)) → (a → b) → (s → t)
lens ∷ ∀ s t a b. (s → Tuple a (b → t)) → (a → b) → (s → t)
lens inputWithMap server =
  dimap
    inputWithMap
    (\(Tuple b backHomeFn) → backHomeFn b)
    (first server)

-- Compare to dimap: in lens we hide the function (b → t) in the first function `inputWithMap` in a Tuple.
-- dimap ∷ ∀   a b s t.            (s →       a) → (b → t)  → (a → b) → (s → t)
-- lens  ∷ ∀ p a b s t. Strong p ⇒ (s → Tuple a    (b → t)) → (a → b) → (s → t)

type Lens s t a b = forall p. Strong p => p a b -> p s t
type Lens' s a = Lens s s a a

-- with type alias
lens' ∷ ∀ s t a b. (s → Tuple a (b → t)) → Lens s t a b
lens' inputWithMap server =
  dimap
    inputWithMap
    (\(Tuple b backHomeFn) → backHomeFn b)
    (first server)

mapBackToString :: String -> Tuple Number (Number -> String)
mapBackToString s = Tuple (readFloat s) show

mapBackToUTF8 :: String -> Tuple String (String -> String)
mapBackToUTF8 s = Tuple (fromUTF8 s) toUTF8

-- TODO: Types

-- lens1 ∷ Lens String String Number Number
-- lens1 ∷ Lens' String Number
lens1 ∷ (Number → Number) → (String → String)
lens1 = lens mapBackToString

lens2 ∷ (String → String) → String → String
lens2 = lens mapBackToUTF8

lensCompound ∷ (Number → Number) → String → String
lensCompound = lens1 >>> lens2

-- TODO: make it working
-- (lens mapBackToUTF8 -- lens 1
--   <<< lens mapBackToString) -- lens 2
--   ((+) 1.0) -- server
--   (withOctets pack [0xAB, 0xCD]) -- "4.1416"

-----------------------
-- Give us a choice! --
-----------------------

main ∷ Effect Unit
main = do
  log $ show $ tst1 5.0 -------------------------------------------------- 15.0 (3 * 5.0)
  log $ show $ tst2 5.0 -------------------------------------------------- 10.0 (3 * 5.0 - 5.0)
  log $ show $ tst3 5.0 -------------------------------------------------- 18.0 ((5 + 1) * 3.0)
  log $ show $ tst4 5.0 -------------------------------------------------- 13.0 ((5 + 1) * 3.0 - 5.0)
  log $ show $ modified "3.1416" ----------------------------------------- "4.1416"
  log $ show $ modified "" ----------------------------------------------- "1.0"
  log $ show $ fromCharArray [ '⇔', '✓', '✗', '→' ] ---------------------- "⇔✓✗→"
  log $ show $ "⇔✓✗→" ---------------------------------------------------- "⇔✓✗→" (PureScript Strings are already Unicode!)
  log $ show $ bossMadeMe1 "3.1416" -------------------------------------- "4.1416"
  log $ show $ bossMadeMe2 "3.1416" -------------------------------------- "4.1416"
  log $ show $ dimap readFloat show oneServer "3.1416" ------------------- "4.1416"
  log $ show $ iso readFloat show oneServer "3.1416" --------------------- "4.1416"
  log $ show $ strongOneServer (Tuple "3.1416" "someLog") ---------------- (Tuple "4.1416" "someLog")
  log $ show $ strongOneServer (Tuple "3.1416" 42) ----------------------- (Tuple "4.1416" 42)
  log $ show $ lensCompound oneServer "3.1416" --------------------------- "4.1416"
