module Main where

import Prelude

import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Number (fromString, isNaN)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)

-- =======================
-- What's a profunctor? ==
-- =======================

--   - It has input and output.
--   - You can bolt on something to the "output" end to modify the output.
--   - You can bolt on something to the "input" end to convert input.

-- Profunctors are not just functions. Profunctors are a general way to conceive of IO.

-- dimap creates a profunctor
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

-- ===================================
-- Our First Profunctor Optic - Iso ==
-- ===================================

-- When s == t and a == b, another way to look at `dimap (s → a) (a → s)` is that it has the potential to create an isomorphism between s and a,
-- e.g. converting a map to a list of tuples and back again.
-- In this case, and if there is no funny business (ie deleting entries), the two can be used interchangeably.

-- Iso is just a different view on profunctors.
iso ∷ ∀ p s t a b. Profunctor p ⇒ (s → a) → (b → t) → p a b → p s t
iso = dimap

-- Using a type alias:
type Iso s t a b = ∀ p. Profunctor p ⇒ p a b → p s t

iso' ∷ ∀ s t a b. (s → a) → (b → t) → Iso s t a b
iso' s2a b2t = dimap s2a b2t -- Meaning that if you give me (s → a) → (b → t), I'll give you back an Iso.

-- In general, a profunctor optic is a function with the signature `p a b → p s t`.

-- ========================
-- A Stronger Profunctor ==
-- ========================

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

-- It's strong because we can transport more information (by using a Tuple).
instance Strong (→) where
  first a2b (Tuple a c) = Tuple (a2b a) c
  second a2b (Tuple c a) = Tuple c (a2b a) -- the same: second = (<$>)

-- Let's make The Plus One Server strong.
-- It carries a log.
strongOneServer ∷ ∀ log. Tuple String log → Tuple String log
strongOneServer = dimap
  (\(Tuple s myLog) → Tuple (readFloat s) myLog)
  (\(Tuple b myLog) → Tuple (show b) myLog)
  (first ((+) 1.0))

-- What if we want to carry a map back to the incoming object?
--            Tuple       map#1          map#2
--            Tuple (String → Number)  (a → String)
-- → String → Tuple           Number   (a → String) 
-- mapBackHome ∷ String → Tuple Number (a → String)
mapBackHome ∷ ∀ a. Show a ⇒ String → Tuple Number (a → String)
mapBackHome s = Tuple (readFloat s) show -- `show` is our "map back" from `readFloat`

-- We are packing the covariant function (b → t) into the contravariant function and hiding it in a Tuple.
-- lens ∷ ∀ p s t a b. Strong p ⇒ (s → Tuple a (b → t)) → p a b → p s t
-- Applied to Profunctor Function:
-- lens creates a function which takes an `s` and creates a `t`:       (s → t)
-- lens ∷ ∀ p s t a b. Strong p ⇒ (s → Tuple a (b → t)) → (a → b) → (s → t)
-- lens generates a function taking an `s` and producing a `t` (s → t)
-- lens ∷ ∀ s t a b. (s → Tuple a (b → t)) → (a → b) → (s → t)
lens ∷ ∀ s t a b. (s → Tuple a (b → t)) → Lens s t a b
lens inputWithMap server =
  dimap
    inputWithMap
    (\(Tuple b backHomeFn) → backHomeFn b)
    (first server)

-- Compare to dimap: in lens we hide the function (b → t) in the first function `inputWithMap` in a Tuple.
-- `lens` is like a strong - using a Tuple - `dimap`.
-- dimap ∷ ∀   a b s t.            (s →       a) → (b → t)  → (a → b) → (s → t)
-- lens  ∷ ∀ p a b s t. Strong p ⇒ (s → Tuple a    (b → t)) → (a → b) → (s → t)

-- Lens is a strong profunctor. (Strong means that it uses a Tuple.)
type Lens s t a b = ∀ p. Strong p ⇒ p a b → p s t -- (a → b) → (s → t)
type Lens' s a = Lens s s a a ---------------------- (a → a) → (s → s)

-- In general, when you "zoom out" from a lens, you want to get back to where you started, so Lens' is enough for most practical cases.

-- with type alias
lens' ∷ ∀ s t a b. (s → Tuple a (b → t)) → Lens s t a b
lens' inputWithMap server =
  dimap
    inputWithMap
    (\(Tuple b backHomeFn) → backHomeFn b)
    (first server)

mapBackToString ∷ String → Tuple Number (Number → String)
mapBackToString s = Tuple (readFloat s) show

mapBackToUTF8 ∷ String → Tuple String (String → String)
mapBackToUTF8 s = Tuple (fromUTF8 s) toUTF8

lens1 ∷ Lens' String Number
lens1 = lens mapBackToString

lens2 ∷ Lens' String String
lens2 = lens mapBackToUTF8

-- the same:
-- lensCompound ∷ (Number → Number) → (String → String)
lensCompound ∷ Lens' String Number
lensCompound = lens1 >>> lens2

-- ====================
-- Give Us a Choice! ==
-- ====================

-- You go either the Left or Right path using Either (a sum type).

-- similar to Strong which used a Tuple (a product type)
class Profunctor p ⇐ Choice p where
  left ∷ ∀ a b c. p a b → p (Either a c) (Either b c)
  right ∷ ∀ a b c. p a b → p (Either c a) (Either c b)

-- applied to profunctor Function (→)
-- class Profunctor p ⇐ Choice p where
--   left  ∷ ∀ a b c. (a → b) → (Either a c) → (Either b c)
--   right ∷ ∀ a b c. (a → b) → (Either c a) → (Either c b)

instance Choice (→) where
  left a2b (Left a) = Left $ a2b a
  left _ (Right c) = Right c
  right = (<$>)

-- how I'll use the oneServer
-- put the oneServer in the left path and use the left path
myUseOneServer = dimap (\s → Left (readFloat s)) (either show identity) (left oneServer)

-- how my colleague will use the oneServer
-- put the oneServer in the left path and use the right path
colleagueUseOneServer = dimap (\_ → Right "One-Server ignored") (either show identity) (left oneServer)

-- Another choice we can make is whether to attempt the computation at all.
-- If our server can process the information, great, and if not, we provide a sensible default.
sensibleDefault ∷ String → Either String Number
sensibleDefault s = if isNaN (readFloat s) then (Left s) else Right (readFloat s)

-- In lens-land, this is called a Prism.
prism ∷ ∀ p s t a b. Choice p ⇒ (s → Either t a) → (b → t) → p a b → p s t
prism to from server = dimap to (either identity from) (right server)

-- ===========================
-- But What About Security? ==
-- ===========================

-- "The Plus One Server" is so hot that hackers have noticed it and are trying to reverse engineer it to understand its inner workings and exploit its vulnerabilities.
-- The boss, dismayed, starts saying stuff like "we need to lock this thing down" and "does anyone here know about end-to-end encryption?"

-- We would love to have something like that but so far it doesn't work:
-- secureOneServer = dimap
--   (\s password → readFloat s) ----------- we want to secure our number (= 4.1416) with a password
--   (\locked → show (locked "passw0rd")) -- locked is a function that takes a password and results in the original number (= 4.1416)
--   oneServer
--   "3.1416"

-- The `Closed` class extends the `Profunctor` class to work with functions.
class Profunctor p ⇐ Closed p where
  closed ∷ ∀ a b x. p a b → p (x → a) (x → b)

-- Applied to profunctor Function:
-- class Profunctor p ⇐ Closed (→) where
--   closed ∷ ∀ a b x. (a → b) → (x → a) → (x → b)

--                             2
--                      ---------------
--                      |             |
--   closed ∷ ∀ a b x. (a → b) → (x → a) → x → b
--                                |        |
--                                ---------- 
--                                    1

-- A way to think about lock is that it delays/defers application of our function until the point when a password is provided.
-- Another way to imagine it is that it sends the function to the end-user and lets them apply it with their password.
-- I would guess that this is the exact algorithm WhatsApp uses for their end-to-end encryption.

instance Closed (→) where
  closed = (<<<)

lock = closed

-- `closed` allows to provide oneServer with an additional argument! (In our case it's a password.)
-- type variable `x` in class Closed would be the password to access our oneServer.
-- Actually, in our sample code we don't check the password, the idea is the same.
-- dimap ∷ ∀ p a b c d. Profunctor p ⇒ (a → b) → (c → d) → p b c → p a d

lockedOneServer ∷ String → Number
lockedOneServer =
  dimap
    (\s → \_password → readFloat s) --------------  String (= "3.1416") → (Password → Number (= 3.1416))
    (\locked → locked "not-so-secret-password") -- (Password → Number (= 4.1416)) → Number (= 4.1416)
    (lock oneServer) ----------------------------- (Password → Number (= 3.1416)) → (Password → Number (= 4.1416))

-- Start with (lock oneServer) and see what it needs for input and what output it creates.
-- Output goes to second function of dimap (\locked → …).

-- In functional-programming land, passwords can be anything. Strings, Ints, ... and FUNCTIONS!
-- Password-protection with a function is called a GRATE.
-- Here, (s → a) will be our "function" password.
-- Replace x with (s → a) in closed:
-- closed ∷ ∀ a b x s. (a → b) → ((s → a) → a) → ((s → a) → b)
-- grate ∷ ∀ s t a b p. Closed p ⇒ (((s → a) → b) → t) → p a b → p s t
-- grate ∷ ∀ s t a b p. Closed p ⇒ (((s → a) → b) → t) → (a → b) → (s → t)
grate unlock server =
  dimap
    (\s → \f → f s) ---    s → ((s → a) → a)
    unlock ------------   ((s → a) → b) → t
    (lock server) -----  ((s → a) → a) → ((s → a) → b)

-- checking types
grate' ∷ ∀ s t a b. (((s → a) → b) → t) → (a -> b) → (s -> t)
grate' unlock server =
  dimap
    ((\s → \f → f s) ∷ s → ((s → a) → a))
    (unlock ∷ ((s → a) → b) → t)
    (lock server ∷ ((s → a) → a) → ((s → a) → b))

-- rgb = s = RGB
-- t       = RGB
-- a       = Number
-- b       = Number
grate'' ∷ ∀ rgb t a b. (((rgb → a) → b) → t) → (a -> b) → (rgb -> t)
grate'' unlock server =
  dimap
    ((\rgb → \f → f rgb) ∷ rgb → ((rgb → a) → a)) ------ rgb is hidden inside the function password
    (unlock ∷ ((rgb → a) → b) → t)
    (lock server ∷ ((rgb → a) → a) → ((rgb → a) → b))

data RGB = RGB Number Number Number

derive instance Generic RGB _
instance Show RGB where
  show = genericShow

-- secret algorithms/password functions: red, green, blue
red ∷ RGB → Number
red (RGB r _ _) = r

green ∷ RGB → Number
green (RGB _ g _) = g

blue ∷ RGB → Number
blue (RGB _ _ b) = b

mySecretFilterApplication ∷ ((RGB → Number) → Number) → RGB -- (((s → a) → b) → t) in grate = unlock function
mySecretFilterApplication = \f → RGB (f red) (f green) (f blue)

-- Calculation of new RGB value is completely hidden.
grateOneServer ∷ RGB → RGB
grateOneServer = grate mySecretFilterApplication oneServer

-- When thinking about password protecting something or, more generally, hiding the application of an algorithm, closed profunctors and grates are your friend!

-- =================
-- Make me a Star ==
-- =================

-- Star turns a Functor into a Profunctor.

-- There are lots of nice profunctors, and we'd like to work with them somehow.
-- One such profunctor is the KLEISLI PROFUNCTOR.
-- It is a profunctor with a side effect mixed in.
-- So if you take the Function profunctor we've seen before a → b,
-- a KLEISLI PROFUNCTOR is that with a little something extra like write to a log, read from an environment or launch a rocket.
-- The signature for a KLEISLI PROFUNCTOR, also called a Star, is:
newtype Star ∷ ∀ k. (k → Type) → Type → k → Type
newtype Star f a b = Star (a → f b)

-- Here, f represents the side effect and b represents the output value.
-- This could be something like Log Int or Array String or RocketLauncher Number.

instance Functor f ⇒ Functor (Star f a) where
  map f (Star g) = Star (map f <<< g)

-- with type annotaions
-- instance Functor f ⇒ Functor (Star f a) where
--   map ∷ ∀ c d. (c → d) → Star f a c → Star f a d
--   map (f ∷ c → d) (Star (g ∷ a → f c)) = Star (map f <<< g) ∷ Star f a d

instance Functor f ⇒ Profunctor (Star f) where
  dimap ∷ ∀ a b c d. (a → b) → (c → d) → Star f b c → Star f a d
  dimap ein aus (Star f) = Star (map aus <<< f <<< ein)

class (Strong p, Choice p) <= Wander p where
  wander
    ∷ ∀ s t a b
    . (∀ f. Applicative f ⇒ (a → f b) → s → f t)
    → p a b
    → p s t

-- Applicative f = Identity
instance Wander Function where
  wander ∷ ∀ s t a b. (∀ f. Applicative f ⇒ (a → f b) → s → f t) → (a → b) → (s → t)
  wander kleisli ourfunc input = unwrap ((kleisli (Identity <<< ourfunc)) input)

type Client = { name ∷ String, balance ∷ Number }

actOnBalance ∷ ∀ f. Applicative f ⇒ (Number → f Number) → Client → f Client
actOnBalance f client = { name: client.name, balance: _ } <$> f client.balance

-- balanceClient is a lens
balanceClient = wander actOnBalance

-- traversal is a lens
traversal = wander traverse

traverseList = traversal <<< lens (\s → Tuple (readFloat s) show)

-- ====================
-- Folding up values --
-- ====================

-- Unfortunately this paragraph is too chaotic. Therefore only small part of it
-- implemented.

-- So far, our workhorse profunctor has been the function, and even when we
-- wandered into Kleisli-land, we did so to import our results back to the world
-- of functions. Remembering that optics are functions between profunctors, when
-- we give a Function to an optic as an argument, we call that a Setter. This is
-- how "The Plus One Server" worked - it is a function (profunctor) that we
-- passed to an OPTIC (= FUNCTION ACTING ON PROFUNCTORS) and it set something on
-- the inside of a larger structure (ie an array of integers in a traversal or a
-- RGB channel in a grate).

-- While this metaphor is helpful, it is admittedly like a bad monad tutorial ,
-- in that it specializes the idea of profunctors too narrowly. PROFUNCTORS ARE
-- A GENERALIZED WAY TO REASON ABOUT I/O, and there's more to I/O than just
-- functions.

-- One thing that comes up a lot with I/O is intentionally suppressing the
-- output. For example, you can ignore the output value and instead return
-- something else. In Purescript, this is called `Forget`.

-- forget `b` (covariant output transformation: `b → r`)
newtype Forget ∷ ∀ k. Type → Type → k → Type
newtype Forget r a b = Forget (a → r)

derive instance Functor (Forget r a)

instance Profunctor (Forget r) where
  dimap f _ (Forget z) = Forget (z <<< f)

instance Strong (Forget r) where
  first (Forget z) = Forget (z <<< fst)
  second (Forget z) = Forget (z <<< snd)

-- ordinary Function profunctor
dontForget = dimap (const true) (const 42) identity

-- "forgets" to apply output transformation (= const 42)
-- dimap :: (a → b) → (c → d) → Forget (b → r) → Forget (a → r)
-- dimap :: (a → b) →    _    → Forget (b → r) → Forget (a → r)
forget ∷ ∀ a. Forget Boolean a Int
forget = dimap (const true) (const 42) (Forget identity)

runForget (Forget f) = f

lens'' i = dimap i (\(Tuple b f) → f b) <<< first

-- forgetLens = lens'' (\a -> Tuple (readFloat a) show) (Forget identity) "3.1416" -- Forget 3.1416

-- ff = runForget $ lens'' forgetLens

-- view profunctor = runForget (profunctor (Forget identity))
-- view $ lens (\a -> Tuple (readFloat a) show) (Forget identity) "3.1416" -- 3.1416

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
  log $ show $ myUseOneServer "3.1416" ----------------------------------- "4.1416"
  log $ show $ colleagueUseOneServer "3.1416" ---------------------------- "One-Server ignored"
  log $ show $ prism sensibleDefault show oneServer "3.1416" ------------- "4.1416"
  log $ show $ prism sensibleDefault show oneServer "not a number" ------- "1.0" 
  log $ show $ lockedOneServer "3.1416" ---------------------------------- "4.1416"
  log $ show $ grateOneServer (RGB 1.0 2.0 3.0) -------------------------- (RGB 2.0 3.0 4.0)
  log $ show $ balanceClient oneServer { name: "Mike", balance: 0.0 } ---- { balance: 1.0, name: "Mike" }
  log $ show $ traversal oneServer [ 1.0, 2.0, 3.0 ] --------------------- [2.0, 3.0, 4.0]
  log $ show $ traverseList oneServer [ "1.0", "2.0", "3.0" ] ------------ ["2.0", "3.0", "4.0"]
  log $ show $ dontForget "3.1416" --------------------------------------- 42
  log $ show $ runForget forget "3.1416" --------------------------------- true
