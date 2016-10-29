{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ratings.Glicko
  ( Glicko(..)
  , HasGlicko(..)
  , updateGlicko
  ) where


import Ratings.Types


------------------------------------------------------------------------------
data Glicko = Glicko
  { _glickoRating   :: Rating
  , _glickoRD       :: RD
  } deriving (Eq,Show,Read)


------------------------------------------------------------------------------
-- | This type class forces the user to provide a static c value to be used
-- for their whole system.  Typically you'll provide a simple orphan instance
-- for Glicko like this:
--
-- > instance HasGlicko Glicko where
-- >   mkGlicko = Glicko           
-- >   glickoRating = _glickoRating
-- >   glickoRD = _glickoRD
-- >   glickoC _ = 34.6
-- 
-- Alternatively, if you really want to avoid the orphan instance you can use
-- a newtype wrapper around Glicko.
class HasGlicko a where
    mkGlicko :: Rating -> RD -> a
    glickoRating :: a -> Rating
    glickoRD :: a -> RD
    glickoC :: a -> Double

q :: Double
q = log 10 / 400

g :: RD -> RD
g (RD rd) = RD $ 1 / sqrt (1 + 3 * q*q * rd*rd / (pi*pi))

e :: HasGlicko glicko => Rating -> glicko -> Double
e (Rating me) them =
    1 / (1 + 10 ** (negate $ _unRD (g $ glickoRD them) *
                    (me - _unRating (glickoRating them)) / 400))

d2 :: HasGlicko glicko => Rating -> [glicko] -> Double
d2 me opps = 1 / (q*q * sum (map f opps))
  where
    f opp = (_unRD $ g $ glickoRD opp) ^ (2::Int) * e me opp * (1 - e me opp)


------------------------------------------------------------------------------
-- | Initial rating to use when we have no prior knowledge of a player's
-- strength.
initialGlicko :: Glicko
initialGlicko = Glicko 1500 350


------------------------------------------------------------------------------
-- | Updates a player's glicko strength based on all outcomes in a single
-- rating period. 
updateGlicko
    :: HasGlicko glicko
    => Int
    -- ^ Number of rating periods since rating was last updated
    -> [(glicko, Score)]
    -- ^ Outcomes against each player
    -> glicko
    -- ^ Previous glicko strength estimate
    -> glicko
    -- ^ New glicko strength estimate
updateGlicko t opps me = mkGlicko r' rd'
  where
    r = glickoRating me
    rd0 = _unRD $ glickoRD me
    c = glickoC me
    rd = min (_unRD $ _glickoRD initialGlicko)
             (sqrt $ rd0*rd0 + c*c * fromIntegral t)
    d2Inv = 1.0 / d2 (glickoRating me) (map fst opps)
    rdInv = 1.0 / (rd*rd)
    r' = Rating $ _unRating r + (q * foo / (rdInv + d2Inv))
    rd' = RD $ sqrt $ 1 / (rdInv + d2Inv)
    foo = sum $ map f opps
    f (opp, Score s) = _unRD (g (glickoRD opp)) * (s - e r opp)


testOpps :: [(Glicko, Score)]
testOpps =
  [ (Glicko 1400 30, 1)
  , (Glicko 1550 100, 0)
  , (Glicko 1700 300, 0)
  ]

--instance HasGlicko Glicko where
--    mkGlicko = Glicko           
--    glickoRating = _glickoRating
--    glickoRD = _glickoRD
--    glickoC _ = 34.6
--
--testGlicko = updateGlicko 0 testOpps (Glicko 1500 200) == (Glicko 1464.1064627569112 151.39890244796933)

