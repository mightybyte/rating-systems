{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ratings.Types
  ( Rating(..)
  , RD(..)
  , Score(..)
  , Result(..)
  , res
  ) where


------------------------------------------------------------------------------
-- | An estimate of a player's strength.
newtype Rating = Rating { _unRating :: Double }
  deriving (Eq,Show,Read,Num,Fractional)


------------------------------------------------------------------------------
-- | An estimate of the deviation of a player's rating.
newtype RD = RD { _unRD :: Double }
  deriving (Eq,Show,Read,Num,Fractional)


------------------------------------------------------------------------------
-- | The outcome of a matchup.  1 = win, 1/2 = draw, 0 = loss.
newtype Score = Score { _unScore :: Double }
  deriving (Eq,Show,Read,Num,Fractional)


------------------------------------------------------------------------------
-- | Outcome of a game that can end in only win/loss/draw.
data Result = Win | Loss | Draw


------------------------------------------------------------------------------
-- | Convenient function for converting a Result to a Score.
res :: Result -> Score
res Win = Score 1
res Loss = Score 0
res Draw = Score (1/2)



