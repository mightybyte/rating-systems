{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ratings.Types
  ( Rating(..)
  , RD(..)
  , Score(..)
  , Result(..)
  , resultScore
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
resultScore :: Result -> Score
resultScore Win = Score 1
resultScore Loss = Score 0
resultScore Draw = Score (1/2)



