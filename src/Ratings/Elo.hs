{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ratings.Elo
  ( Elo(..)
--  , HasElo(..)
  , updateEloGame
  , updateEloMatch
  ) where


import Ratings.Types

newtype Elo = Elo { _unElo :: Rating }
  deriving (Eq,Show,Read,Num,Fractional)

q :: Elo -> Double
q (Elo (Rating r)) = 10 ** (r / 400.0)

e :: Elo -> Elo -> Double
e me opp = q me / (q me + q opp)


------------------------------------------------------------------------------
updateEloGame :: Double -> Elo -> Score -> Elo -> Elo
updateEloGame k opp (Score s) me = Elo $ Rating $
    _unRating (_unElo me) + k * (s - e me opp)


------------------------------------------------------------------------------
updateEloMatch :: Double -> [(Elo, Score)] -> Elo -> Elo
updateEloMatch k games me = foldr f me games
  where
    f (opp, s) i = updateEloGame k opp s i


