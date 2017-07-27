{-# LANGUAGE QuasiQuotes #-}

module Plot.Labels where

import Data.Foldable
import Data.Monoid hiding ((<>))
import Data.Semigroup (Semigroup(..))

import Language.R
import Language.R.QQ

data Units = IN | CM | MM

inches :: Units
inches = IN

cm :: Units
cm = CM

mm :: Units
mm = MM

data Size = Size { units :: Units, width :: Double, height :: Double }

small :: Size
small = Size { units = cm, width = 8, height = 8 }

data Labels =
  Labels
  { title :: Last String
  , subtitle :: Last String
  , caption :: Last String
  , x :: Last String
  , y :: Last String
  , colour :: Last String
  }

instance Semigroup Labels where
  (<>) a b =
    Labels
    { title = combine title
    , subtitle = combine subtitle
    , caption = combine caption
    , x = combine x
    , y = combine y
    , colour = combine colour
    }
    where
      combine f = (<>) (f a) (f b)

instance Monoid Labels where
  mappend = (<>)
  mempty =
    Labels
    { title = mempty
    , subtitle = mempty
    , caption = mempty
    , x = mempty
    , y = mempty
    , colour = mempty
    }

labels :: Labels
labels = mempty

setLabels :: Labels -> SomeSEXP s -> R s (SomeSEXP s)
setLabels (Labels {..}) p = do
  labellers <- (sequence . map sequence)
    [ (\l -> [r| labs(title = l_hs) |]) <$> getLast title
    , (\l -> [r| labs(subtitle = l_hs) |]) <$> getLast subtitle
    , (\l -> [r| labs(caption = l_hs) |]) <$> getLast caption
    , (\l -> [r| labs(x = l_hs) |]) <$> getLast x
    , (\l -> [r| labs(y = l_hs) |]) <$> getLast y
    ]
  applyLabellers labellers
  where
    mayApplyLabeller res = maybe (pure res) (\l -> [r| res_hs + l_hs |])
    applyLabellers = foldlM mayApplyLabeller p
