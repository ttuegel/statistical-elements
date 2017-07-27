{-# LANGUAGE QuasiQuotes #-}

module Plot
    ( module Plot.Labels
    , displayWidget
    ) where

import Control.Monad (void)
import Language.R
import Language.R.QQ
import Web.Browser (openBrowser)

import Plot.Labels


-- | Display an R htmlwidget in a local browser window.
displayWidget :: (forall s. R s (SomeSEXP s)) -> IO ()
displayWidget plot = do
  [url] <- runRegion $ do
    p <- plot
    (<$>) fromSomeSEXP
      [r|
        library(htmlwidgets)
        tmp <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(p_hs, tmp)
        tmp
      |]
  void (openBrowser url)
