{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.MainPage where

import Lucid

html :: Html ()
html =
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ "Main"
      script_ [defer_ "", src_ "//localhost:8000/elm.js"] ""
      script_ [defer_ "", src_ "//localhost:8000/main.js"] ""
    body_ $
      div_ [id_ "elm"] ""
