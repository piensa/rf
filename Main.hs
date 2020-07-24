{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Functor.Identity (Identity)


import Obelisk.Route

import qualified Obelisk.Frontend as O
import qualified Obelisk.Backend as O
import qualified Obelisk.Route as O
import qualified Obelisk.Route.TH as O
import qualified Obelisk.Run as O

import Reflex.Dom.Core (el, text)


data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()

fullRouteEncoder
  :: O.Encoder (Either T.Text) Identity (O.R (O.FullRoute BackendRoute FrontendRoute)) O.PageName
fullRouteEncoder = O.mkFullRouteEncoder
  (O.FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> O.PathSegment "missing" $ O.unitEncoder mempty)
  (\case
      FrontendRoute_Main -> O.PathEnd $ O.unitEncoder mempty)

concat <$> mapM O.deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]

commonStuff :: String
commonStuff = "Here is a string"

frontend :: O.Frontend (O.R FrontendRoute)
frontend = O.Frontend
  { O._frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
  , O._frontend_body = do
      el "h1" $ text "Obelisk!"
      el "p" $ text $ T.pack commonStuff

      el "div" $ do
      return ()
  }

backend :: O.Backend BackendRoute FrontendRoute
backend = O.Backend
  { O._backend_run = \serve -> serve $ const $ return ()
  , O._backend_routeEncoder = fullRouteEncoder
  }


main = O.run 8001 (O.runServeAsset "static")  backend frontend
