module Backend where

import Obelisk.Backend
import Route

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
