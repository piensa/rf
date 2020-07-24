module Main where

import Backend (backend)
import Frontend (frontend)
import Obelisk.Run (run, runServeAsset)

main = run 8001 (runServeAsset "static")  backend frontend
