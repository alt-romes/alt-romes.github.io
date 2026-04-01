#!/bin/sh

cabal run control-site --allow-newer=base,template-haskell,containers,time -- watch --port 25565 --host 0.0.0.0
