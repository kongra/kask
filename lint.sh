#!/bin/sh

cabal exec hlint `find src/  -name "*.hs"`
cabal exec hlint `find app/  -name "*.hs"`
cabal exec hlint `find test/ -name "*.hs"`