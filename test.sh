#!/usr/bin/env bash

find src test examples -type f | grep .hs | entr sh -c 'stack test'
