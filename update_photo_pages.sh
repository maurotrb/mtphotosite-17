#!/bin/bash

rm content/photos/*.md
cd src
./PhotoPagesFromCSV.hs
cd ..
