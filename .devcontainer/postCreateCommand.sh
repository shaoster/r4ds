#!/bin/sh
sudo apt-get update
# Interactive plots require xdg
sudo apt-get install -y --no-install-recommends xdg-utils libpoppler-cpp-dev
sudo rm -rf /var/lib/apt/lists/*
# Set up the renv
R -e 'install.packages("renv");renv::restore()'
# Set up git hooks
git config core.hooksPath .hooks