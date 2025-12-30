# Exercises for R for Data Science

This repo records the exercises done while following https://r4ds.hadley.nz/

Each R file represents one chapter (e.g. `1.R` is `1 Data visualization`).


# Environment Setup

There's a couple of magical things happening here so we have a portable and reproducible setup.

1. The environment is built on top of the docker container found at https://hub.docker.com/r/rocker/tidyverse.
   The configurations for the container build in vscode live in .devcontainer/devcontainer.json and those configurations

    > Mounts the R package cache to a host directory so environment setups don't take forever after the first time,
      which can easily take more than 10 minutes.

    > Install some additional OS packages into the docker container that are dependencies of used R packages.

    > Restore the renv state from the lockfile (see next).

    > Set up some git environment configurations (like pre-commit hooks).
  
2. The R project itself has its packages managed by R. The state is stored in renv.lock.

# Basic Development Workflow

The workflow for development comes from here: https://code.visualstudio.com/docs/devcontainers/containers#_quick-start-open-an-existing-folder-in-a-container

Selecting this repository as the folder in question causes VSCode to do the rest of the set up
automagically, though the first time might take quite some time (10+ minutes).

Subsequently, anything that's done in the project should be saved via git, as the workspace volume
itself is torn down and re-created from git after each session; thus there's some pre-commit hooks
in place to ensure that the state of the project is okay after each commit.

