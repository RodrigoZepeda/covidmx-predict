#!/bin/bash
#From https://github.com/rocker-org/rocker-versioned2/blob/29153661ee3463eaa1021ea49bf4f3bcf9d76196/scripts/install_tidyverse.sh

set -e

## build ARGs
NCPUS=${NCPUS:--1}

# a function to install apt packages only if they are not installed
function apt_install() {
    if ! dpkg -s "$@" >/dev/null 2>&1; then
        if [ "$(find /var/lib/apt/lists/* | wc -l)" = "0" ]; then
            apt-get update
        fi
        apt-get install -y --no-install-recommends "$@"
    fi
}

#This can probably be optimized haven't checked
apt_install \
    libxml2-dev \
    libcairo2-dev \
    libgit2-dev \
    default-libmysqlclient-dev \
    libpq-dev \
    libsasl2-dev \
    libsqlite3-dev \
    libssh2-1-dev \
    libxtst6 \
    libcurl4-openssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    unixodbc-dev \
    wget

install2.r --error --skipinstalled -n "$NCPUS" -r https://cloud.r-project.org/ \
    RCurl \
    dplyr \
    remotes \
    scales \
    pacman \
    tidyverse \
    lubridate \
    glue \
    ggtext \
    bsts \
    cli

# Clean up
rm -rf /var/lib/apt/lists/*
rm -rf /tmp/downloaded_packages

## Strip binary installed lybraries from RSPM
## https://github.com/rocker-org/rocker-versioned2/issues/340
strip /usr/local/lib/R/site-library/*/libs/*.so

# Check the tidyverse core packages' version
echo -e "Check the BSTS package...\n"

R -q -e "library(bsts)"

echo -e "\nInstall bsts package, done!"
