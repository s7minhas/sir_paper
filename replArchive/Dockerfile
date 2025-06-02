########################
# Dockerfile: RStudio 3.4.4 with 
# necessary dev libraries for installing
# Cairo, igraph, and sys commands
# like pdfcrop
########################

# RStudio base image based
# on when we started this project
# FROM --platform=linux/amd64 rocker/rstudio:3.4.4
FROM rocker/rstudio:3.4.4

# go root to install packages
USER root

# config debian stretch archived repos
# and ignore expired sigs
RUN echo "deb http://archive.debian.org/debian stretch main contrib non-free"       >  /etc/apt/sources.list && \
    echo "deb http://archive.debian.org/debian-security stretch/updates main contrib non-free" >> /etc/apt/sources.list && \
    echo 'Acquire::Check-Valid-Until \"false\";' > /etc/apt/apt.conf.d/99ignore-valid-until

# update and install needed deps
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcairo2-dev \
    libxt-dev \
    libx11-dev \
    libxext-dev \
    libxrender-dev \
    libjpeg-dev \
    libtiff5-dev \
    libpng-dev \
    libpango1.0-dev \
    libxml2-dev \
    libglpk-dev \
    texlive-extra-utils \
    ghostscript \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

