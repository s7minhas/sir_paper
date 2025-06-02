FROM registry.codeocean.com/codeocean/r-base:3.4.4-ubuntu16.04
USER root

RUN echo "deb [trusted=yes] http://archive.debian.org/debian stretch main contrib non-free"        > /etc/apt/sources.list && \
    echo "deb [trusted=yes] http://archive.debian.org/debian-security stretch/updates main contrib non-free" >> /etc/apt/sources.list

RUN apt-get -o Acquire::Check-Valid-Until=false update && \
    apt-get install -y --allow-unauthenticated --no-install-recommends \
        libcairo2-dev \
        libxt-dev \
        libx11-dev \
        libxext-dev \
        libxrender-dev \
        libtiff5-dev \
        libpng-dev \
        libpango1.0-dev \
        libxml2-dev \
        libglpk-dev \
        texlive-extra-utils \
        ghostscript && \
    apt-get clean && rm -rf /var/lib/apt/lists/*
