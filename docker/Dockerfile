FROM rocker/r-ver:4.2.2

ARG TARGETARCH
ARG QUARTO_VERSION="1.3.260"

COPY r-requirements.txt ./

RUN apt-get -y update \
    && apt-get install -y \
        curl \
        gdebi \
        libcurl4-openssl-dev \
        libudunits2-dev \
        libgdal-dev \
        libgeos-dev \
        libproj-dev \
        libnetcdf-dev \
    && apt-get -y dist-upgrade
RUN if [ $TARGETARCH = "arm64" ] ; \
    then curl -LO https://github.com/quarto-dev/quarto-cli/releases/download/v$QUARTO_VERSION/quarto-$QUARTO_VERSION-linux-arm64.deb \
        && gdebi --non-interactive quarto-$QUARTO_VERSION-linux-arm64.deb ; \
    else curl -LO https://github.com/quarto-dev/quarto-cli/releases/download/v$QUARTO_VERSION/quarto-$QUARTO_VERSION-linux-amd64.deb \
        && gdebi --non-interactive quarto-$QUARTO_VERSION-linux-amd64.deb ; \
    fi
RUN Rscript -e "install.packages('remotes')" \
    && Rscript -e "lapply(strsplit(readLines('r-requirements.txt'), split = '=='), function(pkg) remotes::install_version(pkg[[1]], version = pkg[[2]]))"

CMD [ "/usr/bin/bash" ]