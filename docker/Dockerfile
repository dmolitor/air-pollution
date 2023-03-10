FROM rocker/r-ver:4.2.2

ARG TARGETARCH
ARG QUARTO_VERSION="1.2.335"
ARG PANDOC_VERSION="3.1.1"

COPY r-requirements.txt ./

RUN apt-get -y update \
    && apt-get install -y libcurl4-openssl-dev libudunits2-dev libgdal-dev libgeos-dev libproj-dev libnetcdf-dev
RUN if [ $TARGETARCH = "arm64" ] ; \
    then apt-get -y dist-upgrade \
        && apt-get install -y git npm wget \
        && apt-get -y update \
        && npm install -g sass \
        && wget https://github.com/jgm/pandoc/releases/download/$PANDOC_VERSION/pandoc-$PANDOC_VERSION-1-arm64.deb \
        && dpkg -i pandoc-$PANDOC_VERSION-1-arm64.deb \
        && rm pandoc-$PANDOC_VERSION-1-arm64.deb \
        && cd /usr/local/src \
        && git clone --depth 1 https://github.com/quarto-dev/quarto-cli --branch v$QUARTO_VERSION \
        && cd quarto-cli \
        && sed -i 's/https:\/\/github.com\/denoland\/deno\/releases\/download/https:\/\/github.com\/LukeChannings\/deno-arm64\/releases\/download/' configure.sh \
        && sed -i 's/DENOFILES=deno-x86_64-unknown-linux-gnu.zip/DENOFILES=deno-linux-arm64.zip/' package/scripts/common/utils.sh \
        && sed -i 's/DENO_DIR=deno-x86_64-unknown-linux-gnu/DENO_DIR=deno-linux-arm64/' package/scripts/common/utils.sh \
        && ./configure.sh \
        && mkdir package/dist/bin/tools/deno-x86_64-unknown-linux-gnu/ \
        && ln -s /usr/local/src/quarto-cli/package/dist/bin/tools/deno-linux-arm64/deno package/dist/bin/tools/deno-x86_64-unknown-linux-gnu/deno \
        && rm package/dist/bin/tools/pandoc \
        && ln -s /usr/bin/pandoc package/dist/bin/tools/pandoc \
        && rm package/dist/bin/tools/dart-sass/sass \
        && ln -s /usr/local/bin/sass package/dist/bin/tools/dart-sass/sass ; \
    else apt-get install -y curl gdebi libcurl4-openssl-dev \
        && curl -LO https://quarto.org/download/latest/quarto-linux-amd64.deb \
        && gdebi --non-interactive quarto-linux-amd64.deb ; \
    fi
RUN Rscript -e "install.packages('remotes')" \
    && Rscript -e "lapply(strsplit(readLines('r-requirements.txt'), split = '=='), function(pkg) remotes::install_version(pkg[[1]], version = pkg[[2]]))"

CMD [ "/usr/bin/bash" ]