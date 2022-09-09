FROM rocker/r-ver:4.2.1

LABEL org.opencontainers.image.licenses="GPL-2.0-or-later" \
      org.opencontainers.image.source="https://github.com/rocker-org/rocker-versioned2" \
      org.opencontainers.image.vendor="IMSS" \
      org.opencontainers.image.authors="Rodrigo Zepeda <rodrigo.zepeda@imss.gob.mx>"

#Remove files from rstudio build
RUN rm -rf /rocker_scripts

#Add my own files
COPY /dbuild /rocker_scripts

RUN  chmod -R +x /rocker_scripts && \
     /rocker_scripts/install_tidyverse.sh
