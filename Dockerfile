FROM rocker/r-ver:4.2.1

LABEL org.opencontainers.image.licenses="GPL-2.0-or-later" \
      org.opencontainers.image.source="https://github.com/RodrigoZepeda/covidmx-predict/" \
      org.opencontainers.image.vendor="IMSS" \
      org.opencontainers.image.authors="Rodrigo Zepeda <rodrigo.zepeda@imss.gob.mx>"

#Remove files from rstudio build
RUN rm -rf /rocker_scripts

#Add my own files
COPY /docker_dir version.txt main.R ./

#Install everything
RUN  chmod -R +x /rocker_scripts && \
     chmod -R +x main.R && \
     /rocker_scripts/install_tidyverse.sh

ENTRYPOINT ["Rscript","./main.R"]