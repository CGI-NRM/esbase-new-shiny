FROM rocker/shiny-verse:4.3.0

EXPOSE 3838

RUN Rscript -e 'install.packages("remotes")' && \
    Rscript -e 'remotes::install_version("shinyBS", upgrade="never", version="0.61.1")' && \
    Rscript -e 'remotes::install_version("DT", upgrade="never", version="0.28")' \
    Rscript -e 'remotes::install_version("stringr", upgrade="never", version="4.3.0")'

RUN Rscript -e 'remotes::install_github("cgi-nrm/esbaser")'

COPY ./R /
COPY ./www /www

CMD R -e 'shiny::runApp("/app.R", port = 3838, host = "0.0.0.0")'
