FROM rocker/shiny-verse
# COPY ./R /R
EXPOSE 3838
RUN Rscript -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("shiny", upgrade="never", version="1.7.4")'
RUN Rscript -e 'remotes::install_version("shinyBS", upgrade="never", version="0.61.1")'
RUN Rscript -e 'remotes::install_version("DT", upgrade="never", version="0.28")'

RUN Rscript -e 'remotes::install_github("cgi-nrm/esbaser")'

CMD R -e 'shiny::runApp("R/app.R", port = 3838, host = "0.0.0.0")'
