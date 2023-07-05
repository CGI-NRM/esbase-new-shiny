FROM rocker/shiny-verse:4.3.0

EXPOSE 3838

RUN Rscript -e 'install.packages("remotes")' && \
    Rscript -e 'remotes::install_version("shinyBS", upgrade="never", version="0.61.1")' && \
    Rscript -e 'remotes::install_version("shinyjs", upgrade="never", version="2.1.0")' && \
    Rscript -e 'remotes::install_version("DT", upgrade="never", version="0.28")' && \
    Rscript -e 'remotes::install_version("rhandsontable", upgrade="never", version="0.3.8")' && \
    Rscript -e 'remotes::install_version("tinytex", upgrade="never", version="0.45")' && \
    Rscript -e 'remotes::install_version("knitr", upgrade="never", version="1.43")' && \
    Rscript -e 'remotes::install_version("kableExtra", upgrade="never", version="1.3.4")' && \
    Rscript -e 'remotes::install_version("logging", upgrade="never", version="0.10.108")' && \
    Rscript -e 'remotes::install_version("rlang", upgrade="never", version="1.1.1")' && \
    Rscript -e 'remotes::install_version("fastmap", upgrade="never", version="1.1.1")'

# shiny-verse includes the tidyverse which includes tibble,
#    Rscript -e 'remotes::install_version("tibble", upgrade="never", version="3.2.1")' && \
#    Rscript -e 'remotes::install_version("dplyr", upgrade="never", version="1.1.2")' && \
#    Rscript -e 'remotes::install_version("stringr", upgrade="never", version="1.5.0")' && \

RUN Rscript -e 'print(3)' && \
    Rscript -e 'tinytex::install_tinytex()' && \
    Rscript -e 'tinytex::tlmgr_install(c("booktabs", "multirow", "wrapfig", "float", "colortbl", "pdflscape", "tabu", "threeparttable", "threeparttablex", "ulem", "makecell", "xcolor", "trimspaces", "environ", "adjustbox", "fancyhdr", "datetime2"))'

COPY ./.esbaser_cache_break_timestam[p] /.esbaser_cache_break_timestamp

RUN Rscript -e 'remotes::install_github("cgi-nrm/esbaser")'

COPY ./R /
COPY ./www /www

CMD R -e 'shiny::runApp("/app.R", port = 3838, host = "0.0.0.0")'
