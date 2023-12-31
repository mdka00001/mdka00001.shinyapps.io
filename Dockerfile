FROM rocker/shiny:4.2.1

RUN R -e "install.packages(c('data.table','mixtox','ggplot2','gridExtra','shinyscreenshot','bayestestR','BAS','markdown','knitr','readr','stringr'))"
RUN install2.r rsconnect tibble dplyr stringr rtweet htmltools lubridate bslib reactable data.table mixtox ggplot2 gridExtra shinyscreenshot bayestestR BAS markdown knitr readr stringr

WORKDIR /home/mixtox
COPY ui.R ui.R 
COPY server.R server.R
COPY deploy.R deploy.R
COPY sample_input.txt sample_input.txt
COPY mix.txt mix.txt 
CMD Rscript deploy.R