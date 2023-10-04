FROM rocker/shiny:4.2.1
# Install the installr package if not already installed
RUN R -e "if (!require(installr)) install.packages('installr')"

# Load the installr package
RUN R -e "library(installr)"

# Update R using the updateR() function
RUN R -e "updateR()"
RUN R -e "install.packages(c('rsconnect','data.table','mixtox','ggplot2','gridExtra','shinyscreenshot','bayestestR','BAS','markdown','knitr','readr','stringr'))"
RUN install2.r rsconnect tibble dplyr stringr rtweet htmltools lubridate bslib reactable data.table mixtox ggplot2 gridExtra shinyscreenshot bayestestR BAS markdown knitr readr stringr
WORKDIR /home/shinytweet
COPY ui.R ui.R 
COPY server.R server.R
COPY deploy.R deploy.R
CMD Rscript deploy.R