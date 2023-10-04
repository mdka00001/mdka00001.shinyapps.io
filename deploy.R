# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); 
  require(installr)
} #load / install+load installr

updateR() 

install.packages("rsconnect", dependencies=TRUE, repos="https://cran.r-project.org/src/contrib/rsconnect_1.1.1.tar.gz")
library(rsconnect)

# a function to stop the script when one of the variables cannot be found
# and to strip quotation marks from the secrets when you supplied them
error_on_missing_name <- function(name) {
  var <- Sys.getenv(name, unset = NA)
  if(is.na(var)) {
    stop(paste0("cannot find ", name, " !"), call. = FALSE)
  }
  gsub("\"", "", var)
}

# Authenticate
rsconnect::setAccountInfo(name='mdka00001',
			  token='90B6D77B475D0DF86B202B32CB0C6837',
			  secret='k/PwfavfidHS0e8FtjAPje08upQU+PqQOY0LOj0o')
deployApp(appFiles = c("ui.R", "server.R"))