R.version.string
packages_to_install <- c(
  "BAS", "MASS", "Matrix", "R6", "RColorBrewer", "Rcpp", "askpass", "base64enc",
  "bayestestR", "bit", "bit64", "bslib", "cachem", "cli", "clipr", "colorspace",
  "commonmark", "cpp11", "crayon", "curl", "data.table", "datawizard", "digest",
  "ellipsis", "evaluate", "fansi", "farver", "fastmap", "fontawesome", "fs",
  "ggplot2", "glue", "gridExtra", "gtable", "highr", "hms", "htmltools", "httpuv",
  "insight", "isoband", "jquerylib", "jsonlite", "knitr", "labeling", "later",
  "lattice", "lifecycle", "magrittr", "markdown", "memoise", "mgcv", "mime",
  "minpack.lm", "mixtox", "munsell", "nlme", "openssl", "packrat", "pillar",
  "pkgconfig", "prettyunits", "progress", "promises", "rappdirs", "readr", "renv",
  "rlang", "rstudioapi", "sass", "scales", "shiny", "shinyscreenshot",
  "sourcetools", "stringi", "stringr", "sys", "tibble", "tidyselect", "tzdb", "utf8",
  "uuid", "vctrs", "viridisLite", "vroom", "withr", "xfun", "xtable", "yaml"
)

install.packages("rsconnect", dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages(packages_to_install)
for (package in packages_to_install) {
  library(package, character.only = TRUE)
}

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
setAccountInfo(name = error_on_missing_name("SHINY_ACC_NAME"),
               token = error_on_missing_name("TOKEN"),
               secret = error_on_missing_name("SECRET"))
deployApp(appFiles = c("ui.R", "server.R"), forceUpdate=T)
