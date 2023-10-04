
rsconnect::setAccountInfo(name = Sys.getenv("SHINY_ACC_NAME"),
               token = Sys.getenv("TOKEN"),
               secret = Sys.getenv("SECRET"))
deployApp(appFiles = c("ui.R", "server.R"))