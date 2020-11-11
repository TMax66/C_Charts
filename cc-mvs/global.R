library("shiny")
library("datasets")
library("qicharts2")
library("tidyverse")
library("plotly")
library("googledrive")
# library(dplyr)
library("lubridate")
library("shinycssloaders")
library("shinyjs")

###IL CODICE SEGUENTE SI FA SOLO LA PRIMA VOLTA CHE SI ACCEDE AL DRIVE DI GOOGLE
# options(gargle_oauth_cache = ".secrets")
# gargle::gargle_oauth_cache()
# drive_auth()
# list.files(".secrets/")

# options(
#   gargle_oauth_cache = ".secrets",
#   gargle_oauth_email = TRUE
# )
# drive_auth()
# sheets_auth(token = drive_token())
#   mydrive<-drive_find(type = "spreadsheet") 
# id<-mydrive[1,2]
# dati<-read_sheet(id$id)


