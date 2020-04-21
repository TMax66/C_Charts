library(shiny)
library(datasets)
library(qicharts2)
library(ggplot2)
library(plotly)
library(googlesheets)
library(dplyr)
library(lubridate)
library(shinycssloaders)
library(shinyjs)

########################
#token <- gs_auth(cache = FALSE)
#gd_token()
# saveRDS(token, file = "googlesheets_token.rds")
gs_auth(token = "googlesheets_token.rds")
suppressMessages(gs_auth(token = "googlesheets_token.rds", verbose = FALSE))

ccsiero <-gs_title("vasierologia")
ccmicro <-gs_title("ccvamicrobiologia")



