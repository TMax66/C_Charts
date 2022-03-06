library("shiny")
library("datasets")
library("tidyverse")
library("plotly")
library("googledrive")
library("googlesheets4")
library("lubridate")
library("shinycssloaders")
library("shinyjs")
library(RSQLite)
#library(pool)
library(readxl)
library(openxlsx)
library(here)


#Creazione dbase SQLite
dbcchart <- dbPool(RSQLite::SQLite(), dbname = "dbcchart.sqlite")

dati <- data.frame( mmpp = character(),
                    data = character(),
                    piastra = character(),
                    lotto = character(),
                    operatore = character(),
                    ct1 = character(),
                    ct2 = character(),
                    stringsAsFactors = FALSE)


#Importazione dati storici da excel in SQLite

# storico <- read_excel(here("NUOVA VERSIONE CON RSQLight", "dati.xlsx"))

storico  <- read.csv( here("NUOVA VERSIONE CON RSQLight", "dati.csv"), sep=";")

storico$data <- dmy(storico$data)

dbWriteTable(dbcchart, "dati", dati, temporary = FALSE, overwrite = FALSE)
data <- dbReadTable(dbcchart, "dati")
quary <- sqlAppendTable(dbcchart, "dati", storico, row.names = FALSE)

dbExecute(dbcchart, quary)


dbGetQuery(dbcchart, "SELECT * FROM dati")















