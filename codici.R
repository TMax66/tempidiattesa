library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(DBI)
library(odbc)


#connessione al dbase dei tempi di risposta----

con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02", 
                         Database = "TempiDiRisposta", Port = 1433)

