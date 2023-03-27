library(tidyverse)
library(readr)
library(readxl)
library(openxlsx)
library(here)
library(lubridate)
library(DBI)
library(odbc)


#connessione al dbase dei tempi di risposta----

# con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02", 
#                          Database = "TempiDiRisposta", Port = 1433)



con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02", 
                      Database = "IZSLER", Port = 1433)
source("sql.R")


taut <- con %>% tbl(sql(queryTaut)) %>% as_tibble() 
#saveRDS(taut, file = "taut.RDS")

dati <- readRDS(file = "taut.RDS" )






# questo codice calcola per ogni prova i tempi di firma, di risposta e il tempo di attesa e considera se la prova
# è eseguita  nella struttura accettante o se è eseguita in altra struttura....

tempi <- function(dati, prova)
{
  dati %>% 
    filter(prova == prova) %>% 
    mutate(
      in_out = case_when(
        stracc != repprova ~ paste("out"), 
        TRUE ~ paste("in")), 
      
      taut = (Data_RDP-Data)/86400,
      
      trisp = case_when(
        in_out == "in" ~ (Data_Fine_Analisi-Data_Accettazione)/86400,
        TRUE~  ((Data_Fine_Analisi- Data_Carico)/86400)
      ), 
      tfirma = (Data_RDP-Data_Fine_Analisi), 
      
      torg =  case_when(
        in_out == "in" ~ (Data_Inizio_Analisi-Data_Accettazione)/86400,
        TRUE~  ((Data_Inizio_Analisi- Data_Carico)/86400)
      )
      
      
    ) %>% 
    filter(tfirma >= 0)
  
  
}

tempi(dati, prova = "Grasso") %>% 
  group_by(in_out) %>% 
  summarise(Mtaut = mean(taut), 
            Mtrisp = mean(trisp, na.rm = TRUE), 
            Mtorg = mean(torg, na.rm = TRUE))






# tempi <- taut %>% 
#   filter(prova == "Malattia di Aujeszky gE: anticorpi") %>% 
#   mutate(
#     in_out = case_when(
#     stracc != repprova ~ paste("out"), 
#     TRUE ~ paste("in")), 
#     
#     taut = (Data_RDP-Data)/86400,
#     
#     trisp = case_when(
#       in_out == "in" ~ (Data_Fine_Analisi-Data_Accettazione)/86400,
#       TRUE~  ((Data_Fine_Analisi- Data_Carico)/86400)
#     ), 
#     tfirma = (Data_RDP-Data_Fine_Analisi)
#     ) %>% 
#   filter(tfirma >= 0)
#   
# 
# 
# 
# 
# tempi %>% 
#   group_by(in_out) %>% 
#   summarise(Mtaut = mean(taut), 
#             Mtrisp = mean(trisp, na.rm = TRUE))
#  
#     
#     
    
