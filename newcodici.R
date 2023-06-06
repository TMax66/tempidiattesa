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


saveRDS(taut, file = "taut.RDS")

dati <- readRDS(file = "taut.RDS" )





# questo codice calcola per ogni prova i tempi di firma, di risposta e il tempo di attesa e considera se la prova
# è eseguita  nella struttura accettante o se è eseguita in altra struttura....

# tempi <- function(dati, prova)
# {
#   dati %>% 
#     filter(prova == prova) %>% 
#     mutate(
#       in_out = case_when(
#         stracc != repprova ~ paste("out"), 
#         TRUE ~ paste("in")), 
#       taut = (Data_RDP-Data)/86400,
#       trisp = case_when(
#         in_out == "in" ~ (Data_Fine_Analisi-Data_Accettazione)/86400,
#         TRUE~  ((Data_Fine_Analisi- Data_Carico)/86400)
#       ), 
#       tfirma = (Data_RDP-Data_Fine_Analisi), 
#       torg =  case_when(
#         in_out == "in" ~ (Data_Inizio_Analisi-Data_Accettazione)/86400,
#         TRUE~  ((Data_Inizio_Analisi- Data_Carico)/86400)
#       )
#     ) %>% 
#     filter(tfirma >= 0)
# }


 




dt <- dati %>% 
  select(-labanalisi) %>% 
  mutate(
    in_out = case_when(
      repacc != repanalisi ~ paste("out"), 
      TRUE ~ paste("in"))) %>% 
    mutate( 
      trisp = case_when(
        in_out == "in" ~ interval(dtreg, dtfine)/ddays(1), 
        TRUE~  (interval(dtcarico, dtfine)/ddays(1))
      ))
     # stracc2= abbreviate(stracc))  

saveRDS(dt, "dt.RDS")

dt <- readRDS("dt.RDS")


dtlong <- dt %>% 
  mutate(
    tcircest = interval(dtprel, dtconf)/ddays(1), 
    treg = interval(dtconf, dtreg)/ddays(1), 
    tprog = interval(dtreg, dtprog)/ddays(1), 
    tprep = interval(dtconf, dtinvio)/ddays(1), 
    ttrasf = interval(dtinvio, dtcarico)/ddays(1),
    tcircint = interval(dtconf, dtinizio)/ddays(1),
    tprogr2 = interval(dtcarico, dtinizio)/ddays(1),
    ttecnico = interval(dtinizio, dtfine)/ddays(1), 
    tesecuzione = interval(dtreg, dtfine)/ddays(1), 
    tesecuzionalt = interval(dtcarico, dtfine)/ddays(1),
    taut = interval(dtconf, primoRdp)/ddays(1)
  ) %>% 
  filter(tesecuzione >= 0, 
         tesecuzionalt >= 0) %>%   
  pivot_longer(cols = 26:37, names_to = "tempi", values_to = "giorni")





