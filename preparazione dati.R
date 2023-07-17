library(tidyverse)
library(readr)
library(readxl)
library(openxlsx)
library(here)
library(lubridate)
library(DBI)
library(odbc)
library(survival)
library(data.table)

#connessione al dbase dei tempi di risposta----

# con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02", 
#                          Database = "TempiDiRisposta", Port = 1433)



con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02", 
                      Database = "IZSLER", Port = 1433)
source("sql.R")


taut <- con %>% tbl(sql(queryTaut)) %>% as_tibble()


#saveRDS(taut, file = "taut.RDS")
write_feather(taut, "taut.feather")


dati <- as.data.frame(read_feather("taut.feather"))


# Questo codice serve per la simulazione dei centri di costo in /COGEPERF/app/app_costiricavi/R/Nuovo Controllo di Gestione
# BGSOBIPV <- taut %>%
#   filter(repacc %in% c( "Sede Territoriale di Bergamo",
#                                "Sede Territoriale di Sondrio",
#                                "Sede Territoriale di Binago",
#                                "Sede Territoriale di Pavia")) %>%
#   select(numero, settore, finalita, finprova, categprove, gruppo, prova, Tecnica,
#          mp, Codice_Gruppo, Codice_Prova,Codice_Tecnica,Chiave, Codgruppo,COdprovaNMgruppi,
#          CodTecnicaNMgruppi,ChiaveGruppi,
#          repanalisi, labanalisi, nesami) %>% 
#   saveRDS("bgsobipv.rds")



#datix <- readRDS(file = "taut.RDS" )


#questo codice definisce la singola prova/tecnica come out se è eseguita da un laboratorio di una struttura diversa dall'accettante
# e come in se è eseguita in laboratori della stessa struttura

dt <- dati %>% 
  select(-c(31:39) ) %>% 
  mutate(
    in_out = case_when(
      repacc != repanalisi ~ paste("out"), 
      TRUE ~ paste("in"))) %>% 
  mutate( # questo codice calcola il tempo di risposta condizionalmente al categoria in/out
    #cosi come previsto dal sistema in uso
    trisp = case_when(
      in_out == "in" ~ interval(dtreg, dtfine)/ddays(1), 
      TRUE~  (interval(dtcarico, dtfine)/ddays(1))
    )) 




# stracc2= abbreviate(stracc))  

#saveRDS(dt, "dt.RDS")
write_feather(dt, "dt.feather")

#dt <- readRDS("dt.RDS")
dt <- read_feather("dt.feather")


dtwide <- dt %>% 
  mutate(
    tcircest = interval(dtprel, dtconf)/ddays(1), 
    treg = interval(dtconf, dtreg)/ddays(1), 
    tprog = interval(dtreg, dtprog)/ddays(1), 
    tprep = interval(dtconf, dtinvio)/ddays(1), 
    ttrasf = interval(dtinvio, dtcarico)/ddays(1),
    tcircint = interval(dtconf, dtinizio)/ddays(1),
    tprogr2 = interval(dtcarico, dtinizio)/ddays(1),
    ttecnico = interval(dtinizio, dtfine)/ddays(1), 
    tsecuzione = interval(dtreg, dtfine)/ddays(1), 
    tesecuzionalt = interval(dtcarico, dtfine)/ddays(1),
    taut = interval(dtconf, primoRdp)/ddays(1)
  ) %>% 
  write_feather("dtwide.feather")



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
    tsecuzione = interval(dtreg, dtfine)/ddays(1), 
    tesecuzionalt = interval(dtcarico, dtfine)/ddays(1),
    taut = interval(dtconf, primoRdp)/ddays(1)
  ) %>%  
  pivot_longer(cols = 32:43, names_to = "tempi", values_to = "giorni") %>%  
  filter(giorni >= 0) %>% 
  as.data.table()

write_feather(dtlong,  "dtlong.feather")

dtlong <- read_feather("dtlong.feather")


# questo codice calcola le statistiche di tutte le tipologie di tempi
stats <- 
  dtlong %>% as.data.table %>% 
  .[, list(
    "min"         = min(giorni, na.rm = T),
    "25pc" = quantile(giorni, 0.25, na.rm = T),
    "median"      = median(giorni, na.rm = T),
    "75pc" = quantile(giorni, 0.75, na.rm = T),
    "95pc" = quantile(giorni, 0.95, na.rm = T),
    "98pc" = quantile(giorni, 0.98, na.rm = T),
    "max"         = max(giorni, na.rm = T),
    "mean"        = mean(giorni, na.rm = T),                 
    "sd"          = sd(giorni, na.rm = T)), by = tempi]    

#saveRDS(stats, file = "statisticheTempi.RDS")
write_feather(stats, "stats.feather")

stats <- read_feather("stats.feather")


st <- stats %>% 
  pivot_longer(cols = 2:9, names_to = "stats", values_to = "times") %>%
  filter(stats == "98pc") %>%
  select(tempi,times) #seleziona solo i valori di tempi corrispondenti al 98 percentile


dtlongClean <- dtlong %>% as.data.table() %>% 
  .[, tetto := ifelse(tempi == st$tempi, st$times, "X")] %>% 
  .[!is.na(giorni)|giorni >= 0 & giorni <= tetto,  ] # questo codice definisce per tutti i tempi il valore di tetto ( dato dal 
# 98 percentile sopra selezionato), da usare come filtro per tagliare fuori dalle analisi valori estremi e rari

write_feather(dtlongClean, "dtlongClean.feather")


