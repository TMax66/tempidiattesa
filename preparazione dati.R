# library(tidyverse)
# library(readr)
# library(readxl)
# library(openxlsx)
# library(here)
# library(lubridate)
# library(DBI)
# library(odbc)
# library(survival)
# library(data.table)
# library(fst)


pkg()
#connessione al dbase dei tempi di risposta----

# con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02", 
#                          Database = "TempiDiRisposta", Port = 1433)
# con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02", 
#                   Database = "IZSLER", Port = 1433)


 

source("sql.R")

conIZSLER <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "dbprod02.izsler.it",
                             Database = "IZSLER", Port = 1433)




taut <- conIZSLER %>% DBI::dbGetQuery( queryTaut)

write_fst(taut, here("dati", "datiperIF.fst"))
saveRDS(taut, here("dati", "datiperIF.RDS"))

#write.csv(taut, here("dati", "datiperIF.csv"))

dati <- readRDS(here("dati", "datiperIF.RDS"))

#saveRDS(taut, file = "taut.RDS")
write_fst(taut, here("dati",  "taut.fst"))


dati <- read_fst(  here("dati", "taut.fst"))


saveRDS(taut, here("dati", "datiperizslerNet.RDS"))

dati %>% 
  mutate(chiaveVN = NA, 
         prova = NA, 
         codprova = NA) %>%   
  filter(!is.na(chiaveVNgruppo), 
         tecnica != "Calcolo") %>%  
  distinct(chiaveVNgruppo, .keep_all = TRUE) %>%  
  
  bind_rows(
    dati %>% 
      filter(is.na(chiaveVNgruppo))
  ) %>%  
  mutate(vn = ifelse(is.na(chiaveVN), chiaveVNgruppo, chiaveVN)) -> dati
  
 # write_fst( here("dati", "analisi.fst"))


# questo codice definisce la singola prova/tecnica come "out" se è eseguita da un laboratorio di una 
# struttura diversa dall'accettante e come "in" se è eseguita in laboratori della stessa struttura

dt <- dati %>% 
 # select(-c(31:39) ) %>% 
  mutate(
    in_out = case_when(
      repacc != repanalisi ~ paste("out"), 
      TRUE ~ paste("in")))
  # mutate( # questo codice calcola il tempo di risposta condizionalmente al categoria in/out
  #   #cosi come previsto dal sistema in uso
  #   trisp = case_when(
  #     in_out == "in" ~ interval(dtreg, dtfine)/ddays(1), 
  #     TRUE~  (interval(dtcarico, dtfine)/ddays(1))
  #   )) 

#dt<-dt %>%  select(-trisp)

saveRDS(dt, here("dati", "dati_tempi_izsler.RDS"))

dt <- readRDS( here("dati", "dati_tempi_izsler.RDS"))




# stracc2= abbreviate(stracc))  

#saveRDS(dt, "dt.RDS")
write_fst(dt, here("dati",  "dt.fst"))

#dt <- readRDS("dt.RDS")
dt <- read_fst(here("dati",  "dt.fst"))


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
    taut = interval(dtconf, dtpRDP)/ddays(1)
  ) %>%  
  write_fst(here("dati","dtwide.fst"))



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
    taut = interval(dtconf, dtpRDP)/ddays(1)
  ) %>%   
  pivot_longer(cols = 30:41, names_to = "tempi", values_to = "giorni") %>%   
  filter(giorni >= 0) %>% 
  as.data.table()

write_fst(here("dati", "dtlong.fst"))

dtlong <- read_fst(here("dati", "dtlong.fst"))


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
write_fst(stats, here("dati","stats.fst"))

stats <- read_fst(here("dati", "stats.fst"))


st <- stats %>% 
  pivot_longer(cols = 2:9, names_to = "stats", values_to = "times") %>%
  filter(stats == "98pc") %>%
  select(tempi,times) #seleziona solo i valori di tempi corrispondenti al 98 percentile


dtlongClean <- dtlong %>% as.data.table() %>% 
  .[, tetto := ifelse(tempi == st$tempi, st$times, "X")] %>% 
  .[!is.na(giorni)|giorni >= 0 & giorni <= tetto,  ] # questo codice definisce per tutti i tempi il valore di tetto ( dato dal 
# 98 percentile sopra selezionato), da usare come filtro per tagliare fuori dalle analisi valori estremi e rari

write_fst(dtlongClean, here("dati","dtlongClean.fst"))


