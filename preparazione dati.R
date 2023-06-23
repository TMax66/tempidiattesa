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
#datix <- readRDS(file = "taut.RDS" )

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
  pivot_longer(cols = 31:42, names_to = "tempi", values_to = "giorni") %>%  as.data.table()

write_feather(dtlong,  "dtlong.feather")

dtlong <- read_feather("dtlong.feather")

stats <- 
  dtlong %>% as.data.table %>% 
  .[, list(
    "min"         = min(giorni, na.rm = T),
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
  select(tempi,times) 


dtlongClean <- dtlong %>% as.data.table() %>% 
  .[, tetto := ifelse(tempi == st$tempi, st$times, "X")] %>% 
  .[!is.na(giorni)|giorni >= 0 & giorni <= tetto,  ]

write_feather(dtlongClean, "dtlongClean.feather")


