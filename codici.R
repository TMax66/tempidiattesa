library(tidyverse)
library(readr)
library(readxl)
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

dt <- readRDS("taut.RDS")
rm(taut)




dt %>% 
  # mutate(daescl = is.na(dt$DataOra_Primo_RDP_Completo_Firmato)) %>% 
  distinct(nconf, .keep_all = TRUE) %>%
  group_by(Finalita) %>% 
  summarise(n = n(), 
            esami = sum(Tot_Eseguiti, na.rm = TRUE), 
            taut = mean((Data_RDP-Data)/86400))  %>%  View()
  #count() %>% 
  #filter(daescl == FALSE) %>% 
  arrange(desc(n)) %>% View()

dt %>% 
  distinct(nconf, .keep_all = TRUE) %>% View()



dt %>% 
  group_by(Finalita) %>% 
  count() %>% 
  arrange(desc(n))

dt %>% 
  # filter(repprova == "Sede Territoriale di Bergamo" &
  #          Finalita == "Piano Paratubercolosi") %>% 
  mutate(taut = (Data_RDP-Data)/86400) %>% 
  distinct(nconf,.keep_all = TRUE) %>% 
  filter(Finalita == "Diagnostica") %>% 
  group_by(repprova, giorno) %>%
  summarise(Mtaut= mean(taut, na.rm=TRUE), 
            n=n()) %>% 
  arrange(desc(n)) %>% View()

  
  
  
  # ggplot()+
  # aes(x=taut)+
  # geom_histogram()+
  # xlim(c(0, 25))
  
