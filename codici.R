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

##questo codice permette di fleggare il conferimento come conferimento con prove inviate ad altri lab o conferimento
 ## con prove eseguite tutte nel laboratorio di conferimento del campione, nel primo caso è sufficiente che ci sia una sola prova eseguita
  ## in un laboratorio differente da quello che ha ricevuto il conferimento dal proprietario per essere fleggato come confeirmento con altri lab


altrilab <- dt %>% 
  mutate(daescl = is.na(dt$Data_RDP), 
         altrilab = ifelse(is.na(Data_Invio), 0, 1)) %>%  
  group_by(nconf) %>% 
  summarise(altrilab = sum(altrilab, na.rm = TRUE)) %>% 
  filter(altrilab>=1) %>% 
  distinct(nconf) %>% 
  mutate(Altrilab = "conferimento con altri lab")  


dt %>% 
  left_join(altrilab, by="nconf") %>%  
  filter(is.na(Altrilab)) %>%   
  # mutate(daescl = is.na(dt$Data_RDP)) %>%  View()
  # filter(daescl == FALSE) %>%  View()
  mutate(taut = (Data_RDP-Data)/86400) %>%  
  group_by(nconf,   Finalita, taut) %>% 
  summarise(Esami = sum(Tot_Eseguiti, na.rm = TRUE)) %>% 
  group_by(Finalita) %>% 
  summarise(n=n(), 
            Esami = sum(Esami), 
            TAUT = quantile(taut,probs = 0.50, na.rm= TRUE)) %>%  View()
  
  


  




  
  


# summarise(n = n(), 
  #         
  #           taut = (Data_RDP-Data)/86400)  %>%  
  #count() %>% 
  #filter(daescl == FALSE) %>% 
    
  distinct(nconf, .keep_all = TRUE) %>%
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
  
