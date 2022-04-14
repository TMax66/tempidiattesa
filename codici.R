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


taut <- taut %>% mutate(nconf = paste(nconf, year(Data_Accettazione)), 
                    stracc = iconv(stracc, to='ASCII//TRANSLIT'), 
                    strapp = iconv(strapp, to='ASCII//TRANSLIT'), 
                    # Finalita = iconv(Finalita, to='ASCII//TRANSLIT'), 
                    repprova = iconv(repprova, to='ASCII//TRANSLIT'), 
                    Laboratorio = iconv(Laboratorio, to='ASCII//TRANSLIT'))

saveRDS(taut, file = "taut.RDS")
dt <- readRDS("taut.RDS")



fin <- con %>% tbl(sql(queryFin)) %>% as_tibble() 


fin <- fin %>% 
  pivot_wider(names_from = "Descrizione", values_from = "Descrizione") 

finalita <- fin %>% 
  unite("finalita", 2:173, na.rm = TRUE, remove = FALSE) %>% 
  mutate(multiF =  rowSums(!is.na(select(., -Numero)))-1) %>%   
  select(nconf = Numero, finalita, multiF) 

saveRDS(finalita, file ="finalita.RDS")


dt <- dt %>% 
  left_join(finalita, by = "nconf")    



# library(tidyverse)
# 
# dt %>% 
#   filter(nconf == "1053 2021") %>% 
#   group_by(nconf,Finalita,prova, Data_Prelievo, Data, stracc, strapp, Data_Accettazione, 
#            Data_Invio, Data_Carico, Data_Inizio_Analisi, Data_Fine_Analisi, DataOra_Primo_RDP_Completo_Firmato,
#   ) %>% 
#   #select(nconf, Finalita, prova) %>% 
#   count() %>% View()






##questo codice permette di fleggare il conferimento come conferimento con prove inviate ad altri lab o conferimento
 ## con prove eseguite tutte nel laboratorio di conferimento del campione, nel primo caso è sufficiente che ci sia una sola prova eseguita
  ## in un laboratorio differente da quello che ha ricevuto il conferimento dal proprietario per essere fleggato come conferirmento con altri lab


altrilab <- dt %>% 
  mutate(daescl = is.na(dt$Data_RDP), 
         altrilab = ifelse(stracc == repprova, 0,1))%>%   
  group_by(nconf) %>% 
  summarise(altrilab = sum(altrilab, na.rm = TRUE)) %>% 
  filter(altrilab>=1) %>% 
  distinct(nconf) %>% 
  mutate(Altrilab = "conferimento con altri lab")  


# conf <- dt %>% 
#   left_join(altrilab, by="nconf") %>%  ## questo join marca il conferimento come conferimento con altri laboratori o senza.
#   mutate(Altrilab = ifelse(is.na(Altrilab), "No altri lab", "Altri Lab" )) 


dt %>% 
  left_join(altrilab, by="nconf") %>%  ## questo join marca il conferimento come conferimento con altri laboratori o senza.
  mutate(Altrilab = ifelse(is.na(Altrilab), "No altri lab", "Altri Lab" )) %>%  

  # mutate(daescl = is.na(dt$Data_RDP)) %>%  View()
  # filter(daescl == FALSE) %>%  View()
  mutate(taut = (Data_RDP-Data)/86400) %>%  
  group_by(nconf,   finalita, Altrilab, multiF,  repprova, laboratorio, taut ) %>% 
  summarise(Esami = sum(Tot_Eseguiti, na.rm = TRUE)) %>%   
  group_by(finalita, Altrilab, multiF) %>% 
  summarise(n=n(), 
            Esami = sum(Esami), 
            mTAUT = min(taut, na.rm=TRUE), 
            "25perc" = quantile(taut,probs = 0.25, na.rm= TRUE), 
            MdTAUT = quantile(taut,probs = 0.50, na.rm= TRUE), 
            "75perc" = quantile(taut,probs = 0.75, na.rm= TRUE), 
            maxTAUT = max(taut, na.rm=TRUE)) %>% View()
  
  
# ###codice per cervap---
# dt %>% 
#   filter(reg == "Alimenti Uomo") %>% 
#   group_by(Finalita, nconf) %>% 
#   count() %>% 
#   group_by(Finalita) %>% 
#   count(Finalita) %>%  
#   filter( !str_detect(Finalita, "Progetto")) %>% 
#   write.xlsx(file = "finalitaAU.xlsx")
  
  



conf %>% 
  filter(Altrilab == "Altri Lab") %>%  View()
  


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
  



###finalità


