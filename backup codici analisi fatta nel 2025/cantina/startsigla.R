library(tidyverse)
library(readr)
library(readxl)
library(openxlsx)
library(here)
library(lubridate)

dt <- readRDS("taut.RDS")
fin <- readRDS("fin.RDS")

finalita <- fin %>% 
  mutate(Descrizione =  sapply(Descrizione, iconv, from = "latin1", to = "UTF-8", sub = ""))
  



fin <- fin %>% 
  pivot_wider(names_from = "Descrizione", values_from = "Descrizione") 

finalita <- fin %>% 
  unite("finalita", 3:167, na.rm = TRUE, remove = FALSE) %>% 
  mutate(multiF =  rowSums(!is.na(select(., -Numero)))-1) %>%   
  select(nconf = Numero, finalita, multiF)
saveRDS(finalita, file ="finalita.RDS")
  



dt %>% 
  left_join(altrilab, by="nconf") %>% View()  ## questo join marca il conferimento come conferimento con altri laboratori o senza.
  mutate(Altrilab = ifelse(is.na(Altrilab), "No altri lab", "Altri Lab" )) %>%
  select(nconf, Altrilab, stracc, repprova) %>% 
  unique() %>%  
  group_by(stracc, Altrilab) %>% 
  count() %>% 
  pivot_wider(names_from = "Altrilab", values_from = "n") 
  
  dt %>% 
  left_join(altrilab, by="nconf") %>%  ## questo join marca il conferimento come conferimento con altri laboratori o senza.
  #mutate(Altrilab = ifelse(altrilab == 0, "No altri lab", "Altri Lab" )) %>% View()
    #filter(Altrilab == "No altri lab") %>% 
  select(nconf,stracc, repprova, finalita, Altrilab) %>% 
  unique() %>% 
  # group_by(stracc, repprova) %>% 
  # count() %>% 
    pivot_wider(names_from = "repprova", values_from = "repprova") %>%  
    write.xlsx(file = "network.xlsx")




#, values_fill = "0")

  