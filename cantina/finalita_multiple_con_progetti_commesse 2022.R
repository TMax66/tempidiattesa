dtwide <- read_feather("dtwide.feather")

dtwide %>% # <- prendi i dati dal file preparazione dati nel progetto tempidiattesa
  select(numero, finalita, finprova) %>% 
  #distinct(numero) %>%  View()
  filter(str_detect(finalita, "Progetto")) %>% 
  mutate(confconprj = ifelse(finalita != finprova, "si", "no")) %>%  
  filter(confconprj == "si") %>%  
  unique() %>%  
  group_by(finalita) %>% 
  tally() %>% 
  write.xlsx(file = "prg.xlsx")


dtwide %>% 
  select(numero, finalita, finprova) %>% 
  #distinct(numero) %>%  View()
  filter(str_detect(finalita, "Commessa:")) %>% 
  mutate(confcomm = ifelse(finalita != finprova, "si", "no")) %>%  
  filter(confcomm == "si") %>%  
  unique() %>%  
  group_by(finalita) %>% 
  tally() %>% 
  write.xlsx(file = "comm.xlsx")

dtwide %>% # <- prendi i dati dal file preparazione dati nel progetto tempidiattesa
  select(numero, finalita, finprova) %>% 
  group_by(finalita, finprova) %>% 
  tally() %>%  View()
 