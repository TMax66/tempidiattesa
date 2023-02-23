dt %>% 
  left_join(altrilab, by="nconf") %>%  ## questo join marca il conferimento come conferimento con altri laboratori o senza.
  mutate(Altrilab = ifelse(is.na(Altrilab), "No altri lab", "Altri Lab" )) %>%
  select(nconf, Altrilab, stracc, repprova) %>% 
  unique() %>%  
  group_by(stracc, Altrilab) %>% 
  count() %>% 
  pivot_wider(names_from = "Altrilab", values_from = "n")  
  
  
  dt %>% 
  left_join(altrilab, by="nconf") %>%  ## questo join marca il conferimento come conferimento con altri laboratori o senza.
  mutate(Altrilab = ifelse(is.na(Altrilab), "No altri lab", "Altri Lab" )) %>%
    #filter(Altrilab == "No altri lab") %>% 
  select(nconf,stracc, repprova, finalita, Altrilab) %>% 
  unique() %>% 
  # group_by(stracc, repprova) %>% 
  # count() %>% 
    pivot_wider(names_from = "repprova", values_from = "repprova") %>%  View()
    

  