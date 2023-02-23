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

  