pkg()


#dt <- read.csv("C:/Users/vito.tranquillo/Desktop/Git Projects/tempidiattesa/dati/df_final_all.csv")
dt <- read.csv("C:/Users/vito.tranquillo/Desktop/Git Projects/tempidiattesa/dati/df_processed_all.csv")



dt %>% 
  select(conferimento_univoco, 
         anno_conferimento, 
         repprop, 
         repacc, 
         repanalisi, 
         Tipoprel, 
         tipoconf, unifinalita, in_out, dtconf_day, tempo_attesa, tempo_attesa_feriali, tempo_attesa_festivi)->dt_plot



#andamento del tempo di attesa delle richieste conferite alla ST Bergamo
#


dt %>% 
  filter(repprop== "Sede Territoriale di Bergamo") %>% 
  mutate(unifinalita = recode(unifinalita, 
                              True = "conf con una finalità", 
                              False = "conf con finalità multipla")) %>% 
  
  ggplot()+
  aes(x = anno_conferimento, y = tempo_attesa, group = anno_conferimento)+
  facet_grid(in_out~unifinalita )+
  geom_boxplot()+
 )


dt_plot %>% 
  filter(unifinalita == "False") %>% 
  mutate(unifinalita = recode(unifinalita, 
                              True = "conf con una finalità", 
                              False = "conf con finalità multipla")) %>% 
   
  
  ggplot()+
  aes(x = anno_conferimento, y = tempo_attesa, group = anno_conferimento)+
  facet_grid(in_out~repprop )+
  geom_boxplot()
 


###################################################################################################

# tempo di attesa ( solo giorni feriali)

library(flextable)

dt %>% 
  select(conferimento_univoco, 
         anno_conferimento, 
         settore, 
         Tipoprel,
         repprop, 
         repacc, 
         repanalisi, 
         Tipoprel, 
         tipoconf, 
         unifinalita, 
         in_sede, 
         dtconf_day, 
         tempo_attesa, 
         tempo_attesa_feriali, 
         tempo_attesa_festivi) -> dt



dt %>% 
  filter(#unifinalita == "TRUE", 
    anno_conferimento >= 2019,
         in_sede == "in", 
         str_detect(repacc, "Sede"), 
         repacc != "Sede Territoriale di Milano (IS)") %>%  
  group_by(anno_conferimento, repacc) %>% 
  summarise( 
    media_taut = round(mean(tempo_attesa_feriali, na.rm = T), 1),
    stdev = round(sd(tempo_attesa_feriali, na.rm = T), 1),
    min = min(tempo_attesa_feriali, na.rm = TRUE), 
    max = max(tempo_attesa_feriali, na.rm = TRUE), 
    mediana = median(tempo_attesa_feriali, na.rm= TRUE), 
      nconf = n()
    ) %>%   
  mutate("media(sd) - mediana" = paste(media_taut, "(", stdev, ")", "-", mediana)) %>%    
   
  select(anno_conferimento, repacc, mediana) %>% 
  pivot_wider(names_from = anno_conferimento, values_from = mediana ) %>%  
 
  
  flextable() %>% 
  colformat_double(digits = 1)



  hline( i = 2) %>% 
  merge_v(j = ~ in_out) %>% 
  add_footer_lines("in = conferimenti con campioni eseguiti nella struttura accettante" ) %>% 
  add_footer_lines("out = conferimenti con almeno un campione inviato ad altri laboratori")



dt %>% 
  select(conferimento_univoco, 
         anno_conferimento, 
         repprop, 
         repacc, 
         repanalisi, 
         Tipoprel, 
         tipoconf, unifinalita, in_out, dtconf_day, tempo_attesa, tempo_attesa_feriali, tempo_attesa_festivi) %>% 
  filter(in_out == "in", 
         unifinalita == "False")   










dt %>%
    filter( dtconf_day != "Sunday",
           anno_conferimento >= 2019) %>%
    mutate(tg= ifelse(in_out == "in", 2.19, 7.5 ),
           in_tg = ifelse(tempo_attesa_feriali <= tg, 1, 0)) %>%
    group_by(in_out) %>%
      summarise(tg_ris = sum(in_tg),
                n = n()) %>%
    mutate("%" = round(100*tg_ris/n, 1))





#### - uso della funzione di sopravvivenza come descrittore del tempo di attesa
#### 

library(survival)  
dtx <- dt %>% 
  filter(anno_conferimento == 2024,
         in_sede == "in") 



plot(survfit(Surv(dtx$tempo_attesa_feriali)~1))
