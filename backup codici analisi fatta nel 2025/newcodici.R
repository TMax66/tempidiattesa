# analisi dati

df <- read_fst(here("dati", "dtlongClean.fst")) # <-  dataset preparato dalla query principale ( vedi preparazione dati.r)
 







dftaut <- df[tempi == "taut",] %>%   
  .[, c("Numero", "settore", "giorni", "finprova", "in_out",  "repacc")] %>%   
  .[, distinct(.)] %>%  
  .[, `:=`(count = .N), by = Numero] %>% 
  .[,  fin := ifelse(count >1, "multifin", "onefin")] %>% 
  .[, c("Numero", "repacc", "settore", "finprova", "in_out", "giorni")]
  





library(plotrix)


dftaut %>% 
  mutate(duplicati = duplicated(Numero, .keep_all = TRUE)) %>%    
  pivot_wider(names_from = in_out, values_from = in_out) %>% View()
  
  mutate(in_out = ifelse(!is.na(`in`) & is.na(out), "in",
                         ifelse(is.na(`in`) & !is.na(out),"out", "out"))) %>%   View()
  


  distinct(Numero, .keep_all = TRUE) %>%   
  select(-finprova) %>%  View()
  group_by(repacc, settore, in_out) %>% 
  summarise(taut = mean(giorni), 
            se = std.error(giorni, na.rm = TRUE), 
            inf = taut - se*1.97, 
            sup = taut + se*1.97) %>% View()
  
  ggplot()+
  aes(x = repacc, y = taut, col = in_out)
    geom_pointrange(aes(ymin = inf, ymax = sup))+
    coord_flip()+
    facet_wrap(.~ settore, scales = "free")
 
  
# dtw <- dftaut %>% 
#   pivot_wider(names_from = finprova, values_from = giorni, values_fill=0) %>%   
#   select(-c(2:5)) %>% View()
#   mutate(numero = paste0("nconf/", numero)) %>% 
#   as.data.table()
  
  


dtw[, max:= do.call(pmax, .SD)]

dtw %>%  select(numero, max) %>% 
  left_join(
    dftaut %>% 
      pivot_wider(names_from = finprova, values_from = giorni, values_fill=0) %>% 
      select(1:4) %>% 
      mutate(numero = as.character(numero)) , by = "numero"
  ) %>%  View()
  
  











# Descrittiva tempi di registrazione
treg <- taut[tempi == "treg",] %>%
  .[, c("numero", "settore", "giorni", "finalita", "repacc")] %>% 
  .[, distinct(.)] %>%  
  .[, `:=`(count = .N), by = numero] %>% 
  .[,  fin := ifelse(count >1, "multifin", "onefin")]  



write_feather(treg, "treg.feather")

treg %>% as.data.frame() %>% 
  group_by(repacc, settore, finalita, fin) %>% 
  summarise(
    n = n(),
    min = min(giorni), 
    p25 = quantile(giorni, 0.25),
    mediana = median(giorni), 
    p75 = quantile(giorni, 0.75),
    p90 = quantile(giorni, 0.90),
    p95 = quantile(giorni, 0.95),
    media = mean(giorni),
    max = max(giorni)
  ) -> STtreg

write_feather(STtreg, "STtreg.feather")


STtreg %>% 
  ggplot()+
  aes(x = mediana, col=fin)+
  xlim(0, 10)+
  geom_boxplot()
  



# Descrittiva dei tempi di attesa
# 1. filtro i record corrispondenti solo al taut nella colonna tempi

taut <- taut[tempi == "taut",] %>%
  .[, c("numero", "settore", "giorni", "finalita", "repacc")] %>% 
  .[, distinct(.)] %>%  
  .[, `:=`(count = .N), by = numero] %>% 
  .[,  fin := ifelse(count >1, "multifin", "onefin")]  
write_feather(taut, "tempidiattesa.feather")


taut %>% as.data.frame() %>% 
  group_by(repacc, settore, finalita, fin) %>% 
  summarise(
    n = n(),
    min = min(giorni), 
    p25 = quantile(giorni, 0.25),
    mediana = median(giorni), 
    p75 = quantile(giorni, 0.75),
    p90 = quantile(giorni, 0.90),
    p95 = quantile(giorni, 0.95),
    media = mean(giorni),
    max = max(giorni)
  ) -> summaryTableTaut

write_feather(summaryTableTaut, "sumTaut.feather")



#segmentazione taut

library(rsample)     # data splitting 
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)    

Sys.setlocale("LC_TIME", "English")
taut <- read_feather("dtwide.feather")

tree <- taut %>% 
  mutate(dconf = weekdays(dtconf), 
         dstart = weekdays(dtinizio))  

m1 <- rpart(
  formula = taut ~ settore+dconf+dstart+
    categprove+ NrCampioni+in_out+treg+ 
    tprog+ tprep+ttrasf+ tcircint+tprogr2+ttecnico+trisp,
  data    = tree,
  method  = "anova"
)
  
 
treeS <- tree %>% filter(repacc == "Sede Territoriale di Parma")
  
m1 <- rpart(
  formula = taut ~ in_out + tcircint,
  data    = treeS,
  method  = "anova"
)
  
rpart.plot(m1)  
  

#survivor curve----

plot(survfit(Surv(taut$giorni)~taut$settore))



#...altri tempi uso dtwide con le colonne per ogni tempo



































































# questo codice calcola per ogni prova i tempi di firma, di risposta e il tempo di attesa e considera se la prova
# è eseguita  nella struttura accettante o se è eseguita in altra struttura....

# tempi <- function(dati, prova)
# {
#   dati %>% 
#     filter(prova == prova) %>% 
#     mutate(
#       in_out = case_when(
#         stracc != repprova ~ paste("out"), 
#         TRUE ~ paste("in")), 
#       taut = (Data_RDP-Data)/86400,
#       trisp = case_when(
#         in_out == "in" ~ (Data_Fine_Analisi-Data_Accettazione)/86400,
#         TRUE~  ((Data_Fine_Analisi- Data_Carico)/86400)
#       ), 
#       tfirma = (Data_RDP-Data_Fine_Analisi), 
#       torg =  case_when(
#         in_out == "in" ~ (Data_Inizio_Analisi-Data_Accettazione)/86400,
#         TRUE~  ((Data_Inizio_Analisi- Data_Carico)/86400)
#       )
#     ) %>% 
#     filter(tfirma >= 0)
# }

# 
# 
# setkey(dtlong %>% as.data.table(), tempi)
# 
# dtlong2 <- merge(dtlong, stats, all.x = T)


# dtlongClean <- dtlong %>% as_tibble() %>% 
#   mutate("tetto" = ifelse(tempi == st$tempi, st$times, "X")) %>%  
#   filter(!is.na(giorni)| giorni >= 0, 
#          giorni <= tetto)

