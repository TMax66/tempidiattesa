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


saveRDS(taut, file = "taut.RDS")

dati <- readRDS(file = "taut.RDS" )





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



dati <- dati %>%
  mutate(dtprel = as.Date(strftime(Data_Prelievo,format = "%Y-%m-%d")),
         dtconf = as.Date(strftime(dtconf, format = "%Y-%m-%d")),
         dtreg = as.Date(strftime(dtreg, format = "%Y-%m-%d" )),
         dtprog = as.Date(strftime(dtprog, format = "%Y-%m-%d" )),
         dtinvio = as.Date(strftime(Data_Invio, format = "%Y-%m-%d")),
         dtcarico = as.Date(strftime(Data_Carico, format = "%Y-%m-%d")),
         dtinizio = as.Date(strftime(Data_Inizio_Analisi, format = "%Y-%m-%d")),
         dtfine = as.Date(strftime(Data_Fine_Analisi, format = "%Y-%m-%d")),
         dt1rdp = as.Date(strftime(Data_Primo_RDP_Completo_Firmato, format = "%Y-%m-%d")))


dt <- dati %>% 
  select(numero = Numero, 
         finalita,
         dtprel = Data_Prelievo,
         dtconf,
         dtreg,
         dtprog,
         dtinvio, 
         dtcarico, 
         dtinizio,
         dtfine,  
         dt1rdp,  
         stracc = repacc,  
         repprop,
         repanalisi, 
         labanalisi, 
         categprove, 
         gruppo,
         prova,
         mp) %>% 
#filter(prova == "BHV1/Rinotracheite Infettiva Bovina:anticorpi verso gE del virus") %>% 
  mutate(
    in_out = case_when(
      stracc != repanalisi ~ paste("out"), 
      TRUE ~ paste("in"))) %>% 
    mutate( 
      trisp = case_when(
        in_out == "in" ~ (dtfine-dtreg),
        TRUE~  ((dtfine - dtcarico))
      ), 
      stracc2= abbreviate(stracc))  

saveRDS(dt, "dt.RDS")


network <- dt %>% 
  group_by(stracc, repanalisi) %>% 
  count() %>% 
  pivot_wider(names_from = "repanalisi", values_from = "n", values_fill = 0) %>% 
  #filter(finalita == "Controllo alimenti") %>% 
  ungroup() %>%  #select(stracc, `Sede Territoriale di Bergamo`) %>% 
  #select(- finalita) %>% 
  filter(stracc == "Sede Territoriale di Sondrio") %>% 
  column_to_rownames("stracc") 


 

network <- dt %>% 
  #filter(stracc == "Sede Territoriale di Cremona") %>% 
  #filter(finalita == "Esame trichinoscopico") %>% 
  filter(repanalisi == "Sede Territoriale di Cremona") %>% 
  #group_by(repanalisi, stracc) %>% 
  group_by(stracc, repanalisi) %>% 
  count() 

net <- graph_from_data_frame(d=network, directed=T) 

plot(net, edge.arrow.size=0.1, edge.width=E(net)$n/1000)

plot(simplify(net), vertex.size= 0.01,edge.arrow.size=0.001,vertex.label.cex = 0.75,
     vertex.label.color = "black"  ,vertex.frame.color = adjustcolor("white", alpha.f = 0),
     vertex.color = adjustcolor("white", alpha.f = 0),edge.color=adjustcolor(1, alpha.f = 0.15),
     display.isolates=FALSE,vertex.label=ifelse(page_rank(net)$vector > 0.1 , "important nodes", NA))


# library(igraph)
# 
# net <- graph_from_incidence_matrix(network)





# adj_mat <- as_adjacency_matrix(
#   graph_from_data_frame(network, 
#                         directed = TRUE))
# 
# 
# 
# graph_obj <- graph.adjacency(adj_mat)
# 
# plot.igraph(graph_obj,
#             layout=layout.star(graph_obj),
#             vertex.color=NA, vertex.frame.color = NA
# )
# 
# net <-  graph.data.frame(network, directed=T)

#net <- simplify(net)


#net <- graph_from_incidence_matrix(network)



  #filter(trisp >= 0) %>% 
  # group_by(Finalita, prova, in_out, stracc, repprova) %>% 
  # summarise(n = n(),
  #   trisp = mean(trisp, na.rm = TRUE)) %>%  
  
  #group_by(stracc, in_out) %>% 
  # summarise(n = n(),
  #           trisp = mean(trisp, na.rm = TRUE)) %>%  
ggplot()+
  aes(x = stracc2, y = trisp)+
  geom_violin()+
  #geom_jitter(alpha = 0.3)+
  stat_summary(fun = "mean", color = "red")+
  facet_wrap(~ in_out, nrow = 1, scales = "free")+
  theme_classic()+
  coord_flip()+
  theme(
    panel.grid.major = element_line()
  )+
  
  labs(x= "Anno", y= "IF", title = "")
  
  
    



#taut = (Data_RDP-Data)/86400) %>%  View()
    
  #   
  #   
  #   
  # 
  #   trisp = case_when(
  #     in_out == "in" ~ (Data_Fine_Analisi-Data_Accettazione)/86400,
  #     TRUE~  ((Data_Fine_Analisi- Data_Carico)/86400)
  #   ), 
  #   tfirma = (Data_RDP-Data_Fine_Analisi), 
  #   torg =  case_when(
  #     in_out == "in" ~ (Data_Inizio_Analisi-Data_Accettazione)/86400,
  #     TRUE~  ((Data_Inizio_Analisi- Data_Carico)/86400)
  #   )
  # ) %>% 
  # filter(tfirma >= 0)  
  


x <- dati %>% filter(nconf == 88)

tempi(x, prova = "Grasso") %>% 
  group_by(in_out) %>% 
  summarise(Mtaut = mean(taut), 
            Mtrisp = mean(trisp, na.rm = TRUE), 
            Mtorg = mean(torg, na.rm = TRUE))






# tempi <- taut %>% 
#   filter(prova == "Malattia di Aujeszky gE: anticorpi") %>% 
#   mutate(
#     in_out = case_when(
#     stracc != repprova ~ paste("out"), 
#     TRUE ~ paste("in")), 
#     
#     taut = (Data_RDP-Data)/86400,
#     
#     trisp = case_when(
#       in_out == "in" ~ (Data_Fine_Analisi-Data_Accettazione)/86400,
#       TRUE~  ((Data_Fine_Analisi- Data_Carico)/86400)
#     ), 
#     tfirma = (Data_RDP-Data_Fine_Analisi)
#     ) %>% 
#   filter(tfirma >= 0)
#   
# 
# 
# 
# 
# tempi %>% 
#   group_by(in_out) %>% 
#   summarise(Mtaut = mean(taut), 
#             Mtrisp = mean(trisp, na.rm = TRUE))
#  
#     
#     
    
