pkg()
library(DBI)
library(odbc)
library(RPostgreSQL)


con <- DBI::dbConnect(dbDriver("PostgreSQL"),
                      dbname = "c4h_pg",
                      host = "izsler-pgsql-prod-new.c2iwisaj7ujj.eu-south-1.rds.amazonaws.com",
                      port = 5432,
                      # user = "c4h_front",
                      # password = "c4h_front"
                      user = "c4h_pg",
                      password = "c4h_pg"
)

query_darwin <- "Select * from ute_darwin_vm01"
darwin <- dbGetQuery(con, query_darwin)

saveRDS(darwin, "darwin0306.RDS")

cdc <- DBI::dbReadTable(con, 'ute_treenode_cdc_vm03')

anagrafica_cdc <- cdc |> filter(cod_ricl == "CDC") |> 
  select(cdc = cod_cdc,
         dipartimento = cdcdescliv1,
         struttura_complessa = cdcdescliv2,
         nome_cdc = desc_cdc)

saveRDS(anagrafica_cdc, here("tautapp", "anagrafica_cdc.rds"))

 

esame_da_escludere <- c("Motivi di riemissione del Rapporto di Prova", 
                        "Motivi di mancata esecuzione di prove richieste")


readRDS("darwin0306.RDS") |> 
  mutate(cdc_erogante = cdc_cod) |> 
  
  mutate(
    in_out = case_when(
      cdc_accettante != cdc_erogante ~ "out", 
      TRUE ~ "in"
    )
  ) |>
  
  mutate(
    cdc_erogante = case_when(
      cdc_erogante == "5311" ~ "5312",
      cdc_erogante == "531C" ~ "5312",
      cdc_erogante %in% c("4520", "4530", "4540") ~ "4500", 
      .default = cdc_erogante
    )
  ) |> 
  mutate(
    cdc_accettante = case_when(
      cdc_accettante == "5311" ~ "5312",
      cdc_accettante == "531C" ~ "5312",
      cdc_accettante %in% c("4520", "4530", "4540") ~ "4500", 
      .default = cdc_accettante
    )
  ) |>  
  
  
  
  
  filter(
    anno_accettaz >= 2026, 
    !is.na(inizio_analisi),
    #se_attivita_stampa == "S", 
    #is.na(progetto_ricerca), 
    settore_accettazione != 6
  ) |> 
  
  
  
  mutate(routine_instampa = case_when(
    cdc_cod == 4700 & se_attivita_stampa == "S" ~ "Si",
    cdc_cod == 4700 & se_attivita_stampa == "N" ~ "No",
    .default = NA
  )) |> 
  
  
  
  # STEP 6: Esclusione attività del latte  routine non emesse in stampa
  filter(routine_instampa == "Si" | is.na(routine_instampa)) |>
  
  # =====================================================
# ESCLUSIONE ACCETTAZIONI CHE CONTENGONO L'ESAME Motivi di riemissione del Rapporto di Prova
# =====================================================
group_by(anno_accettaz, numero_accettaz) |>
  filter(
    !any(nome_prodotto %in% esame_da_escludere, na.rm = TRUE) # questo codice esclude tutte le accettazioni in cui ci sia almeno una programmazione "Motivi di riemissione del Rapporto di Prova"
    # questo perchè fino a quando non dispongo della data du emissione del primo rdp completo rischia di sovrastimare il tempo di utenza......andrà tolto
  ) |>
  ungroup() |>
  
  select(
    cdc_accettante,
    cdc_erogante,
    anno_accettaz,
    numero_accettaz,
    settore_accettazione,
    descrizione_materiale,
    descrizione_matrice,
    nome_prodotto, 
    descrizione_prodotto,
    quantity,
    data_accettazione, 
    inizio_analisi,
    fine_analisi,
    data_conferma_rdp
  ) |>   
  
  
  mutate(
    settore = case_when(
      settore_accettazione == 1 ~ "Sanità Animale", 
      settore_accettazione == 2 ~ "Alimenti Uomo", 
      settore_accettazione == 3 ~ "Alimenti Zootecnici", 
      settore_accettazione == 4 ~ "Altri Controlli (cosmetici, ambientali..)", 
      .default = "altro"
    )
  ) -> dati


# # ======================================================
# # ESCLUSIONE ACCETTAZIONI CON INIZIO ANALISI < ACCETTAZIONE  esclude le accettazioni la cui data di inizio analisi è antecedente alla data di accettazione
# # ======================================================
# 
#1. Preparazione delle date (solo conversione a Date)
df_check <- dati |>
  mutate(
    data_accettazione_date = as.Date(data_accettazione),
    inizio_analisi_date    = as.Date(inizio_analisi)
  )

# 2. Identificazione delle accettazioni anomale
accettazioni_anomale <- df_check |>
  mutate(
    flag_inizio_prima_accettazione =
      !is.na(inizio_analisi_date) &
      inizio_analisi_date < data_accettazione_date
  ) |>
  group_by(anno_accettaz, numero_accettaz) |>
  summarise(
    accettazione_da_escludere = any(flag_inizio_prima_accettazione),
    .groups = "drop"
  ) |> 
  filter(accettazione_da_escludere)

# # 3. Dataset delle accettazioni escluse (per audit / analisi separata)
# df_esclusi <- df_check |>
#   semi_join(
#     accettazioni_anomale,
#     by = c("anno_accettaz", "numero_accettaz")
#   )

# 4. Dataset pulito: ESCLUDE TUTTE le accettazioni anomale
df_clean <- df_check |>
  anti_join(
    accettazioni_anomale,
    by = c("anno_accettaz", "numero_accettaz")
  )

# df_esami <- df_clean %>%
#   distinct(
#     numero_accettaz,
#     anno_accettaz,
#     nome_prodotto,
#     cdc_erogante,
#     .keep_all = TRUE
#   )




# =====================================================
# DATASET BASE PER APP
# df_clean = dataset già pulito dalle accettazioni anomale
# =====================================================

df_app <- df_clean %>%  # <- forse è unitile questo passaggio perchè le date in df_clean sono già in formato data
  mutate(
    data_acc_d = as.Date(data_accettazione),
    inizio_d   = as.Date(inizio_analisi),
    fine_d     = as.Date(fine_analisi),
    data_rdp_d = as.Date(data_conferma_rdp),
    key_acc    = paste(anno_accettaz, numero_accettaz, sep = "_")
  )

saveRDS(df_app, here("tautapp", "df_app.RDS"))

# ======================================================
# FUNZIONE GIORNI LAVORATIVI
# ======================================================

giorni_lavorativi <- function(data_inizio, data_fine) {
  
  data_inizio <- as.Date(data_inizio)
  data_fine <- as.Date(data_fine)
  
  out <- rep(NA_integer_, length(data_inizio))
  
  ok <- !is.na(data_inizio) & 
    !is.na(data_fine) & 
    data_fine >= data_inizio
  
  if (!any(ok)) {
    return(out)
  }
  
  # Intervallo coerente con as.integer(data_fine - data_inizio):
  # escludo data_inizio e includo data_fine
  start <- data_inizio[ok] + 1
  end <- data_fine[ok]
  
  n_giorni <- as.integer(end - start) + 1
  
  # Se data_inizio == data_fine, n_giorni diventa 0
  n_giorni[n_giorni < 0] <- 0
  
  settimane_intere <- n_giorni %/% 7
  resto <- n_giorni %% 7
  
  conteggio <- settimane_intere * 5
  
  giorno_settimana_start <- as.POSIXlt(start)$wday
  # wday: domenica = 0, lunedì = 1, ..., sabato = 6
  
  for (i in 0:5) {
    giorno <- (giorno_settimana_start + i) %% 7
    
    conteggio <- conteggio + ifelse(
      resto > i & giorno %in% 1:5,
      1,
      0
    )
  }
  
  out[ok] <- as.integer(conteggio)
  
  out
}






# =====================================================
# 1. kpi_accettazioni
# =====================================================

kpi_accettazioni <- df_app %>%
  mutate(
    data_acc_d = as.Date(data_acc_d),
    data_rdp_d = as.Date(data_rdp_d),
    esame_out = cdc_accettante != cdc_erogante
  ) %>%
  group_by(anno_accettaz, numero_accettaz) %>%
  summarise(
    cdc_accettante = first(cdc_accettante),
    settore = first(settore),
    
    data_acc = min(data_acc_d, na.rm = TRUE),
    data_rdp = max(data_rdp_d, na.rm = TRUE),
    
    n_righe_dataset = n(),
    n_esami_out = sum(esame_out, na.rm = TRUE),
    accettazione_out = any(esame_out, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    tipo_accettazione = if_else(accettazione_out, "OUT", "IN"),
    
    tempo_attesa_calendario = as.integer(data_rdp - data_acc),
    tempo_attesa_utente = giorni_lavorativi(data_acc, data_rdp),
    
    settimana = floor_date(data_acc, unit = "week", week_start = 1)
  )


saveRDS(kpi_accettazioni, here("tautapp", "kpi_accettazioni.RDS"))


# =====================================================
# TREND GIÀ PRE-AGGREGATI PER L'APP
# =====================================================

trend_totale <- kpi_accettazioni %>%
  group_by(settimana, tipo_accettazione) %>%
  summarise(
    livello = "Totale",
    settore = "Tutti",
    cdc_accettante = "Tutti",
    n_accettazioni = n(),
    mediana_attesa = median(tempo_attesa_utente, na.rm = TRUE),
    media_attesa = mean(tempo_attesa_utente, na.rm = TRUE),
    .groups = "drop"
  )

trend_settore <- kpi_accettazioni %>%
  group_by(settimana, settore, tipo_accettazione) %>%
  summarise(
    livello = "Settore",
    cdc_accettante = "Tutti",
    n_accettazioni = n(),
    mediana_attesa = median(tempo_attesa_utente, na.rm = TRUE),
    media_attesa = mean(tempo_attesa_utente, na.rm = TRUE),
    .groups = "drop"
  )

trend_cdc <- kpi_accettazioni %>%
  group_by(settimana, settore, cdc_accettante, tipo_accettazione) %>%
  summarise(
    livello = "CDC",
    n_accettazioni = n(),
    mediana_attesa = median(tempo_attesa_utente, na.rm = TRUE),
    media_attesa = mean(tempo_attesa_utente, na.rm = TRUE),
    .groups = "drop"
  )

kpi_trend_attesa <- bind_rows(
  trend_totale,
  trend_settore,
  trend_cdc
)

saveRDS(kpi_trend_attesa, here("tautapp","kpi_trend_attesa.rds"))




# ==============================================================================
# CODICE PER VALUTAZIONE TEMPO TECNICO E INCIDENZA SU TAUT
# ==============================================================================

library(dplyr)
library(stringr)
library(lubridate)

# =====================================================
# PIPELINE COMPLETA
# Costruzione kpi_accettazioni_in_adj
# con benchmark tecnico per settore + nome_prodotto
# e fallback su benchmark generale per nome_prodotto
# =====================================================


# =====================================================
# 0. PARAMETRI E FUNZIONI DI SUPPORTO
# =====================================================

# Numero minimo di osservazioni per considerare stabile
# il benchmark settore + prodotto
soglia_min_benchmark <- 30


max_na_num <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}


min_na_num <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  min(x, na.rm = TRUE)
}


# =====================================================
# 1. SELEZIONE DELLE SOLE ACCETTAZIONI IN
#    A LIVELLO ESAME/RIGA
# =====================================================
# Regola:
# una accettazione è IN solo se nessun esame della stessa
# accettazione è erogato da CDC diverso dal CDC accettante.
#
# Inoltre vengono escluse le righe con descrizione_prodotto
# "Non codificato - Multipla", come nella pipeline precedente.
# =====================================================

df_in_esami <- df_app %>%
  mutate(
    esame_out = cdc_accettante != cdc_erogante
  ) %>%
  group_by(anno_accettaz, numero_accettaz) %>%
  filter(
    !any(esame_out, na.rm = TRUE),
    !str_detect(
      coalesce(descrizione_prodotto, ""),
      "Non codificato - Multipla"
    )
  ) %>%
  ungroup()


# =====================================================
# 2. CALCOLO DELLA DURATA TECNICA OSSERVATA
# =====================================================
# La durata tecnica viene calcolata in giorni lavorativi
# tra inizio_analisi e fine_analisi.
# =====================================================

df_in_esami <- df_in_esami %>%
  mutate(
    inizio_analisi_d = as.Date(inizio_analisi),
    fine_analisi_d = as.Date(fine_analisi),
    durata_tecnica = giorni_lavorativi(inizio_analisi_d, fine_analisi_d)
  )


# =====================================================
# 3. BENCHMARK TECNICO SETTORIALE
# =====================================================
# Benchmark calcolato per combinazione:
# settore + nome_prodotto
# =====================================================

benchmark_settore_prodotto <- df_in_esami %>%
  filter(!is.na(durata_tecnica)) %>%
  group_by(settore, nome_prodotto) %>%
  summarise(
    n_settore_prodotto = n(),
    tecnico_mediano_settore = median(durata_tecnica, na.rm = TRUE),
    tecnico_p75_settore = as.numeric(
      quantile(durata_tecnica, 0.75, na.rm = TRUE)
    ),
    tecnico_p90_settore = as.numeric(
      quantile(durata_tecnica, 0.90, na.rm = TRUE)
    ),
    .groups = "drop"
  )


# =====================================================
# 4. BENCHMARK TECNICO GENERALE DI FALLBACK
# =====================================================
# Benchmark calcolato per:
# nome_prodotto
#
# Serve quando la combinazione settore + prodotto
# ha una numerosità inferiore a soglia_min_benchmark.
# =====================================================

benchmark_prodotto <- df_in_esami %>%
  filter(!is.na(durata_tecnica)) %>%
  group_by(nome_prodotto) %>%
  summarise(
    n_prodotto = n(),
    tecnico_mediano_generale = median(durata_tecnica, na.rm = TRUE),
    tecnico_p75_generale = as.numeric(
      quantile(durata_tecnica, 0.75, na.rm = TRUE)
    ),
    tecnico_p90_generale = as.numeric(
      quantile(durata_tecnica, 0.90, na.rm = TRUE)
    ),
    .groups = "drop"
  )


# =====================================================
# 5. ASSEGNAZIONE DEL BENCHMARK A OGNI ESAME
# =====================================================
# Regola:
# - se settore + nome_prodotto ha almeno soglia_min_benchmark
#   osservazioni, uso il benchmark settoriale;
# - altrimenti uso il benchmark generale per nome_prodotto.
# =====================================================

df_in_esami_bench <- df_in_esami %>%
  left_join(
    benchmark_settore_prodotto,
    by = c("settore", "nome_prodotto")
  ) %>%
  left_join(
    benchmark_prodotto,
    by = "nome_prodotto"
  ) %>%
  mutate(
    usa_benchmark_settoriale =
      !is.na(n_settore_prodotto) &
      n_settore_prodotto >= soglia_min_benchmark,
    
    tecnico_atteso_mediano = if_else(
      usa_benchmark_settoriale,
      tecnico_mediano_settore,
      tecnico_mediano_generale
    ),
    
    tecnico_atteso_p75 = if_else(
      usa_benchmark_settoriale,
      tecnico_p75_settore,
      tecnico_p75_generale
    ),
    
    tecnico_atteso_p90 = if_else(
      usa_benchmark_settoriale,
      tecnico_p90_settore,
      tecnico_p90_generale
    ),
    
    n_esami_benchmark = if_else(
      usa_benchmark_settoriale,
      n_settore_prodotto,
      n_prodotto
    ),
    
    benchmark_usato = case_when(
      usa_benchmark_settoriale ~ "settore_prodotto",
      !is.na(n_prodotto) ~ "prodotto_generale",
      TRUE ~ "benchmark_mancante"
    )
  )


# =====================================================
# 6. COSTRUZIONE DATASET A LIVELLO ACCETTAZIONE
# =====================================================
# Una riga = una accettazione IN.
#
# Il tempo tecnico atteso dell'accettazione è il massimo
# dei tempi tecnici attesi degli esami richiesti.
# =====================================================

kpi_accettazioni_in_adj <- df_in_esami_bench %>%
  group_by(anno_accettaz, numero_accettaz) %>%
  summarise(
    cdc_accettante = first(cdc_accettante),
    settore = first(settore),
    
    data_acc = min(data_acc_d, na.rm = TRUE),
    data_rdp = max(data_rdp_d, na.rm = TRUE),
    
    n_esami = n_distinct(nome_prodotto),
    n_righe_esami = n(),
    
    tempo_attesa_utente = giorni_lavorativi(data_acc, data_rdp),
    
    tecnico_atteso_mediano = max_na_num(tecnico_atteso_mediano),
    tecnico_atteso_p75 = max_na_num(tecnico_atteso_p75),
    tecnico_atteso_p90 = max_na_num(tecnico_atteso_p90),
    
    tecnico_effettivo_max = max_na_num(durata_tecnica),
    
    n_esami_benchmark_min = min_na_num(n_esami_benchmark),
    
    n_benchmark_settoriale = sum(
      benchmark_usato == "settore_prodotto",
      na.rm = TRUE
    ),
    
    n_benchmark_generale = sum(
      benchmark_usato == "prodotto_generale",
      na.rm = TRUE
    ),
    
    n_benchmark_mancante = sum(
      benchmark_usato == "benchmark_mancante",
      na.rm = TRUE
    ),
    
    n_settori_accettazione = n_distinct(settore),
    
    .groups = "drop"
  ) %>%
  mutate(
    tipo_accettazione = "IN",
    
    benchmark_prevalente = case_when(
      n_benchmark_settoriale > n_benchmark_generale ~
        "prevalentemente_settoriale",
      n_benchmark_generale > n_benchmark_settoriale ~
        "prevalentemente_generale",
      n_benchmark_settoriale == 0 & n_benchmark_generale == 0 ~
        "benchmark_mancante",
      TRUE ~ "misto"
    ),
    
    scostamento_vs_mediana =
      tempo_attesa_utente - tecnico_atteso_mediano,
    
    scostamento_vs_p75 =
      tempo_attesa_utente - tecnico_atteso_p75,
    
    scostamento_vs_p90 =
      tempo_attesa_utente - tecnico_atteso_p90,
    
    extra_vs_mediana_pos =
      pmax(0, scostamento_vs_mediana),
    
    extra_vs_p75_pos =
      pmax(0, scostamento_vs_p75),
    
    extra_vs_p90_pos =
      pmax(0, scostamento_vs_p90)
  )



# =====================================================
# SALVATAGGIO OUTPUT
# =====================================================

saveRDS(
  kpi_accettazioni_in_adj, here("tautapp",  
                                "kpi_accettazioni_in_adj.rds"
  ))

saveRDS(
  benchmark_settore_prodotto, here("tautapp",  
                                   "benchmark_settore_prodotto.rds"
  ))

saveRDS(
  benchmark_prodotto,  here("tautapp",  
                            "benchmark_prodotto.rds"
  ))






# # =====================================================
# # 7. CONTROLLI DI QUALITÀ
# # =====================================================
# 
# # 7.1 Copertura benchmark
# controllo_copertura_benchmark <- kpi_accettazioni_in_adj %>%
#   summarise(
#     accettazioni = n(),
#     benchmark_mancante = sum(is.na(tecnico_atteso_mediano)),
#     perc_benchmark_mancante = round(
#       100 * benchmark_mancante / accettazioni,
#       2
#     )
#   )
# 
# 
# # 7.2 Tipo di benchmark prevalente a livello accettazione
# controllo_benchmark_prevalente <- kpi_accettazioni_in_adj %>%
#   count(benchmark_prevalente) %>%
#   mutate(
#     perc = round(100 * n / sum(n), 1)
#   )
# 
# 
# # 7.3 Distribuzione numero settori per accettazione
# controllo_settori_accettazione <- kpi_accettazioni_in_adj %>%
#   count(n_settori_accettazione)
# 
# 
# # 7.4 Distribuzione target tecnico atteso mediano
# controllo_tecnico_atteso <- kpi_accettazioni_in_adj %>%
#   summarise(
#     accettazioni = n(),
#     mediana_tecnico_atteso = median(
#       tecnico_atteso_mediano,
#       na.rm = TRUE
#     ),
#     p25_tecnico_atteso = as.numeric(
#       quantile(tecnico_atteso_mediano, 0.25, na.rm = TRUE)
#     ),
#     p75_tecnico_atteso = as.numeric(
#       quantile(tecnico_atteso_mediano, 0.75, na.rm = TRUE)
#     ),
#     p90_tecnico_atteso = as.numeric(
#       quantile(tecnico_atteso_mediano, 0.90, na.rm = TRUE)
#     )
#   )
# 
# 
# # Stampa controlli
# print(controllo_copertura_benchmark)
# print(controllo_benchmark_prevalente)
# print(controllo_settori_accettazione)
# print(controllo_tecnico_atteso)
# 















