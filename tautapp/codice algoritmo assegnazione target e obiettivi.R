
library(dplyr)
library(tidyr)
library(purrr)

periodo_inizio <- as.Date("2026-01-01")
periodo_fine   <- as.Date("2026-12-31")

benchmark_tecnico <- "tecnico_atteso_mediano"

margini_test <- c(0, 1, 2, 3)

target_strategico <- 95


# df_cdc_base <- kpi_accettazioni_in_adj %>%
#   mutate(
#     data_acc = as.Date(data_acc),
#     tecnico_atteso = .data[[benchmark_tecnico]]
#   ) %>%
#   filter(
#     tipo_accettazione == "IN",
#     data_acc >= periodo_inizio,
#     data_acc <= periodo_fine,
#     !is.na(tecnico_atteso)
#   )

df_cdc_settore_base <- kpi_accettazioni_in_adj %>%
  mutate(
    data_acc = as.Date(data_acc),
    tecnico_atteso = tecnico_atteso_mediano
  ) %>%
  filter(
    tipo_accettazione == "IN",
    !is.na(tecnico_atteso)
  )


profilo_cdc_settore <- df_cdc_settore_base %>%
  group_by(
    dipartimento,
    struttura_complessa,
    cdc_accettante,
    nome_cdc,
    settore
  ) %>%
  summarise(
    accettazioni_in = n(),
    
    mediana_attesa = median(tempo_attesa_utente, na.rm = TRUE),
    p75_attesa = quantile(tempo_attesa_utente, 0.75, na.rm = TRUE),
    p90_attesa = quantile(tempo_attesa_utente, 0.90, na.rm = TRUE),
    
    mediana_tecnico_atteso = median(tecnico_atteso, na.rm = TRUE),
    p75_tecnico_atteso = quantile(tecnico_atteso, 0.75, na.rm = TRUE),
    p90_tecnico_atteso = quantile(tecnico_atteso, 0.90, na.rm = TRUE),
    
    extra_mediano = median(
      tempo_attesa_utente - tecnico_atteso,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    classe_complessita = case_when(
      mediana_tecnico_atteso <= 1 ~ "Bassa",
      mediana_tecnico_atteso <= 2 ~ "Media",
      mediana_tecnico_atteso <= 4 ~ "Alta",
      TRUE ~ "Molto alta"
    )
  )



# profilo_cdc <- df_cdc_base %>%
#   group_by(
#     cdc_accettante,
#     nome_cdc,
#     struttura_complessa,
#     dipartimento
#   ) %>%
#   summarise(
#     accettazioni_in = n(),
#     
#     mediana_attesa = median(tempo_attesa_utente, na.rm = TRUE),
#     p75_attesa = quantile(tempo_attesa_utente, 0.75, na.rm = TRUE),
#     p90_attesa = quantile(tempo_attesa_utente, 0.90, na.rm = TRUE),
#     
#     mediana_tecnico_atteso = median(tecnico_atteso, na.rm = TRUE),
#     p75_tecnico_atteso = quantile(tecnico_atteso, 0.75, na.rm = TRUE),
#     p90_tecnico_atteso = quantile(tecnico_atteso, 0.90, na.rm = TRUE),
#     
#     extra_mediano = median(
#       tempo_attesa_utente - tecnico_atteso,
#       na.rm = TRUE
#     ),
#     
#     extra_p90 = quantile(
#       tempo_attesa_utente - tecnico_atteso,
#       0.90,
#       na.rm = TRUE
#     ),
#     
#     .groups = "drop"
#   ) %>%
#   mutate(
#     classe_complessita = case_when(
#       mediana_tecnico_atteso <= 1 ~ "Bassa",
#       mediana_tecnico_atteso <= 2 ~ "Media",
#       mediana_tecnico_atteso <= 4 ~ "Alta",
#       TRUE ~ "Molto alta"
#     )
#   )


performance_margini <- map_dfr(
  margini_test,
  function(margine) {
    
    df_cdc_base %>%
      mutate(
        margine_tecnico = margine,
        target_aggiustato = tecnico_atteso + margine_tecnico,
        entro_target_aggiustato =
          tempo_attesa_utente <= target_aggiustato
      ) %>%
      group_by(cdc_accettante, margine_tecnico) %>%
      summarise(
        accettazioni_adj = n(),
        perc_entro_target_aggiustato = round(
          100 * mean(entro_target_aggiustato, na.rm = TRUE),
          1
        ),
        mediana_target_aggiustato = median(
          target_aggiustato,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  }
)

soglia_minima_calibrazione <- 90

margine_suggerito <- performance_margini %>%
  filter(perc_entro_target_aggiustato >= soglia_minima_calibrazione) %>%
  group_by(cdc_accettante) %>%
  slice_min(margine_tecnico, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    cdc_accettante,
    margine_tecnico_suggerito = margine_tecnico,
    perc_con_margine_suggerito = perc_entro_target_aggiustato
  )


cdc_senza_margine <- performance_margini %>%
  group_by(cdc_accettante) %>%
  summarise(
    max_perc_aggiustata = max(
      perc_entro_target_aggiustato,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  filter(max_perc_aggiustata < soglia_minima_calibrazione)


baseline_standard <- performance_margini %>%
  filter(margine_tecnico == 1) %>%
  select(
    cdc_accettante,
    perc_adj_baseline = perc_entro_target_aggiustato
  )


obiettivi_percentuali <- baseline_standard %>%
  mutate(
    obiettivo_percentuale_anno1 = case_when(
      perc_adj_baseline >= 95 ~ 95,
      perc_adj_baseline >= 90 ~ 95,
      perc_adj_baseline >= 80 ~ pmin(95, perc_adj_baseline + 5),
      perc_adj_baseline >= 70 ~ pmin(95, perc_adj_baseline + 7),
      perc_adj_baseline >= 60 ~ pmin(95, perc_adj_baseline + 10),
      TRUE ~ pmin(80, perc_adj_baseline + 10)
    ),
    
    classe_obiettivo = case_when(
      perc_adj_baseline >= 95 ~ "Mantenimento",
      perc_adj_baseline >= 90 ~ "Consolidamento",
      perc_adj_baseline >= 80 ~ "Miglioramento moderato",
      perc_adj_baseline >= 70 ~ "Miglioramento significativo",
      perc_adj_baseline >= 60 ~ "Recupero",
      TRUE ~ "Piano specifico"
    )
  )


target_giorni_informativo <- df_cdc_base %>%
  group_by(cdc_accettante) %>%
  summarise(
    target_giorni_p75_osservato = ceiling(
      quantile(tempo_attesa_utente, 0.75, na.rm = TRUE)
    ),
    target_giorni_p90_osservato = ceiling(
      quantile(tempo_attesa_utente, 0.90, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(
    target_giorni_suggerito = pmin(
      target_giorni_p75_osservato,
      5
    )
  )


proposta_target_cdc <- profilo_cdc %>%
  left_join(
    baseline_standard,
    by = "cdc_accettante"
  ) %>%  
  left_join(
    obiettivi_percentuali,
    by = "cdc_accettante"
  ) %>%
  left_join(
    margine_suggerito,
    by = "cdc_accettante"
  ) %>%
  left_join(
    target_giorni_informativo,
    by = "cdc_accettante"
  ) %>%
  left_join(
    cdc_senza_margine,
    by = "cdc_accettante"
  ) %>%
  mutate(
    margine_tecnico_suggerito = if_else(
      is.na(margine_tecnico_suggerito),
      NA_real_,
      as.numeric(margine_tecnico_suggerito)
    ),
    
    nota_margine = case_when(
      !is.na(margine_tecnico_suggerito) ~
        paste0(
          "Margine +",
          margine_tecnico_suggerito,
          " sufficiente per raggiungere almeno ",
          soglia_minima_calibrazione,
          "%"
        ),
      is.na(margine_tecnico_suggerito) ~
        paste0(
          "Non raggiunge ",
          soglia_minima_calibrazione,
          "% nemmeno con margine +3"
        )
    ),
    
    target_strategico_percentuale = target_strategico
  ) %>% 
  select(
    dipartimento,
    struttura_complessa,
    cdc_accettante,
    nome_cdc,
    accettazioni_in,
    classe_complessita,
    mediana_attesa,
    p75_attesa,
    p90_attesa,
    mediana_tecnico_atteso,
    extra_mediano,
    extra_p90,
    perc_adj_baseline,
    classe_obiettivo,
    obiettivo_percentuale_anno1,
    target_strategico_percentuale,
    margine_tecnico_suggerito,
    perc_con_margine_suggerito,
    nota_margine,
    target_giorni_suggerito,
    target_giorni_p75_osservato,
    target_giorni_p90_osservato
  ) %>%
  arrange(
    classe_obiettivo,
    perc_adj_baseline
  )
