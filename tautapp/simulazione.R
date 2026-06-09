# =====================================================
# SIMULAZIONE EFFETTO DI DILUIZIONE DEGLI ESAMI VINCOLANTI
# =====================================================
# Obiettivo:
# simulare un sistema in cui i TDR per esame risultano molto performanti,
# ma una quota di accettazioni presenta tempi di attesa utenza elevati
# perché episodi vincolanti, rari per singolo esame, si distribuiscono
# su diverse tipologie di esame e diverse accettazioni.
# =====================================================

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(stringr)

set.seed(1234)

# =====================================================
# 1. PARAMETRI GENERALI
# =====================================================

n_accettazioni <- 3000

target_utenza <- 3

tipi_esame <- tibble(
  nome_prodotto = c(
    "Esame_A_sierologico",
    "Esame_B_sierologico",
    "Esame_C_batteriologico",
    "Esame_D_virologico",
    "Esame_E_specialistico"
  ),
  
  # Tempo tecnico atteso "puro/osservato" dell'esame
  tecnico_atteso = c(1, 1, 2, 4, 6),
  
  # Target TDR istituzionale = tecnico atteso + tempo organizzativo
  # Qui simuliamo l'effetto di un +1 giorno.
  target_tdr = c(2, 2, 3, 5, 7),
  
  # Frequenza con cui ciascun esame viene richiesto
  probabilita_richiesta = c(0.35, 0.25, 0.20, 0.13, 0.07),
  
  # Probabilità che lo specifico esame abbia un episodio critico.
  # Queste probabilità sono basse: il TDR aggregato per esame resterà alto.
  probabilita_episodio_critico = c(0.015, 0.020, 0.030, 0.040, 0.050),
  
  # Ritardo aggiuntivo quando l'episodio critico si verifica.
  ritardo_critico = c(2, 2, 2, 3, 3)
)

# Numero esami per accettazione
# Molte accettazioni hanno 1-3 esami; alcune ne hanno di più.
distribuzione_n_esami <- tibble(
  n_esami = 1:6,
  probabilita = c(0.25, 0.30, 0.22, 0.13, 0.07, 0.03)
)

# Tempo di firma simulato a livello accettazione
# In molti casi 0-1 giorno, pochi casi 2 giorni.
distribuzione_tempo_firma <- tibble(
  tempo_firma = c(0, 1, 2),
  probabilita = c(0.30, 0.65, 0.05)
)

# =====================================================
# 2. CREAZIONE DATASET ACCETTAZIONI
# =====================================================

df_accettazioni_base <- tibble(
  id_accettazione = 1:n_accettazioni,
  n_esami = sample(
    distribuzione_n_esami$n_esami,
    size = n_accettazioni,
    replace = TRUE,
    prob = distribuzione_n_esami$probabilita
  ),
  tempo_firma = sample(
    distribuzione_tempo_firma$tempo_firma,
    size = n_accettazioni,
    replace = TRUE,
    prob = distribuzione_tempo_firma$probabilita
  )
)

# =====================================================
# 3. ESPANSIONE A LIVELLO ESAME PROGRAMMATO
# =====================================================

df_esami <- df_accettazioni_base %>%
  uncount(n_esami, .id = "progressivo_esame") %>%
  mutate(
    nome_prodotto = sample(
      tipi_esame$nome_prodotto,
      size = n(),
      replace = TRUE,
      prob = tipi_esame$probabilita_richiesta
    )
  ) %>%
  left_join(
    tipi_esame,
    by = "nome_prodotto"
  )

# =====================================================
# 4. SIMULAZIONE TDR OSSERVATO PER ESAME
# =====================================================
# Il TDR osservato parte dal tecnico atteso.
# In una piccola quota di casi viene aggiunto un ritardo critico.
# Questi episodi sono rari rispetto al totale delle richieste dello stesso esame.
# =====================================================

df_esami <- df_esami %>%
  mutate(
    episodio_critico = runif(n()) < probabilita_episodio_critico,
    
    tdr_osservato = tecnico_atteso + if_else(
      episodio_critico,
      ritardo_critico,
      0
    ),
    
    esame_entra_tdr = tdr_osservato <= target_tdr
  )

# =====================================================
# 5. IDENTIFICAZIONE DELL'ESAME VINCOLANTE
# =====================================================
# L'esame vincolante è quello con il TDR osservato massimo
# dentro la stessa accettazione.
# In caso di pari merito, vengono mantenuti tutti gli esami a pari massimo.
# =====================================================

df_esami <- df_esami %>%
  group_by(id_accettazione) %>%
  mutate(
    max_tdr_accettazione = max(tdr_osservato, na.rm = TRUE),
    esame_vincolante = tdr_osservato == max_tdr_accettazione
  ) %>%
  ungroup()

# =====================================================
# 6. CALCOLO TEMPO DI ATTESA UTENZA A LIVELLO ACCETTAZIONE
# =====================================================
# L'utente aspetta:
# - la chiusura dell'esame più tardivo
# - più il tempo di firma.
# =====================================================

df_accettazioni <- df_esami %>%
  group_by(id_accettazione) %>%
  summarise(
    n_esami = n(),
    
    max_tdr_osservato = max(tdr_osservato, na.rm = TRUE),
    tecnico_atteso_accettazione = max(tecnico_atteso, na.rm = TRUE),
    
    tutti_esami_entra_tdr = all(esame_entra_tdr),
    almeno_un_esame_fuori_tdr = any(!esame_entra_tdr),
    
    n_esami_vincolanti = sum(esame_vincolante, na.rm = TRUE),
    
    # Se più esami sono vincolanti a pari merito, li concateno
    esami_vincolanti = paste(
      unique(nome_prodotto[esame_vincolante]),
      collapse = "; "
    ),
    
    episodio_critico_vincolante = any(
      episodio_critico & esame_vincolante,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  left_join(
    df_accettazioni_base %>%
      select(id_accettazione, tempo_firma),
    by = "id_accettazione"
  ) %>%
  mutate(
    tempo_attesa_utenza = max_tdr_osservato + tempo_firma,
    
    accettazione_entra_target_utenza =
      tempo_attesa_utenza <= target_utenza,
    
    accettazione_fuori_target_utenza =
      !accettazione_entra_target_utenza,
    
    # Target aggiustato semplificato:
    # confronto tempo utenza vs tecnico atteso dell'accettazione
    accettazione_entra_target_aggiustato =
      tempo_attesa_utenza <= tecnico_atteso_accettazione,
    
    accettazione_fuori_target_aggiustato =
      !accettazione_entra_target_aggiustato
  )

# =====================================================
# 7. INDICATORI COMPLESSIVI
# =====================================================

indicatori_complessivi <- tibble(
  indicatore = c(
    "% esami entro target TDR",
    "% accettazioni con tutti gli esami entro TDR",
    "% accettazioni entro target utenza",
    "% accettazioni entro target aggiustato",
    "% accettazioni con tutti gli esami TDR ok ma fuori target utenza",
    "% accettazioni con episodio critico vincolante",
    "% accettazioni fuori target utenza con episodio critico vincolante"
  ),
  valore = c(
    mean(df_esami$esame_entra_tdr, na.rm = TRUE) * 100,
    
    mean(df_accettazioni$tutti_esami_entra_tdr, na.rm = TRUE) * 100,
    
    mean(df_accettazioni$accettazione_entra_target_utenza, na.rm = TRUE) * 100,
    
    mean(df_accettazioni$accettazione_entra_target_aggiustato, na.rm = TRUE) * 100,
    
    mean(
      df_accettazioni$tutti_esami_entra_tdr &
        df_accettazioni$accettazione_fuori_target_utenza,
      na.rm = TRUE
    ) * 100,
    
    mean(
      df_accettazioni$episodio_critico_vincolante,
      na.rm = TRUE
    ) * 100,
    
    mean(
      df_accettazioni$accettazione_fuori_target_utenza &
        df_accettazioni$episodio_critico_vincolante,
      na.rm = TRUE
    ) * 100
  )
) %>%
  mutate(
    valore = round(valore, 1)
  )

indicatori_complessivi

# =====================================================
# 8. ANALISI PER TIPO DI ESAME
# =====================================================
# Questa è la tabella chiave per mostrare la diluizione:
# ogni esame può avere % TDR molto alta,
# ma una quota dei suoi episodi può essere vincolante
# nelle accettazioni lente.
# =====================================================

analisi_per_esame <- df_esami %>%
  left_join(
    df_accettazioni %>%
      select(
        id_accettazione,
        tempo_attesa_utenza,
        accettazione_fuori_target_utenza,
        accettazione_fuori_target_aggiustato
      ),
    by = "id_accettazione"
  ) %>%
  group_by(nome_prodotto) %>%
  summarise(
    richieste_esame = n(),
    
    esami_entra_tdr = sum(esame_entra_tdr, na.rm = TRUE),
    perc_esami_entra_tdr = round(
      100 * esami_entra_tdr / richieste_esame,
      1
    ),
    
    esami_fuori_tdr = sum(!esame_entra_tdr, na.rm = TRUE),
    perc_esami_fuori_tdr = round(
      100 * esami_fuori_tdr / richieste_esame,
      1
    ),
    
    volte_vincolante = sum(esame_vincolante, na.rm = TRUE),
    perc_vincolante_su_richieste = round(
      100 * volte_vincolante / richieste_esame,
      1
    ),
    
    volte_vincolante_e_fuori_tdr = sum(
      esame_vincolante & !esame_entra_tdr,
      na.rm = TRUE
    ),
    
    volte_vincolante_in_acc_lenta = sum(
      esame_vincolante & accettazione_fuori_target_utenza,
      na.rm = TRUE
    ),
    
    perc_vincolante_lento_su_richieste = round(
      100 * volte_vincolante_in_acc_lenta / richieste_esame,
      1
    ),
    
    quota_acc_lente_spiegate_da_esame = NA_real_,
    
    .groups = "drop"
  )

# Calcolo quota delle accettazioni lente spiegate da ogni esame
n_acc_lente <- sum(df_accettazioni$accettazione_fuori_target_utenza, na.rm = TRUE)

analisi_per_esame <- analisi_per_esame %>%
  mutate(
    quota_acc_lente_spiegate_da_esame = round(
      100 * volte_vincolante_in_acc_lenta / n_acc_lente,
      1
    )
  ) %>%
  arrange(desc(volte_vincolante_in_acc_lenta))

analisi_per_esame

# =====================================================
# 9. ACCETTAZIONI LENTE CON TUTTI GLI ESAMI IN TDR
# =====================================================
# Questa tabella mostra direttamente il caso:
# "Tutti gli esami rispettano il TDR, ma l'utente è fuori target".
# =====================================================

acc_lente_tdr_ok <- df_accettazioni %>%
  filter(
    tutti_esami_entra_tdr,
    accettazione_fuori_target_utenza
  )

sintesi_acc_lente_tdr_ok <- acc_lente_tdr_ok %>%
  summarise(
    accettazioni_lente_tdr_ok = n(),
    perc_su_tutte_accettazioni = round(
      100 * n() / nrow(df_accettazioni),
      1
    ),
    perc_su_accettazioni_lente = round(
      100 * n() / sum(df_accettazioni$accettazione_fuori_target_utenza),
      1
    ),
    n_esami_mediano = median(n_esami),
    tempo_attesa_mediano = median(tempo_attesa_utenza)
  )

sintesi_acc_lente_tdr_ok

# =====================================================
# 10. ESAMI VINCOLANTI NELLE ACCETTAZIONI LENTE
# =====================================================

esami_vincolanti_acc_lente <- df_esami %>%
  left_join(
    df_accettazioni %>%
      select(id_accettazione, accettazione_fuori_target_utenza),
    by = "id_accettazione"
  ) %>%
  filter(
    esame_vincolante,
    accettazione_fuori_target_utenza
  ) %>%
  count(nome_prodotto, sort = TRUE) %>%
  mutate(
    perc = round(100 * n / sum(n), 1)
  )

esami_vincolanti_acc_lente

# =====================================================
# 11. GRAFICO 1 - TDR ALTO MA IMPATTO VINCOLANTE
# =====================================================

grafico_tdr_vs_vincolo <- analisi_per_esame %>%
  ggplot(
    aes(
      x = perc_esami_entra_tdr,
      y = quota_acc_lente_spiegate_da_esame,
      label = nome_prodotto
    )
  ) +
  geom_point(size = 3) +
  geom_text(
    vjust = -0.7,
    size = 3.5
  ) +
  scale_x_continuous(
    limits = c(90, 100),
    breaks = seq(90, 100, 2)
  ) +
  labs(
    title = "TDR elevato ma contributo alle accettazioni lente",
    subtitle = "Un esame può avere TDR alto ma contribuire comunque alle accettazioni lente quando è vincolante",
    x = "% richieste dell'esame entro target TDR",
    y = "% accettazioni lente in cui l'esame è vincolante"
  ) +
  theme_minimal()

grafico_tdr_vs_vincolo

# =====================================================
# 12. GRAFICO 2 - CONTRIBUTO DEGLI ESAMI VINCOLANTI
# =====================================================

grafico_contributo_vincolanti <- esami_vincolanti_acc_lente %>%
  ggplot(
    aes(
      x = reorder(nome_prodotto, n),
      y = n
    )
  ) +
  geom_col(fill = "#2C7FB8") +
  coord_flip() +
  labs(
    title = "Esami vincolanti nelle accettazioni fuori target utenza",
    x = "Esame",
    y = "Numero di volte vincolante in accettazioni lente"
  ) +
  theme_minimal()

grafico_contributo_vincolanti

# =====================================================
# 13. GRAFICO 3 - CONFRONTO INDICATORI GLOBALI
# =====================================================

grafico_indicatori_globali <- indicatori_complessivi %>%
  ggplot(
    aes(
      x = reorder(indicatore, valore),
      y = valore
    )
  ) +
  geom_col(fill = "#41AB5D") +
  coord_flip() +
  geom_text(
    aes(label = paste0(valore, "%")),
    hjust = -0.1,
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, 105)
  ) +
  labs(
    title = "Confronto tra TDR per esame e tempo di attesa utenza",
    x = NULL,
    y = "%"
  ) +
  theme_minimal()

grafico_indicatori_globali

# =====================================================
# 14. OUTPUT OPZIONALI
# =====================================================

# write.csv(indicatori_complessivi, "sim_indicatori_complessivi.csv", row.names = FALSE)
# write.csv(analisi_per_esame, "sim_analisi_per_esame.csv", row.names = FALSE)
# write.csv(sintesi_acc_lente_tdr_ok, "sim_acc_lente_tdr_ok.csv", row.names = FALSE)
# write.csv(esami_vincolanti_acc_lente, "sim_esami_vincolanti_acc_lente.csv", row.names = FALSE)

# ggsave("sim_grafico_tdr_vs_vincolo.png", grafico_tdr_vs_vincolo, width = 10, height = 6, dpi = 300)
# ggsave("sim_grafico_contributo_vincolanti.png", grafico_contributo_vincolanti, width = 10, height = 6, dpi = 300)
# ggsave("sim_grafico_indicatori_globali.png", grafico_indicatori_globali, width = 10, height = 6, dpi = 300)



library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(stringr)

set.seed(1234)

# =====================================================
# 1. PARAMETRI GENERALI MONTE CARLO
# =====================================================

n_simulazioni <- 1000
n_accettazioni <- 3000

target_utenza <- 3

# =====================================================
# 2. DISTRIBUZIONI DEI TIPI DI ESAME
# =====================================================
# In questa tabella non inseriamo solo valori fissi,
# ma parametri che verranno usati per generare variabilità.

tipi_esame <- tibble(
  nome_prodotto = c(
    "Esame_A_sierologico",
    "Esame_B_sierologico",
    "Esame_C_batteriologico",
    "Esame_D_virologico",
    "Esame_E_specialistico"
  ),
  
  # Tempo tecnico medio atteso
  tecnico_medio = c(1, 1, 2, 4, 6),
  
  # Variabilità del tempo tecnico
  tecnico_sd = c(0.3, 0.3, 0.5, 0.8, 1.0),
  
  # Target TDR istituzionale medio:
  # simuliamo un target pari a tecnico medio + 1 giorno
  target_tdr = c(2, 2, 3, 5, 7),
  
  # Probabilità media di richiesta dell'esame
  probabilita_richiesta = c(0.35, 0.25, 0.20, 0.13, 0.07),
  
  # Probabilità media di episodio critico
  probabilita_critica_media = c(0.015, 0.020, 0.030, 0.040, 0.050),
  
  # Intensità media del ritardo critico
  ritardo_critico_medio = c(2, 2, 2, 3, 3)
)

# =====================================================
# 3. DISTRIBUZIONE DEL NUMERO DI ESAMI PER ACCETTAZIONE
# =====================================================

distribuzione_n_esami <- tibble(
  n_esami = 1:6,
  probabilita = c(0.25, 0.30, 0.22, 0.13, 0.07, 0.03)
)

# =====================================================
# 4. DISTRIBUZIONE DEL TEMPO DI FIRMA
# =====================================================

distribuzione_tempo_firma <- tibble(
  tempo_firma = c(0, 1, 2),
  probabilita = c(0.30, 0.65, 0.05)
)

# =====================================================
# 5. FUNZIONE DI UNA SINGOLA SIMULAZIONE
# =====================================================

simula_una_volta <- function(id_simulazione) {
  
  # ---------------------------------------------------
  # 5.1 Variabilità tra simulazioni
  # ---------------------------------------------------
  # Introduciamo piccole variazioni casuali nella probabilità
  # di episodio critico, per evitare che ogni simulazione
  # abbia esattamente gli stessi parametri.
  
  tipi_esame_sim <- tipi_esame %>%
    mutate(
      probabilita_critica_sim = pmin(
        pmax(
          rbeta(
            n(),
            shape1 = probabilita_critica_media * 500,
            shape2 = (1 - probabilita_critica_media) * 500
          ),
          0.001
        ),
        0.20
      )
    )
  
  # ---------------------------------------------------
  # 5.2 Simulazione accettazioni
  # ---------------------------------------------------
  
  df_accettazioni_base <- tibble(
    id_simulazione = id_simulazione,
    id_accettazione = 1:n_accettazioni,
    
    n_esami = sample(
      distribuzione_n_esami$n_esami,
      size = n_accettazioni,
      replace = TRUE,
      prob = distribuzione_n_esami$probabilita
    ),
    
    tempo_firma = sample(
      distribuzione_tempo_firma$tempo_firma,
      size = n_accettazioni,
      replace = TRUE,
      prob = distribuzione_tempo_firma$probabilita
    )
  )
  
  # ---------------------------------------------------
  # 5.3 Espansione a livello esame
  # ---------------------------------------------------
  
  df_esami <- df_accettazioni_base %>%
    uncount(n_esami, .id = "progressivo_esame") %>%
    mutate(
      nome_prodotto = sample(
        tipi_esame_sim$nome_prodotto,
        size = n(),
        replace = TRUE,
        prob = tipi_esame_sim$probabilita_richiesta
      )
    ) %>%
    left_join(
      tipi_esame_sim,
      by = "nome_prodotto"
    )
  
  # ---------------------------------------------------
  # 5.4 Simulazione tempo tecnico osservato
  # ---------------------------------------------------
  # Il tempo tecnico non è fisso: viene estratto da una
  # distribuzione normale troncata a valori >= 0.
  # Poi viene arrotondato a giorni interi.
  
  df_esami <- df_esami %>%
    mutate(
      tecnico_osservato_continuo = rnorm(
        n(),
        mean = tecnico_medio,
        sd = tecnico_sd
      ),
      
      tecnico_osservato = pmax(
        0,
        round(tecnico_osservato_continuo)
      )
    )
  
  # ---------------------------------------------------
  # 5.5 Simulazione episodio critico
  # ---------------------------------------------------
  
  df_esami <- df_esami %>%
    mutate(
      episodio_critico = runif(n()) < probabilita_critica_sim,
      
      ritardo_critico = if_else(
        episodio_critico,
        rpois(n(), lambda = ritardo_critico_medio) + 1,
        0L
      ),
      
      tdr_osservato = tecnico_osservato + ritardo_critico,
      
      esame_entra_tdr = tdr_osservato <= target_tdr
    )
  
  # ---------------------------------------------------
  # 5.6 Identificazione esame vincolante
  # ---------------------------------------------------
  
  df_esami <- df_esami %>%
    group_by(id_simulazione, id_accettazione) %>%
    mutate(
      max_tdr_accettazione = max(tdr_osservato, na.rm = TRUE),
      esame_vincolante = tdr_osservato == max_tdr_accettazione
    ) %>%
    ungroup()
  
  # ---------------------------------------------------
  # 5.7 Costruzione dataset accettazioni
  # ---------------------------------------------------
  
  df_accettazioni <- df_esami %>%
    group_by(id_simulazione, id_accettazione) %>%
    summarise(
      n_esami = n(),
      
      max_tdr_osservato = max(tdr_osservato, na.rm = TRUE),
      tecnico_atteso_accettazione = max(tecnico_medio, na.rm = TRUE),
      
      tutti_esami_entra_tdr = all(esame_entra_tdr),
      almeno_un_esame_fuori_tdr = any(!esame_entra_tdr),
      
      episodio_critico_vincolante = any(
        episodio_critico & esame_vincolante,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    left_join(
      df_accettazioni_base %>%
        select(id_simulazione, id_accettazione, tempo_firma),
      by = c("id_simulazione", "id_accettazione")
    ) %>%
    mutate(
      tempo_attesa_utenza = max_tdr_osservato + tempo_firma,
      
      accettazione_entra_target_utenza =
        tempo_attesa_utenza <= target_utenza,
      
      accettazione_fuori_target_utenza =
        !accettazione_entra_target_utenza,
      
      accettazione_entra_target_aggiustato =
        tempo_attesa_utenza <= tecnico_atteso_accettazione,
      
      accettazione_fuori_target_aggiustato =
        !accettazione_entra_target_aggiustato
    )
  
  # ---------------------------------------------------
  # 5.8 Indicatori complessivi della simulazione
  # ---------------------------------------------------
  
  indicatori <- tibble(
    id_simulazione = id_simulazione,
    
    perc_esami_entra_tdr =
      mean(df_esami$esame_entra_tdr, na.rm = TRUE) * 100,
    
    perc_acc_tutti_esami_entra_tdr =
      mean(df_accettazioni$tutti_esami_entra_tdr, na.rm = TRUE) * 100,
    
    perc_acc_entra_target_utenza =
      mean(df_accettazioni$accettazione_entra_target_utenza, na.rm = TRUE) * 100,
    
    perc_acc_entra_target_aggiustato =
      mean(df_accettazioni$accettazione_entra_target_aggiustato, na.rm = TRUE) * 100,
    
    perc_acc_tdr_ok_ma_utenza_fuori =
      mean(
        df_accettazioni$tutti_esami_entra_tdr &
          df_accettazioni$accettazione_fuori_target_utenza,
        na.rm = TRUE
      ) * 100,
    
    perc_acc_critico_vincolante =
      mean(
        df_accettazioni$episodio_critico_vincolante,
        na.rm = TRUE
      ) * 100,
    
    perc_acc_lente_con_critico_vincolante =
      mean(
        df_accettazioni$accettazione_fuori_target_utenza &
          df_accettazioni$episodio_critico_vincolante,
        na.rm = TRUE
      ) * 100
  )
  
  # ---------------------------------------------------
  # 5.9 Analisi per esame
  # ---------------------------------------------------
  
  analisi_esame <- df_esami %>%
    left_join(
      df_accettazioni %>%
        select(
          id_simulazione,
          id_accettazione,
          accettazione_fuori_target_utenza
        ),
      by = c("id_simulazione", "id_accettazione")
    ) %>%
    group_by(id_simulazione, nome_prodotto) %>%
    summarise(
      richieste_esame = n(),
      
      perc_esami_entra_tdr = mean(esame_entra_tdr, na.rm = TRUE) * 100,
      
      volte_vincolante = sum(esame_vincolante, na.rm = TRUE),
      
      volte_vincolante_in_acc_lenta = sum(
        esame_vincolante & accettazione_fuori_target_utenza,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    )
  
  list(
    indicatori = indicatori,
    analisi_esame = analisi_esame
  )
}

# =====================================================
# 6. ESECUZIONE MONTE CARLO
# =====================================================

risultati_lista <- map(
  1:n_simulazioni,
  simula_una_volta
)

indicatori_mc <- map_dfr(
  risultati_lista,
  "indicatori"
)

analisi_esame_mc <- map_dfr(
  risultati_lista,
  "analisi_esame"
)

# =====================================================
# 7. SINTESI DISTRIBUZIONI INDICATORI
# =====================================================

sintesi_indicatori_mc <- indicatori_mc %>%
  pivot_longer(
    cols = -id_simulazione,
    names_to = "indicatore",
    values_to = "valore"
  ) %>%
  group_by(indicatore) %>%
  summarise(
    media = round(mean(valore, na.rm = TRUE), 1),
    mediana = round(median(valore, na.rm = TRUE), 1),
    p05 = round(quantile(valore, 0.05, na.rm = TRUE), 1),
    p25 = round(quantile(valore, 0.25, na.rm = TRUE), 1),
    p75 = round(quantile(valore, 0.75, na.rm = TRUE), 1),
    p95 = round(quantile(valore, 0.95, na.rm = TRUE), 1),
    min = round(min(valore, na.rm = TRUE), 1),
    max = round(max(valore, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(indicatore)

sintesi_indicatori_mc


indicatori_long <- indicatori_mc %>%
  pivot_longer(
    cols = -id_simulazione,
    names_to = "indicatore",
    values_to = "valore"
  )

ggplot(
  indicatori_long,
  aes(
    x = valore
  )
) +
  geom_histogram(
    bins = 30,
    fill = "#2C7FB8",
    color = "white"
  ) +
  facet_wrap(
    ~ indicatore,
    scales = "free"
  ) +
  labs(
    title = "Distribuzione degli indicatori nelle simulazioni Monte Carlo",
    x = "Valore percentuale",
    y = "Numero simulazioni"
  ) +
  theme_minimal()

ggplot(
  indicatori_long,
  aes(
    x = indicatore,
    y = valore
  )
) +
  geom_boxplot(fill = "#A1D99B") +
  coord_flip() +
  labs(
    title = "Variabilità degli indicatori nelle simulazioni",
    x = NULL,
    y = "Valore percentuale"
  ) +
  theme_minimal()


ggplot(
  indicatori_mc,
  aes(
    x = perc_esami_entra_tdr,
    y = perc_acc_entra_target_utenza
  )
) +
  geom_point(alpha = 0.4, color = "#2C7FB8") +
  labs(
    title = "Relazione tra % esami entro TDR e % accettazioni entro target utenza",
    x = "% esami entro target TDR",
    y = "% accettazioni entro target utenza"
  ) +
  theme_minimal()


ggplot(
  indicatori_mc,
  aes(
    x = perc_acc_tdr_ok_ma_utenza_fuori
  )
) +
  geom_histogram(
    bins = 30,
    fill = "#DE2D26",
    color = "white"
  ) +
  labs(
    title = "Accettazioni con tutti gli esami entro TDR ma fuori target utenza",
    x = "% accettazioni",
    y = "Numero simulazioni"
  ) +
  theme_minimal()


sintesi_esame_mc <- analisi_esame_mc %>%
  group_by(nome_prodotto) %>%
  summarise(
    richieste_mediane = median(richieste_esame, na.rm = TRUE),
    
    perc_tdr_mediana = round(
      median(perc_esami_entra_tdr, na.rm = TRUE),
      1
    ),
    
    perc_tdr_p05 = round(
      quantile(perc_esami_entra_tdr, 0.05, na.rm = TRUE),
      1
    ),
    
    perc_tdr_p95 = round(
      quantile(perc_esami_entra_tdr, 0.95, na.rm = TRUE),
      1
    ),
    
    vincolante_lento_mediana = median(
      volte_vincolante_in_acc_lenta,
      na.rm = TRUE
    ),
    
    vincolante_lento_p95 = quantile(
      volte_vincolante_in_acc_lenta,
      0.95,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(desc(vincolante_lento_mediana))

sintesi_esame_mc
