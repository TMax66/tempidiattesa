library(shiny)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(here)

# =====================================================
# 1. CARICAMENTO DATASET
# =====================================================

kpi_accettazioni <- readRDS(here("dati", "kpi_accettazioni.rds"))
anagrafica_cdc <- readRDS( here("dati", "anagrafica_cdc.rds"))
kpi_accettazioni_in_adj <- readRDS(here("dati", "kpi_accettazioni_in_adj.rds"))


# =====================================================
# PARAMETRI BACK-END PER AGGIUSTAMENTO TECNICO
# =====================================================

benchmark_tecnico_fisso <- "tecnico_atteso_p90"
margine_tecnico_fisso <- 0


# =====================================================
# 2. FUNZIONI DI SUPPORTO
# =====================================================

converti_data <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1970-01-01"))
  }
  
  as.Date(x)
}

q_na <- function(x, p) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  as.numeric(quantile(x, p, na.rm = TRUE))
}

min_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  min(x, na.rm = TRUE)
}

max_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

aggiungi_gruppo_org <- function(df, livello) {
  
  if (livello == "totale") {
    df %>%
      mutate(gruppo_org = "Totale")
    
  } else if (livello == "dipartimento") {
    df %>%
      mutate(gruppo_org = dipartimento)
    
  } else if (livello == "struttura_complessa") {
    df %>%
      mutate(gruppo_org = struttura_complessa)
    
  } else if (livello == "nome_cdc") {
    df %>%
      mutate(gruppo_org = nome_cdc)
    
  } else {
    df %>%
      mutate(gruppo_org = cdc_accettante)
  }
}


q_na <- function(x, p) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  as.numeric(quantile(x, p, na.rm = TRUE))
}

max_na_num <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}


# =====================================================
# 3. NORMALIZZAZIONE ANAGRAFICA CDC
# =====================================================
# Se nel tuo file i nomi colonna sono diversi, modifica questo blocco.

anagrafica_cdc <- anagrafica_cdc %>%
  transmute(
    cdc = as.character(cdc),
    nome_cdc = as.character(nome_cdc),
    struttura_complessa = as.character(struttura_complessa),
    dipartimento = as.character(dipartimento)
  ) %>%
  distinct(cdc, .keep_all = TRUE)

# =====================================================
# 4. NORMALIZZAZIONE KPI ACCETTAZIONI
# =====================================================

kpi_accettazioni <- kpi_accettazioni %>%
  mutate(
    data_acc = converti_data(data_acc),
    data_rdp = converti_data(data_rdp),
    settimana = converti_data(settimana),
    
    settore = as.character(settore),
    cdc_accettante = as.character(cdc_accettante),
    tipo_accettazione = as.character(tipo_accettazione),
    
    tempo_attesa_utente = as.numeric(tempo_attesa_utente)
  )

# =====================================================
# 5. JOIN CON ANAGRAFICA ORGANIZZATIVA
# =====================================================


kpi_accettazioni <- kpi_accettazioni %>%
  left_join(
    anagrafica_cdc,
    by = c("cdc_accettante" = "cdc")
  ) %>%
  mutate(
    nome_cdc = if_else(is.na(nome_cdc), cdc_accettante, nome_cdc),
    struttura_complessa = if_else(
      is.na(struttura_complessa),
      "Non classificato",
      struttura_complessa
    ),
    dipartimento = if_else(
      is.na(dipartimento),
      "Non classificato",
      dipartimento
    )
  )

# ====================================================
# KPI accettazione adjustata per complessita'
# ====================================================

kpi_accettazioni_in_adj <- kpi_accettazioni_in_adj %>%
  mutate(
    data_acc = converti_data(data_acc),
    data_rdp = converti_data(data_rdp),
    
    settore = as.character(settore),
    cdc_accettante = as.character(cdc_accettante),
    tipo_accettazione = as.character(tipo_accettazione),
    
    tempo_attesa_utente = as.numeric(tempo_attesa_utente),
    tecnico_atteso_mediano = as.numeric(tecnico_atteso_mediano),
    tecnico_atteso_p75 = as.numeric(tecnico_atteso_p75),
    tecnico_atteso_p90 = as.numeric(tecnico_atteso_p90),
    tecnico_effettivo_max = as.numeric(tecnico_effettivo_max)
  ) %>%
  left_join(
    anagrafica_cdc,
    by = c("cdc_accettante" = "cdc")
  ) %>%
  mutate(
    nome_cdc = if_else(is.na(nome_cdc), cdc_accettante, nome_cdc),
    struttura_complessa = if_else(is.na(struttura_complessa), "Non classificato", struttura_complessa),
    dipartimento = if_else(is.na(dipartimento), "Non classificato", dipartimento)
  )


# =====================================================
# 6. CONTROLLI MINIMI SUL DATASET
# =====================================================

colonne_necessarie <- c(
  "anno_accettaz",
  "numero_accettaz",
  "settore",
  "cdc_accettante",
  "nome_cdc",
  "struttura_complessa",
  "dipartimento",
  "tipo_accettazione",
  "data_acc",
  "data_rdp",
  "tempo_attesa_utente",
  "settimana"
)

mancanti <- setdiff(colonne_necessarie, names(kpi_accettazioni))

if (length(mancanti) > 0) {
  stop(
    paste(
      "Nel dataset kpi_accettazioni mancano queste colonne:",
      paste(mancanti, collapse = ", ")
    )
  )
}

# =====================================================
# 7. VARIABILI OPZIONALI PER IL DETTAGLIO
# =====================================================

if (!"n_righe_dataset" %in% names(kpi_accettazioni)) {
  if ("n_righe" %in% names(kpi_accettazioni)) {
    kpi_accettazioni <- kpi_accettazioni %>%
      mutate(n_righe_dataset = n_righe)
  } else {
    kpi_accettazioni <- kpi_accettazioni %>%
      mutate(n_righe_dataset = NA_integer_)
  }
}

if (!"n_esami" %in% names(kpi_accettazioni)) {
  kpi_accettazioni <- kpi_accettazioni %>%
    mutate(n_esami = NA_integer_)
}

if (!"n_esami_out" %in% names(kpi_accettazioni)) {
  kpi_accettazioni <- kpi_accettazioni %>%
    mutate(n_esami_out = NA_integer_)
}

# =====================================================
# 8. LISTE PER INPUT UI
# =====================================================

lista_settori_acc <- sort(unique(kpi_accettazioni$settore))
lista_dipartimenti_acc <- sort(unique(kpi_accettazioni$dipartimento))
lista_strutture_acc <- sort(unique(kpi_accettazioni$struttura_complessa))
lista_nome_cdc_acc <- sort(unique(kpi_accettazioni$nome_cdc))
lista_cdc_accettanti <- sort(unique(kpi_accettazioni$cdc_accettante))

data_min_acc <- min(kpi_accettazioni$data_acc, na.rm = TRUE)
data_max_acc <- max(kpi_accettazioni$data_acc, na.rm = TRUE)