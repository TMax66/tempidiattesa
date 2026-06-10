server <- function(input, output, session) {
  
  # ===================================================
  # SEZIONE CDC ACCETTANTI
  # ===================================================
  
  # ---------------------------------------------------
  # Pulsante pulizia filtri CDC accettanti
  # ---------------------------------------------------
  observeEvent(input$reset_filtri_acc, {
    session$reload()
  })
  
  # ---------------------------------------------------
  # Aggiornamento dinamico filtri organizzativi
  # ---------------------------------------------------
  
  observe({
    
    df_filtri <- kpi_accettazioni
    
    if (!is.null(input$settore_acc) && length(input$settore_acc) > 0) {
      df_filtri <- df_filtri %>%
        filter(settore %in% input$settore_acc)
    }
    
    if (!is.null(input$dipartimento_acc) && length(input$dipartimento_acc) > 0) {
      df_filtri <- df_filtri %>%
        filter(dipartimento %in% input$dipartimento_acc)
    }
    
    if (!is.null(input$struttura_acc) && length(input$struttura_acc) > 0) {
      df_filtri <- df_filtri %>%
        filter(struttura_complessa %in% input$struttura_acc)
    }
    
    if (!is.null(input$nome_cdc_acc) && length(input$nome_cdc_acc) > 0) {
      df_filtri <- df_filtri %>%
        filter(nome_cdc %in% input$nome_cdc_acc)
    }
    
    dip <- df_filtri %>%
      distinct(dipartimento) %>%
      arrange(dipartimento) %>%
      pull(dipartimento)
    
    strutture <- df_filtri %>%
      distinct(struttura_complessa) %>%
      arrange(struttura_complessa) %>%
      pull(struttura_complessa)
    
    nomi_cdc <- df_filtri %>%
      distinct(nome_cdc) %>%
      arrange(nome_cdc) %>%
      pull(nome_cdc)
    
    cdc <- df_filtri %>%
      distinct(cdc_accettante) %>%
      arrange(cdc_accettante) %>%
      pull(cdc_accettante)
    
    dip_sel <- input$dipartimento_acc
    dip_sel <- dip_sel[dip_sel %in% dip]
    
    strutture_sel <- input$struttura_acc
    strutture_sel <- strutture_sel[strutture_sel %in% strutture]
    
    nomi_cdc_sel <- input$nome_cdc_acc
    nomi_cdc_sel <- nomi_cdc_sel[nomi_cdc_sel %in% nomi_cdc]
    
    cdc_sel <- input$cdc_accettante
    cdc_sel <- cdc_sel[cdc_sel %in% cdc]
    
    updateSelectInput(
      session,
      "dipartimento_acc",
      choices = dip,
      selected = dip_sel
    )
    
    updateSelectInput(
      session,
      "struttura_acc",
      choices = strutture,
      selected = strutture_sel
    )
    
    updateSelectInput(
      session,
      "nome_cdc_acc",
      choices = nomi_cdc,
      selected = nomi_cdc_sel
    )
    
    updateSelectInput(
      session,
      "cdc_accettante",
      choices = cdc,
      selected = cdc_sel
    )
  })
  
  # ---------------------------------------------------
  # Dataset filtrato CDC accettanti
  # ---------------------------------------------------
  
  dati_accettanti <- reactive({
    
    req(input$tipo_accettazione_acc)
    
    df <- kpi_accettazioni %>%
      filter(
        tipo_accettazione %in% input$tipo_accettazione_acc,
        data_acc >= input$periodo_acc[1],
        data_acc <= input$periodo_acc[2]
      )
    
    if (!is.null(input$settore_acc) && length(input$settore_acc) > 0) {
      df <- df %>%
        filter(settore %in% input$settore_acc)
    }
    
    if (!is.null(input$dipartimento_acc) && length(input$dipartimento_acc) > 0) {
      df <- df %>%
        filter(dipartimento %in% input$dipartimento_acc)
    }
    
    if (!is.null(input$struttura_acc) && length(input$struttura_acc) > 0) {
      df <- df %>%
        filter(struttura_complessa %in% input$struttura_acc)
    }
    
    if (!is.null(input$nome_cdc_acc) && length(input$nome_cdc_acc) > 0) {
      df <- df %>%
        filter(nome_cdc %in% input$nome_cdc_acc)
    }
    
    if (!is.null(input$cdc_accettante) && length(input$cdc_accettante) > 0) {
      df <- df %>%
        filter(cdc_accettante %in% input$cdc_accettante)
    }
    
    df
  })
  
  # ---------------------------------------------------
  # Dataset filtrato CDC accettanti - accettazioni IN aggiustate
  # ---------------------------------------------------
  
  dati_accettanti_in_adj <- reactive({
    
    df <- kpi_accettazioni_in_adj %>%
      filter(
        data_acc >= input$periodo_acc[1],
        data_acc <= input$periodo_acc[2]
      )
    
    if (!is.null(input$settore_acc) && length(input$settore_acc) > 0) {
      df <- df %>%
        filter(settore %in% input$settore_acc)
    }
    
    if (!is.null(input$dipartimento_acc) && length(input$dipartimento_acc) > 0) {
      df <- df %>%
        filter(dipartimento %in% input$dipartimento_acc)
    }
    
    if (!is.null(input$struttura_acc) && length(input$struttura_acc) > 0) {
      df <- df %>%
        filter(struttura_complessa %in% input$struttura_acc)
    }
    
    if (!is.null(input$nome_cdc_acc) && length(input$nome_cdc_acc) > 0) {
      df <- df %>%
        filter(nome_cdc %in% input$nome_cdc_acc)
    }
    
    if (!is.null(input$cdc_accettante) && length(input$cdc_accettante) > 0) {
      df <- df %>%
        filter(cdc_accettante %in% input$cdc_accettante)
    }
    
    df %>%
      aggiungi_gruppo_org(input$livello_org_acc)
  })
  
  # dataset diagnostica cdc
  
  diag_acc_cdc <- reactive({
    
    req(input$diag_cdc)
    req(input$diag_benchmark)
    req(input$diag_periodo)
    
    benchmark_col <- input$diag_benchmark
    
    kpi_accettazioni_in_adj %>%
      mutate(
        data_acc = as.Date(data_acc),
        data_rdp = as.Date(data_rdp),
        
        tecnico_atteso_target = .data[[benchmark_col]],
        target_aggiustato = tecnico_atteso_target,
        
        entro_target_aggiustato =
          tempo_attesa_utente <= target_aggiustato,
        
        fuori_target_aggiustato =
          !entro_target_aggiustato,
        
        scostamento_vs_target =
          tempo_attesa_utente - target_aggiustato,
        
        entro_target_utenza =
          tempo_attesa_utente <= input$diag_target_utenza
      ) %>%
      filter(
        tipo_accettazione == "IN",
        data_acc >= input$diag_periodo[1],
        data_acc <= input$diag_periodo[2],
        nome_cdc == input$diag_cdc
      )
  })
  
  
  output$diag_sintesi_cdc <- renderTable({
    
    df <- diag_acc_cdc()
    
    validate(
      need(nrow(df) > 0, "Nessuna accettazione disponibile per il CDC selezionato.")
    )
    
    df %>%
      summarise(
        CDC = first(nome_cdc),
        Accettazioni_IN = n(),
        
        Entro_target_aggiustato = sum(
          entro_target_aggiustato,
          na.rm = TRUE
        ),
        
        Perc_entro_target_aggiustato = round(
          100 * Entro_target_aggiustato / Accettazioni_IN,
          1
        ),
        
        Fuori_target_aggiustato = sum(
          fuori_target_aggiustato,
          na.rm = TRUE
        ),
        
        Perc_fuori_target_aggiustato = round(
          100 * Fuori_target_aggiustato / Accettazioni_IN,
          1
        ),
        
        Mediana_attesa_utente = round(
          median(tempo_attesa_utente, na.rm = TRUE),
          1
        ),
        
        Mediana_target_aggiustato = round(
          median(target_aggiustato, na.rm = TRUE),
          1
        ),
        
        Mediana_scostamento = round(
          median(scostamento_vs_target, na.rm = TRUE),
          1
        ),
        
        P75_scostamento = round(
          q_na(scostamento_vs_target, 0.75),
          1
        ),
        
        P90_scostamento = round(
          q_na(scostamento_vs_target, 0.90),
          1
        ),
        
        Max_scostamento = round(
          max_na_num(scostamento_vs_target),
          1
        )
      )
  })
  
  output$diag_tab_scostamenti <- renderTable({
    
    df <- diag_acc_cdc()
    
    validate(
      need(nrow(df) > 0, "Nessun dato disponibile.")
    )
    
    df %>%
      mutate(
        classe_scostamento = case_when(
          scostamento_vs_target <= 0 ~ "Entro target",
          scostamento_vs_target == 1 ~ "+1 giorno",
          scostamento_vs_target == 2 ~ "+2 giorni",
          scostamento_vs_target >= 3 ~ ">= +3 giorni",
          TRUE ~ "Non classificato"
        ),
        classe_scostamento = factor(
          classe_scostamento,
          levels = c(
            "Entro target",
            "+1 giorno",
            "+2 giorni",
            ">= +3 giorni",
            "Non classificato"
          )
        )
      ) %>%
      count(classe_scostamento) %>%
      mutate(
        Percentuale = round(100 * n / sum(n), 1)
      ) %>%
      rename(
        Classe_scostamento = classe_scostamento,
        Accettazioni = n
      )
  })
  
  
  output$diag_plot_scostamenti <- renderPlot({
    
    df <- diag_acc_cdc()
    
    validate(
      need(nrow(df) > 0, "Nessun dato disponibile.")
    )
    
    df_plot <- df %>%
      mutate(
        classe_scostamento = case_when(
          scostamento_vs_target <= 0 ~ "Entro target",
          scostamento_vs_target == 1 ~ "+1 giorno",
          scostamento_vs_target == 2 ~ "+2 giorni",
          scostamento_vs_target >= 3 ~ ">= +3 giorni",
          TRUE ~ "Non classificato"
        ),
        classe_scostamento = factor(
          classe_scostamento,
          levels = c(
            "Entro target",
            "+1 giorno",
            "+2 giorni",
            ">= +3 giorni",
            "Non classificato"
          )
        )
      ) %>%
      count(classe_scostamento) %>%
      mutate(
        perc = round(100 * n / sum(n), 1)
      )
    
    ggplot(
      df_plot,
      aes(
        x = classe_scostamento,
        y = perc
      )
    ) +
      geom_col(fill = "#2C7FB8") +
      geom_text(
        aes(label = paste0(perc, "%")),
        vjust = -0.3,
        size = 4
      ) +
      labs(
        x = "Classe di scostamento",
        y = "% accettazioni",
        title = "Distribuzione degli scostamenti dal target aggiustato"
      ) +
      theme_minimal()
  })
  
  output$diag_tab_settore <- renderTable({
    
    df <- diag_acc_cdc()
    
    validate(
      need(nrow(df) > 0, "Nessun dato disponibile.")
    )
    
    df %>%
      group_by(settore) %>%
      summarise(
        Accettazioni_IN = n(),
        
        Entro_target = sum(
          entro_target_aggiustato,
          na.rm = TRUE
        ),
        
        Perc_entro_target = round(
          100 * Entro_target / Accettazioni_IN,
          1
        ),
        
        Fuori_target = sum(
          fuori_target_aggiustato,
          na.rm = TRUE
        ),
        
        Perc_fuori_target = round(
          100 * Fuori_target / Accettazioni_IN,
          1
        ),
        
        Mediana_attesa = round(
          median(tempo_attesa_utente, na.rm = TRUE),
          1
        ),
        
        Mediana_target_aggiustato = round(
          median(target_aggiustato, na.rm = TRUE),
          1
        ),
        
        Mediana_scostamento = round(
          median(scostamento_vs_target, na.rm = TRUE),
          1
        ),
        
        P90_scostamento = round(
          q_na(scostamento_vs_target, 0.90),
          1
        ),
        
        .groups = "drop"
      ) %>%
      arrange(Perc_entro_target)
  })
  
  output$diag_tab_complessita <- renderTable({
    
    df <- diag_acc_cdc()
    
    validate(
      need(nrow(df) > 0, "Nessun dato disponibile.")
    )
    
    df %>%
      mutate(
        classe_complessita = case_when(
          target_aggiustato <= 1 ~ "Bassa",
          target_aggiustato <= 3 ~ "Media",
          target_aggiustato <= 5 ~ "Alta",
          target_aggiustato > 5 ~ "Molto alta",
          TRUE ~ "Non classificato"
        ),
        classe_complessita = factor(
          classe_complessita,
          levels = c(
            "Bassa",
            "Media",
            "Alta",
            "Molto alta",
            "Non classificato"
          )
        )
      ) %>%
      group_by(classe_complessita) %>%
      summarise(
        Accettazioni = n(),
        
        Perc_entro_target = round(
          100 * mean(entro_target_aggiustato, na.rm = TRUE),
          1
        ),
        
        Mediana_attesa = round(
          median(tempo_attesa_utente, na.rm = TRUE),
          1
        ),
        
        Mediana_target_aggiustato = round(
          median(target_aggiustato, na.rm = TRUE),
          1
        ),
        
        Mediana_scostamento = round(
          median(scostamento_vs_target, na.rm = TRUE),
          1
        ),
        
        .groups = "drop"
      )
  })
  
  diag_esami_cdc <- reactive({
    
    acc <- diag_acc_cdc()
    
    validate(
      need(nrow(acc) > 0, "Nessuna accettazione disponibile.")
    )
    
    df_app %>%
      mutate(
        data_acc_d = as.Date(data_acc_d),
        data_rdp_d = as.Date(data_rdp_d),
        inizio_analisi_d = as.Date(inizio_analisi),
        fine_analisi_d = as.Date(fine_analisi),
        
        durata_tecnica_effettiva = giorni_lavorativi(
          inizio_analisi_d,
          fine_analisi_d
        )
      ) %>%
      semi_join(
        acc %>%
          select(anno_accettaz, numero_accettaz),
        by = c("anno_accettaz", "numero_accettaz")
      )
  })
  
  
  diag_accettazioni_complete <- reactive({
    
    acc <- diag_acc_cdc()
    esami <- diag_esami_cdc()
    
    esami_vincolanti <- esami %>%
      group_by(anno_accettaz, numero_accettaz) %>%
      mutate(
        fine_analisi_max_acc = max(
          fine_analisi_d,
          na.rm = TRUE
        ),
        esame_vincolante_effettivo =
          fine_analisi_d == fine_analisi_max_acc
      ) %>%
      ungroup() %>%
      filter(esame_vincolante_effettivo) %>%
      group_by(anno_accettaz, numero_accettaz) %>%
      slice_max(
        order_by = durata_tecnica_effettiva,
        n = 1,
        with_ties = FALSE
      ) %>%
      ungroup() %>%
      select(
        anno_accettaz,
        numero_accettaz,
        nome_prodotto_vincolante = nome_prodotto,
        descrizione_prodotto_vincolante = descrizione_prodotto,
        inizio_analisi_vincolante = inizio_analisi_d,
        fine_analisi_vincolante = fine_analisi_d,
        durata_tecnica_vincolante = durata_tecnica_effettiva
      )
    
    acc %>%
      left_join(
        esami_vincolanti,
        by = c("anno_accettaz", "numero_accettaz")
      ) %>%
      mutate(
        tempo_pre_analitico = giorni_lavorativi(
          data_acc,
          inizio_analisi_vincolante
        ),
        
        tempo_post_analitico = giorni_lavorativi(
          fine_analisi_vincolante,
          data_rdp
        ),
        
        extra_tecnico_effettivo_vs_target =
          durata_tecnica_vincolante - target_aggiustato,
        
        componente_pre = pmax(
          0,
          tempo_pre_analitico
        ),
        
        componente_tecnica_extra = pmax(
          0,
          extra_tecnico_effettivo_vs_target
        ),
        
        componente_post = pmax(
          0,
          tempo_post_analitico
        ),
        
        causa_probabile = case_when(
          !fuori_target_aggiustato ~ "Entro target",
          
          componente_pre >= componente_tecnica_extra &
            componente_pre >= componente_post &
            componente_pre > input$diag_soglia_pre ~
            "Pre-analitico / programmazione",
          
          componente_post >= componente_pre &
            componente_post >= componente_tecnica_extra &
            componente_post > input$diag_soglia_firma ~
            "Post-analitico / firma",
          
          componente_tecnica_extra > 0 &
            componente_tecnica_extra >= componente_pre &
            componente_tecnica_extra >= componente_post ~
            "Durata tecnica effettiva superiore al benchmark",
          
          fuori_target_aggiustato &
            componente_pre <= input$diag_soglia_pre &
            componente_post <= input$diag_soglia_firma &
            componente_tecnica_extra <= 0 ~
            "Scostamento lieve / soglia tecnica",
          
          TRUE ~ "Da approfondire"
        )
      )
  })
  
  output$diag_tab_cause <- renderTable({
    
    df <- diag_accettazioni_complete()
    
    validate(
      need(nrow(df) > 0, "Nessun dato disponibile.")
    )
    
    df %>%
      filter(fuori_target_aggiustato) %>%
      count(causa_probabile) %>%
      mutate(
        Percentuale = round(100 * n / sum(n), 1)
      ) %>%
      rename(
        Causa_probabile = causa_probabile,
        Accettazioni = n
      ) %>%
      arrange(desc(Accettazioni))
  })
  
  
  output$diag_tab_esami_vincolanti <- renderTable({
    
    df <- diag_accettazioni_complete()
    
    validate(
      need(nrow(df) > 0, "Nessun dato disponibile.")
    )
    
    totale_fuori <- sum(df$fuori_target_aggiustato, na.rm = TRUE)
    
    df %>%
      filter(fuori_target_aggiustato) %>%
      group_by(
        nome_prodotto_vincolante,
        descrizione_prodotto_vincolante
      ) %>%
      summarise(
        Accettazioni_fuori_target = n(),
        
        Perc_su_fuori_target = round(
          100 * Accettazioni_fuori_target / totale_fuori,
          1
        ),
        
        Mediana_attesa_utente = round(
          median(tempo_attesa_utente, na.rm = TRUE),
          1
        ),
        
        Mediana_target_aggiustato = round(
          median(target_aggiustato, na.rm = TRUE),
          1
        ),
        
        Mediana_scostamento = round(
          median(scostamento_vs_target, na.rm = TRUE),
          1
        ),
        
        Mediana_pre_analitico = round(
          median(tempo_pre_analitico, na.rm = TRUE),
          1
        ),
        
        Mediana_durata_tecnica_vincolante = round(
          median(durata_tecnica_vincolante, na.rm = TRUE),
          1
        ),
        
        Mediana_post_analitico = round(
          median(tempo_post_analitico, na.rm = TRUE),
          1
        ),
        
        .groups = "drop"
      ) %>%
      arrange(desc(Accettazioni_fuori_target)) %>%
      head(20)
  })
  
  output$diag_tab_casi_sentinella <- renderTable({
    
    df <- diag_accettazioni_complete()
    
    validate(
      need(nrow(df) > 0, "Nessun dato disponibile.")
    )
    
    df %>%
      filter(fuori_target_aggiustato) %>%
      arrange(desc(scostamento_vs_target)) %>%
      select(
        anno_accettaz,
        numero_accettaz,
        settore,
        data_acc,
        data_rdp,
        tempo_attesa_utente,
        target_aggiustato,
        scostamento_vs_target,
        n_esami,
        nome_prodotto_vincolante,
        inizio_analisi_vincolante,
        fine_analisi_vincolante,
        tempo_pre_analitico,
        durata_tecnica_vincolante,
        tempo_post_analitico,
        causa_probabile
      )
  })
  # ---------------------------------------------------
  # Tabella statistiche tempo attesa utente
  # ---------------------------------------------------
  
  output$tab_sintesi_acc <- renderTable({
    
    df <- dati_accettanti() %>%
      aggiungi_gruppo_org(input$livello_org_acc)
    
    validate(
      need(nrow(df) > 0, "Nessun dato disponibile per i filtri selezionati.")
    )
    
    df %>%
      group_by(gruppo_org, tipo_accettazione) %>%
      summarise(
        Accettazioni = n(),
        Media_attesa = round(mean(tempo_attesa_utente, na.rm = TRUE), 2),
        Mediana_attesa = round(median(tempo_attesa_utente, na.rm = TRUE), 2),
        P75_attesa = round(q_na(tempo_attesa_utente, 0.75), 2),
        P90_attesa = round(q_na(tempo_attesa_utente, 0.90), 2),
        Min_attesa = round(min_na(tempo_attesa_utente), 2),
        Max_attesa = round(max_na(tempo_attesa_utente), 2),
        .groups = "drop"
      ) %>%
      rename(
        Livello = gruppo_org,
        Tipologia = tipo_accettazione
      ) %>%
      arrange(Livello, Tipologia)
  })
  
  
  
  
  
  
  # ---------------------------------------------------
  # Dataset valutazione target
  # ---------------------------------------------------
  
  dati_valutazione_acc <- reactive({
    
    # ===================================================
    # 1. Valutazione grezza rispetto al target utenza
    #    SOLO ACCETTAZIONI IN
    # ===================================================
    
    df_grezzo <- dati_accettanti() %>%
      filter(tipo_accettazione == "IN") %>%
      aggiungi_gruppo_org(input$livello_org_acc)
    
    validate(
      need(
        nrow(df_grezzo) > 0,
        "Nessuna accettazione IN disponibile per i filtri selezionati."
      )
    )
    
    valutazione_grezza <- df_grezzo %>%
      mutate(
        target_giorni_utenza = input$target_giorni_in_acc,
        target_percentuale = input$target_perc_in_acc,
        entro_target_utenza = tempo_attesa_utente <= target_giorni_utenza
      ) %>%
      group_by(
        gruppo_org,
        tipo_accettazione,
        target_giorni_utenza,
        target_percentuale
      ) %>%
      summarise(
        Accettazioni = n(),
        Entro_target_utenza = sum(entro_target_utenza, na.rm = TRUE),
        Perc_entro_target_utenza = round(
          100 * Entro_target_utenza / Accettazioni,
          1
        ),
        Mediana_attesa = round(median(tempo_attesa_utente, na.rm = TRUE), 1),
        P90_attesa = round(q_na(tempo_attesa_utente, 0.90), 1),
        .groups = "drop"
      )
    
    # ===================================================
    # 2. Valutazione aggiustata per complessità tecnica
    #    SOLO ACCETTAZIONI IN
    # ===================================================
    
    df_adj <- dati_accettanti_in_adj()
    
    valutazione_adj <- df_adj %>%
      mutate(
        tecnico_atteso = .data[[benchmark_tecnico_fisso]],
        target_aggiustato = tecnico_atteso + margine_tecnico_fisso,
        entro_target_aggiustato = tempo_attesa_utente <= target_aggiustato,
        tipo_accettazione = "IN"
      ) %>%
      filter(!is.na(tecnico_atteso)) %>%
      group_by(gruppo_org, tipo_accettazione) %>%
      summarise(
        Accettazioni_adj = n(),
        Mediana_tecnico_atteso = round(
          median(tecnico_atteso, na.rm = TRUE),
          1
        ),
        Mediana_target_aggiustato = round(
          median(target_aggiustato, na.rm = TRUE),
          1
        ),
        P25_target_aggiustato = round(
          q_na(target_aggiustato, 0.25),
          1
        ),
        P75_target_aggiustato = round(
          q_na(target_aggiustato, 0.75),
          1
        ),
        Perc_target_aggiustato_sotto_target_utenza = round(
          100 * mean(
            target_aggiustato < input$target_giorni_in_acc,
            na.rm = TRUE
          ),
          1
        ),
        Entro_target_aggiustato = sum(
          entro_target_aggiustato,
          na.rm = TRUE
        ),
        Perc_entro_target_aggiustato = round(
          100 * Entro_target_aggiustato / Accettazioni_adj,
          1
        ),
        .groups = "drop"
      )
    
    # ===================================================
    # 3. Unione valutazione grezza + aggiustata
    # ===================================================
    
    valutazione_grezza %>%
      left_join(
        valutazione_adj,
        by = c("gruppo_org", "tipo_accettazione")
      ) %>%
      mutate(
        Scostamento_target_utenza = round(
          Perc_entro_target_utenza - target_percentuale,
          1
        ),
        
        Valutazione_utenza = case_when(
          Perc_entro_target_utenza >= target_percentuale ~ "Buono",
          Perc_entro_target_utenza >= target_percentuale - 5 ~ "Attenzione",
          TRUE ~ "Critico"
        ),
        
        Scostamento_target_aggiustato = round(
          Perc_entro_target_aggiustato - target_percentuale,
          1
        ),
        
        Valutazione_aggiustata = case_when(
          is.na(Perc_entro_target_aggiustato) ~ NA_character_,
          Perc_entro_target_aggiustato >= target_percentuale ~ "Buono",
          Perc_entro_target_aggiustato >= target_percentuale - 5 ~ "Attenzione",
          TRUE ~ "Critico"
        ),
        
        Valutazione_utenza_label = case_when(
          Valutazione_utenza == "Buono" ~ "🟢 Buono",
          Valutazione_utenza == "Attenzione" ~ "🟡 Attenzione",
          Valutazione_utenza == "Critico" ~ "🔴 Critico"
        ),
        
        Valutazione_aggiustata_label = case_when(
          Valutazione_aggiustata == "Buono" ~ "🟢 Buono",
          Valutazione_aggiustata == "Attenzione" ~ "🟡 Attenzione",
          Valutazione_aggiustata == "Critico" ~ "🔴 Critico",
          TRUE ~ "-"
        )
      )
  })
  
  # ---------------------------------------------------
  # Tabella valutazione target
  # ---------------------------------------------------
  
  output$tab_valutazione_acc <- renderTable({
    
    dati_valutazione_acc() %>%
      rename(
        Livello = gruppo_org,
        Tipologia = tipo_accettazione,
        Target_giorni_utenza = target_giorni_utenza,
        Target_percentuale = target_percentuale
      ) %>%
      select(
        Livello,
        Tipologia,
        Accettazioni,
        Target_giorni_utenza,
        Target_percentuale,
        Perc_entro_target_utenza,
        Valutazione_utenza = Valutazione_utenza_label,
        #Accettazioni_adj,
        Mediana_tecnico_atteso,
        Mediana_target_aggiustato,
        #P25_target_aggiustato,
        #P75_target_aggiustato,
        #Perc_target_aggiustato_sotto_target_utenza,
        Perc_entro_target_aggiustato,
        Valutazione_aggiustata = Valutazione_aggiustata_label
      ) %>%
      arrange(Valutazione_utenza, Livello)
  })
  
  # ---------------------------------------------------
  # Grafico valutazione target
  # ---------------------------------------------------
  
  output$plot_valutazione_acc <- renderPlot({
    
    df <- dati_valutazione_acc()
    
    validate(
      need(
        nrow(df) > 0,
        "Nessun dato disponibile per il grafico di valutazione."
      )
    )
    
    df_utenza <- df %>%
      transmute(
        gruppo_org,
        indicatore = "Target tempo utenza",
        Percentuale = Perc_entro_target_utenza,
        Target = target_percentuale,
        Valutazione = Valutazione_utenza
      )
    
    df_aggiustato <- df %>%
      transmute(
        gruppo_org,
        indicatore = "Target aggiustato complessità",
        Percentuale = Perc_entro_target_aggiustato,
        Target = target_percentuale,
        Valutazione = Valutazione_aggiustata
      ) %>%
      filter(!is.na(Percentuale))
    
    df_plot <- bind_rows(df_utenza, df_aggiustato) %>%
      mutate(
        gruppo_plot = reorder(gruppo_org, Percentuale)
      )
    
    validate(
      need(nrow(df_plot) > 0, "Nessun dato disponibile per il grafico.")
    )
    
    ggplot(
      df_plot,
      aes(
        x = Percentuale,
        y = gruppo_plot,
        fill = Valutazione
      )
    ) +
      geom_col(width = 0.3) +
      geom_vline(
        aes(xintercept = Target),
        linetype = "dashed",
        color = "black",
        linewidth = 0.8
      ) +
      geom_text(
        aes(label = paste0(Percentuale, "%")),
        hjust = -0.1,
        size = 3.4
      ) +
      facet_wrap(
        ~ indicatore,
        scales = "free_y"
      ) +
      scale_fill_manual(
        values = c(
          "Buono" = "#2ca25f",
          "Attenzione" = "#fec44f",
          "Critico" = "#de2d26"
        ),
        na.value = "grey80"
      ) +
      scale_x_continuous(
        limits = c(0, 105),
        breaks = seq(0, 100, 10)
      ) +
      labs(
        x = "% accettazioni IN entro target",
        y = "Livello organizzativo",
        fill = "Valutazione",
        title = "Valutazione dei tempi di attesa - Accettazioni IN",
        subtitle = "Confronto tra target utenza e target aggiustato per complessità tecnica"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        strip.text = element_text(size = 10),
        axis.text.y = element_text(size = 8)
      )
  })
  # ---------------------------------------------------
  # Grafico trend tempo attesa
  # ---------------------------------------------------
  
  output$plot_trend_attesa_acc <- renderPlot({
    
    df <- dati_accettanti() %>%
      aggiungi_gruppo_org(input$livello_org_acc)
    
    validate(
      need(nrow(df) > 0, "Nessun dato disponibile per i filtri selezionati.")
    )
    
    df_plot <- df %>%
      group_by(settimana, gruppo_org, tipo_accettazione) %>%
      summarise(
        n_accettazioni = n(),
        mediana_attesa = median(tempo_attesa_utente, na.rm = TRUE),
        media_attesa = mean(tempo_attesa_utente, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        pannello = paste(gruppo_org, "|", tipo_accettazione)
      )
    
    validate(
      need(nrow(df_plot) > 0, "Nessun dato disponibile per il grafico.")
    )
    
    df_linee <- df_plot %>%
      group_by(pannello) %>%
      filter(n() >= 2) %>%
      ungroup()
    
    ggplot(
      df_plot,
      aes(
        x = settimana,
        y = mediana_attesa
      )
    ) +
      geom_line(
        data = df_linee,
        aes(group = pannello),
        linewidth = 1,
        color = "#2C7FB8"
      ) +
      geom_point(
        aes(size = n_accettazioni),
        alpha = 0.8,
        color = "#2C7FB8"
      ) +
      facet_wrap(
        ~ pannello,
        scales = "free_y",
        ncol = 2
      ) +
      labs(
        x = "Settimana",
        y = "Tempo di attesa utente, giorni lavorativi - mediana",
        size = "N. accettazioni",
        title = "Trend settimanale del tempo di attesa utente",
        subtitle = "Pannelli costruiti secondo il livello organizzativo selezionato"
      ) +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # ---------------------------------------------------
  # Dettaglio accettazioni
  # ---------------------------------------------------
  
  output$tab_dettaglio_acc <- renderTable({
    
    req(input$mostra_dettaglio_acc)
    
    df <- dati_accettanti()
    
    validate(
      need(nrow(df) > 0, "Nessun dato disponibile per i filtri selezionati.")
    )
    
    df %>%
      arrange(desc(tempo_attesa_utente)) %>%
      mutate(
        data_acc = format(data_acc, "%d/%m/%Y"),
        data_rdp = format(data_rdp, "%d/%m/%Y")
      ) %>%
      select(
        anno_accettaz,
        numero_accettaz,
        settore,
        dipartimento,
        struttura_complessa,
        nome_cdc,
        cdc_accettante,
        tipo_accettazione,
        data_acc,
        data_rdp,
        tempo_attesa_utente,
        n_righe_dataset,
        n_esami,
        n_esami_out
      ) %>%
      head(100)
  })
}