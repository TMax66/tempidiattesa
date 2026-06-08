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