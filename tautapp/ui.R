ui <- navbarPage(
  
  title = "Monitoraggio accettazioni IN/OUT",
  
  # ===================================================
  # PAGINA 1 - CDC ACCETTANTI
  # ===================================================
  
  tabPanel(
    title = "CDC accettanti",
    
    sidebarLayout(
      
      sidebarPanel(
        
        h4("Filtri organizzativi"),
        
        actionButton(
          inputId = "reset_filtri_acc",
          label = "Pulisci filtri"
        ),
        
        br(),
        br(),
        
        selectInput(
          inputId = "livello_org_acc",
          label = "Livello di aggregazione:",
          choices = c(
            "Totale" = "totale",
            "Dipartimento" = "dipartimento",
            "Struttura complessa" = "struttura_complessa",
            "Centro di costo" = "nome_cdc",
            "CDC accettante" = "cdc_accettante"
          ),
          selected = "totale"
        ),
        
        selectInput(
          inputId = "settore_acc",
          label = "Settore (vuoto = tutti):",
          choices = lista_settori_acc,
          selected = character(0),
          multiple = TRUE
        ),
        
        selectInput(
          inputId = "dipartimento_acc",
          label = "Dipartimento (vuoto = tutti):",
          choices = lista_dipartimenti_acc,
          selected = character(0),
          multiple = TRUE
        ),
        
        selectInput(
          inputId = "struttura_acc",
          label = "Struttura complessa (vuoto = tutte):",
          choices = lista_strutture_acc,
          selected = character(0),
          multiple = TRUE
        ),
        
        selectInput(
          inputId = "nome_cdc_acc",
          label = "Centro di costo (vuoto = tutti):",
          choices = lista_nome_cdc_acc,
          selected = character(0),
          multiple = TRUE
        ),
        
        selectInput(
          inputId = "cdc_accettante",
          label = "CDC accettante (vuoto = tutti):",
          choices = lista_cdc_accettanti,
          selected = character(0),
          multiple = TRUE
        ),
        
        hr(),
        
        h4("Filtri temporali e tipologia"),
        
        checkboxGroupInput(
          inputId = "tipo_accettazione_acc",
          label = "Tipologia accettazione:",
          choices = c("IN", "OUT"),
          selected = c("IN")
        ),
        
        dateRangeInput(
          inputId = "periodo_acc",
          label = "Periodo data accettazione:",
          start = data_min_acc,
          end = data_max_acc
        ),
        
        hr(),
        
        h4("Target tempo utenza"),
        
        numericInput(
          inputId = "target_giorni_in_acc",
          label = "Target giorni lavorativi - IN:",
          value = 2,
          min = 0,
          step = 1
        ),
        
        numericInput(
          inputId = "target_perc_in_acc",
          label = "Target % entro soglia - IN:",
          value = 95,
          min = 0,
          max = 100,
          step = 1
        ),
        
        # numericInput(
        #   inputId = "target_giorni_out_acc",
        #   label = "Target giorni lavorativi - OUT:",
        #   value = 4,
        #   min = 0,
        #   step = 1
        # ),
        # 
        # numericInput(
        #   inputId = "target_perc_out_acc",
        #   label = "Target % entro soglia - OUT:",
        #   value = 95,
        #   min = 0,
        #   max = 100,
        #   step = 1
        # ),
        
        hr(),
        
        checkboxInput(
          inputId = "mostra_dettaglio_acc",
          label = "Mostra elenco accettazioni",
          value = FALSE
        )
      ),
      
      mainPanel(
        
        h3("Monitoraggio CDC accettanti"),
        
        p(
          "Questa sezione analizza il tempo di attesa dell'utente dal punto di vista del CDC che ha effettuato l'accettazione. ",
          "I risultati possono essere aggregati per dipartimento, struttura complessa, centro di costo o singolo CDC accettante."
        ),
        
        hr(),
        
        h4("Statistiche tempo di attesa utente"),
        tableOutput("tab_sintesi_acc"),
        
        hr(),
        
        h4("Valutazione rispetto al target - Accettazioni IN"),
        
        p(
          "La valutazione rispetto al target viene applicata solo alle accettazioni IN. ",
          "Per queste accettazioni il CDC accettante coincide con il CDC erogante, ",
          "rendendo più appropriato il confronto tra tempo di attesa dell'utenza, target assoluto ",
          "e target aggiustato per complessità tecnica."
        ),
        
        tableOutput("tab_valutazione_acc"),
        
        hr(),
        
        h4("Grafico valutazione target - Accettazioni IN"),
        plotOutput("plot_valutazione_acc", height = "650px"),
        
        hr(),
        
        h4("Trend settimanale tempo di attesa utente"),
        plotOutput("plot_trend_attesa_acc", height = "650px"),
        
        conditionalPanel(
          condition = "input.mostra_dettaglio_acc == true",
          hr(),
          h4("Elenco accettazioni"),
          tableOutput("tab_dettaglio_acc")
        )
      )
    )
  ),
  
  # ===================================================
  # PAGINA 2 - CDC EROGANTI
  # ===================================================
  
  tabPanel(
    title = "CDC eroganti",
    
    fluidPage(
      
      h3("Monitoraggio CDC eroganti"),
      
      p(
        "Questa sezione sarà dedicata all'analisi dei CDC eroganti. ",
        "La logica sarà distinta da quella dei CDC accettanti, perché qui l'interesse non è più solo chi ha accettato il campione, ",
        "ma quale CDC ha erogato gli esami, quali nodi della rete sono coinvolti e quali CDC possono essere associati ai tempi di chiusura più lunghi."
      ),
      
      hr(),
      
      h4("Sezione in costruzione"),
      
      tags$ul(
        tags$li("numero di accettazioni OUT in cui il CDC erogante è coinvolto;"),
        tags$li("numero di esami erogati per conto di altri CDC;"),
        tags$li("tempo tecnico di analisi;"),
        tags$li("identificazione del CDC erogante vincolante;"),
        tags$li("network accettante → erogante;"),
        tags$li("ranking degli eroganti associati ai tempi di attesa più elevati.")
      )
    )
  )
)