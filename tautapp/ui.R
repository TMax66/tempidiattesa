ui <- navbarPage(
  
  title = "Monitoraggio tempi di attesa utente IZSLER",
  
  # ===================================================
  # PAGINA HOME
  # ===================================================
  
  tabPanel(
    title = "Home",
    
    fluidPage(
      
      h2("Monitoraggio dei tempi di attesa dell'utenza"),
      
      p(
        "L'applicazione consente di analizzare i tempi di attesa dell'utenza ",
        "nel percorso che va dalla data di accettazione del campione alla data ",
        "di emissione o conferma del Rapporto di Prova."
      ),
      
      p(
        "L'obiettivo principale è affiancare alla lettura descrittiva dei tempi ",
        "uno strumento di valutazione più coerente con la complessità tecnica ",
        "degli esami richiesti nelle singole accettazioni."
      ),
      
      hr(),
      
      h3("Schema del processo"),
      
      p(
        "Lo schema seguente rappresenta le principali date del processo e gli intervalli ",
        "temporali che possono essere misurati. Il tempo di attesa dell'utenza è sempre ",
        "calcolato come intervallo tra la data di accettazione e la data di emissione ",
        "del Rapporto di Prova."
      ),
      
      tags$div(
        style = "text-align: center;",
        tags$img(
          src = "processo_tempi_campione_rdp.png",
          style = "max-width: 100%; height: auto; border: 1px solid #ddd;"
        )
      ),
      
      hr(),
      
      h3("Concetti principali"),
      
      fluidRow(
        
        column(
          width = 6,
          
          div(
            style = "background-color:#f7f9fc; border-left:5px solid #2C7FB8; padding:15px; margin-bottom:15px;",
            
            h4("Tempo di attesa dell'utenza"),
            
            p(
              "È il tempo complessivo osservato dall'utente, calcolato dalla data ",
              "di accettazione alla data di emissione del Rapporto di Prova."
            ),
            
            tags$code("tempo_attesa_utente = data_RdP - data_accettazione"),
            
            p(
              "Questo indicatore è calcolato a livello di accettazione, perché ",
              "l'utente riceve il rapporto solo quando tutti gli esami richiesti ",
              "sono conclusi e validati."
            )
          )
        ),
        
        column(
          width = 6,
          
          div(
            style = "background-color:#f7f9fc; border-left:5px solid #41AB5D; padding:15px; margin-bottom:15px;",
            
            h4("Target assoluto di utenza"),
            
            p(
              "È una soglia unica, espressa in giorni lavorativi, applicata a tutte ",
              "le accettazioni. Ad esempio: chiudere l'accettazione entro 3 giorni."
            ),
            
            tags$code("tempo_attesa_utente <= target_giorni"),
            
            p(
              "È utile per misurare il servizio percepito dall'utente, ma non tiene ",
              "conto della diversa complessità tecnica degli esami richiesti."
            )
          )
        )
      ),
      
      fluidRow(
        
        column(
          width = 6,
          
          div(
            style = "background-color:#f7f9fc; border-left:5px solid #F39C12; padding:15px; margin-bottom:15px;",
            
            h4("Target aggiustato per complessità tecnica"),
            
            p(
              "Il target aggiustato confronta il tempo di attesa dell'utenza con ",
              "un tempo tecnico atteso, stimato sulla base dei tempi osservati ",
              "degli esami richiesti."
            ),
            
            tags$code("target_aggiustato = max(benchmark tecnico degli esami richiesti)"),
            
            p(
              "In questo modo un'accettazione con esami tecnicamente semplici viene ",
              "valutata con una soglia più breve, mentre un'accettazione con esami ",
              "più complessi viene valutata con una soglia più lunga."
            )
          )
        ),
        
        column(
          width = 6,
          
          div(
            style = "background-color:#f7f9fc; border-left:5px solid #C0392B; padding:15px; margin-bottom:15px;",
            
            h4("Diagnostica delle accettazioni fuori target"),
            
            p(
              "Quando una accettazione supera il target aggiustato, la diagnostica ",
              "aiuta a capire dove si genera lo scostamento."
            ),
            
            tags$ul(
              tags$li("tempo pre-analitico: accettazione → inizio analisi;"),
              tags$li("tempo tecnico effettivo: inizio analisi → fine analisi;"),
              tags$li("tempo post-analitico/firma: fine analisi → RdP;"),
              tags$li("esame vincolante: esame che chiude più tardi l'accettazione.")
            )
          )
        )
      ),
      
      hr(),
      
      h3("Metodo adottato nell'app"),
      
      tags$ol(
        tags$li(
          "Le accettazioni vengono classificate come IN o OUT. ",
          "La valutazione rispetto al target aggiustato viene applicata alle accettazioni IN."
        ),
        tags$li(
          "Per ogni esame viene stimato un benchmark tecnico sulla base dei tempi osservati ",
          "tra data di inizio analisi e data di fine analisi."
        ),
        tags$li(
          "Il benchmark tecnico viene calcolato prioritariamente per combinazione settore-esame; ",
          "quando la numerosità non è sufficiente, viene usato il benchmark generale dell'esame."
        ),
        tags$li(
          "Per ogni accettazione viene identificato il tempo tecnico atteso massimo tra gli esami richiesti. ",
          "Questo valore rappresenta il riferimento tecnico dell'accettazione."
        ),
        tags$li(
          "Il tempo di attesa dell'utenza viene confrontato con il target assoluto e, quando richiesto, ",
          "con il target aggiustato per complessità tecnica."
        ),
        tags$li(
          "La sezione diagnostica consente di approfondire le accettazioni fuori target, individuando ",
          "settori, classi di complessità, esami vincolanti e componenti temporali prevalenti."
        )
      ),
      
      hr(),
      
      h3("Come leggere i risultati"),
      
      p(
        "La tabella descrittiva mostra le statistiche dei tempi di attesa dell'utenza. ",
        "La valutazione rispetto al target non viene mostrata automaticamente: deve essere attivata ",
        "dall'utente, perché rappresenta un livello interpretativo ulteriore rispetto alla semplice ",
        "descrizione dei tempi osservati."
      ),
      
      p(
        "Il target aggiustato non modifica il tempo di attesa osservato dall'utente. ",
        "Modifica invece la soglia di confronto, rendendola coerente con la complessità tecnica ",
        "degli esami richiesti nell'accettazione."
      ),
      
      div(
        style = "background-color:#fff3cd; border-left:5px solid #d39e00; padding:15px; margin-top:15px;",
        
        strong("Nota interpretativa: "),
        
        "un valore basso di accettazioni entro target aggiustato non identifica automaticamente ",
        "una criticità tecnica del laboratorio. Indica che il tempo complessivo accettazione-RdP ",
        "supera il riferimento tecnico atteso e richiede una lettura diagnostica delle componenti ",
        "pre-analitica, tecnica e post-analitica."
      )
    )
  ),
  
  
  
  # ===================================================
  # PAGINA 1 - CDC ACCETTANTI
  # ===================================================
  
  tabPanel(
    title = "CDC accettanti",
    
    sidebarLayout(
      
      sidebarPanel(
        width = 3,
        
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
        
        checkboxInput(
          inputId = "mostra_valutazione_acc",
          label = "Mostra valutazione rispetto al target",
          value = FALSE
        ),
        
        conditionalPanel(
          condition = "input.mostra_valutazione_acc == true",
          
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
          
          actionButton(
            inputId = "applica_valutazione_acc",
            label = "Applica target e mostra risultati"
          ),
          
          helpText(
            "Dopo aver impostato entrambi i valori, premere il pulsante per visualizzare ",
            "tabella e grafico di valutazione."
          )
        ),
        
        hr(),
        
        checkboxInput(
          inputId = "mostra_dettaglio_acc",
          label = "Mostra elenco accettazioni",
          value = FALSE
        )
      ),
      
      mainPanel(
        width = 9, 
        
        h3("Monitoraggio CDC accettanti"),
        
        p(
          "Questa sezione analizza il tempo di attesa dell'utente dal punto di vista del CDC che ha effettuato l'accettazione. ",
          "I risultati possono essere aggregati per dipartimento, struttura complessa, centro di costo o singolo CDC accettante."
        ),
        
        hr(),
        
        h4("Statistiche tempo di attesa utente"),
        tableOutput("tab_sintesi_acc"),
        
        conditionalPanel(
          condition = "input.mostra_valutazione_acc == true && input.applica_valutazione_acc > 0",
          
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
          plotOutput("plot_valutazione_acc", height = "650px")
        ),
        
        hr(),
      
        
        checkboxInput(
          inputId = "mostra_trend_attesa_acc",
          label = "Mostra trend settimanale tempo di attesa utente",
          value = FALSE
        ),
        
        conditionalPanel(
          condition = "input.mostra_trend_attesa_acc == true",
          
          h4("Trend settimanale tempo di attesa utente"),
          plotOutput("plot_trend_attesa_acc", height = "500px")
        ),
        
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
  # PAGINA DIAGNOSTICA CDC
  # ===================================================
  
  tabPanel(
    title = "Diagnostica CDC",
    
    sidebarLayout(
      
      sidebarPanel(
        
        h4("Selezione CDC"),
        
        selectInput(
          inputId = "diag_cdc",
          label = "CDC da analizzare:",
          choices = lista_nome_cdc_acc,
          selected = NULL,
          multiple = FALSE
        ),
        
        dateRangeInput(
          inputId = "diag_periodo",
          label = "Periodo data accettazione:",
          start = data_min_acc,
          end = data_max_acc
        ),
        
        selectInput(
          inputId = "diag_benchmark",
          label = "Benchmark tecnico per target aggiustato:",
          choices = c(
            "Mediana tecnica" = "tecnico_atteso_mediano",
            "P75 tecnico" = "tecnico_atteso_p75",
            "P90 tecnico" = "tecnico_atteso_p90"
          ),
          selected = "tecnico_atteso_p75"
        ),
        
        numericInput(
          inputId = "diag_target_utenza",
          label = "Target utenza, giorni lavorativi:",
          value = 3,
          min = 0,
          step = 1
        ),
        
        numericInput(
          inputId = "diag_soglia_pre",
          label = "Soglia pre-analitico rilevante:",
          value = 1,
          min = 0,
          step = 1
        ),
        
        numericInput(
          inputId = "diag_soglia_firma",
          label = "Soglia post-analitico/firma rilevante:",
          value = 1,
          min = 0,
          step = 1
        )
      ),
      
      mainPanel(
        
        h3("Diagnostica del mancato raggiungimento del target aggiustato"),
        
        p(
          "Questa sezione ricostruisce, per il CDC selezionato, le accettazioni IN ",
          "che non rientrano nel target aggiustato per complessità tecnica. ",
          "L'obiettivo è individuare se la criticità è concentrata in specifici settori, ",
          "classi di complessità, esami vincolanti o componenti del processo."
        ),
        
        hr(),
        
        h4("Sintesi CDC"),
        tableOutput("diag_sintesi_cdc"),
        
        hr(),
        
        h4("Distribuzione degli scostamenti"),
        tableOutput("diag_tab_scostamenti"),
        plotOutput("diag_plot_scostamenti", height = "350px"),
        
        hr(),
        
        h4("Analisi per settore"),
        tableOutput("diag_tab_settore"),
        
        hr(),
        
        h4("Analisi per classe di complessità"),
        tableOutput("diag_tab_complessita"),
        
        hr(),
        
        h4("Cause probabili delle accettazioni fuori target"),
        tableOutput("diag_tab_cause"),
        
        hr(),
        
        h4("Esami vincolanti nelle accettazioni fuori target"),
        tableOutput("diag_tab_esami_vincolanti"),
        
        hr(),
        
        h4("Casi sentinella"),
        tableOutput("diag_tab_casi_sentinella")
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