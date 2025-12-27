# app.R - Version finale avec toutes les colonnes BRVM
library(shiny)
library(rvest)
library(dplyr)
library(DT)
library(shinythemes)
library(ggplot2)
library(scales)

# Interface utilisateur
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("📊 Dashboard BRVM - Cotations Complètes"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Paramètres"),
      
      # Bouton de rafraîchissement
      actionButton("refresh", "🔄 Actualiser", 
                   class = "btn-primary",
                   width = "100%"),
      
      br(), br(),
      
      # Filtres
      h5("Filtres"),
      textInput("filter_code", "Symbole ou nom:", 
                placeholder = "ex: BICB, BOAB..."),
      
      selectInput("filter_secteur", "Secteur:",
                  choices = c("Tous", "Banque", "Assurance", "Industrie", "Services"),
                  selected = "Tous"),
      
      selectInput("filter_variation", "Variation:",
                  choices = c("Toutes", "≥ +5%", "+1% à +5%", "-1% à +1%", "-5% à -1%", "≤ -5%"),
                  selected = "Toutes"),
      
      sliderInput("filter_volume", "Volume minimum:",
                  min = 0, max = 50000000, 
                  value = 0, step = 1000000,
                  pre = "", sep = ".", post = " FCFA"),
      
      br(),
      
      # Informations
      wellPanel(
        h5("Statut marché"),
        uiOutput("market_status"),
        br(),
        downloadButton("download_data", "📥 Télécharger CSV"),
        downloadButton("download_report", "📊 Rapport PDF")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        # Onglet 1 : Tableau complet
        tabPanel("📋 Cotations Complètes",
                 div(
                   style = "margin-bottom: 10px;",
                   uiOutput("summary_stats")
                 ),
                 DTOutput("full_table")),
        
        # Onglet 2 : Analyse
        tabPanel("📈 Analyse",
                 fluidRow(
                   column(6, 
                          h4("Performance par société"),
                          plotOutput("performance_chart", height = "400px")),
                   column(6,
                          h4("Distribution des volumes"),
                          plotOutput("volume_distribution", height = "400px"))
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h4("Corrélations"),
                          plotOutput("correlation_heatmap", height = "300px"))
                 )),
        
        # Onglet 3 : Top & Flop
        tabPanel("🏆 Top & Flop",
                 fluidRow(
                   column(6,
                          h4("🏆 Top 5 des hausses"),
                          DTOutput("top_gainers_table")),
                   column(6,
                          h4("📉 Top 5 des baisses"),
                          DTOutput("top_losers_table"))
                 ),
                 br(),
                 fluidRow(
                   column(6,
                          h4("💰 Plus gros volumes"),
                          plotOutput("top_volume_chart", height = "300px")),
                   column(6,
                          h4("📊 Répartition par variation"),
                          plotOutput("variation_pie", height = "300px"))
                 )),
        
        # Onglet 4 : Détails
        tabPanel("🔍 Détails Société",
                 fluidRow(
                   column(4,
                          selectInput("selected_company", "Choisir une société:",
                                      choices = NULL)),
                   column(8,
                          uiOutput("company_details"))
                 ),
                 br(),
                 plotOutput("company_metrics", height = "300px")),
        
        # Onglet 5 : Données brutes
        tabPanel("📄 Données Brutes",
                 verbatimTextOutput("raw_structure"),
                 br(),
                 DTOutput("raw_table"))
      )
    )
  ),
  
  # CSS personnalisé
  tags$head(
    tags$style(HTML("
      .positive { color: #27ae60; font-weight: bold; }
      .negative { color: #c0392b; font-weight: bold; }
      .neutral { color: #7f8c8d; }
      .high-volume { background-color: #e8f5e9 !important; }
      .low-volume { background-color: #ffebee !important; }
      .summary-card {
        padding: 10px;
        border-radius: 5px;
        background-color: #f8f9fa;
        margin-bottom: 10px;
      }
      .market-up { color: #27ae60; }
      .market-down { color: #c0392b; }
      .market-neutral { color: #7f8c8d; }
    "))
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Données réactives
  brvm_data <- reactiveVal(NULL)
  last_update <- reactiveVal(Sys.time())
  
  # Fonction d'extraction des données
  extract_brvm_data <- function() {
    url <- "https://www.brvm.org"
    
    tryCatch({
      # Lire la page
      page <- read_html(url)
      
      # Extraire la table complète avec le bon sélecteur
      table_node <- page %>%
        html_node("table.table.table-hover.table-striped")
      
      if(!is.null(table_node)) {
        # Extraire le tableau
        raw_table <- html_table(table_node, fill = TRUE)
        
        # Nettoyer les noms de colonnes
        colnames(raw_table) <- c("Symbole", "Nom", "Volume", 
                                 "Cours_veille", "Cours_ouverture", 
                                 "Cours_cloture", "Variation_pct")
        
        # Nettoyer et convertir les données
        cleaned_data <- raw_table %>%
          filter(!is.na(Symbole) & Symbole != "") %>%
          mutate(
            # Nettoyer le volume (supprimer espaces et convertir)
            Volume = as.numeric(gsub("[^0-9]", "", Volume)),
            
            # Nettoyer les cours (remplacer virgules)
            Cours_veille = as.numeric(gsub(",", ".", gsub("[^0-9,.]", "", Cours_veille))),
            Cours_ouverture = as.numeric(gsub(",", ".", gsub("[^0-9,.]", "", Cours_ouverture))),
            Cours_cloture = as.numeric(gsub(",", ".", gsub("[^0-9,.]", "", Cours_cloture))),
            
            # Nettoyer la variation
            Variation_pct = as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", Variation_pct))),
            
            # Calculer la variation absolue
            Variation_absolue = Cours_cloture - Cours_veille,
            
            # Catégoriser la variation
            Variation_categorie = case_when(
              Variation_pct >= 5 ~ "≥ +5%",
              Variation_pct >= 1 ~ "+1% à +5%",
              Variation_pct >= -1 ~ "-1% à +1%",
              Variation_pct >= -5 ~ "-5% à -1%",
              TRUE ~ "≤ -5%"
            ),
            
            # Déterminer la tendance
            Tendance = case_when(
              Variation_pct > 0 ~ "Hausse",
              Variation_pct < 0 ~ "Baisse",
              TRUE ~ "Stable"
            ),
            
            # Couleur pour affichage
            Variation_html = sprintf(
              '<span class="%s">%+.2f%%</span>',
              ifelse(Variation_pct > 0, "positive",
                     ifelse(Variation_pct < 0, "negative", "neutral")),
              Variation_pct
            ),
            
            # Volume formaté
            Volume_formatted = format(Volume, big.mark = " ", decimal.mark = ",", scientific = FALSE),
            
            # Valeur de marché estimée (volume × cours de clôture)
            Valeur_marche = Volume * Cours_cloture,
            
            # Timestamp
            Derniere_maj = Sys.time()
          ) %>%
          arrange(desc(Volume))  # Trier par volume décroissant
        
        # Mettre à jour le timestamp
        last_update(Sys.time())
        
        return(cleaned_data)
        
      } else {
        showNotification("Table non trouvée - vérifiez la structure HTML", 
                         type = "warning", duration = 5)
        return(NULL)
      }
      
    }, error = function(e) {
      showNotification(paste("Erreur d'extraction:", e$message), 
                       type = "error", duration = 5)
      return(NULL)
    })
  }
  
  # Charger les données
  load_brvm_data <- function() {
    showNotification("Chargement des données BRVM...", type = "message", duration = 2)
    data <- extract_brvm_data()
    if(!is.null(data)) {
      brvm_data(data)
      # Mettre à jour la liste des sociétés
      updateSelectInput(session, "selected_company",
                        choices = c("", sort(unique(data$Symbole))),
                        selected = "")
    }
  }
  
  # Chargement initial
  observe({
    load_brvm_data()
  })
  
  # Données filtrées
  filtered_data <- reactive({
    data <- brvm_data()
    if(is.null(data)) return(NULL)
    
    filtered <- data
    
    # Filtre par symbole/nom
    if(input$filter_code != "") {
      search_term <- tolower(input$filter_code)
      filtered <- filtered %>%
        filter(grepl(search_term, tolower(Symbole)) | 
                 grepl(search_term, tolower(Nom)))
    }
    
    # Filtre par catégorie de variation
    if(input$filter_variation != "Toutes") {
      filtered <- filtered %>%
        filter(Variation_categorie == input$filter_variation)
    }
    
    # Filtre par volume
    filtered <- filtered %>%
      filter(Volume >= input$filter_volume)
    
    return(filtered)
  })
  
  # OUTPUT: Tableau principal
  output$full_table <- renderDT({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame(Message = "Aucune donnée disponible"),
                       options = list(dom = 't')))
    }
    
    datatable(
      data %>%
        select(Symbole, Nom, Volume_formatted, Cours_veille, 
               Cours_ouverture, Cours_cloture, Variation_html, Tendance) %>%
        rename(
          "Symbole" = Symbole,
          "Nom" = Nom,
          "Volume" = Volume_formatted,
          "Veille (FCFA)" = Cours_veille,
          "Ouverture (FCFA)" = Cours_ouverture,
          "Clôture (FCFA)" = Cours_cloture,
          "Variation (%)" = Variation_html,
          "Tendance" = Tendance
        ),
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        dom = 'Blfrtip',
        buttons = list(
          list(extend = 'copy', text = 'Copier'),
          list(extend = 'csv', text = 'CSV'),
          list(extend = 'excel', text = 'Excel'),
          list(extend = 'pdf', text = 'PDF')
        ),
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/French.json'
        ),
        scrollX = TRUE
      ),
      extensions = c('Buttons', 'Scroller'),
      rownames = FALSE,
      escape = FALSE,  # Important pour le HTML
      class = 'display compact stripe hover order-column',
      callback = JS("
        table.on('draw.dt', function() {
          // Ajouter des classes CSS basées sur les valeurs
          $('td:contains(\"Hausse\")').addClass('positive');
          $('td:contains(\"Baisse\")').addClass('negative');
        })
      ")
    ) %>%
      formatCurrency(c('Veille (FCFA)', 'Ouverture (FCFA)', 'Clôture (FCFA)'), 
                     currency = "", digits = 0, mark = " ", dec.mark = ",") %>%
      formatStyle(
        'Tendance',
        backgroundColor = styleEqual(
          c("Hausse", "Baisse", "Stable"),
          c("#d4edda", "#f8d7da", "#e2e3e5")
        )
      ) %>%
      formatStyle(
        'Volume',
        background = styleColorBar(data$Volume, '#3498db'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # OUTPUT: Statistiques résumées
  output$summary_stats <- renderUI({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) return(NULL)
    
    avg_var <- mean(data$Variation_pct, na.rm = TRUE)
    total_vol <- sum(data$Volume, na.rm = TRUE)
    up_count <- sum(data$Tendance == "Hausse", na.rm = TRUE)
    down_count <- sum(data$Tendance == "Baisse", na.rm = TRUE)
    
    tagList(
      div(class = "summary-card",
          fluidRow(
            column(3, h5("📈 Moyenne variation:", sprintf("%+.2f%%", avg_var))),
            column(3, h5("💰 Volume total:", format(total_vol, big.mark = " "))),
            column(3, h5("✅ Hausses:", up_count)),
            column(3, h5("❌ Baisses:", down_count))
          )
      )
    )
  })
  
  # OUTPUT: Graphique de performance
  output$performance_chart <- renderPlot({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) return(NULL)
    
    # Prendre les 20 premières sociétés
    plot_data <- head(data, 20)
    
    ggplot(plot_data, aes(x = reorder(Symbole, Variation_pct), y = Variation_pct)) +
      geom_bar(stat = "identity", aes(fill = Tendance), alpha = 0.8) +
      scale_fill_manual(values = c("Hausse" = "#27ae60", 
                                   "Baisse" = "#c0392b",
                                   "Stable" = "#7f8c8d")) +
      coord_flip() +
      labs(title = "Performance des sociétés (top 20)",
           x = "Symbole",
           y = "Variation (%)") +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank())
  })
  
  # OUTPUT: Distribution des volumes
  output$volume_distribution <- renderPlot({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) return(NULL)
    
    ggplot(data, aes(x = log10(Volume + 1))) +
      geom_histogram(fill = "#3498db", bins = 20, alpha = 0.7) +
      labs(title = "Distribution des volumes (échelle logarithmique)",
           x = "log10(Volume)",
           y = "Nombre de sociétés") +
      theme_minimal()
  })
  
  # OUTPUT: Tableau des gagnants
  output$top_gainers_table <- renderDT({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) return(NULL)
    
    top_gainers <- data %>%
      filter(Variation_pct > 0) %>%
      arrange(desc(Variation_pct)) %>%
      head(5) %>%
      select(Symbole, Nom, Variation_pct, Cours_cloture, Volume_formatted)
    
    datatable(
      top_gainers,
      options = list(dom = 't', paging = FALSE, searching = FALSE),
      rownames = FALSE,
      colnames = c("Symbole", "Nom", "Variation %", "Clôture (FCFA)", "Volume")
    ) %>%
      formatPercentage('Variation_pct', 2) %>%
      formatCurrency('Cours_cloture', currency = "", digits = 0, mark = " ")
  })
  
  # OUTPUT: Tableau des perdants
  output$top_losers_table <- renderDT({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) return(NULL)
    
    top_losers <- data %>%
      filter(Variation_pct < 0) %>%
      arrange(Variation_pct) %>%
      head(5) %>%
      select(Symbole, Nom, Variation_pct, Cours_cloture, Volume_formatted)
    
    datatable(
      top_losers,
      options = list(dom = 't', paging = FALSE, searching = FALSE),
      rownames = FALSE,
      colnames = c("Symbole", "Nom", "Variation %", "Clôture (FCFA)", "Volume")
    ) %>%
      formatPercentage('Variation_pct', 2) %>%
      formatCurrency('Cours_cloture', currency = "", digits = 0, mark = " ")
  })
  
  # OUTPUT: Graphique des volumes
  output$top_volume_chart <- renderPlot({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) return(NULL)
    
    top_volume <- data %>%
      arrange(desc(Volume)) %>%
      head(10)
    
    ggplot(top_volume, aes(x = reorder(Symbole, Volume), y = Volume/1e6)) +
      geom_bar(stat = "identity", fill = "#e74c3c", alpha = 0.8) +
      coord_flip() +
      labs(title = "Top 10 des volumes échangés",
           x = "Symbole",
           y = "Volume (millions FCFA)") +
      theme_minimal()
  })
  
  # OUTPUT: Camembert des variations
  output$variation_pie <- renderPlot({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) return(NULL)
    
    variation_summary <- data %>%
      count(Variation_categorie) %>%
      mutate(Percentage = n / sum(n) * 100)
    
    ggplot(variation_summary, aes(x = "", y = n, fill = Variation_categorie)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "RdYlGn", direction = -1) +
      labs(title = "Répartition par catégorie de variation",
           fill = "Catégorie") +
      theme_void() +
      theme(legend.position = "right")
  })
  
  # OUTPUT: Détails société
  output$company_details <- renderUI({
    data <- filtered_data()
    selected <- input$selected_company
    
    if(is.null(data) || selected == "") return(NULL)
    
    company <- data %>% filter(Symbole == selected)
    if(nrow(company) == 0) return(NULL)
    
    tagList(
      h3(company$Nom, " (", company$Symbole, ")"),
      hr(),
      fluidRow(
        column(6,
               h4("📊 Indicateurs"),
               tableOutput("company_metrics_table")
        ),
        column(6,
               h4("📈 Performance"),
               plotOutput("company_chart", height = "200px")
        )
      )
    )
  })
  
  # OUTPUT: Métriques société
  output$company_metrics_table <- renderTable({
    data <- filtered_data()
    selected <- input$selected_company
    if(is.null(data) || selected == "") return(NULL)
    
    company <- data %>% filter(Symbole == selected)
    
    data.frame(
      Indicateur = c("Cours clôture", "Cours veille", "Variation", 
                     "Volume", "Valeur marché", "Tendance"),
      Valeur = c(
        format(company$Cours_cloture, big.mark = " ", decimal.mark = ","),
        format(company$Cours_veille, big.mark = " ", decimal.mark = ","),
        sprintf("%+.2f%%", company$Variation_pct),
        format(company$Volume, big.mark = " "),
        format(company$Valeur_marche, big.mark = " ", scientific = FALSE),
        company$Tendance
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # OUTPUT: Statut marché
  output$market_status <- renderUI({
    data <- brvm_data()
    update_time <- last_update()
    
    if(is.null(data)) {
      return(HTML("<span class='market-neutral'>⏳ Chargement...</span>"))
    }
    
    avg_var <- mean(data$Variation_pct, na.rm = TRUE)
    status_class <- if(avg_var > 0) "market-up" else if(avg_var < 0) "market-down" else "market-neutral"
    status_icon <- if(avg_var > 0) "📈" else if(avg_var < 0) "📉" else "➡"
    
    HTML(paste(
      "<strong>Mise à jour:</strong><br>",
      format(update_time, "%H:%M:%S"), "<br><br>",
      "<strong>Indice moyen:</strong><br>",
      sprintf("<span class='%s'>%s %+.2f%%</span><br>", status_class, status_icon, avg_var),
      "<strong>Sociétés:</strong> ", nrow(data)
    ))
  })
  
  # OUTPUT: Données brutes
  output$raw_table <- renderDT({
    data <- brvm_data()
    if(is.null(data)) return(NULL)
    
    datatable(
      data %>% select(-Variation_html, -Volume_formatted),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # OUTPUT: Structure HTML
  output$raw_structure <- renderPrint({
    cat("=== STRUCTURE DE LA TABLE BRVM ===\n\n")
    cat("Colonnes disponibles:\n")
    cat("1. Symbole - Code de la société\n")
    cat("2. Nom - Nom complet\n")
    cat("3. Volume - Nombre d'actions échangées\n")
    cat("4. Cours_veille - Cours de la veille\n")
    cat("5. Cours_ouverture - Cours d'ouverture\n")
    cat("6. Cours_cloture - Cours de clôture\n")
    cat("7. Variation_pct - Variation en pourcentage\n")
    cat("\nDonnées extraites:", format(last_update(), "%Y-%m-%d %H:%M:%S"), "\n")
  })
  
  # Téléchargement CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("brvm_complet_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    },
    content = function(file) {
      data <- filtered_data()
      if(!is.null(data)) {
        write.csv2(data %>% select(-Variation_html, -Volume_formatted), 
                   file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
  
  # Observers
  observeEvent(input$refresh, {
    load_brvm_data()
  })
  
  # Rafraîchissement automatique (toutes les 5 minutes)
  auto_refresh <- reactiveTimer(300000)
  
  observe({
    auto_refresh()
    load_brvm_data()
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)