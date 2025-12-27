############################################################
# PACKAGES
############################################################
library(shiny)
library(rvest)
library(DT)
library(shinythemes)

############################################################
# UI
############################################################
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  # ===== STYLE =====
  tags$head(
    tags$style(HTML("
                    body {
                      background-color: hsla(50, 33%, 25%, 0.75);
                    }
                    .well {
                      background-color: #FFFFFF;
                        border-radius: 8px;
                      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
                    }
                    .tab-content {
                      background-color: #FFFFFF;
                        padding: 15px;
                      border-radius: 8px;
                      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
                    }
                    h2 {
                      font-weight: 700;
                      color: #2C3E50;
                    }
                    .date-extraction {
                      color: #7F8C8D;
                        font-size: 14px;
                      margin-bottom: 15px;
                    }
                    "))
  ),
  
  titlePanel("📈 Cours et analyse des actions – BRVM"),
  
  # ===== DATE D'EXTRACTION =====
  fluidRow(
    column(
      12,
      div(
        class = "date-extraction",
        textOutput("date_extraction")
      )
    )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      actionButton(
        "refresh",
        "Actualiser les données",
        icon = icon("sync"),
        class = "btn-primary btn-block"
      ),
      
      br(),
      
      downloadButton(
        "downloadData",
        "Télécharger les données",
        icon = icon("download"),
        class = "btn-success btn-block"
      ),
      
      hr(),
      p(strong("Source : "), "BRVM")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        
        tabPanel("📋 Tableau complet", DTOutput("table_all")),
        tabPanel("🚀 Top hausses", DTOutput("table_up")),
        tabPanel("🔻 Top baisses", DTOutput("table_down")),
        
        tabPanel(
          "📈 Graphiques",
          
          fluidRow(
            column(12, plotOutput("plot_variation", height = "400px"))
          ),
          
          br(),
          
          fluidRow(
            column(6, plotOutput("plot_top_up", height = "350px")),
            column(6, plotOutput("plot_top_down", height = "350px"))
          ),
          
          br(),
          
          fluidRow(
            column(12, plotOutput("plot_hist", height = "350px"))
          )
        )
      )
    )
  )
)

############################################################
# SERVER
############################################################
server <- function(input, output, session) {
  
  # ---------- Date d'extraction ----------
  extraction_time <- reactiveVal(Sys.time())
  
  output$date_extraction <- renderText({
    paste(
      "Date d’extraction :",
      format(extraction_time(), "%d/%m/%Y à %H:%M:%S")
    )
  })
  
  # ---------- Données BRVM ----------
  get_brvm_data <- function() {
    url <- "https://www.brvm.org/fr/cours-actions/0"
    page <- read_html(url)
    
    page %>%
      html_node("section#block-system-main table.table-striped") %>%
               html_table(fill = TRUE)
               }

brvm_data <- reactiveVal(get_brvm_data())

observeEvent(input$refresh, {
  brvm_data(get_brvm_data())
  extraction_time(Sys.time())
})

# ---------- Nettoyage ----------
data_clean <- reactive({
  df <- brvm_data()
  df$`Variation (%)` <- as.numeric(
    gsub(",", ".", gsub("%", "", df$`Variation (%)`))
  )
  df
})

# ---------- TABLEAUX ----------
output$table_all <- renderDT({
  datatable(data_clean(), options = list(pageLength = 15, scrollX = TRUE))
})

output$table_up <- renderDT({
  datatable(
    data_clean()[order(-data_clean()$`Variation (%)`), ][1:10, ],
    options = list(pageLength = 10)
  )
})

output$table_down <- renderDT({
  datatable(
    data_clean()[order(data_clean()$`Variation (%)`), ][1:10, ],
    options = list(pageLength = 10)
  )
})

# ---------- GRAPHIQUES ----------
output$plot_variation <- renderPlot({
  df <- data_clean()
  barplot(
    df$`Variation (%)`,
    names.arg = df$Libellé,
    las = 2,
    col = "steelblue",
    main = "Variation journalière des actions (%)",
    ylab = "Variation (%)",
    cex.names = 0.6
  )
})

output$plot_top_up <- renderPlot({
  df <- data_clean()[order(-data_clean()$`Variation (%)`), ][1:10, ]
  barplot(
    df$`Variation (%)`,
    names.arg = df$Libellé,
    col = "darkgreen",
    las = 2,
    main = "Top 10 hausses (%)",
    ylab = "Variation (%)"
  )
})

output$plot_top_down <- renderPlot({
  df <- data_clean()[order(data_clean()$`Variation (%)`), ][1:10, ]
  barplot(
    df$`Variation (%)`,
    names.arg = df$Libellé,
    col = "firebrick",
    las = 2,
    main = "Top 10 baisses (%)",
    ylab = "Variation (%)"
  )
})

output$plot_hist <- renderPlot({
  hist(
    data_clean()$`Variation (%)`,
    breaks = 20,
    col = "gray70",
    border = "white",
    main = "Distribution des variations journalières",
    xlab = "Variation (%)"
  )
})

# ---------- Téléchargement ----------
output$downloadData <- downloadHandler(
  filename = function() {
    paste0("BRVM_actions_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write.csv(data_clean(), file, row.names = FALSE)
  }
)
}

############################################################
# LANCEMENT
############################################################
shinyApp(ui, server)
