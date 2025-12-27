# Installer les packages nécessaires si ce n'est pas déjà fait
# install.packages(c("shiny", "rvest", "DT"))

library(shiny)
library(rvest)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("Cours des actions BRVM"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("refresh", "Actualiser les données"),
      br(), br(),
      downloadButton("downloadData", "Télécharger le tableau")
    ),
    
    mainPanel(
      DTOutput("table_brvm")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Fonction pour récupérer les données
  get_brvm_data <- function() {
    url <- "https://www.brvm.org/fr/cours-actions/0"
    page <- read_html(url)
    data <- page %>%
      html_node("section#block-system-main table.table-striped") %>%
      html_table(fill = TRUE)
    return(data)
  }
  
  # Stocker les données réactives
  brvm_data <- reactiveVal(get_brvm_data())
  
  # Actualiser les données quand on clique sur le bouton
  observeEvent(input$refresh, {
    brvm_data(get_brvm_data())
  })
  
  # Afficher le tableau
  output$table_brvm <- renderDT({
    datatable(brvm_data(), options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Télécharger le tableau
  output$downloadData <- downloadHandler(
    filename = function() { "data_brvm.csv" },
    content = function(file) {
      write.csv(brvm_data(), file, row.names = FALSE)
    }
  )
}

# Lancer l'application
shinyApp(ui, server)
