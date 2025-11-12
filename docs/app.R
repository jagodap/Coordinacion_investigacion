#Sys.setlocale("LC_ALL", "en_US.UTF-8")
#options(encoding = "UTF-8")

library(shiny)
library(htmlwidgets)
library(timevis)
library(dplyr)
library(shinylive)
library(httpuv)
library(rsconnect)
library(htmltools)
library(markdown)

ui <- fluidPage(
  titlePanel("Proyectos 2025"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("toc"),
      width = 3,
      uiOutput("req_filter"),
      uiOutput("tipo_filter"),
      uiOutput("finan_filter"),
      uiOutput("orien_filter"),
      uiOutput("convo_filter"),
      actionButton("reset_filters", "Resetear Filtros")
    ),
    
    mainPanel(
      timevisOutput("timeline"),
      div(style = "margin-bottom: 50px; padding: 20px; background-color: #f5f5f5;",
          uiOutput("markdown_content"))
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive file reader for datos.csv
  timeline_data <- reactiveFileReader(
    intervalMillis = 2000,  # Check every 2 seconds
    session = session,
    filePath = "datos.csv",
    readFunc = function(filePath) {
      tryCatch({
        data <- read.csv(filePath, fileEncoding = "UTF-8")
        return(data)
      }, error = function(e) {
        showNotification(paste("Error reading datos.csv:", e$message), type = "error")
        # Return empty data frame with expected structure if file doesn't exist
        return(data.frame(
          Req = character(),
          Tipo = character(),
          Finan = character(),
          Orien = character(),
          Convo = character(),
          start = character(),
          end = character(),
          content = character(),
          group = character()
        ))
      })
    }
  )
  
  # Reactive file reader for lista.md
  markdown_content <- reactiveFileReader(
    intervalMillis = 2000,  # Check every 2 seconds
    session = session,
    filePath = "lista.md",
    readFunc = function(filePath) {
      tryCatch({
        if (file.exists(filePath)) {
          return(includeMarkdown(filePath))
        } else {
          return(div("lista.md file not found"))
        }
      }, error = function(e) {
        showNotification(paste("Error reading lista.md:", e$message), type = "error")
        return(div("Error loading content"))
      })
    }
  )
  
  # Dynamic filter inputs based on current data
  output$req_filter <- renderUI({
    data <- timeline_data()
    if (!is.null(data) && nrow(data) > 0) {
      choices <- unique(data$Req)
      checkboxGroupInput("filtro_req", "Filtro por requisito:",
                         choices = choices,
                         selected = choices)
    }
  })
  
  output$tipo_filter <- renderUI({
    data <- timeline_data()
    if (!is.null(data) && nrow(data) > 0) {
      choices <- unique(data$Tipo)
      checkboxGroupInput("filtro_tipo", "Filtro por Tipo:",
                         choices = choices,
                         selected = choices)
    }
  })
  
  output$finan_filter <- renderUI({
    data <- timeline_data()
    if (!is.null(data) && nrow(data) > 0) {
      choices <- unique(data$Finan)
      checkboxGroupInput("filtro_finan", "Filtro por Financiamiento:",
                         choices = choices,
                         selected = choices)
    }
  })
  
  output$orien_filter <- renderUI({
    data <- timeline_data()
    if (!is.null(data) && nrow(data) > 0) {
      choices <- unique(data$Orien)
      checkboxGroupInput("filtro_orien", "Filtro por Orientación:",
                         choices = choices,
                         selected = choices)
    }
  })
  
  output$convo_filter <- renderUI({
    data <- timeline_data()
    if (!is.null(data) && nrow(data) > 0) {
      choices <- unique(data$Convo)
      checkboxGroupInput("filtro_convo", "Filtro por Convocatoria:",
                         choices = choices,
                         selected = choices)
    }
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- timeline_data()
    req(data, input$filtro_req, input$filtro_tipo, input$filtro_finan, 
        input$filtro_orien, input$filtro_convo)
    
    if (nrow(data) == 0) return(data)
    
    data %>%
      filter(Req %in% input$filtro_req,
             Tipo %in% input$filtro_tipo,
             Finan %in% input$filtro_finan,
             Orien %in% input$filtro_orien,
             Convo %in% input$filtro_convo)
  })
  
  # Render timeline
  output$timeline <- renderTimevis({
    data <- filtered_data()
    if (!is.null(data) && nrow(data) > 0) {
      timevis(data)
    } else {
      # Create empty timeline if no data
      timevis(data.frame())
    }
  })
  
  # Render markdown content
  output$markdown_content <- renderUI({
    markdown_content()
  })
  
  # Reset all filters
  observeEvent(input$reset_filters, {
    data <- timeline_data()
    if (!is.null(data) && nrow(data) > 0) {
      updateCheckboxGroupInput(session, "filtro_req", selected = unique(data$Req))
      updateCheckboxGroupInput(session, "filtro_tipo", selected = unique(data$Tipo))
      updateCheckboxGroupInput(session, "filtro_finan", selected = unique(data$Finan))
      updateCheckboxGroupInput(session, "filtro_orien", selected = unique(data$Orien))
      updateCheckboxGroupInput(session, "filtro_convo", selected = unique(data$Convo))
    }
  })
  
  # Optional: Show only available options based on other filters
  observe({
    data <- timeline_data()
    req_filt <- input$filtro_req
    tipo_filt <- input$filtro_tipo
    
    if (!is.null(data) && nrow(data) > 0 && !is.null(req_filt) && !is.null(tipo_filt)) {
      current_data <- data %>%
        filter(Req %in% req_filt,
               Tipo %in% tipo_filt)
      
      # You can add logic here to update other filters based on current selection
    }
  })
}

shinyApp(ui, server)