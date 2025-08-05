#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(htmlwidgets)
library(shiny)
library(timevis)
library(dplyr)

timeline_data=read.csv("/home/camilo/Documentos/U de Chile/2025/Coordinación Investigación/datos.csv")

ui <- fluidPage(
  titlePanel("Proyectos 2025"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxGroupInput("filtro_req", "Filtro por requisito:",
                         choices = unique(timeline_data$Req),
                         selected = unique(timeline_data$Req)),
      checkboxGroupInput("filtro_tipo", "Filtro por Tipo:",
                         choices = unique(timeline_data$Tipo),
                         selected = unique(timeline_data$Tipo)),
      actionButton("reset_filters", "Resetear Filtros")
    ),
    mainPanel(
      timevisOutput("timeline")
    )
  )
)



server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$filtro_req, input$filtro_tipo)
    
    timeline_data %>%
      filter(Req %in% input$filtro_req,
             Tipo %in% input$filtro_tipo)
  })
  
  # Render timeline
  output$timeline <- renderTimevis({
    timevis(filtered_data())
    #HTML(rmarkdown::render("linea.Rmd", output_format = rmarkdown::html_fragment()))
    
    
  })
  
  # Reset all filters
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "filtro_req", selected = unique(timeline_data$Req))
    updateCheckboxGroupInput(session, "filtro_tipo", selected = unique(timeline_data$Tipo))
    
  })
  # Optional: Show only available options based on other filters
  observe({
    current_data <- timeline_data %>%
      filter(Req %in% input$Req,
             Tipo %in% input$Tipo)
    
    
  })
  
  
  
  
}

shinyApp(ui, server)











