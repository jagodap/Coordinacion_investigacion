Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding = "UTF-8")



library(shiny)
library(htmlwidgets)
library(timevis)
library(dplyr)
library(shinylive)
library(httpuv)
library(rsconnect)
library(htmltools)
library(markdown)


timeline_data <- read.csv("datos.csv", fileEncoding = "UTF-8")


ui <- fluidPage(
  titlePanel("Proyectos 2025"),
  
  
  
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("toc"),
            width = 3,
      checkboxGroupInput("filtro_req", "Filtro por requisito:",
                         choices = unique(timeline_data$Req),
                         selected = unique(timeline_data$Req)),
      checkboxGroupInput("filtro_tipo", "Filtro por Tipo:",
                         choices = unique(timeline_data$Tipo),
                         selected = unique(timeline_data$Tipo)),
      checkboxGroupInput("filtro_finan", "Filtro por Financiamiento:",
                         choices = unique(timeline_data$Finan),
                         selected = unique(timeline_data$Finan)),
      
      actionButton("reset_filters", "Resetear Filtros")
    ),
    
    
    mainPanel(
      timevisOutput("timeline"),
      
      div(style = "margin-bottom: 50px; padding: 20px; background-color: #f5f5f5;",
          includeMarkdown("texto/lista.md"))
      
    )
  )
)



server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$filtro_req, input$filtro_tipo, input$filtro_finan)
    
    timeline_data %>%
      filter(Req %in% input$filtro_req,
             Tipo %in% input$filtro_tipo,
             Finan %in% input$filtro_finan
             
      )
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





#deployApp()


