# Required libraries
library(shiny)
library(RSQLite)
library(ggplot2)
library(DBI)

# Database setup (this should ideally be in a separate initialization script)
conn <- dbConnect(RSQLite::SQLite(), "my_database.sqlite")
dbWriteTable(conn, "mtcars", mtcars, overwrite = TRUE)
dbDisconnect(conn)

# Module UI function
mod_histogram_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          ns("var"),
          "Variable",
          choices = c("cyl", "mpg"),
          selected = "mpg"
        ),
        sliderInput(
          ns("bins"),
          "Number of bins:",
          min = 1,
          max = 50,
          value = 30
        )
      ),
      mainPanel(
        plotOutput(ns("distPlot"))
      )
    )
  )
}

# Module server function
mod_histogram_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Connect to database within the server function
    conn <- dbConnect(RSQLite::SQLite(), "my_database.sqlite")
    
    # Reactive data
    data <- reactive({
      dbReadTable(conn, "mtcars")
    })
    
    # Create histogram
    output$distPlot <- renderPlot({
      validate(need(input$var != "", "Please select a variable"))
      df <- data()
      x <- df[[input$var]]
      bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = input$bins + 1)
      hist(x, 
           breaks = bins, 
           col = "darkgray", 
           border = "white", 
           main = paste("Histogram of", input$var),
           xlab = input$var)
    })
    
    # Disconnect when session ends
    session$onSessionEnded(function() {
      dbDisconnect(conn)
    })
  })
}

# Main app UI
ui <- fluidPage(
  titlePanel("Dynamic Histogram for Cyl and MPG"),
  mod_histogram_ui("gr8")
)

# Main app server
server <- function(input, output, session) {
  mod_histogram_server("gr8")
}

# Run the application
shinyApp(ui = ui, server = server)
  
  