library(shiny)

ui <- fluidPage(
  titlePanel("My first App"),
  
  fluidRow(
    titlePanel("Row 1"),
    column(4,"Column 1"),
    column(4,"Column 2"),
    column(4,"Column 3")
  ),
  fluidRow(
    titlePanel("Row 2"),
    column(6,"Column 1"),
    column(6,"Column 2")
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

