library(shiny)
library(shinyasync)

ui <- fluidPage(
  sliderInput("n", "Rows", 1e3, 1e7, 1e5),
  plotOutput("p"),
  textOutput("status")
)

server <- function(input, output, session) {
  pool_async(workers = 4)             # daemons shared by all sessions in this process

  r <- reactiveAsync({
    x <- rnorm(input$n)               # runs in a worker, not the session
    density(x)
  })

  output$p      <- renderPlot(plot(r()))
  output$status <- renderText(status_async(r))
}

shinyApp(ui, server)
