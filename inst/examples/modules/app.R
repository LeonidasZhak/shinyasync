library(shiny)
library(shinyasync)

# A module that fits a density to random data. Everything inside the module
# server is ordinary Shiny; reactiveAsync() captures the namespaced `input`
# and the module's local objects like any other reactive.
densityUI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("n"), "Rows", 1e3, 1e7, 1e5),
    plotOutput(ns("p")),
    textOutput(ns("status"))
  )
}

densityServer <- function(id, sd = 1) {
  moduleServer(id, function(input, output, session) {
    simulate <- function(n) density(rnorm(n, sd = sd))   # shipped to the worker

    d <- reactiveAsync(simulate(input$n))

    output$p      <- renderPlot(plot(d(), main = session$ns("")))
    output$status <- renderText(status_async(d))

    d   # return the async reactive so the caller can use it
  })
}

ui <- fluidPage(
  fluidRow(
    column(6, densityUI("left")),
    column(6, densityUI("right"))
  ),
  textOutput("both")
)

server <- function(input, output, session) {
  pool_async(workers = 2)               # one call for the whole app is enough

  left  <- densityServer("left", sd = 1)
  right <- densityServer("right", sd = 3)

  output$both <- renderText(
    sprintf("Bandwidths: %.3f and %.3f", left()$bw, right()$bw)
  )
}

shinyApp(ui, server)
