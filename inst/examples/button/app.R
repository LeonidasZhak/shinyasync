library(shiny)
library(shinyasync)

fit_model <- function(formula, data) {
  Sys.sleep(3)                        # stand-in for slow work
  lm(formula, data = data)
}

ui <- fluidPage(
  selectInput("formula", "Model", c("mpg ~ wt", "mpg ~ wt + hp", "mpg ~ .")),
  actionButton("go", "Fit"),
  actionButton("cancel", "Cancel"),
  textOutput("status"),
  verbatimTextOutput("summary")
)

server <- function(input, output, session) {
  pool_async(workers = 2)

  model <- reactiveAsync({
    fit_model(as.formula(input$formula), mtcars)
  }) |>
    bindEvent(input$go)

  observe(cancel_async(model)) |> bindEvent(input$cancel)

  output$status  <- renderText(paste("Status:", status_async(model)))
  output$summary <- renderPrint(summary(model()))
}

shinyApp(ui, server)
