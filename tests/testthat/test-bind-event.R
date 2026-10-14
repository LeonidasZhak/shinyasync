test_that("bindEvent gates the task and the result still updates", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync(input$n * 10) |> shiny::bindEvent(input$go)
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1)
    expect_equal(status_async(r), "idle")

    session$setInputs(go = 1)
    wait_for(function() identical(peek(r), 10), session)

    session$setInputs(n = 2)
    expect_equal(status_async(r), "success")
    expect_equal(peek(r), 10)

    session$setInputs(go = 2)
    wait_for(function() identical(peek(r), 20), session)
  })
})

test_that("bindEvent works on observeAsync", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    o <- observeAsync(input$n) |> shiny::bindEvent(input$go)
  }, {
    session$setInputs(n = 1)
    expect_equal(o$status(), "idle")
    session$setInputs(go = 1)
    wait_for(function() o$status() == "success", session)
  })
})
