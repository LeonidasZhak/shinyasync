test_that("a silent failure clears a previous error", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      n <- input$n
      if (n < 0) stop("negative")
      req(n > 0)
      n
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = -1)
    wait_for(function() status_async(r) == "error", session)
    expect_error(shiny::isolate(r()), "negative")

    session$setInputs(n = 0)
    wait_for(function() status_async(r) == "idle", session)
    expect_equal(peek(r), "<halted>")
  })
})

test_that("cancelling a task that was started after an error clears the error", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      n <- input$n
      if (n < 0) stop("negative")
      Sys.sleep(5)
      n
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = -1)
    wait_for(function() status_async(r) == "error", session)
    session$setInputs(n = 1)
    wait_for(function() status_async(r) == "running", session)
    cancel_async(r)
    expect_equal(shiny::isolate(status_async(r)), "idle")
    expect_equal(peek(r), "<halted>")
  })
})
