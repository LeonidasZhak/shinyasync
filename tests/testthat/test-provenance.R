inputs_of <- function(r) shiny::isolate(attr(r, "shinyasync")$inputs())
stale <- function(r) shiny::isolate(is_stale(attr(r, "shinyasync")))

test_that("a run records the reads the current value was computed from", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    data <- shiny::reactive(input$k * 2)
    r <- reactiveAsync({
      req(input$n)
      seed <- isolate(input$seed)
      Sys.sleep(1)
      input$n + data()
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1, k = 2, seed = 9)
    expect_null(inputs_of(r))
    wait_for(function() identical(peek(r), 5), session)
    expect_equal(inputs_of(r), list(`input$n` = 1, `data()` = 4))

    session$setInputs(n = 3)
    wait_for(function() identical(peek(r), 7), session)
    expect_equal(inputs_of(r), list(`input$n` = 3, `data()` = 4))
  })
})

test_that("staleness tracks inputs, ignores isolated reads and re-runs", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      seed <- isolate(input$seed)
      Sys.sleep(isolate(input$wait))
      input$n
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1, seed = 1, wait = 1)
    expect_true(stale(r))
    wait_for(function() identical(peek(r), 1), session)
    expect_false(stale(r))
    expect_equal(inputs_of(r), list(`input$n` = 1))

    # Isolated reads are not part of the provenance.
    session$setInputs(seed = 2, wait = 1)
    session$flushReact()
    expect_equal(status_async(r), "success")
    expect_false(stale(r))

    session$setInputs(n = 2)
    expect_equal(status_async(r), "running")
    expect_true(stale(r))
    wait_for(function() identical(peek(r), 2), session)
    expect_false(stale(r))
  })
})

test_that("a worker-side req() halt keeps the previous value and its inputs", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      x <- input$n
      req(x > 0)
      x
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1)
    wait_for(function() identical(peek(r), 1), session)
    session$setInputs(n = -1)
    wait_for(function() status_async(r) != "running", session)
    expect_equal(peek(r), 1)
    expect_equal(inputs_of(r), list(`input$n` = 1))
    expect_true(stale(r))
  })
})

test_that("a superseded run does not overwrite the inputs of the last value", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync({
      Sys.sleep(input$wait)
      input$n
    })
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1, wait = 0)
    wait_for(function() identical(peek(r), 1), session)
    session$setInputs(n = 2, wait = 5)
    expect_equal(status_async(r), "running")
    cancel_async(r)
    session$flushReact()
    expect_equal(inputs_of(r), list(`input$wait` = 0, `input$n` = 1))
  })
})

# The scenario from issue #2: without a guard a chained async would briefly
# combine a new input with an upstream value computed from the old one.
test_that("a chained async waits for its upstream and then re-runs", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    wind <- reactiveAsync({
      Sys.sleep(1)
      paste0("wind-", input$spot)
    })
    scored <- reactiveAsync(paste(wind(), input$spot))
    log <- new.env()
    log$seen <- character()
    shiny::observe(log$seen <- c(log$seen, scored()))
  }, {
    session$setInputs(spot = "a")
    wait_for(function() identical(peek(scored), "wind-a a"), session)
    session$setInputs(spot = "b")
    expect_equal(peek(scored), "wind-a a")
    # Nothing else touches scored: the stored inputs of wind must re-run it.
    wait_for(function() identical(peek(scored), "wind-b b"), session)
    expect_equal(log$seen, c("wind-a a", "wind-b b"))
    expect_false("wind-a b" %in% log$seen)
  })
})

test_that("a halting upstream read counts as stale", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    gated <- shiny::reactive(shiny::req(input$n))
    r <- reactiveAsync(gated() * 2)
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1)
    wait_for(function() identical(peek(r), 2), session)
    expect_false(stale(r))
    session$setInputs(n = NULL)
    session$flushReact()
    expect_true(stale(r))
    expect_equal(peek(r), 2)
  })
})

test_that("an upstream error propagates down the chain", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    wind <- reactiveAsync({
      if (input$spot == "bad") stop("no such spot")
      paste0("wind-", input$spot)
    })
    scored <- reactiveAsync(paste(wind(), input$spot))
    output$x <- shiny::renderText(scored())
  }, {
    session$setInputs(spot = "a")
    wait_for(function() identical(peek(scored), "wind-a a"), session)
    session$setInputs(spot = "bad")
    wait_for(function() status_async(scored) == "error", session)
    expect_error(shiny::isolate(scored()), "no such spot")
    # The error is re-thrown until wind recomputes, as wind() itself would.
    session$setInputs(spot = "c")
    value <- function() tryCatch(shiny::isolate(scored()), error = function(e) NULL)
    wait_for(function() identical(value(), "wind-c c"), session)
    expect_equal(status_async(wind), "success")
  })
})

test_that("staleness follows a chain of async reactives", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    a <- reactiveAsync({
      Sys.sleep(1)
      paste0("a-", input$spot)
    })
    b <- reactiveAsync(paste0("b(", a(), ")"))
    final <- reactiveAsync(paste(b(), input$spot))
    log <- new.env()
    log$seen <- character()
    shiny::observe(log$seen <- c(log$seen, final()))
  }, {
    session$setInputs(spot = "x")
    wait_for(function() identical(peek(final), "b(a-x) x"), session)
    session$setInputs(spot = "y")
    expect_true(stale(a))
    expect_true(stale(b))
    wait_for(function() identical(peek(final), "b(a-y) y"), session)
    expect_equal(log$seen, c("b(a-x) x", "b(a-y) y"))
    expect_false(stale(b))
  })
})

test_that("an upstream gated with bindEvent() holds the downstream until the next event", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    wind <- reactiveAsync(paste0("wind-", input$spot)) |> shiny::bindEvent(input$go)
    scored <- reactiveAsync(paste(wind(), input$spot))
    output$x <- shiny::renderText(scored())
  }, {
    session$setInputs(spot = "a", go = 1)
    wait_for(function() identical(peek(scored), "wind-a a"), session)
    session$setInputs(spot = "b")
    session$flushReact()
    expect_equal(peek(scored), "wind-a a")
    session$setInputs(go = 2)
    wait_for(function() identical(peek(scored), "wind-b b"), session)
  })
})

test_that("an isolated upstream read is neither waited for nor depended on", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    wind <- reactiveAsync({
      Sys.sleep(1)
      paste0("wind-", input$spot)
    })
    scored <- reactiveAsync(paste(input$spot, isolate(wind())))
    output$x <- shiny::renderText(scored())
  }, {
    # As with reactive(), a halting isolate(wind()) never re-runs scored on
    # its own; it runs again when input$spot changes.
    session$setInputs(spot = "a")
    wait_for(function() status_async(wind) == "success", session)
    session$setInputs(spot = "a2")
    wait_for(function() identical(peek(scored), "a2 wind-a"), session)
    session$setInputs(spot = "b")
    wait_for(function() identical(peek(scored), "b wind-a"), session)
    expect_equal(status_async(wind), "running")
  })
})

test_that("a module's returned async reactive is waited for", {
  local_pool()
  windServer <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      list(wind = reactiveAsync({
        Sys.sleep(1)
        paste0("wind-", input$spot)
      }))
    })
  }
  shiny::testServer(function(input, output, session) {
    mod <- windServer("w")
    scored <- reactiveAsync(paste(mod$wind(), input$k))
    log <- new.env()
    log$seen <- character()
    shiny::observe(log$seen <- c(log$seen, scored()))
  }, {
    session$setInputs(`w-spot` = "a", k = 1)
    wait_for(function() identical(peek(scored), "wind-a 1"), session)
    session$setInputs(`w-spot` = "b")
    wait_for(function() identical(peek(scored), "wind-b 1"), session)
    expect_equal(log$seen, c("wind-a 1", "wind-b 1"))
  })
})

test_that("a downstream waiting on its upstream reports running and drops its stale run", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    wind <- reactiveAsync({
      Sys.sleep(1)
      paste0("wind-", input$spot)
    })
    scored <- reactiveAsync({
      Sys.sleep(input$wait)
      paste(wind(), input$spot, input$k)
    })
    log <- new.env()
    log$seen <- character()
    shiny::observe(log$seen <- c(log$seen, scored()))
  }, {
    session$setInputs(spot = "a", k = 1, wait = 0)
    wait_for(function() identical(peek(scored), "wind-a a 1"), session)
    expect_equal(status_async(scored), "success")

    # A slow run on the old upstream value starts, then the upstream goes stale.
    session$setInputs(wait = 2, k = 2)
    session$flushReact()
    expect_equal(status_async(scored), "running")
    session$setInputs(spot = "b")
    session$flushReact()
    expect_equal(status_async(scored), "running")
    expect_true(has_value_async(scored))
    expect_equal(peek(scored), "wind-a a 1")
    expect_null(attr(scored, "shinyasync")$current)

    wait_for(function() identical(peek(scored), "wind-b b 2"), session)
    expect_equal(status_async(scored), "success")
    expect_false("wind-a a 2" %in% log$seen)
    expect_false("wind-a b 2" %in% log$seen)
  })
})

test_that("waiting status follows the upstream and clears when it settles without a run", {
  local_pool()
  shiny::testServer(function(input, output, session) {
    wind <- reactiveAsync({
      Sys.sleep(1)
      paste0("wind-", input$spot)
    })
    scored <- reactiveAsync({
      req(input$ok)
      paste(wind(), input$spot)
    })
    output$x <- shiny::renderText(scored())
  }, {
    session$setInputs(spot = "a", ok = TRUE)
    wait_for(function() identical(peek(scored), "wind-a a"), session)
    session$setInputs(spot = "b")
    session$flushReact()
    expect_equal(status_async(scored), "running")
    # The req() now halts: no longer waiting, and nothing runs.
    session$setInputs(ok = NULL)
    session$flushReact()
    expect_equal(status_async(scored), "success")
    wait_for(function() status_async(wind) == "success", session)
    expect_equal(status_async(scored), "success")
    expect_equal(peek(scored), "wind-a a")
  })
})
