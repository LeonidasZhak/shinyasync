test_that("pool_async is shared across sessions and released by the last one", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer(if (mirai::daemons_set()) mirai::daemons(0))

  s1 <- shiny::MockShinySession$new()
  s2 <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(s1, pool_async(workers = 1))
  expect_true(mirai::daemons_set())
  shiny::withReactiveDomain(s2, pool_async(workers = 1))
  expect_equal(pool_state$refs, 2L)

  s1$close()
  expect_true(mirai::daemons_set())
  s2$close()
  expect_false(mirai::daemons_set())
  expect_false(pool_state$owned)
})

test_that("pool_async leaves user-managed daemons alone", {
  local_pool(1)
  s <- shiny::MockShinySession$new()
  expect_message(res <- shiny::withReactiveDomain(s, pool_async(workers = 3)), "already set")
  expect_false(res)
  s$close()
  expect_true(mirai::daemons_set())
})

test_that("pool_async restarts a pool that was torn down externally", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer({
    if (mirai::daemons_set()) mirai::daemons(0)
    pool_state$owned <- FALSE
    pool_state$refs <- 0L
  })

  s1 <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(s1, pool_async(workers = 1))
  mirai::daemons(0)

  s2 <- shiny::MockShinySession$new()
  expect_true(shiny::withReactiveDomain(s2, pool_async(workers = 1)))
  expect_true(mirai::daemons_set())
  expect_equal(pool_state$refs, 2L)
  s2$close()
  expect_true(mirai::daemons_set())
  s1$close()
  expect_false(mirai::daemons_set())
})

test_that("the first task starts a default pool held by its session", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer({
    if (mirai::daemons_set()) mirai::daemons(0)
    pool_state$owned <- FALSE
    pool_state$refs <- 0L
  })
  local_mocked_bindings(default_workers = function() 1L)

  shiny::testServer(function(input, output, session) {
    r <- reactiveAsync(input$n + 1)
    output$x <- shiny::renderText(r())
  }, {
    # setInputs() flushes, which runs the first task.
    expect_message(session$setInputs(n = 1), "starting a pool")
    expect_true(mirai::daemons_set())
    expect_true(pool_state$owned)
    expect_equal(pool_state$refs, 1L)
    wait_for(function() identical(peek(r), 2), session)
    session$close()
    expect_false(mirai::daemons_set())
  })
})

test_that("every session that runs a task holds the default pool", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer({
    if (mirai::daemons_set()) mirai::daemons(0)
    pool_state$owned <- FALSE
    pool_state$refs <- 0L
  })
  local_mocked_bindings(default_workers = function() 1L)

  run_task <- function(s) {
    shiny::withReactiveDomain(s, {
      # Dispatched from inside a module, as most tasks are.
      m <- shiny::moduleServer("m", function(input, output, session) {
        r <- reactiveAsync(Sys.getpid())
        shiny::observe(r())
        r
      })
      s$flushReact()
      m
    })
  }
  s1 <- shiny::MockShinySession$new()
  s2 <- shiny::MockShinySession$new()
  r1 <- run_task(s1)
  r2 <- run_task(s2)
  expect_equal(pool_state$refs, 2L)
  expect_true(isTRUE(s2$userData$shinyasync_holds_pool))
  wait_for(function() status_async(r1) == "success" && status_async(r2) == "success", s2)

  s1$close()
  expect_true(mirai::daemons_set())
  expect_equal(pool_state$refs, 1L)
  s2$close()
  expect_false(mirai::daemons_set())
})

test_that("an explicit pool_async() in server() and the first task hold the pool once", {
  skip_if(mirai::daemons_set(), "daemons already set")
  withr::defer({
    if (mirai::daemons_set()) mirai::daemons(0)
    pool_state$owned <- FALSE
    pool_state$refs <- 0L
  })

  shiny::testServer(function(input, output, session) {
    pool_async(workers = 1)
    r <- reactiveAsync(input$n + 1)
    output$x <- shiny::renderText(r())
  }, {
    session$setInputs(n = 1)
    wait_for(function() identical(peek(r), 2), session)
    expect_equal(pool_state$refs, 1L)
    session$close()
    expect_false(mirai::daemons_set())
  })
})

test_that("a task on a named compute profile does not start the default pool", {
  skip_if(mirai::daemons_set(), "daemons already set")
  mirai::daemons(1, dispatcher = TRUE, .compute = "other")
  withr::defer(mirai::daemons(0, .compute = "other"))

  m <- mirai_async(Sys.getpid(), .compute = "other")
  expect_false(mirai::daemons_set())
  expect_false(pool_state$owned)
  expect_type(m[], "integer")
})
