test_that("input$x, input[['x']] and reactive calls are hoisted", {
  input <- shiny::reactiveValues(n = 5, m = 2)
  data <- shiny::reactive(1:3)
  a <- analyze_expr(quote({
    x <- rnorm(input$n) + input[["m"]]
    sum(data(), input$n)
  }), environment())

  expect_length(a$hoisted, 3)
  expect_equal(a$hoisted[[1]], quote(input$n))
  expect_equal(a$hoisted[[2]], quote(input[["m"]]))
  expect_equal(a$hoisted[[3]], quote(data()))
  expect_false("input" %in% all.names(a$expr))
  expect_true(all(c(".sa_1", ".sa_2", ".sa_3") %in% all.names(a$expr)))
})

test_that("non-reactive calls and quoted code are left alone", {
  input <- list(n = 1)
  f <- function() 1
  a <- analyze_expr(quote({ input$n + f(); quote(f()) }), environment())
  expect_length(a$hoisted, 0)
})

test_that("req(), validate() and invalidateLater() run on the session unless they use locals", {
  input <- shiny::reactiveValues(n = 5)
  req <- shiny::req
  a <- analyze_expr(quote({
    shiny::invalidateLater(1000)
    n <- req(input$n)
    y <- n * 2
    req(y > 0)
    y
  }), environment())

  expect_equal(a$hoisted[[1]], quote(shiny::invalidateLater(1000)))
  expect_equal(a$hoisted[[2]], quote(req(input$n)))
  expect_length(a$hoisted, 2)
  expect_true("req" %in% all.names(a$expr))
})

test_that("resolve() evaluates hoisted reads and ships user globals transitively", {
  input <- shiny::reactiveValues(n = 3)
  helper <- function(x) x + offset
  offset <- 100
  fit <- function(n) helper(n)
  cap <- new_capture(quote(fit(input$n) + extra), environment(), globals = list(extra = 1))
  job <- shiny::isolate(cap$resolve())

  expect_equal(job$args, list(.sa_1 = 3))
  expect_setequal(names(job$globals), c("fit", "extra"))
  fit_env <- environment(job$globals$fit)
  expect_identical(parent.env(fit_env), globalenv())
  expect_setequal(ls(fit_env), c("fit", "helper", "offset"))
  expect_identical(environment(fit_env$helper), fit_env)

  for (nm in names(job$globals)) assign(nm, job$globals[[nm]], envir = globalenv())
  withr::defer(rm(list = names(job$globals), envir = globalenv()))
  expect_equal(eval(job$expr, job$args, globalenv()), 104)
})

test_that("attached packages used by the expression are recorded", {
  env <- new.env(parent = globalenv())
  deps <- collect_globals(quote(stats::sd(x) + median(y)), env)
  expect_true("stats" %in% deps$pkgs)
  expect_length(deps$values, 0)
})

test_that("unresolvable reactive reads give an informative error", {
  input <- shiny::reactiveValues(x1 = 1)
  cap <- new_capture(quote(input[[paste0("x", 1)]]), environment())
  expect_error(shiny::isolate(cap$resolve()), "reactive object")
})

test_that("isolate() reads are evaluated on the session without a dependency", {
  input <- shiny::reactiveValues(n = 5, seed = 1)
  isolate <- shiny::isolate
  a <- analyze_expr(quote({
    k <- 2
    isolate(input$seed) + input$n + isolate(input$n * k)
  }), environment())

  expect_equal(a$hoisted[[1]], quote(isolate(input$seed)))
  expect_equal(a$hoisted[[2]], quote(input$n))
  expect_equal(a$hoisted[[3]], quote(shiny::isolate(input$n)))
  expect_equal(a$expr[[3]], quote(.sa_1 + .sa_2 + (.sa_3 * k)))
})

test_that("req() and validate() inside conditionals stay in the worker", {
  input <- shiny::reactiveValues(a = NULL, b = NULL)
  req <- shiny::req
  a <- analyze_expr(quote(if (is.null(input$a)) 0 else req(input$b)), environment())

  expect_equal(a$hoisted, list(quote(input$a), quote(input$b)))
  expect_equal(a$expr, quote(if (is.null(.sa_1)) 0 else req(.sa_2)))

  a <- analyze_expr(quote({
    req(input$a)
    x <- input$a && shiny::validate(shiny::need(input$b, "b"))
    for (i in 1:2) req(input$b)
  }), environment())
  expect_equal(a$hoisted[[1]], quote(req(input$a)))
  expect_length(a$hoisted, 3)
  expect_true("validate" %in% all.names(a$expr))
  expect_equal(sum(all.names(a$expr) == "req"), 1)
})

test_that("NULL server variables are shipped", {
  env <- new.env(parent = globalenv())
  env$limit <- NULL
  deps <- collect_globals("limit", env)
  expect_true("limit" %in% names(deps$values))
  expect_null(deps$values$limit)
})

test_that("assigning to a server object ships it; assigning to reactive values errors", {
  rv <- shiny::reactiveValues(count = 0)
  df <- data.frame(x = 1)
  a <- analyze_expr(quote({ df$y <- 2; df }), environment())
  expect_true("df" %in% a$free)

  expect_error(
    analyze_expr(quote({ res <- 1; rv$result <- res; rv$count + 1 }), environment()),
    "assigns to reactive values"
  )
})

test_that("lambda parameters shadow reactive objects", {
  input <- shiny::reactiveValues(a = 1)
  a <- analyze_expr(quote(sapply(xs, function(input) input$a)), environment())
  expect_length(a$hoisted, 0)
  a <- analyze_expr(quote(lapply(1:2, function(i) input$a + i)), environment())
  expect_length(a$hoisted, 1)
})

test_that("reactive reads inside formulas are hoisted and formula symbols are soft globals", {
  input <- shiny::reactiveValues(x = 1)
  y <- shiny::reactive(1)
  w <- 1:3
  a <- analyze_expr(quote(lm(y ~ input$x + w, data = d)), environment())
  expect_equal(a$hoisted, list(quote(input$x)))
  expect_equal(a$expr, quote(lm(y ~ .sa_1 + w, data = d)))
  expect_true(all(c("y", "w") %in% a$soft))

  deps <- collect_globals(a$free, environment(), soft = a$soft)
  expect_equal(deps$values$w, 1:3)
  expect_false("y" %in% names(deps$values))
})

test_that("helpers with same-named free variables keep their own bindings", {
  k <- 100
  f1 <- local({ k <- 1; function() k })
  f2 <- local({ k <- 2; function() k })
  rec <- function(n) if (n <= 0) 0 else n + rec(n - 1)
  cap <- new_capture(quote(f1() + f2() + k + rec(3)), environment())
  job <- shiny::isolate(cap$resolve())

  expect_equal(job$globals$k, 100)
  expect_equal(job$globals$f1(), 1)
  expect_equal(job$globals$f2(), 2)
  expect_equal(job$globals$rec(3), 6)
})

test_that("closure free names are cached across runs", {
  helper <- function(x) x + 1
  cap <- new_capture(quote(helper(input$n)), environment())
  input <- shiny::reactiveValues(n = 1)
  shiny::isolate(cap$resolve())
  local_mocked_bindings(free_names = function(x) stop("findGlobals should not run again"))
  expect_silent(job <- shiny::isolate(cap$resolve()))
  expect_equal(job$globals$helper(1), 2)
})
