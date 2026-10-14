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
