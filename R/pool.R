pool_state <- new.env(parent = emptyenv())
pool_state$refs <- 0L
pool_state$owned <- FALSE
pool_state$announced <- FALSE

#' Start a daemon pool
#'
#' Starts a `mirai` daemon pool shared by every session in this R process. The
#' pool is reference-counted: each call made inside a session holds it until
#' that session ends, and a call made outside a session holds it until the app
#' stops. The pool shuts down when the last holder releases it.
#'
#' You need not call it. The first task that runs while no daemons are set
#' starts a pool with the defaults, with a message saying so, and the session
#' it runs in holds that pool as if it had called `pool_async()`. Call it
#' yourself, before any task, to choose the number of workers or to use
#' remote daemons.
#'
#' If you already manage [mirai::daemons()] yourself, skip this; calling it
#' anyway does nothing.
#'
#' @param workers Number of local daemons. Defaults to one less than the
#'   number of cores.
#' @param ... Passed to [mirai::daemons()], e.g. `url` or `remote` for remote
#'   pools. A dispatcher is always used, because cancellation requires one.
#'
#' @return `TRUE` invisibly if this call started or joined a shinyasync pool,
#'   `FALSE` if daemons were already managed elsewhere.
#' @export
pool_async <- function(workers = NULL, ...) {
  # Daemons may have been torn down externally (e.g. mirai::daemons(0)) since
  # this process started a pool; a pool that no longer exists is not owned.
  # Holders of the old pool keep their reference, so the new pool outlives
  # them all.
  if (pool_state$owned && !mirai::daemons_set()) pool_state$owned <- FALSE
  if (!pool_state$owned) {
    if (mirai::daemons_set()) {
      message("shinyasync: mirai daemons are already set; using them as they are.")
      return(invisible(FALSE))
    }
    args <- list(...)
    if (!is.null(workers) || is.null(args$url)) {
      args$n <- workers %||% default_workers()
    }
    args$dispatcher <- TRUE
    do.call(mirai::daemons, args)
    pool_state$owned <- TRUE
  }
  pool_state$refs <- pool_state$refs + 1L

  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) {
    shiny::onStop(release_pool)
  } else {
    session$onSessionEnded(release_pool)
  }
  invisible(TRUE)
}

release_pool <- function() {
  pool_state$refs <- max(0L, pool_state$refs - 1L)
  if (pool_state$refs == 0L && pool_state$owned) {
    mirai::daemons(0)
    pool_state$owned <- FALSE
    pool_state$shinyasync_holds_pool <- FALSE
  }
  invisible()
}

# Hold the shinyasync pool for the session a task runs in, starting it if no
# daemons are set, as a pool_async() call in that session's server function
# would. Each session holds once; a module's session proxy shares its root's
# userData, so a task dispatched inside moduleServer() counts for the whole
# session. Outside a session the process holds once, released at app stop.
ensure_pool <- function() {
  set <- mirai::daemons_set()
  if (set && !pool_state$owned) return(invisible())  # user-managed daemons
  session <- shiny::getDefaultReactiveDomain()
  holder <- if (is.null(session)) pool_state else session$userData
  if (set && isTRUE(holder$shinyasync_holds_pool)) return(invisible())
  holder$shinyasync_holds_pool <- TRUE
  if (!set && !pool_state$announced) {
    pool_state$announced <- TRUE
    message(
      "shinyasync: starting a pool of ", default_workers(), " local daemons. ",
      "Call pool_async() in your server function to size it."
    )
  }
  pool_async()
}

default_workers <- function() max(1L, parallel::detectCores() - 1L, na.rm = TRUE)
