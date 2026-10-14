pool_state <- new.env(parent = emptyenv())
pool_state$refs <- 0L
pool_state$owned <- FALSE
pool_state$warned <- FALSE

#' Start a daemon pool
#'
#' Starts a `mirai` daemon pool shared by every session in this R process. The
#' pool is reference-counted: each call made inside a session holds it until
#' that session ends, and a call made outside a session holds it until the app
#' stops. The pool shuts down when the last holder releases it.
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
      args$n <- workers %||% max(1L, parallel::detectCores() - 1L, na.rm = TRUE)
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
  }
  invisible()
}

warn_if_no_daemons <- function() {
  if (!pool_state$warned && !mirai::daemons_set()) {
    pool_state$warned <- TRUE
    warning(
      "shinyasync: no mirai daemons are set, so each task starts a new R process ",
      "and cannot be cancelled. Call pool_async() in your server function.",
      call. = FALSE
    )
  }
}
