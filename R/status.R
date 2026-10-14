#' Status of an async reactive
#'
#' A reactive read of the task status, for spinners and disabled buttons.
#' Reading the status of a [reactiveAsync()] that has not run yet starts it,
#' just as reading its value would, so a UI that waits for `"success"` before
#' reading the value does not wait forever.
#'
#' @param r An object created by [reactiveAsync()], [observeAsync()] or
#'   [reactiveValAsync()].
#'
#' @return One of `"idle"`, `"running"`, `"success"` or `"error"`.
#' @export
status_async <- function(r) {
  task <- get_task(r, "status_async")
  task$start()
  task$status()
}

#' Cancel a running async task
#'
#' Stops the running task, if any, and drops queued tasks. The last successful
#' value is kept and the status becomes `"idle"`. Interrupting a task that is
#' already executing requires a daemon pool with a dispatcher, which
#' [pool_async()] always uses; otherwise the result is discarded when it
#' arrives.
#'
#' @inheritParams status_async
#' @return `NULL`, invisibly.
#' @export
cancel_async <- function(r) {
  get_task(r, "cancel_async")$cancel()
}
