# Provenance ---------------------------------------------------------------
#
# Every run of a reactiveAsync() records the reactive reads it was computed
# from (see new_capture()$inputs). A chained async reactive uses them to wait
# for its upstream: while an upstream task runs, reading it returns the
# previous value, so a downstream expression could otherwise combine a new
# input$x with an upstream value computed from the old one, which a chain of
# plain reactive() expressions never does.

# Is `task`'s current value out of date with respect to the reads it was
# computed from? TRUE before the first result, when any of those reads now
# evaluates to something else (compared with identical()), when a read halts,
# or when an upstream reactiveAsync() is itself stale. An error in the task is
# re-thrown, as a read of its value would. A reactive read: every value it
# compares is a dependency, so a guard re-runs when the upstream catches up.
is_stale <- function(task) {
  err <- task$error()
  if (!is.null(err)) stop(err)
  value <- task$value()
  stored <- task$inputs()
  if (is.null(value)) return(TRUE)
  for (up in task$capture$upstream()) {
    if (is_stale(attr(up, "shinyasync", exact = TRUE))) return(TRUE)
  }
  live <- tryCatch(
    list(task$capture$inputs()),
    shiny.silent.error = function(e) NULL
  )
  if (is.null(live)) return(TRUE)
  !identical(stored, live[[1]])
}

# Are any of the reactiveAsync() objects `capture` reads stale? A reactive read
# like is_stale(). An upstream error is re-thrown.
upstream_stale <- function(capture) {
  for (up in capture$upstream()) {
    if (is_stale(attr(up, "shinyasync", exact = TRUE))) return(TRUE)
  }
  FALSE
}

# Is any upstream of `task` running, waiting included? Read while `task` waits
# so its status shows "running" only while something upstream is in flight.
upstream_running <- function(task) {
  if (is.null(task$capture)) return(FALSE)
  for (up in task$capture$upstream()) {
    if (identical(attr(up, "shinyasync", exact = TRUE)$current_status(), "running")) {
      return(TRUE)
    }
  }
  FALSE
}

# The halt resolve() signals while an upstream is stale: a silent error, so an
# observer stops quietly, with a class run_trigger() can tell from a req().
waiting_halt <- function() {
  stop(structure(
    class = c("shinyasync_waiting", "shiny.silent.error", "error", "condition"),
    list(message = "waiting for an upstream async reactive", call = NULL)
  ))
}
