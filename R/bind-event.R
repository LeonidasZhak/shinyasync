# shiny's bindEvent.reactiveExpr would read the async result inside isolate(),
# so outputs would never update when a task resolves. Bind the event to the
# trigger observer instead and keep the result read reactive.

#' @exportS3Method shiny::bindEvent
bindEvent.reactiveAsync <- function(x, ..., ignoreNULL = TRUE, ignoreInit = FALSE,
                                    once = FALSE, label = NULL) {
  task <- get_task(x, "bindEvent")
  task$bound <- TRUE
  shiny::bindEvent(
    task$trigger, ...,
    ignoreNULL = ignoreNULL, ignoreInit = ignoreInit, once = once, label = label
  )
  x
}

#' @exportS3Method shiny::bindEvent
bindEvent.observeAsync <- function(x, ..., ignoreNULL = TRUE, ignoreInit = FALSE,
                                   once = FALSE, label = NULL) {
  task <- get_task(x, "bindEvent")
  task$bound <- TRUE
  shiny::bindEvent(
    task$trigger, ...,
    ignoreNULL = ignoreNULL, ignoreInit = ignoreInit, once = once, label = label
  )
  x
}
