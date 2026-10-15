#' Start a mirai with dependency capture
#'
#' Evaluates `expr` on a `mirai` daemon the way [reactiveAsync()] does, but
#' returns the `mirai` itself instead of a reactive, for manual control: set
#' it on a [reactiveValAsync()], or start several and collect them yourself.
#' A raw [mirai::mirai()] ships only what you hand it; `mirai_async()` sends
#' the user-defined functions and objects `expr` refers to, and the functions
#' they call, along automatically, and attaches the packages it uses.
#'
#' Reactive reads in `expr` (`input$x`, `data()`) and `req()`, `validate()`,
#' `invalidateLater()` and `isolate()` calls are evaluated on the session
#' thread before the task starts, under the same rules as in
#' [reactiveAsync()]. Called inside an [shiny::observe()], they behave as they
#' would in plain Shiny: the reads register dependencies on that observer, a
#' failed `req()` halts it, and `invalidateLater()` reschedules it. Outside a
#' reactive context `expr` must not read reactives.
#'
#' The task starts immediately. Errors while resolving `expr` on the session,
#' such as a reactive read that cannot be hoisted, are raised by the call;
#' errors in the worker arrive as the mirai's error value and are re-thrown
#' when a [reactiveValAsync()] holding it is read.
#'
#' @inheritParams reactiveAsync
#' @param ... Passed to [mirai::mirai()], e.g. `.timeout` or `.compute`. Pass
#'   data to the worker with `globals`, not here.
#'
#' @return A `mirai`.
#' @seealso [reactiveValAsync()], whose setter accepts the result.
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   server <- function(input, output, session) {
#'     pool_async(workers = 2)
#'
#'     ranking <- reactiveValAsync()
#'     observe({
#'       # rank_spots() and the helpers it calls are shipped to the worker.
#'       ranking(mirai_async(rank_spots(spots, days = input$days)))
#'     }) |> bindEvent(input$rank)
#'
#'     output$ranking <- renderTable(ranking())
#'     output$status  <- renderText(status_async(ranking))
#'   }
#' }
mirai_async <- function(expr, ..., globals = NULL, env = parent.frame(), quoted = FALSE) {
  force(env)
  if (!quoted) expr <- substitute(expr)
  capture <- new_capture(expr, env, globals)
  dispatch_job(capture$resolve(), ...)
}
