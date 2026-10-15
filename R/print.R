# Printing -----------------------------------------------------------------
#
# print() shows what dependency capture resolved: the calls hoisted to the
# session, the objects and packages shipped to the worker, and the rewritten
# worker expression. Nothing is evaluated and the task is not started, so it
# is safe at any point in a server function.

#' @export
print.reactiveAsync <- function(x, ...) print_async(x)

#' @export
print.observeAsync <- function(x, ...) print_async(x)

#' @export
print.reactiveValAsync <- function(x, ...) print_async(x)

print_async <- function(x) {
  print_task(attr(x, "shinyasync", exact = TRUE))
  invisible(x)
}

# In shiny::devmode(), each task prints itself on its first run, when the
# analysis is final. options(shinyasync.print = FALSE) turns it off.
devmode_print <- function(task) {
  if (isTRUE(task$printed)) return(invisible())
  task$printed <- TRUE
  on <- shiny::get_devmode_option("shinyasync.print", default = FALSE)
  if (isTRUE(on)) print_task(task)
  invisible()
}

print_task <- function(task) {
  status <- shiny::isolate(task$status())
  cli::cat_line(cli::format_inline("{.cls {task$kind}} {task$label}"))
  cli::cat_line("Status: ", style_status(status))

  if (is.null(task$capture)) return(invisible())
  d <- task$capture$describe()
  if (!is.null(d$error)) {
    cli::cat_line(cli::col_red(cli::symbol$cross), " ", d$error)
  }
  if (is.null(d$hoisted)) return(invisible())

  print_section(
    "Evaluated on the session",
    vapply(d$hoisted, function(k) cli::format_inline("{.code {k}}"), "")
  )
  shipped <- c(
    vapply(names(d$shipped), function(nm) {
      cli::format_inline("{.code {nm}} {.cls {class_of(d$shipped[[nm]])}}")
    }, ""),
    vapply(names(d$globals), function(nm) {
      cli::format_inline("{.code {nm}} {.cls {class_of(d$globals[[nm]])}} from {.arg globals}")
    }, "")
  )
  print_section("Shipped to the worker", shipped)
  print_section(
    "Attached in the worker",
    vapply(d$pkgs, function(p) cli::format_inline("{.pkg {p}}"), "")
  )
  # worker_expr() wraps a package-free expression in a bare `{`; show the
  # expression as written instead.
  expr <- d$expr
  if (is.call(expr) && identical(expr[[1]], quote(`{`)) && length(expr) == 2L) expr <- expr[[2]]
  print_section("Worker expression", deparse(expr, width.cutoff = 70L), bullets = FALSE)
  invisible()
}

print_section <- function(title, lines, bullets = TRUE) {
  cli::cat_line(cli::style_bold(title))
  if (!length(lines)) {
    cli::cat_line("  ", cli::col_grey("none"))
  } else if (bullets) {
    cli::cat_line("  ", cli::symbol$bullet, " ", lines)
  } else {
    cli::cat_line("  ", lines)
  }
}

style_status <- function(status) {
  switch(
    status,
    running = cli::col_yellow(status),
    success = cli::col_green(status),
    error = cli::col_red(status),
    cli::col_grey(status)
  )
}

class_of <- function(x) if (is.function(x)) "function" else class(x)[[1]]
