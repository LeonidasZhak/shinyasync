.onLoad <- function(libname, pkgname) {
  shiny::register_devmode_option(
    "shinyasync.print",
    devmode_message = paste(
      "Printing what each async reactive captures on its first run.",
      "Set `options(shinyasync.print = FALSE)` to turn this off."
    ),
    devmode_default = TRUE
  )
}
