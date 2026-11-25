#' Launch the Interactive Bilingual statarbor Web Application
#'
#' Launches the interactive Shiny web interface bundled with `statarbor`,
#' supporting English and Turkish UI switching, automated assumption-guided
#' analysis, CLD-annotated plotting, advanced modeling (two-group, Firth binary,
#' ordinal CLM, factorial interaction GLM), multivariate PCA/correlation,
#' reproducible R script export, and formatted Word (.docx) reporting.
#'
#' @param launch.browser Logical; if `TRUE` (default in interactive sessions),
#'   opens the application in the default web browser.
#' @param display.mode Shiny display mode (default `"normal"`).
#'
#' @return Called for its side effect of launching the Shiny application.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(launch.browser = interactive(), display.mode = "normal") {
  needed <- c("shiny", "DT", "ggplot2")
  missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0L) {
    stop(
      sprintf(
        "The following packages are required to run the statarbor app: %s. Install them via install.packages().",
        paste(missing_pkgs, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny", package = "statarbor")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Could not locate the bundled Shiny application directory in 'statarbor'.",
         call. = FALSE)
  }

  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    display.mode = display.mode
  )
}
