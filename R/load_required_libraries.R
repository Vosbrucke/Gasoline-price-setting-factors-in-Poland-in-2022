#' Install any missing package and load it if available.
#'
#' @param packages Character vector of package names to ensure are installed.
#' @return Invisibly returns TRUE when the package loop finishes.
#' @examples
#' \dontrun{install_if_missing(c('ggplot2', 'dplyr'))}
install_if_missing <- function(packages) {
  installed_packages <- rownames(installed.packages())

  for (pkg in packages) {
    if (!pkg %in% installed_packages) {
      tryCatch(
        install.packages(pkg, repos = "https://cloud.r-project.org"),
        error = function(e) {
          message(sprintf("Skipping unavailable package: %s", pkg))
        }
      )
    }

    if (requireNamespace(pkg, quietly = TRUE)) {
      library(pkg, character.only = TRUE, quietly = TRUE)
    }
  }

  invisible(TRUE)
}

packages <- c(
  "data.table",
  "dplyr",
  "readr",
  "stringr",
  "tidyr",
  "lubridate",
  "rvest",
  "openxlsx",
  "xml2",
  "gdata",
  "ggplot2",
  "patchwork",
  "glue",
  "ggh4x",
  "tidyverse",
  "viridis",
  "cowplot",
  "rmarkdown",
  "eurostat",
  "wesanderson",
  "ggtext",
  "magrittr",
  "sf",
  "rstudioapi"
)

install_if_missing(packages)
