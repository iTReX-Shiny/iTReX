############################
## iTReX runner functions ##
## Author: Yannick Berker ##
############################

# Consider distributing these into the .R files where they are used
#' @import shiny
#' @importFrom dplyr %>% do filter group_by mutate rename_with summarize ungroup
# https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html
#' @importFrom ggplot2
#'   aes
#'   coord_cartesian coord_flip
#'   element_blank element_rect element_text
#'   geom_bar geom_boxplot geom_jitter geom_label geom_line geom_point
#'     geom_ribbon geom_vline geom_text geom_tile
#'   ggplot ggtitle
#'   labs
#'   margin
#'   scale_color_manual
#'   scale_fill_gradient scale_fill_gradient2 scale_fill_manual
#'   scale_x_continuous scale_x_discrete
#'   scale_y_continuous scale_y_discrete
#'   theme theme_bw theme_gray theme_void
#'   unit
#'   xlab
#'   ylab
#' @importFrom rlang .data
#' @importFrom stats complete.cases cov na.omit reorder sd setNames
#' @importFrom utils read.csv write.csv

# https://github.com/rstudio/shinythemes/issues/18
#' @importFrom shinythemes shinytheme
#' @importFrom shinyWidgets dropdownButton

# https://stackoverflow.com/a/68393286
.datatable.aware <- TRUE # nolint: object_name_linter.

# Record local installation source, commit and dirty status
install_wd <- getwd()

get_git_commit_hash <- function(dir) {
  tryCatch(
    git2r::revparse_single(repo = dir, revision = "HEAD")$sha,
    error = function(cond) NULL
  )
}
install_hash <- get_git_commit_hash(install_wd)

is_git_dir_dirty <- function(dir) {
  tryCatch(
    any(vapply(git2r::status(repo = dir), length, numeric(1)) > 0),
    error = function(cond) FALSE
  )
}
install_dirty <- is_git_dir_dirty(install_wd)

#' Report iTReX version
#'
#' @param long Whether to print the full version string with git commit hash.
#' @examples
#' version()
#' version(long = TRUE)
#' @export
version <- function(long = FALSE) {
  desc_file <- system.file("DESCRIPTION", package = "iTReX")
  version <- desc::desc_get_version(file = desc_file)
  if (!long) {
    return(version)
  }

  itrex_ver_str <- function(s_source, hash = NULL, dirty = FALSE, full = TRUE) {
    s_hash <- if (!is.null(hash)) substr(hash, 1, 7) else "unknown"
    s_dirty <- if (dirty) ", dirty" else ""
    glue::glue(
      "iTReX v{version} (installed from {s_source}",
      if (full) ", git commit {s_hash}{s_dirty}" else "",
      ")",
    )
  }

  hash <- desc::desc_get_field("RemoteSha", default = NULL, file = desc_file)
  if (!is.null(hash)) {
    return(itrex_ver_str("Git", hash))
  }

  s_source <- "local directory"
  if (!is.null(install_hash)) {
    s_source <- paste(s_source, install_wd)
    return(itrex_ver_str(s_source, install_hash, install_dirty))
  }

  s_source <- paste("unknown", s_source)
  if (!is.null(run_wd)) {
    run_hash <- get_git_commit_hash(run_wd)
    run_dirty <- is_git_dir_dirty(run_wd)
    if (!is.null(run_hash)) {
      s_source <- paste(s_source, "- working directory is", run_wd)
      return(itrex_ver_str(s_source, run_hash, run_dirty))
    }
  }

  return(itrex_ver_str(s_source, full = FALSE))
}

set_env <- function() {
  # Verify that pandoc is installed - see Readme.md
  stopifnot(rmarkdown::pandoc_available())

  Sys.setenv(LANGUAGE = "en")
  for (category in c("LC_ALL", "LC_MESSAGES")) {
    for (locale in c("en_US.UTF-8", "en_US.utf8", "en_US", "en", "English")) {
      if (suppressWarnings(Sys.setlocale(category, locale)) != "") {
        # Also call Sys.setenv to overwrite LC_COLLATE=C from R CMD check
        args <- setNames(list(locale), category)
        do.call(Sys.setenv, args)
        break
      }
    }
  }

  options(
    show.error.locations = TRUE,
    showWarnCalls = TRUE,
    knitr.package.progress = FALSE,
    knitr.chunk.echo = FALSE,
    knitr.chunk.message = FALSE,
    knitr.chunk.warning = FALSE,
    knitr.chunk.error = TRUE,
    readr.show_col_types = FALSE,
    rmarkdown.render.message = FALSE,
    omnipath.logfile = "none"
  )
  knitr::opts_chunk$set(dpi = 300)
}

run_wd <- NULL

#' Run iTReX Shiny app
#'
#' @importFrom shiny runApp
#' @export
run <- function() {
  # Remember pre-Shiny working directory
  utils::assignInMyNamespace("run_wd", getwd())

  # Running within working copy using RStudio or
  # > shiny::runApp("inst/shiny")
  # will work iff iTReX is installed as a package.
  shiny::runApp(system.file("shiny", package = "iTReX"))
}

#' Run iTReX tests
#'
#' @param testnames Passed to [shinytest::testApp]
#' @param quiet Passed to [shinytest::testApp]
#' @examples
#' test(testnames = c("tabs_home"))
#' \dontshow{
#' # https://github.com/r-lib/processx/issues/294
#' processx::supervisor_kill()
#' }
#' @importFrom shiny runApp
#' @export
test <- function(testnames = NULL, quiet = FALSE) {
  # Verify that shinytest and depencies are installed - see Readme.md
  stopifnot(shinytest::dependenciesInstalled())

  # Running within working copy using RStudio or
  # > shinytest::testApp("inst/shiny")
  # will work iff iTReX is installed as a package
  # (see also style_check_test_dir()).
  shinytest::testApp(system.file("shiny", package = "iTReX"), testnames, quiet)

  # Record a new test within working copy using
  # > shinytest::recordTest("inst/shiny")

  # Then clean up script and run using, e.g.,
  # > shinytest::testApp("inst/shiny", "all_tabs.R")
  # to create expected results.
}
