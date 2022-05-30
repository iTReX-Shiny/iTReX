#############################
## iTReX maintenance tools ##
## Author: Yannick Berker  ##
#############################

#' Save arguments in rds files named after arguments.
#'
#' @param ... Variables to be saved (not quoted).
debug_save <- function(...) {
  debug_dir <- Sys.getenv("ITREX_DEBUG_DIR")
  if (nchar(debug_dir) == 0) {
    return(invisible(NULL))
  }

  vars <- list(...)
  varnames <- vapply(substitute(list(...)), deparse, character(1))[-1]
  for (i in seq_along(vars)) {
    saveRDS(vars[[i]], file.path(debug_dir, paste0(varnames[i], ".rds")))
  }
}

#' Run (and update) tests in the working copy.
#'
#' @param ... Parameters (such as testnames) passed to [shinytest::testApp()].
#' @param pkg The directory holding the working copy.
test_and_update <- function(..., pkg = getwd()) {
  tests <- shinytest::testApp(appDir = file.path(pkg, "inst", "shiny"), ...)
  failures <- which(vapply(tests$results, function(res) !res$pass, logical(1)))
  testnames_fail <- lapply(tests$results[failures], function(res) res$name)
  update_tests(pkg = pkg, testnames = testnames_fail)
}

#' Update test-expected in the working copy.
#'
#' @param ... Parameters passed to [shinytest::snapshotUpdate()].
#' @param pkg The directory holding the working copy.
update_tests <- function(..., pkg = getwd()) {
  shinytest::snapshotUpdate(appDir = file.path(pkg, "inst", "shiny"), ...)
}

#' Record a new test and store it in the working copy.
#'
#' @param pkg The directory holding the working copy.
record_test <- function(pkg = getwd()) {
  shinytest::recordTest(file.path(pkg, "inst", "shiny"))
}

#' Build manuals (including vignettes) in the working copy.
#'
#' @param pkg The directory holding the working copy.
build_manuals <- function(pkg = getwd()) {
  devtools::document(pkg = pkg)

  shiny_docs_path <- file.path(pkg, "inst", "shiny", "www", "docs")

  devtools::build_manual(pkg = pkg, path = shiny_docs_path)
  pdf_file <- dir(shiny_docs_path, "^iTReX_[0-9\\.]+\\.pdf$", full.names = TRUE)
  stopifnot(length(pdf_file) == 1)
  fs::file_move(pdf_file, file.path(shiny_docs_path, "iTReX-Package-Manual.pdf"))

  tryCatch(devtools::build_vignettes(pkg = pkg), error = function(cond) {
    warning("build_vignettes() failed, falling back to rmarkdown::render()")
    rmarkdown::render(file.path(pkg, "vignettes", "iTReX-User-Manual.Rmd"))
  })
  html_files <- list.files(file.path(pkg, "doc"), "\\.html$", full.names = TRUE)
  stopifnot(length(html_files) > 0)
  for (html_file in html_files) {
    file.copy(html_file, shiny_docs_path, overwrite = TRUE)
  }
}

drop_cols <- "wellID"
layout_cols <- c(
  fileName = "Plate",
  rowID = "Row",
  colID = "Column",
  sampleID = "Sample",
  wellType = "WellType",
  name = "Treatment",
  concentration = "Concentration",
  batch = "Replicate",
  add_on = "AddOn",
  value = "Readout"
)

#' Convert layout from old to new format. Can be safely applied to new layouts.
#'
#' @param df A data frame.
#' @param is_demo Whether to apply further normalization to minimize entries.
clean_and_convert_layout <- function(df, is_demo = FALSE) {
  # drop columns
  df <- df[, !(colnames(df) %in% drop_cols)]

  # rename columns
  df <- data.table::setnames(
    df, names(layout_cols), layout_cols,
    skip_absent = TRUE
  )

  missing_cols <- setdiff(layout_cols, colnames(df))
  if (length(missing_cols)) {
    stop("Layout file is missing required columns: ", toString(missing_cols))
  }

  # order columns
  df <- df[, layout_cols]

  # order rows
  df <- df[order(df$Plate, df$Row, df$Column), ]

  id_cols <- c("Plate", "Row", "Column")
  if (anyDuplicated(df[, id_cols])) {
    stop("Layout contains duplicate (Plate, Row, Column) combinations.")
  }

  if (is_demo) {
    n_actual <- nrow(df)
    # Effectively, length(unique(df$Plate)) * length(unique(df$Row)) * ...
    n_expected <- prod(vapply(
      id_cols, function(col) length(unique(df[, col])), numeric(1)
    ))

    if (n_actual != n_expected) {
      stop(glue::glue(
        "Layout does not appear to be complete: ",
        "actuals rows, {n_actual}; expected rows, {n_expected}."
      ))
    }

    df$Replicate[df$WellType == "empty"] <- 1
  }

  # standardize contents
  char_columns <- c("Plate", "Row", "Sample", "WellType", "Treatment", "AddOn")
  numeric_columns <- c("Column", "Concentration", "Replicate", "Readout")
  df <- dplyr::mutate_at(df, char_columns, as.character)
  df <- dplyr::mutate_at(df, numeric_columns, as.numeric)

  df$Plate <- sub("^sample_p", "", df$Plate)

  sample <- unique(df$Sample)
  if (!is.na(sample) && length(sample) == 1 && nchar(sample) > 0) {
    df$Plate <- sub(paste0("^\\Q", sample, "\\E_p"), "", df$Plate)
  }

  df$WellType <- sub("^sample$", "therapy", df$WellType)
  df$WellType <- sub("^treatment$", "therapy", df$WellType)

  if (all(is.na(df$Readout))) {
    df$Sample <- NA
  }

  df$Treatment <- sub("_(control|drug)$", "", df$Treatment)

  no_treatment <- df$WellType %in% c("empty", "untreated")
  df$Treatment[no_treatment] <- NA
  df$Concentration[no_treatment] <- NA

  df$WellType[df$WellType == "TRcontrol"] <- "TRC"

  required_well_types <- c("neg", "pos", "therapy")
  for (well_type in required_well_types) {
    if (!sum(df$WellType == well_type)) {
      stop(glue::glue("Missing required WellType {well_type} in layout."))
    }
  }

  df
}

#' Check test names against pattern.
#'
#' @param pkg The directory holding the working copy.
check_testnames <- function(pkg = getwd()) {
  shinytest_dir <- file.path(pkg, "inst", "shiny", "tests", "shinytest")
  suffix <- "\\.R$"
  shinytest_files <- dir(shinytest_dir, suffix, full.names = TRUE)
  testnames <- sub(suffix, "", basename(shinytest_files))
  testname_pattern <- paste0(
    "^((",
    "(mono|combo)_(sample(-sep)?|cohort)(_[^_]+)?_", # data, details
    "(qc|qc-mra|mra|xra|heatmap|qmh|omics|hitnet-omics)(_([^_]+|step))?", # mods, details
    ")|(",
    "tabs_[^_]+(_[^_]+)?", # tabs, details
    "))$"
  )
  invalid <- testnames[!grepl(testname_pattern, testnames)]
  if (length(invalid) > 0) {
    stop("Invalid test names: ", toString(invalid))
  }
}

clean_xlsx <- function(xlsx_file, is_matrix = NULL, test_path = NULL) {
  if (is.null(is_matrix)) {
    is_matrix <- grepl("_Readout", xlsx_file, fixed = TRUE)
  }

  # Constants
  left_style <- openxlsx::createStyle(halign = "left")
  center_style <- openxlsx::createStyle(halign = "center")
  left_top_style <- openxlsx::createStyle(
    halign = "left", valign = "top", wrapText = TRUE
  )

  test_results <- c(
    "MRA_TherapyParameters_BT-40_ST08.xlsx" = file.path(
      "mono_sample_BT-40_mra_step-expected", "01d_tr.xlsx"
    ),
    "MRA_TherapyParameters_INF-R-153_ST13.xlsx" = file.path(
      "mono_sample_INF-R-153_mra_step-expected", "01d_tr.xlsx"
    ),
    "CRA_TherapyParameters_INF-R-1632_ST09.xlsx" = file.path(
      "combo_sample-sep_xra_step-expected", "03d_combo.xlsx"
    )
  )
  find_result <- endsWith(basename(xlsx_file), names(test_results))

  if (any(find_result)) {
    n_sheets <- 2
    # Read results from test output
    result_file <- file.path(test_path, test_results[which(find_result)])
    wb <- openxlsx::loadWorkbook(result_file)
  } else {
    n_sheets <- length(openxlsx::getSheetNames(xlsx_file))
    wb <- openxlsx::createWorkbook(creator = "iTReX")
  }

  # Format sheets by writing to and reading from data frame
  for (s in seq_len(n_sheets)) {
    new_name <- if (s == 1) "Data" else "Instructions"

    # (Not necessary for test results)
    if ((any(find_result) && (s == 1))) {
      df <- openxlsx::readWorkbook(wb, s)
      openxlsx::renameWorksheet(wb, s, new_name)
      is_layout <- TRUE
      s1_cols <- NULL
    } else {
      df <- openxlsx::read.xlsx(xlsx_file, s)

      if (is_matrix) {
        colnames(df)[1] <- " "
      }

      # Convert old layout to new one
      if (s == 1) {
        colnames(df)[colnames(df) == "Conc_nM"] <- "Concentration"

        # Convert old data columns
        df_cols <- colnames(df)
        old_cols <- c(names(layout_cols), drop_cols)
        if ((length(df_cols) == length(old_cols) &&
          all(sort(df_cols) == sort(old_cols))
        ) || (
          length(df_cols) == length(layout_cols) && all(df_cols == layout_cols)
        )) {
          df <- clean_and_convert_layout(df, is_demo = TRUE)
        }
        s1_cols <- colnames(df)
      } else if (s == 2) {
        df$Column[df$Column == "Conc_nM"] <- "Concentration"

        # Convert old instructions rows
        if (any(df$Column %in% drop_cols)) {
          for (old in seq_along(layout_cols)) {
            # drop entries
            df <- df[!(df$Column %in% layout_cols), ]

            # rename entries
            df$Column <- gsub(
              names(layout_cols)[old], layout_cols[old], df$Column
            )

            # order entries
            df <- df[order(match(df$Column, layout_cols)), ]
          }
        }

        s2_col1_entries <- df$Column
        if (!is.null(s1_cols) && !all(s2_col1_entries == s1_cols)) {
          stop("Incorrect column 1 in ", basename(xlsx_file))
        }
      }

      openxlsx::addWorksheet(wb, new_name)
      is_layout <- (s == 1) && !is_matrix
      openxlsx::writeDataTable(wb, s, df,
        tableName = new_name, firstColumn = !is_layout, withFilter = is_layout,
        tableStyle = if (is_matrix) "TableStyleMedium9" else "TableStyleLight9"
      )
    }

    openxlsx::freezePane(wb, s, firstRow = TRUE, firstCol = is_matrix)
    widths <- if (s == 1) "auto" else c(18, 110)
    openxlsx::setColWidths(wb, s, seq_len(ncol(df)), widths)

    if (is_matrix) {
      n_sheets <- 1
      break
    }
  }
  openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(wb, xlsx_file)
  for (s in seq_len(n_sheets)) {
    df <- openxlsx::readWorkbook(wb, s)
    rows <- seq_len(1 + nrow(df))
    cols <- seq_len(ncol(df))
    if (is_matrix) {
      openxlsx::addStyle(wb, s, center_style, rows, cols, gridExpand = TRUE)
    } else if (s == 1) {
      openxlsx::addStyle(wb, s, center_style, rows, cols, gridExpand = TRUE)
      openxlsx::addStyle(wb, s, left_style, 1, cols, gridExpand = TRUE)
    } else {
      openxlsx::addStyle(wb, s, left_top_style, rows, cols, gridExpand = TRUE)
    }
    widths <- wb$colWidths[[s]]
    openxlsx::setColWidths(wb, s, seq_along(widths), as.numeric(widths) + 5)
  }
  openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)
  clean_zip(xlsx_file)
}

clean_zip <- function(zip_file, my_time = "2021-01-01T00:00:00Z") {
  exdir <- tempfile()
  utils::unzip(zip_file, exdir = exdir)

  unlink(file.path(exdir, "__MACOSX"), recursive = TRUE)
  unlink(file.path(exdir, ".DS_Store"), recursive = TRUE)

  repeat {
    children <- list.files(exdir, full.names = TRUE)
    has_single_dir <- length(children) == 1 && dir.exists(children[1])
    if (has_single_dir) {
      exdir <- children[1]
    } else {
      break
    }
  }

  if (endsWith(zip_file, ".xlsx")) {
    core_files <- dir(exdir, "^core.xml$", recursive = TRUE, full.names = TRUE)
    pattern <- "<dcterms:created(.*)>.*</dcterms:created>"
    replace <- paste0("<dcterms:created\\1>", my_time, "</dcterms:created>")
    for (core_file in core_files) {
      core_content <- gsub(
        pattern, replace, readLines(core_file, warn = FALSE)
      )
      writeLines(core_content, core_file)
    }
  }

  if (endsWith(zip_file, ".zip")) {
    matrix_files <- list.files(
      exdir, "\\.(txt|xlsx)$",
      full.names = TRUE, recursive = TRUE
    )
    no__n <- function(filename) {
      file.path(
        dirname(filename), sub("_N([^_]+)$", "_\\1", basename(filename))
      )
    }
    fs::file_move(matrix_files, vapply(matrix_files, no__n, character(1)))

    xlsx_matrix_files <- list.files(
      exdir, "\\.xlsx$",
      full.names = TRUE, recursive = TRUE
    )
    lapply(xlsx_matrix_files, clean_xlsx, is_matrix = TRUE)
  }

  all_files <- list.files(
    exdir,
    all.files = TRUE, full.names = TRUE, recursive = TRUE
  )
  lapply(all_files, Sys.setFileTime, my_time)

  tmp_file <- tempfile()
  children <- list.files(exdir, full.names = TRUE)
  zip::zipr(tmp_file, children, include_directories = FALSE)
  fs::file_move(tmp_file, zip_file)
}

#' Prepare demo data (xlsx and zip) in the working copy.
#'
#' @param pkg The directory holding the working copy.
prepare_demos <- function(pkg = getwd()) {
  demo_path <- file.path(pkg, "inst", "shiny", "www", "demo")
  test_path <- file.path(pkg, "inst", "shiny", "tests", "shinytest")

  xlsx_files <- dir(demo_path, "\\.xlsx$", full.names = TRUE)
  lapply(xlsx_files, clean_xlsx, test_path = test_path)

  zip_files <- dir(demo_path, "\\.zip$", full.names = TRUE)
  lapply(zip_files, clean_zip)

  invisible()
}

#' Prepare working copy for commit.
#'
#' @param pkg The directory holding the working copy.
#' @param manual Whether to run [build_manuals()].
prepare <- function(pkg = getwd(), manual = TRUE) {
  options(styler.quiet = TRUE)
  styler::style_dir(
    # Sync with allowed values in assert_filetype, currently at
    # https://github.com/r-lib/styler/blob/bf559f68/R/set-assert-args.R#L74
    path = pkg, filetype = c("r", "rmd", "rmarkdown", "rnw", "rprofile")
  )

  check_testnames(pkg = pkg)

  prepare_demos(pkg = pkg)

  options(omnipath.logfile = "none")

  if (manual) {
    build_manuals(pkg = pkg)
  }
}

#' Prepare and check iTReX in the working copy.
#'
#' @param pkg The directory holding the working copy.
#' @param strict Whether to check for CRAN and BioConductor compatibility.
prep_and_check <- function(pkg = getwd(), strict = FALSE) {
  prepare(pkg, manual = FALSE)
  print(lintr::lint_dir(
    path = pkg,
    pattern = "(?i)\\.(R|Rmd)$",
    exclusions = "doc/"
  ))
  print(devtools::check(
    pkg = pkg,
    document = TRUE,
    manual = TRUE,
    cran = strict, # TRUE: Sys.setenv("_R_CHECK_EXCESSIVE_IMPORTS_" = "20") in
    # https://github.com/wch/r-source/blob/37b76f9/src/library/tools/R/check.R
    remote = strict, # TRUE: "New submission"
    incoming = strict,
    env_vars = c("_R_CHECK_PKG_SIZES_THRESHOLD_" = if (strict) "5" else "250"),
    quiet = TRUE,
    vignettes = TRUE,
  ))
  utils::capture.output(build_manuals(pkg = pkg))
  if (strict) {
    BiocCheck::BiocCheck(package = pkg) # TRUE: "Remotes"
  }
}

#' Install from GitHub and test
test_github <- function() {
  devtools::install_github(
    "iTReX-Shiny/iTReX",
    dependencies = TRUE, force = TRUE
  )
  devtools::unload("iTReX")
  iTReX::test()
}

#' Generate system dependencies for README.md
#'
#' @param pkg The directory holding the working copy.
show_system_deps <- function(pkg = getwd()) {
  distribution <- "ubuntu"
  release <- "20.04"

  system_requirements <- function(path = NULL,
                                  packages = NULL,
                                  suggests = TRUE) {
    suggests <- tolower(suggests)
    url <- glue::glue(
      "https://packagemanager.rstudio.com/__api__/repos/1/sysreqs?",
      "distribution={distribution}&release={release}&suggests={suggests}"
    )
    if (is.null(path)) {
      body <- paste("Imports:", toString(packages))
    } else {
      body <- readLines(file.path(path, "DESCRIPTION"))
    }
    res <- httr::content(httr::POST(url, body = body))

    unlist(c(
      res$pre_install,
      lapply(res$dependencies, `[[`, "pre_install"),
      res$install_scripts,
      lapply(res$dependencies, `[[`, "install_scripts")
    ))
  }

  # Verify correctness of system_requirements with suggests = TRUE
  stopifnot(all(
    sort(unique(system_requirements(path = pkg))) ==
      sort(remotes::system_requirements(distribution, release, path = pkg))
  ))

  bioc_deps <- c(
    # to install iTReX
    "devtools",
    # https://bioconductor.org/packages/release/bioc/html/ComplexHeatmap.html
    "circlize", "GetoptLong", "colorspace", "clue", "RColorBrewer",
    "GlobalOptions", "png", "Cairo", "digest", "IRanges", "matrixStats",
    "foreach", "doParallel",
    # https://bioconductor.org/packages/release/bioc/html/IRanges.html
    "stats4",
    # https://bioconductor.org/packages/release/bioc/html/imageHTS.html
    "tools", "Biobase", "hwriter", "methods", "vsn", "stats", "utils", "e1071",
    # https://bioconductor.org/packages/release/bioc/html/Biobase.html
    "methods",
    # https://bioconductor.org/packages/release/bioc/html/vsn.html
    "methods", "affy", "limma", "lattice", "ggplot2",
    # https://bioconductor.org/packages/release/bioc/html/affy.html
    "affyio", "BiocManager", "graphics", "grDevices", "methods",
    "preprocessCore", "stats", "utils", "zlibbioc",
    # https://bioconductor.org/packages/release/bioc/html/affyio.html
    "zlibbioc", "methods",
    # https://bioconductor.org/packages/release/bioc/html/preprocessCore.html
    "stats",
    # https://bioconductor.org/packages/release/bioc/html/zlibbioc.html
    # none
    # https://bioconductor.org/packages/release/bioc/html/limma.html
    "grDevices", "graphics", "stats", "utils", "methods",
    # https://bioconductor.org/packages/release/bioc/html/OmnipathR.html
    "checkmate", "curl", "digest", "dplyr", "httr", "igraph", "jsonlite",
    "later", "logger", "magrittr", "progress", "purrr", "rappdirs", "readr",
    "readxl", "rlang", "stats", "stringr", "tibble", "tidyr", "tidyselect",
    "tools", "utils", "xml2", "yaml"
  )

  message(distribution, " ", release)
  for (suggests in c(FALSE, TRUE)) {
    reqs <- c(
      system_requirements(packages = bioc_deps, suggests = suggests),
      system_requirements(path = pkg, suggests = suggests)
    )
    if (!length(reqs)) {
      next
    }
    prefix <- gsub("[^ ]+$", "", reqs[1])
    reqs <- sort(unique(reqs))
    reqs <- reqs[order(grepl("lib", reqs, fixed = TRUE))]
    packages <- gsub(prefix, "", reqs)
    command <- paste0("sudo ", prefix, paste(packages, collapse = " "))
    command <- strwrap(command, width = 80 - 2, indent = 3, exdent = 3 + 4)
    command <- paste(command, collapse = " \\\n")
    message(command)
  }
}
