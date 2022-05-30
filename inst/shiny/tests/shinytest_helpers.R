url <- Sys.getenv("ITREX_TEST_URL")
if (nchar(url)) {
  app <- shinytest::ShinyDriver$new("../..", seed = 0, debug = "all", loadTimeout = 600e3, url = url)
} else {
  app <- shinytest::ShinyDriver$new("../..", seed = 0, debug = "all")
}

options(error = function() print(app$getDebugLog(type = "all")))
on.exit(options(error = NULL))
app$setWindowSize(1920, 1080)

testname <- tools::file_path_sans_ext(
  unlist(lapply(sys.frames(), function(frame) frame$ofile))[1]
)
app$snapshotInit(testname)

demo_path <- function(filename) {
  file.path("..", "..", "www", "demo", paste0("iTReX-Demo_", filename))
}

set_window <- function(scale = NULL) {
  if (!is.null(scale)) {
    app$executeScript(paste0(
      # works in Chrome, but not in PhantomJS
      # "document.documentElement.style.webkitTransformOrigin = '0 0';",
      # "document.documentElement.style.webkitTransform <- 'scale(", scale, ")';",

      # works in Chrome, but not in PhantomJS (and is non-standard,
      # see https://developer.mozilla.org/en-US/docs/Web/CSS/zoom)
      # "document.documentElement.style.zoom = ", scale, ";",

      # scale content
      "document.body.style.webkitTransformOrigin = '0 0';",
      "document.body.style.webkitTransform = 'scale(", scale, ")';",

      # adapt screen width and min-height
      "document.body.style['width'] = '", 100 / scale, "vw';",
      "document.body.style['min-height'] = '", 100 / scale, "vh';"
    ))
  } else {
    scale <- 1
  }
  app$setWindowSize(1920 * scale, 1080 * scale)
}

itrex_tabnames <- c(
  "QCN-mod",
  "MRA-mod/Sample Screen",
  "MRA-mod/Cohort Screen",
  "CRA-mod/Sample Screen",
  "CRA-mod/Cohort Screen",
  "HitNet-mod",
  "Omics-mod",
  "About",
  "FAQs"
)

qcn_tabnames <- c(
  "Screen Summary",
  "Raw Count Distribution",
  "Plate Layout",
  "Well Distribution",
  "Replicate Scatter Distribution",
  "Controls QC",
  "Viability Heatmap",
  "Therapy Control Response"
)

mra_tabnames <- c(
  "MRA Therapy Parameters",
  "iHeatmap",
  "MRA DSS_asym Waterfall Plots",
  "sDSS_asym Waterfall Plots"
)

cra_tabnames <- c(
  "CRA Therapy Parameters",
  "CRA DSS_asym Waterfall Plot",
  "CRA dcDSS_asym & dPI Table",
  "dcDSS_asym Waterfall Plot & dPI Matrix"
)

tab2names <- list(
  "iTReX" = itrex_tabnames,
  "QC_mod" = qcn_tabnames,
  "MRA_mod" = mra_tabnames,
  "CRA_mod" = cra_tabnames
)

tab2downloads <- list(
  "iTReX" = NULL,
  "QC_mod" = NULL,
  "MRA_mod" = c(
    "xls_table" = "tr.xlsx",
    "Heathtml" = "heatmap.html",
    "dsshtml" = "dss.html",
    "sdsshtml" = "sdss.html"
  ),
  "CRA_mod" = c(
    "xls_table2" = "tr.xlsx",
    "dsshtml_combo" = "dss.html",
    "xls_tablecombo" = "combo.xlsx",
    "comboreport" = "combo.html"
  )
)

get_tab_snapshots <- function(tabset, prefix = "", with_downloads = FALSE,
                              last_i = 0, wait_for_first = FALSE) {
  max_len_path <- 100
  full_prefix <- file.path(
    "iTReX", "inst", "shiny", "tests", "shinytest",
    paste0(testname, "-expected"), paste0("00_", prefix)
  )
  max_len_filename <- max_len_path - nchar(full_prefix)
  max_len_download <- max_len_filename - 1 # "d"
  max_len_tabname <- max_len_filename - 5 # ".json"

  tab_names <- tab2names[[tabset]]
  for (i in seq_along(tab_names)) {
    tabname <- tab_names[i]
    wait <- (i == 1 & wait_for_first) | (i > 1)
    args <- list(tab = tabname, wait_ = wait, values_ = wait, timeout_ = 300e3)
    names(args)[1] <- tabset
    do.call(app$setInputs, args)

    Sys.sleep(1)

    number <- formatC(last_i + i, width = 2, flag = "0")
    tabname <- gsub("([ /]|&.*$)", "", tabname)
    tabname <- stringr::str_trunc(tabname, max_len_tabname, ellipsis = "$")
    filename <- paste0(number, "_", prefix, tabname, ".json")
    app$snapshot(filename = filename)

    if (with_downloads && !is.null(download <- tab2downloads[[tabset]][i])) {
      download <- stringr::str_trunc(download, max_len_download, ellipsis = "$")
      filename <- paste0(number, "d", "_", prefix, download)

      button <- names(tab2downloads[[tabset]])[i]
      app$snapshotDownload(button, filename = filename)

      # Make downloads reproducible
      current_dir <- paste0(app$getSnapshotDir(), "-current")
      full_filename <- file.path(current_dir, filename)
      stopifnot(file.exists(full_filename))

      if (endsWith(full_filename, ".html")) {
        html <- readr::read_file(full_filename)
        pattern <- paste0("(", paste(
          '(?<="cur_data":")[^"]+(?=")',
          '(?<="visdat":\\{")[^"]+(?=")',
          '(?<="attrs":\\{")[^"]+(?=")',
          '(?<=")[^"]+(?=":\\{"x":\\{\\},"y":\\{\\})',
          sep = ")|("
        ), ")")
        cd <- unique(stringr::str_extract_all(html, pattern)[[1]])
        for (j in seq_along(cd)) {
          html <- stringr::str_replace_all(html, cd[j], paste0("cur_data", j))
        }
        readr::write_file(html, full_filename)
      } else if (endsWith(full_filename, ".xlsx")) {
        iTReX:::clean_zip(full_filename) # nolint: undesirable_operator.
      }
    }
  }
  i
}

run_hitnet_omics <- function(readout_demo, omics_demo, use_drugbank,
                             hitnet_threshold = NULL, omics_threshold = NULL) {
  app$uploadFile(layout_table_file = demo_path(
    paste0("MRA_LayoutAndReadouts_", readout_demo, ".xlsx")
  ))
  app$setInputs(upload_reference_samples = TRUE)
  app$uploadFile(reference_samples_file = demo_path("Controls_DKFZ_ST10-min.xlsx"))
  app$snapshot(filename = "00_Home.json")
  app$setInputs(start_oca = "click", timeout_ = 1500e3)
  Sys.sleep(1)
  app$snapshot(filename = "01_Home.json")

  if (use_drugbank) {
    drugbank_dir <- Sys.getenv("DRUGBANK_DIR")
    userpwd <- Sys.getenv("DRUGBANK_USERPWD")
    if (!nchar(userpwd) && !nchar(drugbank_dir)) {
      message("Skipping DrugBank tests...")
      return()
    }

    handle <- curl::new_handle(userpwd = userpwd, unrestricted_auth = FALSE)

    get_drugbank <- function(container, filename) {
      path <- file.path(drugbank_dir, filename)
      if (file.exists(path)) {
        return(path)
      }

      url <- paste0(
        "https://go.drugbank.com/releases/5-1-8/downloads/", container
      )
      zip_file <- tempfile(fileext = ".zip")
      curl::curl_download(url, zip_file, handle = handle)
      exdir <- if (nchar(drugbank_dir)) drugbank_dir else tempdir()
      path <- utils::unzip(zip_file, files = filename, exdir = exdir)

      return(path)
    }

    links_file <- get_drugbank("target-all-uniprot-links", "uniprot links.csv")
    targets_file <- get_drugbank("target-all-polypeptide-ids", "all.csv")
  }

  if (!is.null(hitnet_threshold)) {
    app$setInputs(iTReX = "HitNet-mod")
    app$setInputs(HitNetLicense = TRUE)
    if (use_drugbank) {
      app$setInputs(DTanno = "DBupload")
      app$uploadFile(file_drugbank = links_file)
      app$uploadFile(file_drugtarget = targets_file)
    } else {
      app$uploadFile(Tupload_input = demo_path("Targets_ST03-min.xlsx"))
    }
    app$setInputs(threshold = hitnet_threshold)
    app$setInputs(HitNet = "click", timeout_ = 1500e3)
    Sys.sleep(1)
    app$snapshot(filename = "02_HitNet.json")
  }

  if (!is.null(omics_threshold)) {
    app$setInputs(iTReX = "Omics-mod")
    app$setInputs(OmicsLicense = TRUE)
    app$uploadFile(fileOmics = demo_path(paste0("Omics_", omics_demo, ".xlsx")))
    if (use_drugbank) {
      app$setInputs(DTannoO = "DBupload")
      app$uploadFile(file_drugbankO = links_file)
      app$uploadFile(file_drugtargetO = targets_file)
    } else {
      app$uploadFile(Tupload_inputO = demo_path("Targets_ST03-min.xlsx"))
    }
    app$setInputs(threshold_O = omics_threshold)
    app$setInputs(Omics = "click", timeout_ = 1500e3)
    Sys.sleep(1)
    app$snapshot(filename = "03_Omics.json")
  }
}

shutdown <- function() {
  app$stop()
  gc()
}
