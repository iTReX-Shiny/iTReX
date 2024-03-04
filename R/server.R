############################
## iTReX server script    ##
## Author: Dina ElHarouni ##
############################

error_notification <- function(cond) {
  debug_dir <- Sys.getenv("ITREX_DEBUG_DIR")
  if (nchar(debug_dir)) {
    cat(paste(cond), file = file.path(debug_dir, "last.err"))
  }
  showNotification(paste0(cond), duration = NULL, id = "error", type = "error")
}

update_all_pid_select_inputs <- function(session, pids) {
  select_input_names <- c(
    "sample", "sample_MRA", "sample_CRA", "sample_net", "sample_omics"
  )
  for (name in select_input_names) {
    updateSelectInput(session, name, label = "Sample", choices = pids)
  }
}

read_matrix <- function(filename) {
  ext <- tools::file_ext(filename)
  if (ext == "xlsx") {
    openxlsx::read.xlsx(filename, rowNames = TRUE)
  } else {
    file_contents <- readr::read_file(filename)
    matrix_pattern <- stringr::regex(paste0(
      # start of matrix (sequence of lines)
      "^(?:",
      # start of line
      "(?:|<>|[A-Z]:?)",
      # one line (at least 6 numbers)
      "(?:\\s*([,;\\s])\\s*(-?[\\d\\.]+|NA|NaN):?){6,}",
      # end of line
      "(?:[,;\\s])?\\n",
      # end of matrix (at least 4 lines)
      "){4,}$"
    ), multiline = TRUE)
    matrix_matches <- stringr::str_match(file_contents, matrix_pattern)
    if (is.null(matrix_matches) || anyNA(matrix_matches)) {
      stop("File ", filename, " does not match expected pattern.")
    }
    read.csv(
      text = matrix_matches[1],
      sep = matrix_matches[2],
      check.names = FALSE,
      row.names = 1
    )
  }
}

read_inputs <- function(input, project_dir) {
  is_single_sample <- input$number_of_samples == "single_sample"
  is_single_file <- input$layout_and_readouts == "single_file"
  is_single_sample_and_file <- is_single_sample && is_single_file

  if (is.null(input$layout_table_file)) {
    stop("No screen/layout table selected.")
  }

  layout_table <- openxlsx::read.xlsx(input$layout_table_file$datapath)

  layout_table <- clean_and_convert_layout(layout_table)

  layout_table$Column <- stringr::str_pad(layout_table$Column, 2, pad = "0")

  if (is_single_sample_and_file) {
    pids <- unique(layout_table$Sample)
    if (length(pids) > 1) {
      stop("Too many samples found in file: ", toString(pids))
    }
    readouts <- NULL
  } else {
    # cohort or individual sample with readouts separate from layout
    path <- input$readout_matrices_file$datapath
    if (endsWith(path, ".zip")) {
      readouts <- data.frame(path = utils::unzip(path,
        junkpaths = TRUE, exdir = file.path(project_dir, ".unzip")
      ))
      readouts$basename <- basename(readouts$path)

      # ignore hidden files
      readouts <- readouts[!startsWith(readouts$basename, "."), ]

      # sort readouts alphabetically
      readouts <- readouts[order(readouts$basename), ]

      # remove everything after the last underscore
      readouts$sample_id <- gsub("_[^_]*$", "", readouts$basename)
    } else {
      readouts <- data.frame(path = path)
      readouts$basename <- input$readout_matrices_file$name
      readouts$sample_id <- tools::file_path_sans_ext(
        sub("^iTReX-Demo_MRA_Readout-Imaging_", "", readouts$basename)
      )
    }
    pids <- unique(readouts$sample_id)

    if (nchar(Sys.getenv("ITREX_DEBUG"))) {
      cat("Found cohort:", paste(pids, collapse = " "), "\n")
    }
  }

  debug_save(layout_table)

  list(layout_table, pids, readouts)
}

fill_layout_table <- function(input, layout_table, readouts, PID) {
  if (nchar(Sys.getenv("ITREX_DEBUG"))) {
    cat("Reading sample", PID, "\n")
  }

  readout_table <- layout_table

  readout_table$PlateDisplayName <- paste0(PID, "_", readout_table$Plate)
  readout_table$PlateDisplayNameOld <- paste0(PID, "_N", readout_table$Plate)

  if (!is.null(readouts)) {
    readout_table$Sample <- PID

    readouts_pid <- readouts[readouts$sample_id == PID, ]

    matrices <- lapply(readouts_pid$path, read_matrix)
    names(matrices) <- readouts_pid$basename

    # Check that matrices have the same size
    nrow_mat <- unique(vapply(matrices, nrow, numeric(1)))
    if (length(nrow_mat) != 1) {
      stop("Matrix files for '", PID, "' have different numbers of rows.")
    }
    ncol_mat <- unique(vapply(matrices, ncol, numeric(1)))
    if (length(ncol_mat) != 1) {
      stop("Matrix files for '", PID, "' have different numbers of columns.")
    }
    matrix <- do.call(rbind, matrices)

    debug_save(matrix)

#  #  if (length(unique(readout_table$PlateDisplayName)) > 1) {
#      # Compare
#      # > L <- list(data.frame(f = c("a", "b")), data.frame(f = c("c", "d")))
#      # > names(L) <- c("X", "Y")
#      # > rownames(do.call(rbind, L))
#
#      # rownames(matrix) includes the actual file names
#      # table$PlateDisplayName happen to be our expected file names
#     # table$PlateDisplayNameOld for backward compatibility
#   #   all_names_matrix <- rep(rownames(matrix), each = ncol(matrix))
#
#    #  if (!all(startsWith(all_names_matrix, readout_table$PlateDisplayName)) &&
#    #    !all(startsWith(all_names_matrix, readout_table$PlateDisplayNameOld))) {
#     #   stop(glue::glue(
#     #     "Suffixes of matrix input files (after final '_' in file names) ",
#     #     "do not match Plate IDs in the layout for sample '{PID}'."
#     #   ))
#    #  }
#   # }

    # Check that matrices and layout are compatible
    n_mat <- nrow(matrix) * ncol(matrix)
    nrow_table <- nrow(readout_table)
    if (n_mat != nrow_table) {
      stop(glue::glue(
        "Number of entries in matrix readout files ({n_mat} for sample ",
        "'{PID}') differs from number of rows in the layout ({nrow_table})."
      ))
    }

    readout_table$Readout <- matrix(t(matrix))
  }
  readout_table <- readout_table[!is.na(readout_table$Readout), ]

  # Workaround: include sample ID in output plots (to avoid confusion between
  # samples) without changing plotting function only using plate ID.
  readout_table$Plate <- readout_table$PlateDisplayName

  debug_save(readout_table)

  combo_table <- readout_table[!is.na(readout_table$AddOn), ]
  if (nrow(combo_table) == 0) {
    if (input$type_of_normalization == "per_plate") {
      screenData <- normalize_by_plate(readout_table)
    }
    if (input$type_of_normalization == "per_treat") {
      screenData <- normalize_by_treat(readout_table)
    }
    if (input$type_of_normalization == "norm_both") {
      screenData <- normalize_by_treat_and_plate(readout_table)
    }
  } else {
    single_table <- readout_table[is.na(readout_table$AddOn), ]
    pos_table <- readout_table[readout_table$WellType == "pos", ]
    combo_table <- rbind(combo_table, pos_table)

    combo_table$Plate <- paste0(combo_table$Plate, "_combo")
    combo_table$Treatment <- paste0("combo_", combo_table$Treatment)

    if (input$type_of_normalization == "per_plate") {
      single_table <- normalize_by_plate(single_table)
      combo_table <- normalize_by_plate(combo_table)
    }
    if (input$type_of_normalization == "per_treat") {
      single_table <- normalize_by_treat(single_table)
      combo_table <- normalize_by_treat(combo_table)
    }
    if (input$type_of_normalization == "norm_both") {
      single_table <- normalize_by_treat_and_plate(single_table)
      combo_table <- normalize_by_treat_and_plate(combo_table)
    }

    screenData <- rbind(combo_table, single_table)
  }

  if (input$type_of_readout == "cell_death") {
    screenData$normVal <- (1 - screenData$normVal)
  }

  list(readout_table, screenData)
}

output_screen_summary <- function(output, QCN_list) {
  debug_save(QCN_list)

  DT_renderDataTable_round <- function(df) {
    DT::renderDataTable(
      DT::datatable(df, rownames = TRUE) %>%
        DT::formatRound(seq_len(ncol(df)), digits = 2)
    )
  }

  output$text1 <- renderText("Screen Summary")
  if (!is.null(QCN_list$summary)) {
    output$summary <- DT_renderDataTable_round(QCN_list$summary)
  } else {
    output$summary <- DT_renderDataTable_round(QCN_list$summary_mono)
    output$summary2 <- DT_renderDataTable_round(QCN_list$summary_combo)
  }

  output$text2 <- renderText("Raw Count Distribution")
  output$screen <- renderPlot(plot(QCN_list$g1))

  output$text3 <- renderText("Plate Layout")
  output$layout1 <- renderPlot(
    patchwork::wrap_plots(QCN_list$g2) + patchwork::guide_area(),
    width = 1300, height = 900
  )

  output$text4 <- renderText("Raw Count Distribution per WellType")
  output$well <- renderPlot(
    patchwork::wrap_plots(QCN_list$g4) + patchwork::guide_area(),
    width = 1100, height = 900
  )

  output$text9 <- renderText("Replicate Correlation Plot")
  output$scatter <- renderPlot(
    patchwork::wrap_plots(QCN_list$psc) + patchwork::guide_area(),
    width = 800, height = 600
  )

  output$text6 <- renderText(
    "Positive and Negative Controls Viability Quality Check"
  )

  output$textsmall1 <- renderText("Positive Control")
  output$BzCl <- renderPlot(
    patchwork::wrap_plots(QCN_list$g5) + patchwork::guide_area(),
    width = 1000, height = 600
  )

  output$textsmall2 <- renderText("Negative Control")
  output$DMSO <- renderPlot(
    patchwork::wrap_plots(QCN_list$g6) + patchwork::guide_area(),
    width = 1000, height = 600
  )

  output$text7 <- renderText("UnNormalized Heatmaps")
  output$normalized <- renderPlot(
    patchwork::wrap_plots(g7 <- QCN_list$g7) + patchwork::guide_area(),
    width = 1000, height = 600
  )

  output$text8 <- renderText("Normalized Heatmaps")
  output$unnormalized <- renderPlot(
    patchwork::wrap_plots(QCN_list$g8) + patchwork::guide_area(),
    width = 1000, height = 600
  )

  output$text10 <- renderText("Therapy Response Curves")
  if (length(QCN_list$g10$data) == 0) {
    output$text11 <- renderText(paste(
      "Therapy Response Curves (TRCs) are missing.",
      "You can indicate TRCs using the 'TRC' well type in the layout file."
    ))
  } else {
    output$STSlikeplot <- renderPlot(plot(QCN_list$g10))
  }
}

plot_igraph <- function(network_l) {
  set.seed(0)
  igraph::plot.igraph(
    network_l$OPI_g,
    edge.arrow.size = 0.05,
    edge.curved = 0,
    vertex.color = igraph::V(network_l$OPI_g)$color,
    vertex.frame.color = network_l$igraph_df$color_frame,
    vertex.label.color = "black",
    vertex.label.cex = 0.7,
    layout = igraph::layout_with_dh,
  )
}

render_plot_igraph <- function(network_l) {
  renderPlot(plot_igraph(network_l), width = 700, height = 550)
}

waterfall_with_hover_drcs <- function(output_dir, df) {
  drug_names <- df$Drug.Name
  files <- file.path(
    output_dir, "dose_response_curves", paste0(drug_names, ".png")
  )
  file_sizes <- file.info(files)$size
  files_good <- is.finite(file_sizes) & file_sizes > 0
  image_uris <- purrr::map_if(
    files, files_good, ~ base64enc::dataURI(file = .),
    .else = ~""
  )
  plotly::plot_ly(
    x = "analysis",
    y = df$Drug.Name,
    z = matrix(df$DSS_asym),
    type = "heatmap",
    hoverinfo = "z",
    # interleave drug_names and image_uris
    customdata = c(rbind(drug_names, image_uris)),
    colors = "Blues",
    width = 600,
    height = 1250
  ) %>% htmlwidgets::onRender(readLines("../js/tooltip_new.js"))
}

itrex_server <- function(input, output, session) {
  itrex_env <- rlang::new_environment()

  query_modal <- modalDialog(
    title = "iTReX Disclaimer and Terms of Use",
    tags$b("Disclaimer"),
    tags$p(
      "iTReX is a web-based R/Shiny application for interactive analysis and
      exploration of mono- and combination therapy dose response profiling data
      and drug target interaction network mapping."
    ),
    tags$b("Terms of Use"),
    tags$ul(
      tags$li(
        "The user is responsible for the quality of uploaded data."
      ),
      tags$li(
        "All uploaded data have to be fully anonymized or pseudonymized.",
        tags$br(), "Do not upload any personal data."
      ),
      tags$li(
        "In case of data obtained from human material, the user confirms the
        receipt of informed consent from the patient/donor that allows the use
        of the material for research purposes and/or the use of the data
        obtained from the human material for research purposes."
      ),
      tags$li(
        "The application is under development and might be subject to changes.
        It has not been verified and has not been clinically validated."
      ),
      tags$li(
        "iTReX is only for research purpose, any use of the data in a clinical
        setting or for diagnostic and/or treatment stratification is in
        the sole responsibility of the treating physician/user."
      ),
      tags$li(
        "The user is responsible for the quality and correctness of the
        uploaded data. This includes any technical wet lab and screening
        procedures."
      ),
      tags$li(
        "The user is responsible for providing the layout/drug information
        corresponding to the uploaded data."
      ),
      tags$li(
        "Uploaded data are saved only temporarily for the time of the iTReX
        session."
      ),
      tags$li(
        "Data are automatically deleted after the session is closed/the user
        disconnects from the iTReX web application."
      ),
    ),
    tags$b(
      "Read the full Disclaimer, Terms of Use and Data Privacy Policy in",
      a(href = "docs/iTReX-Data-Privacy.pdf", "English"), "or",
      a(href = "docs/iTReX-Datenschutz.pdf", "German", .noWS = "after"), ".",
    ),
    easyClose = FALSE,
    footer = tagList(
      actionButton("decline", "Decline"),
      div(style = "width:30px; display:inline-block;"),
      actionButton(
        "accept", "Accept Disclaimer, Terms of Use and Data Privacy Policy"
      ),
    ),
  )

  observeEvent(input$terms_accepted, {
    req(!input$terms_accepted)
    showModal(query_modal)
    observeEvent(input$accept, {
      removeModal()
      session$sendCustomMessage("terms-accepted", message = TRUE)
    })
    observeEvent(input$decline, session$close())
  })

  observeEvent(input$number_of_samples, {
    is_single_sample <- input$number_of_samples == "single_sample"

    updateRadioButtons(
      session, "layout_and_readouts",
      selected = if (is_single_sample) "single_file" else "separate_files",
    )

    selectors <- c(
      "#layout_and_readouts [value='single_file']",
      "#type_of_analysis [value='StepA']"
    )
    for (selector in selectors) {
      shinyjs::toggleState(condition = is_single_sample, selector = selector)
    }

    if (!is_single_sample) {
      updateRadioButtons(
        session, "type_of_analysis",
        selected = "OCA",
      )
    }
  })

  shinyjs::hide("readout_matrices_file")
  observeEvent(input$layout_and_readouts, {
    is_separate_file <- input$layout_and_readouts == "separate_files"

    middle <- if (is_separate_file) "" else "including Readouts"
    layout_table_file_label <- paste("Layout Table", middle, "(.xlsx)")
    shinyjs::html("layout_table_file-label", layout_table_file_label)

    shinyjs::toggle(
      "readout_matrices_file",
      anim = TRUE, condition = is_separate_file
    )
  })

  shinyjs::hide("reference_samples_file")
  observeEvent(input$upload_reference_samples, {
    condition <- input$upload_reference_samples
    shinyjs::toggle("reference_samples_file", anim = TRUE, condition = condition)
    shinyjs::toggleState("MRA_sdss", condition = condition)
    shinyjs::toggleState("CRA_sdss", condition = condition)
  })

  shinyjs::hide("Amin_slider")
  observeEvent(input$Amin_select, {
    is_other_amin <- input$Amin_select == "other"
    shinyjs::toggle("Amin_slider", anim = TRUE, condition = is_other_amin)
  })

  shinyjs::hide("conc_text")
  observeEvent(input$conc_select, {
    is_other_conc <- input$conc_select == "other"
    shinyjs::toggle("conc_text", anim = TRUE, condition = is_other_conc)
  })

  output$layout_table_uploaded <- reactive(!is.null(input$layout_table_file))
  outputOptions(output, "layout_table_uploaded", suspendWhenHidden = FALSE)

  output$readout_matrices_uploaded <- reactive(!is.null(input$readout_matrices_file))
  outputOptions(output, "readout_matrices_uploaded", suspendWhenHidden = FALSE)

  output$reference_samples_uploaded <- reactive(!is.null(input$reference_samples_file))
  outputOptions(output, "reference_samples_uploaded", suspendWhenHidden = FALSE)

  project_dir <- tempfile(pattern = "iTReX")
  dir.create(project_dir)

  ## -- One Click Analysis -------------------------------------------------- ##
  observeEvent(input$start_oca, {
    tryCatch(
      {
        withProgress(message = "iTReX analysis", value = 0, {
          layout_table <- pids <- readouts <- readout_table <- screenData <- NULL
          c(layout_table, pids, readouts) %<-% read_inputs(input, project_dir)

          update_all_pid_select_inputs(session, pids)

          n_pid <- length(pids)
          for (i_pid in seq_along(pids)) {
            progr_text <- glue::glue("#{i_pid}/{n_pid}: ")
            PID <- pids[i_pid]

            ## -------------------------------------------------------------- ##
            ## iTReX One Click Analysis script for mono- and Combotherapies

            c(readout_table, screenData) %<-% fill_layout_table(
              input, layout_table, readouts, PID
            )

            output_dir <- iTReX.dirs(project_dir = project_dir, PID = PID)

            ## QCN
            write.csv(screenData, file = file.path(output_dir, "pre_process", paste0(PID, "_screenData.csv")))

            incProgress(0.2 / n_pid, detail = paste0(progr_text, "QCN"))

            rmarkdown_render("QCmod", file.path(output_dir, paste0(PID, "_QC.html")))

            ## MRA
            heatmap_dir <- file.path(project_dir, "Heatmap_data", "mono")

            # Healthy control reference
            control_dir <- if (input$upload_reference_samples) {
              input$reference_samples_file$datapath
            }

            screenData <- read.csv(file.path(output_dir, "pre_process", paste0(PID, "_screenData.csv")))
            screenData$Column <- gsub("(?<![0-9])([0-9])(?![0-9])", "0\\1", screenData$Column, perl = TRUE)
            screenData <- screenData[screenData$WellType == "therapy", ]
            mono_data <- screenData[is.na(screenData$AddOn), ]

            incProgress(0.5 / n_pid, detail = paste0(progr_text, "MRA/CRA"))

            if (length(unique(mono_data$Replicate)) > 1) {
              mono_rep_data <- mono_data
              mono_rep_data$Treatment <- paste0(mono_rep_data$Treatment, "_rep", mono_rep_data$Replicate)

              MRAlist_rep <- MRA.mod(
                input,
                drdata = mono_rep_data, output_dir = output_dir,
                control_dir = control_dir, PID = PID, run_type = "mono_rep",
                heatmap_dir = heatmap_dir, itrex_env = itrex_env
              )
            }

            MRAlist <- MRA.mod(
              input,
              drdata = mono_data, output_dir = output_dir,
              control_dir = control_dir, PID = PID, run_type = "mono",
              heatmap_dir = heatmap_dir, itrex_env = itrex_env
            )

            nplr_inhibitor <- MRAlist$nplr_inhibitor
            wb <- MRAlist$wb
            save_rds(MRAlist, output_dir, paste0(PID, "_mono"))
            itrex_env[[PID]]$datapoint_mono <- MRAlist

            t <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono.xlsx")))

            n <- length(unique(t$Drug.Name))
            t <- data.table::as.data.table(t)
            ordered <- t[order(-t$DSS_asym), ]
            ordered$index <- seq.int(n, 1)

            rmarkdown_render("DSS_interactive", file.path(output_dir, paste0(PID, "_DSS_interactive.html")))

            data_n <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono.xlsx")))
            data_n$DSS_asym <- as.numeric(data_n$DSS_asym)
            data_n <- data_n[order(-data_n$DSS_asym), ]
            data <- data_n

            n <- length(unique(data$Drug.Name))
            t <- data.table::as.data.table(data)
            ordered <- t[order(-t$DSS_asym, -t$Drug.Name, na.last = FALSE), ]
            ordered$index <- seq.int(1, n)

            rmarkdown_render("DSS_Hits_Waterfall", file.path(output_dir, paste0(PID, "_DSS_Hits_Waterfall.html")))

            data_n <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono.xlsx")))

            rmarkdown_render("sDSS_Waterfall", file.path(output_dir, paste0(PID, "_sDSS_Waterfall.html")))

            ## CRA if any
            combo_table <- readout_table[!is.na(readout_table$AddOn), ]
            if (nrow(combo_table) > 0) {
              heatmap_dir <- file.path(project_dir, "Heatmap_data", "combo")

              ## Healthy control reference
              control_dir <- if (input$upload_reference_samples) {
                input$reference_samples_file$datapath
              }

              screenData <- read.csv(file.path(output_dir, "pre_process", paste0(PID, "_screenData.csv")))
              screenData$Column <- gsub("(?<![0-9])([0-9])(?![0-9])", "0\\1", screenData$Column, perl = TRUE)
              screenData <- screenData[screenData$WellType == "therapy", ]
              combo_data <- screenData[!is.na(screenData$AddOn), ]

              MRAlist <- MRA.mod(
                input,
                drdata = combo_data, output_dir = output_dir,
                control_dir = control_dir, PID = PID, run_type = "combo",
                heatmap_dir = heatmap_dir, itrex_env = itrex_env
              )

              nplr_inhibitor_combo <- MRAlist$nplr_inhibitor
              wb_combo <- MRAlist$wb
              save_rds(MRAlist, output_dir, paste0(PID, "_combo"))
              itrex_env[[PID]]$datapoint_combo <- MRAlist

              data_n <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_combo.xlsx")))
              data_n$DSS_asym <- as.numeric(data_n$DSS_asym)
              data_n <- data_n[order(-data_n$DSS_asym), ]
              data <- data_n


              n <- length(unique(data$Drug.Name))
              t <- data.table::as.data.table(data)
              ordered <- t[order(t$DSS_asym, -t$Drug.Name, na.last = FALSE), ]
              ordered$index <- seq.int(1, n)

              rmarkdown_render("DSS_Hits_Waterfall_combo", file.path(output_dir, paste0(PID, "_DSS_Hits_Waterfall_combo.html")))

              MRAlist <- itrex_env[[PID]]$datapoint_mono

              dir.create(file.path(output_dir, "synergy_plots"))

              nplr_inhibitor <- MRAlist$nplr_inhibitor
              splitlist <- MRAlist$splitlist

              combolist <- itrex_env[[PID]]$datapoint_combo
              nplr_inhibitor2 <- combolist$nplr_inhibitor
              splitlist2 <- combolist$splitlist

              nplr_inhibitor <- rbind(nplr_inhibitor, nplr_inhibitor2)
              splitlist <- c(splitlist, splitlist2)

              CRAlist <- CRA.mod(
                input,
                combo_nplr = nplr_inhibitor, output_dir = output_dir, splitlist = splitlist, PID = PID,
                itrex_env = itrex_env
              )

              Synergy_df <- CRAlist$Synergy_df
              wb <- CRAlist$wb

              single <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono.xlsx")))
              combo <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_combo.xlsx")))
              single_combo <- rbind(single, combo)
              Synergy_df <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_CRA.xlsx")))

              rmarkdown_render("Combo_Report", file.path(output_dir, paste0(PID, "_Combo_Report.html")))
            }

            incProgress(0.3 / n_pid, detail = paste0(progr_text, "Done"))
          }
          output$end_oca <- renderText("One-Click Analysis is complete.")
          output$download_oca <- renderUI(
            downloadButton("Archive", "Download Results")
          )
        })
      },
      error = error_notification
    )
  })
  ## end one click analysis ##
  ## ------------------------------------------------------------------------------------------------------ ##

  # Download Archive
  output$Archive <- downloadHandler(
    paste0("iTReX-Results_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), "_v", version(), ".zip"),
    function(file) {
      zip::zipr(file, dir(project_dir, full.names = TRUE))
    },
    contentType = "application/zip",
  )

  ## --  One click analysis visualization ------------------------------------------------------------------------------------------------ ##

  ## --  QCN module ---------------------------------------------------------------- ##
  observeEvent(input$vis_QCN, {
    tryCatch(
      {
        PID <- input$sample
        output_dir <- file.path(project_dir, PID)

        screendata <- read.csv(file.path(output_dir, "pre_process", paste0(PID, "_screenData.csv")))
        combo_data <- screendata[!is.na(screendata$AddOn), ]
        QCN_list <- itrex_env[[PID]]$QCN_list

        output_screen_summary(output, QCN_list)
      },
      error = error_notification
    )
    ## end visQCN
  })

  # --  vis MRA module ---------------------------------------------------------------- ##
  observeEvent(input$vis_MRA, {
    tryCatch(
      {
        PID <- input$sample_MRA
        output_dir <- file.path(project_dir, PID)

        t <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono.xlsx")))
        t_rep <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono_rep.xlsx")))
        MRA_dss_list <- itrex_env[[PID]]$MRA_dss_list
        MRA_sdss_list <- itrex_env[[PID]]$MRA_sdss_list

        output$parTable <- DT::renderDataTable({
          DT::datatable(t) %>%
            DT::formatRound(setdiff(3:23, 10:11), digits = 2)
        })

        output$parTable_rep <- DT::renderDataTable({
          DT::datatable(t_rep) %>%
            DT::formatRound(setdiff(3:23, 10:11), digits = 2)
        })

        output$heatmap <- plotly::renderPlotly({
          t <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono.xlsx")))
          t$DSS_asym <- as.numeric(t$DSS_asym)
          t <- data.table::as.data.table(t)
          ordered <- t[order(t$DSS_asym, -t$Drug.Name, na.last = TRUE), ]

          waterfall_with_hover_drcs(output_dir, ordered)
        })

        output$dsswfall <- plotly::renderPlotly({
          plotly::ggplotly(MRA_dss_list$pn)
        })
        output$gof1 <- plotly::renderPlotly({
          plotly::ggplotly(MRA_dss_list$gscatter, height = 300, width = 350)
        })
        output$dssgof <- plotly::renderPlotly({
          plotly::ggplotly(MRA_dss_list$hit_rank)
        })

        output$sDSSwfall <- plotly::renderPlotly({
          plotly::ggplotly(MRA_sdss_list$d)
        })
        output$gof2 <- plotly::renderPlotly({
          plotly::ggplotly(MRA_sdss_list$gscatter2, height = 300, width = 350)
        })
        output$sdssgof <- plotly::renderPlotly({
          plotly::ggplotly(MRA_sdss_list$d2)
        })
      },
      error = error_notification
    )
    ## end  vis MRA
  })

  ## --  CRA vis module ---------------------------------------------------------------- ##
  observeEvent(input$vis_CRA, {
    tryCatch(
      {
        PID <- input$sample_CRA
        output_dir <- file.path(project_dir, PID)

        t <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_combo.xlsx")))
        Synergy_df <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_CRA.xlsx")))
        CRA_dss_list <- itrex_env[[PID]]$CRA_dss_list
        CRA_report_list <- itrex_env[[PID]]$CRA_report_list

        output$parTable2 <- DT::renderDataTable({
          DT::datatable(t) %>%
            DT::formatRound(setdiff(3:23, 10:11), digits = 2)
        })
        output$dsswfall_combo <- plotly::renderPlotly({
          plotly::ggplotly(CRA_dss_list$pn)
        })
        output$gof1_combo <- plotly::renderPlotly({
          plotly::ggplotly(CRA_dss_list$gscatter, height = 300, width = 350)
        })
        output$dssgof_combo <- plotly::renderPlotly({
          plotly::ggplotly(CRA_dss_list$hit_rank)
        })
        output$parTablecombo <- DT::renderDataTable({
          DT::datatable(Synergy_df) %>%
            DT::formatRound(2:7, digits = 2)
        })
        output$dcDSSwfall <- plotly::renderPlotly({
          plotly::ggplotly(CRA_report_list$d)
        })
        output$dPI_heatmap <- plotly::renderPlotly({
          plotly::ggplotly(CRA_report_list$dPI, height = 1000, width = 600)
        })
        output$dPI_heatmap_short <- plotly::renderPlotly({
          plotly::ggplotly(CRA_report_list$dPI_sh, height = 700, width = 600)
        })
      },
      error = error_notification
    )
  })

  ## --  step wise analysis module ------------------------------------------------------------------------------------------------------- ##

  ## --  QCN module ---------------------------------------------------------------- ##
  observeEvent(input$viewQCreport, {
    tryCatch(
      {
        withProgress(message = "QCN-mod", value = 0, {
          layout_table <- pids <- readouts <- table <- screenData <- NULL
          c(layout_table, pids, readouts) %<-% read_inputs(input, project_dir)

          update_all_pid_select_inputs(session, pids)

          PID <- pids

          c(readout_table, screenData) %<-% fill_layout_table(
            input, layout_table, readouts, PID
          )

          output_dir <- iTReX.dirs(project_dir = project_dir, PID = PID)

          write.csv(screenData, file = file.path(output_dir, "pre_process", paste0(PID, "_screenData.csv")))

          setProgress(0.4, detail = "Generating Plots")

          rmarkdown_render("QCmod", file.path(output_dir, paste0(PID, "_QC.html")))

          setProgress(1.0, detail = "Done")
        })

        output_screen_summary(output, QCN_list)

        output$download_QCNreport <- renderUI({
          downloadButton("report", "Download Report")
        })

        output$report <- downloadHandler(
          filename = "report.html",
          content = function(file) {
            withProgress(message = "Generating Report", value = 0.7, {
              rmarkdown_render("QCmod", file)


              setProgress(1.0, detail = "Done")
            })
          }
        )
      },
      error = error_notification
    )
  })


  ## --  Monotherapy Analysis ---------------------------------------------------------------- ##
  observeEvent(input$nplranalyzeperreplica, {
    tryCatch(
      {
        withProgress(message = "MRA-mod, this may take few minutes", value = 0.2, {
          PID <- input$sample_MRA
          output_dir <- file.path(project_dir, PID)

          heatmap_dir <- file.path(project_dir, "Heatmap_data", "mono")

          # Healthy control reference
          control_dir <- if (input$upload_reference_samples) {
            input$reference_samples_file$datapath
          }

          screenData <- read.csv(file.path(output_dir, "pre_process", paste0(PID, "_screenData.csv")))
          screenData$Column <- gsub("(?<![0-9])([0-9])(?![0-9])", "0\\1", screenData$Column, perl = TRUE)
          screenData <- screenData[screenData$WellType == "therapy", ]
          mono_data <- screenData[is.na(screenData$AddOn), ]

          setProgress(0.2, detail = "Saving Curves")

          if (length(unique(mono_data$Replicate)) > 1) {
            mono_rep_data <- mono_data
            mono_rep_data$Treatment <- paste0(mono_rep_data$Treatment, "_rep", mono_rep_data$Replicate)

            MRAlist_rep <- MRA.mod(
              input,
              drdata = mono_rep_data, output_dir = output_dir,
              control_dir = control_dir, PID = PID, run_type = "mono_rep",
              heatmap_dir = heatmap_dir, itrex_env = itrex_env
            )
          }

          MRAlist <- MRA.mod(
            input,
            drdata = mono_data, output_dir = output_dir,
            control_dir = control_dir, PID = PID, run_type = "mono",
            heatmap_dir = heatmap_dir, itrex_env = itrex_env
          )

          setProgress(0.5, detail = "Finishing")

          nplr_inhibitor <- MRAlist$nplr_inhibitor
          wb <- MRAlist$wb
          wb_rep <- MRAlist_rep$wb
          nplr_inhibitor_rep <- MRAlist_rep$nplr_inhibitor
          save_rds(MRAlist, output_dir, paste0(PID, "_mono"))
          itrex_env[[PID]]$datapoint_mono <- MRAlist

          setProgress(1.0, detail = "Done")
        })

        output$parTable <- DT::renderDataTable({
          DT::datatable(nplr_inhibitor) %>%
            DT::formatRound(setdiff(3:23, 10:11), digits = 2)
        })

        output$parTable_rep <- DT::renderDataTable({
          DT::datatable(nplr_inhibitor_rep) %>%
            DT::formatRound(setdiff(3:23, 10:11), digits = 2)
        })

        output$xls_table <- downloadHandler(
          filename = paste0("spreadsheet_v", version(), ".xlsx"),
          content = function(file) {
            openxlsx::saveWorkbook(wb, file)
          }
        )

        output$xls_table_rep <- downloadHandler(
          filename = paste0("spreadsheet_v", version(), "_rep.xlsx"),
          content = function(file) {
            openxlsx::saveWorkbook(wb_rep, file)
          }
        )

        withProgress(message = "interactive heatmap", value = 0.2, {
          PID <- input$sample_MRA
          output_dir <- file.path(project_dir, PID)

          dat <- data.frame(x = numeric(0), y = numeric(0))

          t <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono.xlsx")))

          setProgress(0.4, detail = "In progress")

          output$heatmap <- plotly::renderPlotly({
            t <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono.xlsx")))
            t$DSS_asym <- as.numeric(t$DSS_asym)
            t <- data.table::as.data.table(t)
            ordered <- t[order(t$DSS_asym, -t$Drug.Name, na.last = TRUE), ]

            waterfall_with_hover_drcs(output_dir, ordered)
          })

          setProgress(1.0, detail = "Done")
        })

        output$Heathtml <- downloadHandler(
          filename = paste0("interactive_heatmap_v", version(), ".html"),
          content = function(file) {
            rmarkdown_render("DSS_interactive", file)
          }
        )


        withProgress(message = "MRA-mod", value = 0.2, {
          file <- input$layout_table_file
          if (is.null(file)) {
            return()
          }
          PID <- input$sample_MRA
          output_dir <- file.path(project_dir, PID)
          data_n <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono.xlsx")))
          data_n$DSS_asym <- as.numeric(data_n$DSS_asym)
          data_n <- data_n[order(-data_n$DSS_asym), ]
          data <- data_n

          setProgress(0.6, detail = "In progress")

          n <- length(unique(data$Drug.Name))
          t <- data.table::as.data.table(data)
          ordered <- t[order(-t$DSS_asym, -t$Drug.Name, na.last = FALSE), ]
          ordered$index <- seq.int(1, n)

          setProgress(1.0, detail = "Done")
        })

        output$dsswfall <- plotly::renderPlotly({
          pn <- ggplot(data_n, aes(x = reorder(.data$Drug.Name, -.data$DSS_asym), y = .data$DSS_asym, width = 0.75)) +
            geom_bar(stat = "identity", color = "#C96307", lwd = 0.5, fill = "#C96307", na.rm = TRUE) +
            labs(x = "Drug") +
            labs(y = "DSS_asym") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
          plotly::ggplotly(pn)
        })

        output$gof1 <- plotly::renderPlotly({
          data <- data_n[, c("Drug.Name", "DSS_asym", "analysis", "GOF")]
          data$GOF <- as.numeric(data$GOF)
          data$DSS_asym <- as.numeric(data$DSS_asym)
          gscatter <- ggplot(data, aes(x = .data$DSS_asym, y = .data$GOF, label = .data$Drug.Name)) +
            geom_point(color = "steelblue4", size = 2) +
            labs(x = "DSS_asym") +
            labs(y = "GOF") +
            scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
            theme_gray()

          plotly::ggplotly(gscatter, height = 300, width = 350)
        })

        output$dssgof <- plotly::renderPlotly({
          data <- data_n[, c("Drug.Name", "DSS_asym", "DSS_asym_adj", "analysis", "GOF")]
          data$DSS_asym <- as.numeric(data$DSS_asym)
          data$GOF <- as.numeric(data$GOF)
          data$DSS_asym_adj <- as.numeric(data$DSS_asym_adj)

          data <- data[order(-data$DSS_asym_adj), ]
          hit_rank <- ggplot(data, aes(x = reorder(.data$Drug.Name, -.data$DSS_asym_adj), y = .data$DSS_asym, width = 0.75)) +
            geom_bar(stat = "identity", color = "#C96307", lwd = 0.5, fill = "#C96307", na.rm = TRUE) +
            labs(x = "Drug, ranked based on DSS_asym,adj") +
            labs(y = "DSS_asym") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
          plotly::ggplotly(hit_rank)
        })


        output$dsshtml <- downloadHandler(
          filename = paste0("DSS_asym_hits_v", version(), ".html"),
          content = function(file) {
            rmarkdown_render("DSS_Hits_Waterfall", file)
          }
        )

        withProgress(message = "MRA-mod", value = 0.3, {
          file <- input$layout_table_file
          if (is.null(file)) {
            return()
          }
          PID <- input$sample_MRA
          output_dir <- file.path(project_dir, PID)
          data_n <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono.xlsx")))

          setProgress(1.0, detail = "Done")
        })

        output$sDSSwfall <- plotly::renderPlotly({
          dData <- data_n[order(-data_n$sDSS_asym), ]
          dData$sDSS_asym <- as.numeric(dData$sDSS_asym)
          dData$Metric <- ifelse(dData$sDSS_asym > 0, "sDSS_asym > 0", "sDSS_asym < 0")

          d <- ggplot(dData, aes(
            x = reorder(.data$Drug.Name, -.data$sDSS_asym),
            y = .data$sDSS_asym, width = 0.75, fill = .data$Metric
          )) +
            geom_bar(stat = "identity", lwd = 0.5, na.rm = TRUE) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            scale_fill_manual(values = c("darkred", "dodgerblue4")) +
            labs(x = "Drug") +
            labs(y = "sDSS_asym")

          plotly::ggplotly(d)
        })

        output$gof2 <- plotly::renderPlotly({
          data <- data_n[, c("Drug.Name", "sDSS_asym", "analysis", "GOF")]
          data$GOF <- as.numeric(data$GOF)
          data$sDSS_asym <- as.numeric(data$sDSS_asym)
          gscatter2 <- ggplot(data, aes(x = .data$sDSS_asym, y = .data$GOF, label = .data$Drug.Name)) +
            geom_point(color = "steelblue4", size = 2) +
            labs(x = "sDSS_asym") +
            labs(y = "GOF") +
            scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
            theme_gray()

          plotly::ggplotly(gscatter2, height = 300, width = 350)
        })

        output$sdssgof <- plotly::renderPlotly({
          data <- data_n[, c("Drug.Name", "sDSS_asym", "sDSS_asym_adj", "analysis", "GOF")]
          data$GOF <- as.numeric(data$GOF)
          data$sDSS_asym <- as.numeric(data$sDSS_asym)
          data$sDSS_asym_adj <- as.numeric(data$sDSS_asym_adj)
          data$Metric <- ifelse(data$sDSS_asym > 0, "sDSS_asym > 0", "sDSS_asym < 0")

          data <- data[order(-data$sDSS_asym_adj), ]
          sDSS_rank <- ggplot(data, aes(
            x = reorder(.data$Drug.Name, -.data$sDSS_asym_adj), y = .data$sDSS_asym, width = 0.75,
            fill = .data$Metric
          )) +
            geom_bar(stat = "identity", lwd = 0.5, na.rm = TRUE) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            scale_fill_manual(values = c("darkred", "dodgerblue4")) +
            labs(x = "Drug, ranked based on sDSS_asym,adj") +
            labs(y = "sDSS_asym")

          plotly::ggplotly(sDSS_rank)
        })

        output$download_MRAtable <- renderUI({
          downloadButton("xls_table", "Download Spreadsheet")
        })

        output$download_MRAtable_rep <- renderUI({
          downloadButton("xls_table_rep", "Download Spreadsheet")
        })

        output$download_ihp <- renderUI({
          downloadButton("Heathtml", "Download Heatmap")
        })

        output$download_dssWF <- renderUI({
          downloadButton("dsshtml", "Download Report")
        })

        output$download_sdssWF <- renderUI({
          downloadButton("sdsshtml", "Download Report")
        })

        output$sdsshtml <- downloadHandler(
          filename = paste0("sDSS_asym_hits_v", version(), ".html"),
          content = function(file) {
            rmarkdown_render("sDSS_Waterfall", file)
          }
        )
      },
      error = error_notification
    )
  })

  output$MRAcohort_hp <- renderPlot(
    plot_cohort_heatmap(
      input, output, "MRA", file.path(project_dir, "Heatmap_data", "mono")
    ),
    width = reactive(input$MRAc_width),
    height = reactive(input$MRAc_height)
  )

  output$download_MRAcohorthp <- renderUI({
    downloadButton("MRA_hp_download", "Download Heatmap")
  })


  ## --  Combotherapy Analysis ---------------------------------------------------------------- ##
  observeEvent(input$TCA, {
    tryCatch(
      {
        withProgress(message = "CRA-mod, this may take few minutes", value = 0.2, {
          PID <- input$sample_CRA
          output_dir <- file.path(project_dir, PID)

          heatmap_dir <- file.path(project_dir, "Heatmap_data", "combo")

          ## Healthy control reference
          control_dir <- if (input$upload_reference_samples) {
            input$reference_samples_file$datapath
          }

          screenData <- read.csv(file.path(output_dir, "pre_process", paste0(PID, "_screenData.csv")))
          screenData$Column <- gsub("(?<![0-9])([0-9])(?![0-9])", "0\\1", screenData$Column, perl = TRUE)
          screenData <- screenData[screenData$WellType == "therapy", ]
          combo_data <- screenData[!is.na(screenData$AddOn), ]


          setProgress(0.2, detail = "Saving Curves")

          MRAlist <- MRA.mod(
            input,
            drdata = combo_data, output_dir = output_dir,
            control_dir = control_dir, PID = PID, run_type = "combo",
            heatmap_dir = heatmap_dir, itrex_env = itrex_env
          )

          setProgress(0.6, detail = "Finishing")

          nplr_inhibitor_combo <- MRAlist$nplr_inhibitor
          wb_combo <- MRAlist$wb
          save_rds(MRAlist, output_dir, paste0(PID, "_combo"))
          itrex_env[[PID]]$datapoint_combo <- MRAlist

          setProgress(1.0, detail = "Done")
        })

        output$parTable2 <- DT::renderDataTable({
          DT::datatable(nplr_inhibitor_combo) %>%
            DT::formatRound(setdiff(3:23, 10:11), digits = 2)
        })
        output$xls_table2 <- downloadHandler(
          filename = paste0("spreadsheet_v", version(), ".xlsx"),
          content = function(file) {
            openxlsx::saveWorkbook(wb_combo, file)
          }
        )

        withProgress(message = "CRA-mod", value = 0.2, {
          file <- input$layout_table_file
          if (is.null(file)) {
            return()
          }
          PID <- input$sample_CRA
          output_dir <- file.path(project_dir, PID)
          data_n <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_combo.xlsx")))
          data_n$DSS_asym <- as.numeric(data_n$DSS_asym)
          data_n <- data_n[order(-data_n$DSS_asym), ]
          data <- data_n

          setProgress(0.5, detail = "In progress")

          n <- length(unique(data$Drug.Name))
          t <- data.table::as.data.table(data)
          ordered <- t[order(t$DSS_asym, -t$Drug.Name, na.last = FALSE), ]
          ordered$index <- seq.int(1, n)

          setProgress(1.0, detail = "Done")
        })

        output$dsswfall_combo <- plotly::renderPlotly({
          pn <- ggplot(data, aes(x = reorder(.data$Drug.Name, -.data$DSS_asym), y = .data$DSS_asym, width = 0.75)) +
            geom_bar(stat = "identity", color = "#C96307", lwd = 0.5, fill = "#C96307", na.rm = TRUE) +
            labs(x = "Drug") +
            labs(y = "DSS_asym") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
          plotly::ggplotly(pn)
        })

        output$gof1_combo <- plotly::renderPlotly({
          data <- data_n[, c("Drug.Name", "DSS_asym", "analysis", "GOF")]
          data$GOF <- as.numeric(data$GOF)
          data$DSS_asym <- as.numeric(data$DSS_asym)
          gscatter <- ggplot(data, aes(x = .data$DSS_asym, y = .data$GOF, label = .data$Drug.Name)) +
            geom_point(color = "steelblue4", size = 2) +
            labs(x = "DSS_asym") +
            labs(y = "GOF") +
            scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
            theme_gray()

          plotly::ggplotly(gscatter, height = 300, width = 350)
        })

        output$dssgof_combo <- plotly::renderPlotly({
          data <- data_n[, c("Drug.Name", "DSS_asym", "DSS_asym_adj", "analysis", "GOF")]
          data$DSS_asym <- as.numeric(data$DSS_asym)
          data$GOF <- as.numeric(data$GOF)
          data$DSS_asym_adj <- as.numeric(data$DSS_asym_adj)

          data <- data[order(-data$DSS_asym_adj), ]
          hit_rank <- ggplot(data, aes(x = reorder(.data$Drug.Name, -.data$DSS_asym_adj), y = .data$DSS_asym, width = 0.75)) +
            geom_bar(stat = "identity", color = "#C96307", lwd = 0.5, fill = "#C96307", na.rm = TRUE) +
            labs(x = "combo_Drug, ranked based on DSS_asym,adj") +
            labs(y = "combo_DSS_asym") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
          plotly::ggplotly(hit_rank)
        })

        output$dsshtml_combo <- downloadHandler(
          filename = paste0("DSS_Hits_Waterfall_combo_v", version(), ".html"),
          content = function(file) {
            rmarkdown_render("DSS_Hits_Waterfall_combo", file)
          }
        )

        ## enhanced effect
        withProgress(message = "CRA-mod", value = 0.2, {
          file <- input$layout_table_file
          if (is.null(file)) {
            return()
          }
          PID <- input$sample_CRA
          output_dir <- file.path(project_dir, PID)
          screenData <- read.csv(file.path(output_dir, "pre_process", paste0(PID, "_screenData.csv")))
          screenData$Column <- gsub("(?<![0-9])([0-9])(?![0-9])", "0\\1", screenData$Column, perl = TRUE)

          setProgress(0.3, detail = "Generating Spreadsheet(s)")

          MRAlist <- itrex_env[[PID]]$datapoint_mono

          dir.create(file.path(output_dir, "synergy_plots"))

          nplr_inhibitor <- MRAlist$nplr_inhibitor
          splitlist <- MRAlist$splitlist

          combolist <- itrex_env[[PID]]$datapoint_combo
          nplr_inhibitor2 <- combolist$nplr_inhibitor
          splitlist2 <- combolist$splitlist

          nplr_inhibitor <- rbind(nplr_inhibitor, nplr_inhibitor2)
          splitlist <- c(splitlist, splitlist2)

          setProgress(0.6, detail = "Generating Curves")

          CRAlist <- CRA.mod(
            input,
            combo_nplr = nplr_inhibitor, output_dir = output_dir, splitlist = splitlist, PID = PID,
            itrex_env = itrex_env
          )

          setProgress(1.0, detail = "Done")
        })

        Synergy_df <- CRAlist$Synergy_df
        wb <- CRAlist$wb
        output$parTablecombo <- DT::renderDataTable({
          DT::datatable(Synergy_df) %>%
            DT::formatRound(2:7, digits = 2)
        })
        output$xls_tablecombo <- downloadHandler(
          filename = paste0("CRA_spreadsheet_v", version(), ".xlsx"),
          content = function(file) {
            openxlsx::saveWorkbook(wb, file)
          }
        )

        ## dcDSS and waterfall plot
        withProgress(message = "CRA-mod", value = 0.3, {
          file <- input$layout_table_file
          if (is.null(file)) {
            return()
          }
          PID <- input$sample_CRA
          output_dir <- file.path(project_dir, PID)
          single <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono.xlsx")))
          combo <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_combo.xlsx")))
          single_combo <- rbind(single, combo)
          Synergy_df <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_CRA.xlsx")))

          setProgress(0.2, detail = "In progress")

          output$dcDSSwfall <- plotly::renderPlotly({
            combo <- Synergy_df
            combo$Metric <- ifelse(combo$dcDSS_asym > 0, "dcDSS_asym > 0", "dcDSS_asym < 0")
            d <- ggplot(combo, aes(x = reorder(.data$Drug.Name, -.data$dcDSS_asym), y = .data$dcDSS_asym, width = 0.75, fill = .data$Metric)) +
              geom_bar(stat = "identity", lwd = 0.5, na.rm = TRUE, ) +
              labs(x = "Drug") +
              labs(y = "dcDSS_asym") +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
              scale_fill_manual(values = c("darkred", "dodgerblue4"))
            plotly::ggplotly(d)
          })

          setProgress(0.5, detail = "In progress")

          output$dPI_heatmap <- plotly::renderPlotly({
            long2 <- Synergy_df[, 1:6]

            long2 <- long2 %>% tidyr::gather("Dose", "dPI", .data$dPI1:.data$dPI5)

            long2 <- long2[complete.cases(long2), ]
            h2 <- ggplot(long2, aes(.data$Dose, reorder(.data$Drug.Name, .data$dPI), label1 = .data$Dose)) +
              geom_tile(aes(fill = .data$dPI)) +
              geom_text(aes(label = round(.data$dPI, 2)), size = 3) +
              scale_fill_gradient2(
                low = "darkred",
                mid = "gray100",
                high = "steelblue4",
                midpoint = 0
              ) +
              theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 8, face = "bold"),
                plot.title = element_text(size = 12, face = "bold"),
                axis.text.y = element_text(size = 8, face = "bold")
              ) +
              ggtitle("differential Percentage Inhibition") +
              theme(legend.title = element_text(face = "bold", size = 8)) +
              scale_x_discrete(name = "") +
              scale_y_discrete(name = "")

            plotly::ggplotly(h2, height = 1200, width = 600)
          })

          output$dPI_heatmap_short <- plotly::renderPlotly({
            Synergy_h <- Synergy_df[Synergy_df$dcDSS_asym > 0, ]
            long2 <- Synergy_h[, 1:6]
            long2 <- long2 %>% tidyr::gather("Dose", "dPI", .data$dPI1:.data$dPI5)
            long2 <- long2[complete.cases(long2), ]

            h2 <- ggplot(long2, aes(.data$Dose, reorder(.data$Drug.Name, .data$dPI), label1 = .data$Dose)) + # x and y axes => Var1 and Var2
              geom_tile(aes(fill = .data$dPI)) + # background colors are mapped according to the value column
              geom_text(aes(label = round(.data$dPI, 2)), size = 3) + # write the values
              scale_fill_gradient2(
                low = "darkred",
                mid = "gray100",
                high = "steelblue4",
                midpoint = 0
              ) + # determine the color

              theme(
                panel.grid.major.x = element_blank(), # no gridlines
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 8, face = "bold"),
                plot.title = element_text(size = 12, face = "bold"),
                axis.text.y = element_text(size = 8, face = "bold")
              ) +
              ggtitle("differential Percentage Inhibition, dcDSS_asym > 0") +
              theme(legend.title = element_text(face = "bold", size = 8)) +
              scale_x_discrete(name = "") +
              scale_y_discrete(name = "")

            plotly::ggplotly(h2, height = 700, width = 600)
          })

          setProgress(1.0, detail = "Done")
        })

        output$download_combotable <- renderUI({
          downloadButton("xls_table2", "Download Spreadsheet")
        })

        output$download_comboWF <- renderUI({
          downloadButton("dsshtml_combo", "Download Report")
        })

        output$download_CRAtable <- renderUI({
          downloadButton("xls_tablecombo", "Download Spreadsheet")
        })

        output$comboreport <- downloadHandler(
          filename = paste0("Combo_Report_v", version(), ".html"),
          content = function(file) {
            rmarkdown_render("Combo_Report", file)
          }
        )
        output$download_comboreport <- renderUI({
          downloadButton("comboreport", "Download Report")
        })
      },
      error = error_notification
    )
  })

  output$CRAcohort_hp <- renderPlot(
    plot_cohort_heatmap(
      input, output, "CRA", file.path(project_dir, "Heatmap_data", "combo")
    ),
    width = reactive(input$CRAc_width),
    height = reactive(input$CRAc_height)
  )

  output$download_CRAcohorthp <- renderUI({
    downloadButton("CRA_hp_download", "Download Heatmap")
  })

  ## --  HitNet module ---------------------------------------------------------------- ##
  observeEvent(input$HitNet, {
    tryCatch(
      {
        withProgress(message = "HitNet-mod", value = 0.2, {
          PID <- input$sample_net
          output_dir <- file.path(project_dir, PID)

          setProgress(0.2, detail = "Creating Networks")

          if (input$DTanno == "DBupload") {
            meta <- input$file_drugbank
            meta_drugs <- utils::read.csv(meta$datapath)
            DB <- input$file_drugtarget
            DB_targets <- utils::read.csv(DB$datapath)
            DB_targets <- tidyr::separate_rows(DB_targets, .data$Drug.IDs, convert = TRUE)
            DB_targets$Name <- meta_drugs$Name[match(DB_targets$Drug.IDs, meta_drugs$DrugBank.ID)]
            DT_targets <- DB_targets
          } else {
            meta <- input$Tupload_input
            DT_targets <- readxl::read_excel(meta$datapath)
            colnames(DT_targets)[colnames(DT_targets) == "Drug.Name"] <- "Name"
          }

          threshold <- input$threshold

          if (input$screen_type == "Monotherapy") {
            d_data <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono.xlsx")))
            setProgress(0.4, detail = "In Progress")

            network_l <- HitNet.mod(
              drdata = d_data, DT_targets = DT_targets, rank_based = "DSS_asym", threshold = threshold
            )
            sDSS_asym_exist <- na.omit(d_data$sDSS_asym)
            if (length(sDSS_asym_exist) == 0) {
              s_network_l <- NULL
            } else {
              s_network_l <- HitNet.mod(
                drdata = d_data, DT_targets = DT_targets, rank_based = "sDSS_asym", threshold = threshold
              )
            }
            setProgress(1.0, detail = "Done")

            output$text_hit1 <- renderText({
              "DSS_asym Net"
            })
            if (is.null(network_l)) {
              showModal(modalDialog(
                title = "HitNet-mod",
                paste0("The selected threshold is too low,
                       please increase your threshold."),
                easyClose = TRUE,
                footer = NULL
              ))
            } else {
              output$HitNetplot <- render_plot_igraph(network_l)
            }
            output$text_hit2 <- renderText({
              "sDSS_asym Net"
            })

            if (is.null(s_network_l)) {
              showModal(modalDialog(
                title = "HitNet-mod",
                paste0("The sDSS_asym network is not generated for this sample.
                This may be due to setting the threshold too low, hence increase your threshold, or the reference sample(s)
                was not uploaded."),
                easyClose = TRUE,
                footer = NULL
              ))
            } else {
              output$HitNetplot2 <- render_plot_igraph(s_network_l)
            }
          }
          if (input$screen_type == "Combotherapy") {
            setProgress(0.6, detail = "In Progress")
            d_data <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_CRA.xlsx")))
            network_l <- HitNet.mod(
              drdata = d_data, DT_targets = DT_targets, rank_based = "dcDSS_asym", threshold = threshold
            )
            s_network_l <- NULL
            setProgress(1.0, detail = "Done")

            output$text_hit1 <- renderText({
              "dcDSS_asym Net"
            })
            output$HitNetplot <- render_plot_igraph(network_l)
          }
        })

        output$download_hitnet <- renderUI({
          downloadButton("HitNetreport", "Download Report")
        })

        output$HitNetreport <- downloadHandler(
          filename = paste0("HitNet_Report_v", version(), ".html"),
          content = function(file) {
            rmarkdown_render("HitNet_Report", file)
          }
        )
      },
      error = error_notification
    )
  })

  ## --  Omics module ----------------------------------------------------------------##
  observeEvent(input$Omics, {
    tryCatch(
      {
        withProgress(message = "Omics-mod", value = 0.2, {
          PID <- input$sample_omics
          output_dir <- file.path(project_dir, PID)

          setProgress(0.2, detail = "Creating Networks")

          if (input$DTannoO == "DBupload") {
            meta <- input$file_drugbankO
            meta_drugs <- utils::read.csv(meta$datapath)
            DB <- input$file_drugtargetO
            DB_targets <- utils::read.csv(DB$datapath)
            DB_targets <- tidyr::separate_rows(DB_targets, .data$Drug.IDs, convert = TRUE)
            DB_targets$Name <- meta_drugs$Name[match(DB_targets$Drug.IDs, meta_drugs$DrugBank.ID)]
            DT_targets <- DB_targets
          } else {
            meta <- input$Tupload_inputO
            DT_targets <- readxl::read_excel(meta$datapath)
            colnames(DT_targets)[colnames(DT_targets) == "Drug.Name"] <- "Name"
          }

          threshold <- input$threshold_O

          ## align Omics
          fileOmics <- input$fileOmics
          tableOmics <- openxlsx::read.xlsx(fileOmics$datapath)

          if (input$screen_type_O == "Monotherapy") {
            d_data <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_mono.xlsx")))
            setProgress(0.5, detail = "In Progress")

            network_l <- Omics.mod(
              drdata = d_data, omics_df = tableOmics, DT_targets = DT_targets, rank_based = "DSS_asym", threshold = threshold
            )
            sDSS_asym_exist <- na.omit(d_data$sDSS_asym)
            if (length(sDSS_asym_exist) == 0) {
              s_network_l <- NULL
            } else {
              s_network_l <- Omics.mod(
                drdata = d_data, omics_df = tableOmics, DT_targets = DT_targets, rank_based = "sDSS_asym", threshold = threshold
              )
            }
            setProgress(1.0, detail = "Done")

            output$text_omics1 <- renderText({
              "DSS_asym Net"
            })
            if (is.null(network_l)) {
              showModal(modalDialog(
                title = "Omics-mod",
                paste0("The selected threshold is too low,
                       please increase your threshold."),
                easyClose = TRUE,
                footer = NULL
              ))
            } else {
              output$Omicsplot <- render_plot_igraph(network_l)
            }
            output$text_omics2 <- renderText({
              "sDSS_asym Net"
            })

            if (is.null(s_network_l)) {
              showModal(modalDialog(
                title = "Omics-mod",
                paste0("The sDSS_asym network is not generated for this sample.
                This may be due to setting the threshold too low, hence increase your threshold, or the reference sample(s)
                was not uploaded."),
                easyClose = TRUE,
                footer = NULL
              ))
            } else {
              output$Omicsplot2 <- render_plot_igraph(s_network_l)
            }
          }
          if (input$screen_type_O == "Combotherapy") {
            setProgress(0.6, detail = "In Progress")
            d_data <- readxl::read_xlsx(file.path(output_dir, paste0(PID, "_combo.xlsx")))
            network_l <- Omics.mod(
              drdata = d_data, omics_df = tableOmics, DT_targets = DT_targets, rank_based = "dcDSS_asym", threshold = threshold
            )
            s_network_l <- NULL
            setProgress(1.0, detail = "Done")
            output$text_omics1 <- renderText({
              "dcDSS_asym Net"
            })
            output$Omicsplot <- render_plot_igraph(network_l)
          }
        })

        output$download_omics <- renderUI({
          downloadButton("Omicsreport", "Download Report")
        })

        output$Omicsreport <- downloadHandler(
          filename = paste0("Omics_Report_v", version(), ".html"),
          content = function(file) {
            rmarkdown_render("Omics_Report", file)
          }
        )
      },
      error = error_notification
    )
  })

  ## --  FAQs ---------------------------------------------------------------- ##
  output$Q1 <- renderText({
    "1- Can iTReX Omics module visualize any Omics layer?"
  })
  output$answer1 <- renderText({
    "No, the Omics module currently can only visualize mutations, highly expressed genes
                              and gene fusions,to integrate further omic features please contact the developer
                             with your request to be integrated within 1-3 working days."
  })

  output$Q2 <- renderText({
    "2- Can iTReX CRA module handle more than one drug combination in a fixed single concentration added on the library?"
  })
  output$answer2 <- renderText({
    "The current version of iTReX can analyze one drug combination added to any number of monotherapy library."
  })

  output$Q3 <- renderText({
    "3- How can I upload multiple samples following the same layout?"
  })
  output$answer3 <- renderText({
    "You can use the cohort upload settings, where you upload one layout spreadsheets
      and all your sample plate readouts as multiple .xlsx or .txt files in a zipped folder."
  })

  output$Q4 <- renderText({
    "4- Can I run a cohort analysis and receive a report per individual sample?"
  })
  output$answer4 <- renderText({
    "Yes, after running a cohort analysis you can switch to the module of interest, select the sample to be visualized and press visualize.
      For the Omics-mod you will have to upload the omics data of this specific sample before visualization."
  })

  output$Q5 <- renderText({
    "5- Do I have to normalize the raw data before processing the samples?"
  })
  output$answer5 <- renderText({
    "No, the QCN module performs the normalization from raw values."
  })

  output$Q6 <- renderText({
    "6- Can I combine screens with different layouts for a cohort analysis?"
  })
  output$answer6 <- renderText({
    "No, iTReX currently can only run a cohort analysis performed with one layout for all the screens."
  })

  output$Q7 <- renderText({
    "7- Can I combine multiple screens (technical replicates) into one analysis output?"
  })
  output$answer7 <- renderText({
    "Yes, you will need to name the replicate plates A and B .. etc after the plate number, e.g (SampleID_1A.xlsx, SampleID_1B.xlsx)."
  })

  output$Q8 <- renderText({
    "8- Can I use any kind of multiple well format (24, 96, 384, 1536 well-plates)?"
  })
  output$answer8 <- renderText({
    "Yes, iTReX can process multiple well formats."
  })

  output$Q9 <- renderText({
    "9- Is iTReX restricted to a specific number of concentrations/doses?"
  })
  output$answer9 <- renderText({
    "Yes, iTReX is currently restricted to 5 concentrations/doses. This feature will be flexible to handle less and more doses in iTReX v1.2.0."
  })

  output$Q10 <- renderText({
    "10- If I run a combination screen, can I still visualize the results of the single agents if included in my screen?"
  })
  output$answer10 <- renderText({
    "Yes, you can visualize results of single agents/monotherapies included in a combination screen using the MRA-mod tabs."
  })

  output$Q11 <- renderText({
    "11- Can I exclude outlier positive and negative control wells?"
  })
  output$answer11 <- renderText({
    "Yes, you can identify your fault wells of positive and negative controls after running the QCN-mod and
      turn these wells into 'exclude' in the layout WellType column instead of 'pos' or 'neg' and rerun the sample."
  })

  session$onSessionEnded(function() {
    cat("Session Ended\n")

    future::plan(future::multisession, workers = 2)
    future::future({
      log_dir <- Sys.getenv("ITREX_LOG_DIR")
      if (nchar(log_dir)) {
        cat("Secure Deletion Started\n")
        srm_command <- paste0("srm -r -f -v '", project_dir, "'")
        srm_output <- system(paste(srm_command, "2>&1"), intern = TRUE)

        host_name <- as.character(Sys.info()["nodename"])

        date_time <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
        log_file_name <- paste0(date_time, "-srm-", basename(project_dir), ".log")
        log_file_path <- file.path(log_dir, log_file_name)

        log_file <- file(log_file_path)
        writeLines(c(host_name, date_time, srm_output, "Done."), log_file)
        close(log_file)
        cat("Secure Deletion Finished\n")
      }

      unlink(project_dir, recursive = TRUE)
    })
  })
}
