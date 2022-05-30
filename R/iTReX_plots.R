############################
## iTReX plot functions   ##
## Author: Dina ELHarouni ##
############################

## color palette for QC plotting
cbPalette <- c("gray", "#21698D", "#E8B693", "#CA670D", "#CAD3DC", "darkolivegreen4", "#003152")

## plotting QC controls
platePlotControl <- function(screenData, plotPlate = "all", plotType = "layout",
                             ifCorrected = FALSE, limits = NULL, ncol = 2,
                             nrow = 3, width = 16, height = 16) {
  if (all(plotPlate == "all")) {
    plateList <- unique(screenData$Plate)
  } else {
    plateList <- intersect(unique(screenData$Plate), plotPlate)
  }
  if (length(plateList) == 0) {
    stop("No plate found")
  }
  nCol <- length(unique(screenData$Column))
  nRow <- length(unique(screenData$Row))
  rowSeq <- row_sequence(nRow)
  colSeq <- col_sequence(nCol)
  matPlate <- screenData %>%
    mutate(Row = factor(.data$Row, levels = rev(rowSeq))) %>%
    mutate(Column = factor(.data$Column, levels = colSeq))
  if (plotType == "layout") {
    plotList <- lapply(plateList, function(plateName) {
      p <- filter(matPlate, .data$Plate == plateName) %>%
        ggplot(aes(x = .data$Column, y = .data$Row, fill = .data$WellType)) +
        geom_tile(color = "grey80") +
        xlab("") +
        ylab("") +
        theme_void() +
        ggtitle(plateName) +
        theme(
          axis.text = element_text(size = 6),
          axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
          plot.margin = margin(5, 5, 5, 5)
        )
    })
  } else if (plotType == "viability") {
    if (!"normVal" %in% colnames(matPlate)) {
      stop("No relative viability values found, please normalize the plate first")
    }
    if (is.null(limits)) {
      limits <- c(0, 2)
    }
    matPlate <- mutate(matPlate, normVal = pmax(limits[1], pmin(limits[2], .data$normVal)))
    if (ifCorrected) {
      if (!"normVal.cor" %in% colnames(matPlate)) {
        stop("No edge-corrected viability found, please run edge effect correction first")
      } else {
        matPlate <- mutate(matPlate, normVal = .data$normVal.cor)
      }
    }
    plotList <- lapply(plateList, function(plateName) {
      p <- filter(matPlate, .data$Plate == plateName) %>%
        ggplot(aes(x = .data$Column, y = .data$Row, fill = .data$normVal)) +
        geom_tile(color = "grey80") +
        labs(
          x = "", y = "",
          fill = "viability", title = plateName
        ) +
        theme_void() +
        theme(
          axis.text = element_text(size = 6), axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5), plot.margin = margin(
            5,
            5, 5, 5
          )
        )
      if (!is.null(limits)) {
        p <- p + scale_fill_gradient2(
          limits = limits,
          high = "darkred", low = "#003469", mid = "white", na.value = "grey80",
          midpoint = 1
        )
      } else {
        p <- p + scale_fill_gradient2(high = "darkred", low = "#003469", mid = "white", na.value = "grey80", midpoint = 1)
      }
    })
  } else if (plotType == "zscore") {
    calcZ <- function(x) mutate(x, zscore = (.data$Readout - mean(.data$Readout, na.rm = TRUE)) / sd(.data$Readout, na.rm = TRUE))
    matPlate <- group_by(matPlate, .data$Plate) %>%
      do(calcZ(.data)) %>%
      ungroup()
    if (is.null(limits)) {
      limits <- c(-3, 3)
    }
    matPlate <- mutate(matPlate, zscore = pmax(limits[1], pmin(limits[2], .data$zscore)))
    plotList <- lapply(plateList, function(plateName) {
      p <- filter(matPlate, .data$Plate == plateName) %>%
        ggplot(aes(x = .data$Column, y = .data$Row, fill = .data$zscore)) +
        geom_tile(color = "grey80") +
        labs(
          x = "", y = "",
          fill = "Z score", title = plateName
        ) +
        theme_void() +
        theme(
          axis.text = element_text(size = 6), axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5), plot.margin = margin(
            5,
            5, 5, 5
          )
        )
      if (!is.null(limits)) {
        p <- p + scale_fill_gradient2(
          limits = limits,
          high = "darkred", low = "#003469", mid = "white", na.value = "grey80",
          midpoint = 0
        )
      } else {
        p <- p + scale_fill_gradient2(high = "darkred", low = "#003469", mid = "white", na.value = "grey80", midpoint = 0)
      }
    })
  } else if (plotType == "edgeEffect") {
    if (!"edgeFactor" %in% colnames(matPlate)) {
      stop("No edge effect information found, please fit edge effect first")
    }
    if (is.null(limits)) {
      limits <- c(0, 2)
    }
    matPlate <- mutate(matPlate, normVal = pmax(limits[1], pmin(limits[2], .data$normVal)))
    plotList <- lapply(plateList, function(plateName) {
      p <- filter(matPlate, .data$Plate == plateName) %>%
        ggplot(aes(x = .data$Column, y = .data$Row, fill = .data$edgeFactor)) +
        geom_tile(color = "grey80") +
        labs(
          x = "", y = "",
          fill = "viability", title = plateName
        ) +
        theme_void() +
        theme(
          axis.text = element_text(size = 6), axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5), plot.margin = margin(
            5,
            5, 5, 5
          )
        )
      if (!is.null(limits)) {
        p <- p + scale_fill_gradient2(
          limits = limits,
          high = "darkred", low = "#003469", mid = "white", na.value = "grey80",
          midpoint = 1
        )
      } else {
        p <- p + scale_fill_gradient2(high = "darkred", low = "#003469", mid = "white", na.value = "grey80", midpoint = 1)
      }
    })
  } else {
    stop("Not a valid option for plotType")
  }
  return(plotList)
}

## plotting raw counts for screenData
plotRawCountc <- function(screenData, topN = NULL, ifLog10 = FALSE) {
  if (!is.null(topN)) {
    allNames <- unique(screenData$Plate)
    plotTab <- filter(screenData, .data$Plate %in% allNames[seq_len(min(
      length(allNames),
      topN
    ))])
  } else {
    plotTab <- screenData
  }
  if (ifLog10) {
    plotTab <- mutate(plotTab, Readout = log10(.data$Readout))
  }
  plotTab <- mutate(plotTab, Plate = factor(.data$Plate, levels = rev(unique(.data$Plate))))
  g <- ggplot(plotTab, aes(x = .data$Plate, y = .data$Readout)) +
    geom_point(
      position = position_jitter(width = 0.3, seed = 0),
      alpha = 0.3, color = "grey50"
    ) +
    geom_boxplot(
      fill = NA,
      color = "steelblue4", outlier.shape = NA
    ) +
    coord_flip() +
    theme_bw() +
    xlab("plate") +
    ylab(ifelse(ifLog10,
      "log10 (raw counts)", "raw counts"
    ))
}

## plotting well type distribution for screenData
plotTypeDistc <- function(screenData, plotPlate = "all", ifLog10 = FALSE) {
  if (!"WellType" %in% colnames(screenData)) {
    stop("No well type annotation found")
  }
  if (plotPlate == "all") {
    plateList <- unique(screenData$Plate)
  } else {
    plateList <- intersect(unique(screenData$Plate), plotPlate)
  }
  if (length(plateList) == 0) {
    stop("No plate found")
  }
  pList <- lapply(plateList, function(plateName) {
    plotTab <- filter(screenData, .data$Plate == plateName) %>%
      mutate(WellType = factor(.data$WellType))
    if (length(unique(plotTab$WellType)) <= 1) {
      stop("The well types on plate is less than 1, nothing to plot")
    }
    if (ifLog10) {
      plotTab <- mutate(plotTab, Readout = log10(.data$Readout))
      yLabText <- ylab("log10(raw counts)")
    } else {
      yLabText <- ylab("raw counts")
    }
    ggplot(plotTab, aes(x = .data$WellType, y = .data$Readout, fill = .data$WellType)) +
      geom_point() +
      geom_boxplot(alpha = 0.5) +
      theme_bw() +
      theme(
        legend.position = "none", plot.title = element_text(
          hjust = 0.5,
          face = "bold", size = 10
        ), axis.text.x = element_text(
          face = "bold",
          size = 10
        ), axis.text.y = element_text(size = 10),
        plot.margin = margin(5, 5, 5, 5)
      ) +
      yLabText +
      xlab("well types") +
      ggtitle(plateName) +
      scale_fill_manual(values = cbPalette)
  })
  names(pList) <- plateList
  return(pList)
}

## plot layout
platePlotc <- function(screenData, plotPlate = "all", plotType = "layout",
                       ifCorrected = FALSE, limits = NULL, ncol = 2,
                       nrow = 3, width = 16, height = 16) {
  if (all(plotPlate == "all")) {
    plateList <- unique(screenData$Plate)
  } else {
    plateList <- intersect(unique(screenData$Plate), plotPlate)
  }
  if (length(plateList) == 0) {
    stop("No plate found")
  }
  nCol <- length(unique(screenData$Column))
  nRow <- length(unique(screenData$Row))
  rowSeq <- row_sequence(nRow)
  colSeq <- col_sequence(nCol)
  matPlate <- screenData %>%
    mutate(Row = factor(.data$Row, levels = rev(rowSeq))) %>%
    mutate(Column = factor(.data$Column, levels = colSeq))
  if (plotType == "layout") {
    plotList <- lapply(plateList, function(plateName) {
      p <- filter(matPlate, .data$Plate == plateName) %>%
        ggplot(aes(x = .data$Column, y = .data$Row, fill = .data$WellType)) +
        geom_point(aes(color = factor(.data$WellType)), size = 5) +
        aes(fill = factor(.data$WellType)) +
        scale_color_manual(values = cbPalette) +
        theme_void() +
        xlab("") +
        ylab("") +
        ggtitle(plateName) +
        theme(
          axis.text = element_text(size = 6), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
          plot.margin = margin(5, 5, 5, 5)
        )
    })
  } else if (plotType == "viability") {
    if (!"normVal" %in% colnames(matPlate)) {
      stop("No relative viability values found, please normalize the plate first")
    }
    if (is.null(limits)) {
      limits <- c(0, 2)
    }
    matPlate <- mutate(matPlate, normVal = pmax(limits[1], pmin(limits[2], .data$normVal)))
    if (ifCorrected) {
      if (!"normVal.cor" %in% colnames(matPlate)) {
        stop("No edge-corrected viability found, please run edge effect correction first")
      } else {
        matPlate <- mutate(matPlate, normVal = .data$normVal.cor)
      }
    }
    plotList <- lapply(plateList, function(plateName) {
      p <- filter(matPlate, .data$Plate == plateName) %>%
        ggplot(aes(x = .data$Column, y = .data$Row, fill = .data$normVal)) +
        geom_tile(color = "grey80") +
        labs(
          x = "", y = "",
          fill = "viability", title = plateName
        ) +
        theme_void() +
        theme(
          axis.text = element_text(size = 6), axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5), plot.margin = margin(
            5,
            5, 5, 5
          )
        )
      if (!is.null(limits)) {
        p <- p + scale_fill_gradient2(
          limits = limits,
          high = "red", low = "blue", mid = "white",
          midpoint = 1
        )
      } else {
        p <- p + scale_fill_gradient2(
          high = "red", low = "blue",
          mid = "white", midpoint = 1
        )
      }
    })
  } else if (plotType == "zscore") {
    calcZ <- function(x) mutate(x, zscore = (.data$Readout - mean(.data$Readout)) / sd(.data$Readout))
    matPlate <- group_by(matPlate, .data$Plate) %>%
      do(calcZ(.data)) %>%
      ungroup()
    if (is.null(limits)) {
      limits <- c(-3, 3)
    }
    matPlate <- mutate(matPlate, zscore = pmax(limits[1], pmin(limits[2], .data$zscore)))
    plotList <- lapply(plateList, function(plateName) {
      p <- filter(matPlate, .data$Plate == plateName) %>%
        ggplot(aes(x = .data$Column, y = .data$Row, fill = .data$zscore)) +
        geom_tile(color = "grey80") +
        labs(
          x = "", y = "",
          fill = "Z score", title = plateName
        ) +
        theme_void() +
        theme(
          axis.text = element_text(size = 6), axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5), plot.margin = margin(
            5,
            5, 5, 5
          )
        )
      if (!is.null(limits)) {
        p <- p + scale_fill_gradient2(
          limits = limits,
          high = "red", low = "blue", mid = "white",
          midpoint = 0
        )
      } else {
        p <- p + scale_fill_gradient2(
          high = "red", low = "blue",
          mid = "white", midpoint = 0
        )
      }
    })
  } else if (plotType == "edgeEffect") {
    if (!"edgeFactor" %in% colnames(matPlate)) {
      stop("No edge effect information found, please fit edge effect first")
    }
    if (is.null(limits)) {
      limits <- c(0, 2)
    }
    matPlate <- mutate(matPlate, normVal = pmax(limits[1], pmin(limits[2], .data$normVal)))
    plotList <- lapply(plateList, function(plateName) {
      p <- filter(matPlate, .data$Plate == plateName) %>%
        ggplot(aes(x = .data$Column, y = .data$Row, fill = .data$edgeFactor)) +
        geom_tile(color = "grey80") +
        labs(
          x = "", y = "",
          fill = "viability", title = plateName
        ) +
        theme_void() +
        theme(
          axis.text = element_text(size = 6), axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5), plot.margin = margin(
            5,
            5, 5, 5
          )
        )
      if (!is.null(limits)) {
        p <- p + scale_fill_gradient2(
          limits = limits,
          high = "red", low = "blue", mid = "white",
          midpoint = 1
        )
      } else {
        p <- p + scale_fill_gradient2(
          high = "red", low = "blue",
          mid = "white", midpoint = 1
        )
      }
    })
  } else {
    stop("Not a valid option for plotType")
  }
  return(plotList)
}

## plotting dose response curves
plot.iscreen <- function(drdata, conc_unit,
                         curve = NA, error = NA, IC50 = NA, title = "") {
  dplot <- ggplot(drdata, aes(x = .data$dose, y = .data$IC * 100)) +
    (if (!all(is.na(curve))) {
      geom_ribbon(
        data = curve,
        mapping = aes(
          x = .data$dose,
          y = .data$response,
          ymin = .data$response - error,
          ymax = .data$response + error,
        ),
        alpha = 0.5,
        fill = "grey74",
      )
    }) +
    geom_point(color = "black", size = 2.5) +
    (if (!all(is.na(curve))) {
      geom_line(
        data = curve,
        mapping = aes(x = .data$dose, y = .data$response),
        color = "steelblue4",
        size = 2.5,
      )
    }) +
    labs(x = paste0("Concentration (", conc_unit, ")"), y = "% Inhibition", size = 18) +
    ggtitle(title) +
    coord_cartesian(ylim = c(-50, 150)) +
    theme(
      plot.title = element_text(
        hjust = 0.5, size = 23, face = "bold", margin = margin(r = 20, b = 10),
      ),
      axis.text = element_text(size = 24, face = "bold"),
      axis.title.x = element_text(
        size = 20, color = "slategray4", margin = margin(t = 15, r = 25),
      ),
      axis.title.y = element_text(size = 20, color = "slategray4"),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
    ) +
    scale_x_continuous(trans = "log10", labels = function(n) {
      format(n, scientific = FALSE)
    })

  if (!is.na(IC50) && min(drdata$dose) <= IC50 && IC50 <= max(drdata$dose)) {
    dplot <- dplot + geom_vline(xintercept = IC50, linetype = "dotted")
  }

  plot(dplot)
}

## plotting QC scatter replicates
plotScatter <- function(screenData) {
  PlateScatter <- screenData[screenData$WellType == "therapy", ]
  plateList <- unique(PlateScatter$Plate)
  PlateScatter_rep <- PlateScatter[PlateScatter$Replicate == 2, ]
  PlateScatter <- PlateScatter[PlateScatter$Replicate == 1, ]
  names(PlateScatter_rep)[names(PlateScatter_rep) == "Readout"] <- "Readout_rep"
  PlateScatter <- merge(PlateScatter, PlateScatter_rep, by = c("Treatment", "Concentration"))
  scatterlist <- lapply(plateList, function(plateName) {
    p <- filter(PlateScatter, .data$Plate.x == plateName) %>%
      ggplot(aes(x = log10(.data$Readout), y = log10(.data$Readout_rep))) +
      geom_point(color = "#21698D") +
      ggpubr::stat_cor(method = "pearson") +
      ggtitle(plateName) +
      xlab("log(rawcount)") +
      ylab("replicate log(rawcount)")
  })
  return(scatterlist)
}

## Plot MRA/CRA cohort heatmaps
plot_cohort_heatmap <- function(input, output, mod, heatmap_dir) {
  files <- list.files(path = heatmap_dir, pattern = "*.xlsx", full.names = TRUE)
  if (length(files) == 0) {
    return()
  }
  debug_save(files)

  plot_sdss <- input[[paste0(mod, "_sdss")]] && !is.null(input$reference_samples_file)
  dss_label <- if (plot_sdss) "sDSS_asym" else "DSS_asym"

  tbl <- lapply(files, readxl::read_xlsx)
  debug_save(tbl)
  df1 <- as.data.frame(do.call(rbind, tbl))
  df_xDSS <- df1[, c("Drug.Name", dss_label, "analysis")]
  xDSS_hp <- df_xDSS %>% tidyr::spread(.data$analysis, dss_label)
  xDSS_m <- as.matrix(xDSS_hp[, -1, drop = FALSE])
  rownames(xDSS_m) <- xDSS_hp[, 1]
  xDSS_m <- xDSS_m[rowSums(is.na(xDSS_m)) != ncol(xDSS_m), , drop = FALSE]
  xDSS_m <- t(xDSS_m)
  debug_save(xDSS_m)
  row_cluster <- input[[paste0(mod, "_rowcluster")]]
  col_cluster <- input[[paste0(mod, "_colcluster")]]

  if (input[[paste0(mod, "_include_zp")]]) {
    zprime <- data.frame()
    pids <- rownames(xDSS_m)
    plate_zpr <- do.call(rbind, lapply(pids, function(pid) {
      pid_qc_file <- file.path(heatmap_dir, "..", "QC", paste0(pid, "_QC.xlsx"))
      pid_qc_df <- openxlsx::read.xlsx(pid_qc_file, rowNames = TRUE)
      zprime_columns <- endsWith(rownames(pid_qc_df), "_zprime_r")
      zprime_names <- gsub("_zprime_r$", "", rownames(pid_qc_df)[zprime_columns])
      zprime_names <- paste("Plate", zprime_names)
      zprime_names <- gsub("Plate screen", "Screen", zprime_names, fixed = TRUE)

      zprime_values <- as.numeric(pid_qc_df[zprime_columns, 1])
      names(zprime_values) <- zprime_names
      zprime_values
    }))
    rownames(plate_zpr) <- pids
    debug_save(plate_zpr)

    # assignment of an object of class “expression” is not valid for @‘name’
    # in an object of class “ColorMapping”
    # > plate_zpr_label <- expression("z'"[r])

    # gt_render requires gridtext and complicates vertical alignment.
    # > plate_zpr_label <- ComplexHeatmap::gt_render("z'<sub>r</sub>")

    plate_zpr_label <- "z'"

    row_ha <- ComplexHeatmap::rowAnnotation(
      plate_zpr = plate_zpr,
      col = list(plate_zpr = circlize::colorRamp2(
        c(-1, 0, 1),
        c("gray53", "white", "olivedrab")
      )),
      annotation_legend_param = list(plate_zpr = list(
        title = plate_zpr_label,
        title_position = "topleft",
        legend_direction = "horizontal"
      )),
      annotation_name_gp = grid::gpar(fontsize = 9),
      annotation_name_side = "bottom"
    )
  } else {
    row_ha <- NULL
  }

  more_than_one_sample <- nrow(xDSS_m) > 1
  shinyjs::toggleState(paste0(mod, "_include_sd"), more_than_one_sample)
  if (!more_than_one_sample) {
    shiny::updateCheckboxInput(inputId = paste0(mod, "_include_sd"), value = FALSE)
  }

  if (input[[paste0(mod, "_include_sd")]] && more_than_one_sample) {
    xdss_sd <- apply(xDSS_m, 2, sd, na.rm = TRUE)
    debug_save(xdss_sd)

    # Avoid c(0, 0) range
    ylim <- c(0, max(xdss_sd, .Machine$double.eps, na.rm = TRUE))

    xdss_sd_label <- "Drug SD"
    col_ha <- ComplexHeatmap::columnAnnotation(
      xdss_sd = ComplexHeatmap::anno_barplot(xdss_sd, ylim = ylim),
      annotation_label = list(xdss_sd = xdss_sd_label),
      annotation_name_gp = grid::gpar(fontsize = 9),
      annotation_name_side = "right"
    )
  } else {
    col_ha <- NULL
  }

  cohort_heatmap <- ComplexHeatmap::Heatmap(
    xDSS_m,
    name = dss_label,
    col = circlize::colorRamp2(
      if (plot_sdss) c(-10, 5, 30) else c(0, 10, 50),
      c("steelblue4", "white", "tomato3")
    ),
    clustering_distance_rows = row_cluster,
    clustering_distance_columns = col_cluster,
    row_names_gp = grid::gpar(fontsize = 9),
    column_names_gp = grid::gpar(fontsize = 9),
    right_annotation = row_ha,
    top_annotation = col_ha,
    gap = grid::unit(4, "mm"),
    heatmap_legend_param = list(
      title = dss_label,
      title_position = "topleft",
      legend_direction = "horizontal"
    )
  )

  output[[paste0(mod, "_hp_download")]] <- downloadHandler(
    filename = "cohort_heatmap.html",
    content = function(file) {
      # Use default renderPlot resolution to match plot appearance in browser.
      # https://shiny.rstudio.com/reference/shiny/latest/renderPlot.html
      # This does not make sizes in px equali - downloaded heatmaps
      # are still larger by a factor of ~1333/500.
      dpi <- 72
      width <- input[[paste0(mod, "c_width")]] / dpi
      height <- input[[paste0(mod, "c_height")]] / dpi
      rmarkdown_render("cohort_heatmap", file)
    }
  )

  # estimate heatmap dimensions
  w_offset <- 10 # approx. number of squares for dendrograms, Z', samples
  h_offset <- 30 # approx. number of squares for dendrograms, QC, drugs, legends
  square_size <- 10 # target size of heatmap squares in pixels
  width <- (ncol(xDSS_m) + w_offset) * square_size
  height <- (nrow(xDSS_m) + h_offset) * square_size

  # round dimensions to step size
  step <- 20
  width <- round(width / step) * step
  height <- round(height / step) * step

  shiny::updateSliderInput(inputId = paste0(mod, "c_width"), value = width)
  shiny::updateSliderInput(inputId = paste0(mod, "c_height"), value = height)

  cohort_heatmap <- ComplexHeatmap::draw(
    cohort_heatmap,
    merge_legends = TRUE,
    heatmap_legend_side = "bottom",
    annotation_legend_side = "bottom"
  )
}
