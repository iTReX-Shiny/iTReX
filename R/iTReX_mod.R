############################
## iTReX module functions ##
## Author: Dina ElHarouni ##
############################

MRA.mod <- function(input, drdata, output_dir, control_dir, PID,
                    np = "all", run_type = "mono", heatmap_dir) {
  debug_save(drdata)

  # prepare therapy list
  colnames(drdata)[colnames(drdata) == "normVal"] <- "viability"
  colnames(drdata)[colnames(drdata) == "Concentration"] <- "dose"
  drdata$IC <- 1 - drdata$viability
  splitlist <- split(drdata, f = drdata$Treatment)
  splitlist <- splitlist[vapply(splitlist, function(x) dim(x)[1], numeric(1)) > 0]
  titleframe <- data.frame(names(splitlist))

  n <- length(splitlist)

  # set data frames, sheets and lists to be used
  finaltable2 <- data.frame(matrix(ncol = 7, nrow = n))
  colnames(finaltable2) <- c("DRUG", "IC50", "SLOPE", "MIN", "MAX", "min.conc", "max.conc")

  DSStable2 <- data.frame(matrix(ncol = 11, nrow = n))
  colnames(DSStable2) <- c("Drug.ID", "Drug.Name", "abs_IC25", "abs_IC75", "abs_IC50", "rel_IC50", "Slope", "Imin", "Imax", "MIN.Conc.tested.nM", "MAX.Conc.tested.nM")

  Dinhibit <- data.frame(matrix(ncol = 8, nrow = n))
  colnames(Dinhibit) <- c("DSS_asym", "PI1", "PI2", "PI3", "PI4", "PI5", "GOF", "Amin")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "sheet 1", gridLines = TRUE)
  openxlsx::setColWidths(wb, 1, cols = 17, widths = 40)

  curvelist <- list()

  # start therapy analysis
  openxlsx::setRowHeights(wb, 1, rows = 2:(1 + n), heights = 210)
  for (i in seq_len(n)) {
    # drug parameters
    model <- tryCatch(nplr::nplr(x = splitlist[[i]]$dose, y = splitlist[[i]]$IC, npars = np, silent = TRUE), error = function(err) NA)
    if (is.na(model)) {
      DSStable2[i, ] <- c(i, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA)
      Dinhibit[i, ] <- c("failed", NA, NA, NA, NA, NA, NA, NA)
      curve <- NA
      error <- NA
      IC50 <- NA
    } else {
      yline <- as.data.frame(nplr::getYcurve(model))
      xline <- as.data.frame(nplr::getXcurve(model))
      xline <- 10^(xline)
      curve <- cbind(xline, yline)
      names(curve)[1] <- "dose"
      names(curve)[2] <- "response"
      param <- as.data.frame(nplr::getPar(model))
      max <- max(nplr::getYcurve(model)) * 100
      min <- min(nplr::getYcurve(model)) * 100
      xmid <- 10^(param$params.xmid)
      asymm <- param$params.s
      if (param$params.top >= param$params.bottom) {
        Imax <- param$params.top * 100
        Imin <- param$params.bottom * 100
        slope <- param$params.scal
        Iminr <- max(-10, Imin)
        Imaxr <- min(110, Imax)
        halfCEmax <- (Imaxr + Iminr) / 200
        estimate <- nplr_get_estimates(model, halfCEmax)
        IC50 <- estimate[1, 3]
        estimate <- nplr_get_estimates(model, 0.5)
        absIC50 <- estimate[1, 3]
        estimate <- nplr_get_estimates(model, 0.25)
        absIC25 <- estimate[1, 3]
        estimate <- nplr_get_estimates(model, 0.75)
        absIC75 <- estimate[1, 3]
      } else {
        Imax <- param$params.bottom * 100
        Imin <- param$params.top * 100
        slope <- param$params.scal * -1
        Iminr <- max(-10, Imin)
        Imaxr <- min(110, Imax)
        halfCEmax <- (Imaxr + Iminr) / 200
        row <- which.min(abs(curve$response - halfCEmax))
        IC50 <- curve[row, 1]
        row <- which.min(abs(curve$response - 0.5))
        absIC50 <- curve[row, 1]
        row <- which.min(abs(curve$response - 0.25))
        absIC25 <- curve[row, 1]
        row <- which.min(abs(curve$response - 0.75))
        absIC75 <- curve[row, 1]
      }

      minconc <- min(splitlist[[i]]$dose)
      maxconc <- max(splitlist[[i]]$dose)
      Goodness <- as.data.frame(nplr::getGoodness(model))
      GOF <- Goodness$gof
      GOF <- max(0, GOF)
      error <- (1 - GOF) * 100
      curve$response <- (curve$response) * 100
      IC50 <- ifelse(slope < 0, NA, IC50)
      absIC50 <- ifelse(slope < 0, NA, absIC50)
      absIC25 <- ifelse(slope < 0, NA, absIC25)
      absIC75 <- ifelse(slope < 0, NA, absIC75)
      min <- max(-10, min)
      max <- min(110, max)

      vdata <- data.frame(splitlist[[i]]$dose, splitlist[[i]]$viability, splitlist[[i]]$IC)
      colnames(vdata) <- c("dose", "viability", "IC")
      vdata <- vdata[order(vdata$dose), ]

      combo_table <- drdata[!is.na(drdata$AddOn), ]
      Fit <- as.data.frame(nplr::getFitValues(model))
      Fit$dose <- sort(splitlist[[i]]$dose)
      Fit <- Fit[!duplicated(Fit$dose), ]
      D1 <- Fit[1, 1] * 100
      D2 <- Fit[2, 1] * 100
      D3 <- Fit[3, 1] * 100
      D4 <- Fit[4, 1] * 100
      D5 <- Fit[5, 1] * 100

      DSSinput <- c(i, 0, absIC25, absIC75, absIC50, IC50, slope, Iminr, Imaxr, minconc, maxconc)
      DSSinput2 <- c(i, 0, xmid, slope, Imin, Imax, asymm, minconc, maxconc)

      A_min <- switch(input$Amin_select,
        "var10" = 0.1 * max(100, Imaxr),
        "other" = input$Amin_slider,
        as.numeric(input$Amin_select)
      )

      DSStable2[i, ] <- DSSinput
      Dinhibit[i, ] <- c(
        compute_dss(pars_table = as.data.frame(t(DSSinput2[3:9])), resp_min = A_min, score_type = 1, conc_scale_unused = 1e-9, bottom_corr = FALSE, allow_big_mid = TRUE, int_decreasing = TRUE, curve = curve),
        D1, D2, D3, D4, D5, GOF, A_min
      )
    }
    curvelist[[i]] <- curve
    path <- file.path(output_dir, "dose_response_curves", paste0(titleframe[i, ], ".png"))
    grDevices::png(filename = path)
    plot.iscreen(drdata = splitlist[[i]], curve = curve, error = error, IC50 = IC50, title = as.character(paste0(titleframe[i, ], "_", PID)))
    grDevices::graphics.off()
    openxlsx::insertImage(wb, 1, width = 2.3, height = 2.3, file = path, startRow = 1 + i, startCol = 17, units = "in")
  }

  DSStable2$Drug.Name <- data.frame(names(splitlist))
  finaltable2 <- cbind(DSStable2, Dinhibit)
  finaltable2$Drug.Name <- paste(unlist(titleframe))
  names(curvelist) <- names(splitlist)

  nplr_inhibitor <- finaltable2
  nplr_inhibitor$DSS_asym <- as.numeric(nplr_inhibitor$DSS_asym)
  nplr_inhibitor$DSS_asym <- nplr_inhibitor$DSS_asym / 2
  nplr_inhibitor$GOF <- as.numeric(nplr_inhibitor$GOF)
  nplr_inhibitor$DSS_asym_adj <- nplr_inhibitor$DSS_asym * nplr_inhibitor$GOF

  if (is.null(control_dir)) {
    nplr_inhibitor$controlDSS <- NA
    nplr_inhibitor$sDSS_asym <- NA
    nplr_inhibitor$controlDSS_adj <- NA
    nplr_inhibitor$sDSS_asym_adj <- NA
  } else {
    HControls <- readxl::read_xlsx(control_dir)
    HControls <- data.frame("Drug.Name" = HControls$Drug.Name, "cDSS" = HControls$mean_DSS, "cDSS_adj" = HControls$mean_DSS_adj)
    nplr_inhibitor$controlDSS <- HControls$cDSS[match(nplr_inhibitor$Drug.Name, HControls$Drug.Name)]
    nplr_inhibitor$sDSS_asym <- (as.numeric(nplr_inhibitor$DSS_asym) - as.numeric(nplr_inhibitor$controlDSS))
    nplr_inhibitor$controlDSS_adj <- HControls$cDSS_adj[match(nplr_inhibitor$Drug.Name, HControls$Drug.Name)]
    nplr_inhibitor$sDSS_asym_adj <- (as.numeric(nplr_inhibitor$DSS_asym_adj) - as.numeric(nplr_inhibitor$controlDSS_adj))
  }
  nplr_inhibitor$analysis <- PID

  openxlsx::writeDataTable(wb, 1, nplr_inhibitor)

  if (run_type == "mono") {
    openxlsx::saveWorkbook(wb, file.path(output_dir, paste0(PID, "_mono.xlsx")), overwrite = TRUE)
    saveRDS(curvelist, file.path(output_dir, paste0(PID, "_curves_mono.rds")))
    openxlsx::saveWorkbook(wb, file.path(heatmap_dir, paste0(PID, "_MRA.xlsx")), overwrite = TRUE)
  } else {
    openxlsx::saveWorkbook(wb, file.path(output_dir, paste0(PID, "_combo.xlsx")), overwrite = TRUE)
    saveRDS(curvelist, file.path(output_dir, paste0(PID, "_curves_combo.rds")))
    openxlsx::saveWorkbook(wb, file.path(heatmap_dir, paste0(PID, "_CRA.xlsx")), overwrite = TRUE)
  }

  MRAlist <- list("nplr_inhibitor" = nplr_inhibitor, "wb" = wb, "splitlist" = splitlist)

  return(MRAlist)
}

CRA.mod <- function(combo_nplr, output_dir, splitlist, PID, heatmap_dir) {
  single_combo <- combo_nplr
  single <- single_combo[-grep("combo_", single_combo$Drug.Name), ]
  combo <- single_combo[grep("combo_", single_combo$Drug.Name), ]
  combo$Drug.Name <- gsub("combo_", "", combo$Drug.Name)

  single <- data.frame("Drug.Name" = single$Drug.Name, "cPI1" = single$PI1, "cPI2" = single$PI2, "cPI3" = single$PI3, "cPI4" = single$PI4, "cPI5" = single$PI5)
  rownames(single) <- single$Drug.Name
  combo2 <- combo[, c("PI1", "PI2", "PI3", "PI4", "PI5")]
  rownames(combo2) <- combo$Drug.Name
  intersect_drugs <- intersect(rownames(combo2), rownames(single))
  single <- single[intersect_drugs, ]
  combo2 <- combo2[intersect_drugs, ]
  s_c <- cbind(single, combo2)
  s_c <- s_c[complete.cases(s_c[, 2:11]), ]
  s_c[, 2:11] <- lapply(s_c[, 2:11], function(x) as.numeric(as.character(x)))
  s_c$dPI1 <- (s_c$PI1 - s_c$cPI1)
  s_c$dPI2 <- (s_c$PI2 - s_c$cPI2)
  s_c$dPI3 <- (s_c$PI3 - s_c$cPI3)
  s_c$dPI4 <- (s_c$PI4 - s_c$cPI4)
  s_c$dPI5 <- (s_c$PI5 - s_c$cPI5)

  dD <- s_c[, c("Drug.Name", "dPI1", "dPI2", "dPI3", "dPI4", "dPI5")]
  dsD <- dD[, c("Drug.Name", "dPI1", "dPI2", "dPI3", "dPI4", "dPI5")] # TODO
  dD <- dD[, 1:6]

  long_DF2 <- dsD %>% tidyr::gather("Dose", "dsD", .data$dPI1:.data$dPI5)
  long_DF2$name_dose <- paste(long_DF2$Drug.Name, "_", long_DF2$Dose)

  # get splitl
  conc_df <- do.call(rbind.data.frame, splitlist)
  conc_df <- conc_df[-grep("combo", conc_df$Treatment), ]
  conc_df <- conc_df[order(conc_df$Treatment, conc_df$dose), ]

  conc_df <- unique(conc_df[c("Treatment", "dose")])
  names(conc_df)[names(conc_df) == "Treatment"] <- "Drug.Name"


  long2_final <- long_DF2[order(long_DF2$Drug.Name, long_DF2$Dose), ]

  conc_df <- conc_df %>%
    filter(.data$Drug.Name %in% long2_final$Drug.Name)
  names(conc_df)[names(conc_df) == "Drug.Name"] <- "Treatment"

  long2 <- cbind(long2_final, conc_df)

  Synergy_df <- long2
  Synergy_df <- Synergy_df[, 1:3]
  Synergy_df <- tidyr::spread(Synergy_df, .data$Dose, dsD)


  single_combo <- combo_nplr
  single <- single_combo[-grep("combo", single_combo$Drug.Name), ]
  combo <- single_combo[grep("combo", single_combo$Drug.Name), ]
  combo$Drug.Name <- gsub("combo_", "", combo$Drug.Name)
  rownames(single) <- single$Drug.Name
  rownames(combo) <- combo$Drug.Name
  intersect_drugs <- intersect(rownames(combo), rownames(single))
  single <- single[intersect_drugs, ]
  combo <- combo[intersect_drugs, ]

  single <- data.frame("Drug.Name" = single$Drug.Name, "cDSS" = single$DSS_asym)
  combo$controlDSS <- single$cDSS[match(combo$Drug.Name, single$Drug.Name)]
  combo$dcDSS_asym <- (combo$DSS_asym - combo$controlDSS)

  intersect <- intersect(rownames(combo), Synergy_df$Drug.Name)
  combo <- combo[intersect, ]
  Synergy_df <- Synergy_df[Synergy_df$Drug.Name %in% intersect, ]

  Synergy_df$dcDSS_asym <- combo$dcDSS_asym

  curvelist_mono <- readRDS(file.path(output_dir, paste0(PID, "_curves_mono.rds")))
  curvelist_combo <- readRDS(file.path(output_dir, paste0(PID, "_curves_combo.rds")))
  datapoint_mono <- readRDS(file.path(output_dir, paste0("pre_process/", PID, "_mono.rds")))
  datapoint_combo <- readRDS(file.path(output_dir, paste0("pre_process/", PID, "_combo.rds")))
  GOF_mono <- datapoint_mono$nplr_inhibitor
  GOF_combo <- datapoint_combo$nplr_inhibitor
  splt_mono <- datapoint_mono$splitlist
  splt_combo <- datapoint_combo$splitlist
  names(curvelist_combo) <- gsub("combo_", "", names(curvelist_combo))
  names(splt_combo) <- gsub("combo_", "", names(splt_combo))
  GOF_combo$Drug.Name <- gsub("combo_", "", GOF_combo$Drug.Name)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "sheet 1", gridLines = TRUE)
  openxlsx::setColWidths(wb, 1, cols = 8, widths = 40)

  titleframe <- data.frame(sort(unique(s_c$Drug.Name)))

  for (i in seq_len(nrow(titleframe))) {
    openxlsx::setRowHeights(wb, 1, rows = 1 + i, heights = 210)

    path <- file.path(output_dir, "synergy_plots", paste0(titleframe[i, ], ".png"))

    grDevices::png(filename = path)

    drug_name <- names(splitlist[i])

    curvelist_mono[[drug_name]]$type <- "mono"
    curvelist_mono[[drug_name]]$error_max <- curvelist_mono[[drug_name]]$response + (1 - GOF_mono[GOF_mono$Drug.Name == drug_name, ]$GOF)
    curvelist_mono[[drug_name]]$error_min <- curvelist_mono[[drug_name]]$response - (1 - GOF_mono[GOF_mono$Drug.Name == drug_name, ]$GOF)
    curvelist_combo[[drug_name]]$type <- "combo"
    curvelist_combo[[drug_name]]$error_max <- curvelist_combo[[drug_name]]$response + (1 - GOF_combo[GOF_combo$Drug.Name == drug_name, ]$GOF)
    curvelist_combo[[drug_name]]$error_min <- curvelist_combo[[drug_name]]$response - (1 - GOF_combo[GOF_combo$Drug.Name == drug_name, ]$GOF)
    onecurve_drug <- rbind(curvelist_mono[[drug_name]], curvelist_combo[[drug_name]])
    splt_mono[[drug_name]]$type <- "mono"
    splt_combo[[drug_name]]$type <- "combo"
    onepoint_drug <- rbind(splt_mono[[drug_name]], splt_combo[[drug_name]])

    cplot <- ggplot() +
      geom_ribbon(data = onecurve_drug, aes(x = .data$dose, y = .data$response, group = .data$type, ymin = .data$error_min, ymax = .data$error_max), alpha = 0.5, fill = "grey74") +
      geom_line(data = onecurve_drug, aes(x = .data$dose, y = .data$response, group = .data$type, col = .data$type), size = 1.2) +
      geom_point(data = onepoint_drug, aes(x = .data$dose, y = .data$IC * 100, group = .data$type, col = .data$type), size = 1.2) +
      labs(x = "Concentration (nM)", y = "% Inhibition", size = 18) +
      ggtitle(as.character(paste0(titleframe[i, ], "_", PID))) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), axis.text = element_text(size = 20, face = "bold"), axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)) +
      scale_x_continuous(trans = "log10", labels = function(n) {
        format(n, scientific = FALSE)
      }) +
      scale_color_manual(values = c("deeppink4", "steelblue4"))

    tryCatch(plot(cplot, title = " "), error = function(err) NA)
    grDevices::graphics.off()

    openxlsx::insertImage(wb, 1, width = 2.3, height = 2.3, file = path, startRow = 1 + i, startCol = 8, units = "in")
  }

  openxlsx::writeDataTable(wb, 1, Synergy_df)

  openxlsx::saveWorkbook(wb, file.path(output_dir, paste0(PID, "_CRA.xlsx")), overwrite = TRUE)

  CRAlist <- list("Synergy_df" = Synergy_df, "wb" = wb)

  return(CRAlist)
}

HitNet.mod <- function(drdata, DT_targets, rank_based = "DSS_asym", threshold = 5) {
  interactions <- OmnipathR::import_omnipath_interactions(resources = c("SignaLink3"), organism = 9606)

  debug_save(drdata, DT_targets)

  drdata$Drug.Name <- gsub("combo_", "", drdata$Drug.Name)
  topD <- as.data.frame(drdata[order(-drdata[, c(rank_based)]), c(rank_based, "Drug.Name")])
  topD <- topD[1:threshold, ]$Drug.Name

  topMeta <- DT_targets[DT_targets$Name %in% topD, ]
  targets <- as.vector(unique(topMeta$Gene.Name))
  inter_trial <- interactions[interactions$source_genesymbol %in% targets | interactions$target_genesymbol %in% targets, ]
  OPI_g <- OmnipathR::interaction_graph(interactions = inter_trial)

  vector1 <- c(inter_trial$source_genesymbol)
  keep1 <- vector1[duplicated(vector1)]
  vector2 <- c(inter_trial$target_genesymbol)
  keep2 <- vector2[duplicated(vector2)]
  keep <- unique(c(keep1, keep2))
  complex_trial <- inter_trial[inter_trial$source_genesymbol %in% c(keep) & inter_trial$target_genesymbol %in% c(keep), ]

  ## include drugs
  drugs_add <- data.frame(matrix(ncol = ncol(complex_trial), nrow = nrow(topMeta)))
  colnames(drugs_add) <- colnames(complex_trial)
  drugs_add$source_genesymbol <- topMeta$Name
  drugs_add$source <- topMeta$Name
  drugs_add$target <- topMeta$Gene.Name
  drugs_add$target_genesymbol <- topMeta$Gene.Name
  drugs_add <- drugs_add[drugs_add$target_genesymbol %in% c(complex_trial$target_genesymbol, complex_trial$source_genesymbol), ]
  if (nrow(drugs_add) == 0) {
    network_l <- NULL
  } else {
    drugs_add$is_directed <- 1
    drugs_add$is_stimulation <- 0
    drugs_add$is_inhibition <- 1
    drugs_add$consensus_direction <- 1
    drugs_add$consensus_stimulation <- 0
    drugs_add$consensus_inhibition <- 1

    complex_trial <- rbind(complex_trial, drugs_add)

    # create OPI object
    OPI_g <- OmnipathR::interaction_graph(interactions = complex_trial)

    ## color based
    igraph_df <- as.data.frame(igraph::V(OPI_g)$name)
    colnames(igraph_df) <- "node_names"
    igraph_df$color <- "lightsteelblue3"
    igraph_df$color_frame <- ifelse(igraph_df$node_names %in% targets, "deeppink4", "white")
    igraph_df$node_shape <- ifelse(igraph_df$node_names %in% targets, "square", "circle")
    igraph_df$node_shape <- ifelse(igraph_df$node_names %in% topMeta$Name, "pie", igraph_df$node_shape)
    igraph::V(OPI_g)$color <- igraph_df$color
    igraph::V(OPI_g)$size <- 11
    igraph::V(OPI_g)$shape <- igraph_df$node_shape
    igraph::V(OPI_g)$size <- ifelse(igraph_df$node_names %in% targets, 13, 11)
    igraph_es <- as.data.frame(igraph::as_edgelist(OPI_g))
    igraph::E(OPI_g)$lty <- ifelse(igraph_es$V1 %in% topMeta$Name, "dashed", "solid")
    network_l <- list("OPI_g" = OPI_g, "igraph_df" = igraph_df)
  }
  return(network_l)
}

Omics.mod <- function(drdata, omics_df, interactions, DT_targets, rank_based = "DSS_asym", threshold = 5) {
  network_hit <- HitNet.mod(drdata, DT_targets, rank_based, threshold)
  if (is.null(network_hit)) {
    network_l <- NULL
  } else {
    drdata$Drug.Name <- gsub("combo_", "", drdata$Drug.Name)
    topD <- as.data.frame(drdata[order(-drdata[, c(rank_based)]), c(rank_based, "Drug.Name")])
    topD <- topD[1:threshold, ]$Drug.Name

    topMeta <- DT_targets[DT_targets$Name %in% topD, ]

    targets <- as.vector(unique(topMeta$Gene.Name))

    igraph_df <- network_hit$igraph_df
    OPI_g <- network_hit$OPI_g

    mutations <- omics_df$Mutation
    expression <- omics_df$Expression
    fusion <- omics_df$Fusion

    igraph_df$color <- ifelse(igraph_df$node_names %in% expression, "#A0AFA1", igraph_df$color)
    igraph_df$color <- ifelse(igraph_df$node_names %in% mutations, "#29AF7FFF", igraph_df$color)
    igraph_df$color <- ifelse(igraph_df$node_names %in% fusion, "#DCE319FF", igraph_df$color)
    igraph::V(OPI_g)$color <- igraph_df$color
    igraph::V(OPI_g)$size <- ifelse(igraph_df$node_names %in% targets, 13, 11)

    network_l <- list("OPI_g" = OPI_g, "igraph_df" = igraph_df)
  }
  return(network_l)
}
