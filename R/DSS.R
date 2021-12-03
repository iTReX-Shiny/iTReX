################################################################################
# Developer name: Bhagwan Yadav                                                #
# Developed at Institute for Molecular Medicine Finland (FIMM)                 #
# Rewritten and extended by Yannick Berker, KITZ Heidelberg (v22)              #
# Supports asymmetric integration through trapezoidal integration.             #
################################################################################

#' Compute a Drug Sensitivity Score (DSS)
#'
#' @importFrom bayestestR area_under_curve
#' @importFrom stringr str_replace
#' @importFrom zeallot %<-%
#'
#' @param pars_table can be a data.frame of (in that order!)
#   - (conc_mid, slope, top, conc_min, conc_max),
#   - (conc_mid, slope, bottom, top, conc_min, conc_max), or
#   - (conc_mid, slope, bottom, top, asymm, conc_min, conc_max)
#' @param resp_min TODO
#' @param score_type TODO
#' @param conc_scale_unused TODO
#' @param apply_log_to_conc TODO
#' @param curve TODO
#' @param force_symm TODO
#' @param resp_max TODO
#' @param bottom_corr TODO
#' @param allow_big_mid TODO
#' @param int_decreasing TODO
#' @param allow_nan TODO
#' @export
compute_dss <- function(pars_table,
                        resp_min = 10,
                        score_type = 3,
                        conc_scale_unused = 1e-9,
                        apply_log_to_conc = TRUE,
                        # for compatibility with the DSS package,
                        # do not change order of the parameters above.
                        curve = NA,
                        force_symm = FALSE,
                        resp_max = 100,
                        bottom_corr = TRUE,
                        allow_big_mid = FALSE,
                        int_decreasing = FALSE,
                        allow_nan = TRUE) {
  if (!is.data.frame(pars_table)) {
    stop("pars_table must be a data.frame")
  }

  if (nrow(pars_table) > 1) {
    stop("compute_dss supports only for 1-row parameter tables.")
  }

  if (all(is.na(curve))) {
    stop("curve required for compute_dss")
  }
  curve <- curve[is.finite(curve$dose) & is.finite(curve$response), ]

  if (bottom_corr && resp_min <= 0) {
    stop("resp_min must be strictly positive with bottom_corr.")
  }

  if (bottom_corr && int_decreasing) {
    stop("int_decreasing does not make sense with bottom_corr")
  }

  dss_table <- data.frame(DSS = NA)
  for (row in seq_len(nrow(pars_table))) {
    pars <- as.matrix(pars_table[row, ])

    # sanitize input data
    pars <- as.numeric(str_replace(pars, ",", "."))

    # parameter checking
    if (sum(is.na(pars))) {
      dss_table[row] <- NA
      next
    }

    # parameter unpacking
    slope <- NULL
    if (ncol(pars_table) == 5) {
      c(conc_mid, slope, top, conc_min, conc_max) %<-% pars
      bottom <- 0
      asymm <- 1
    } else if (ncol(pars_table) == 6) {
      c(conc_mid, slope, bottom, top, conc_min, conc_max) %<-% pars
      asymm <- 1
    } else if (ncol(pars_table) == 7) {
      c(conc_mid, slope, bottom, top, asymm, conc_min, conc_max) %<-% pars
    } else {
      stop("Unknown numbers of parameters.")
    }

    # bottom correction
    if (bottom != 0 && bottom_corr) {
      # shift curves such that bottom == 0
      top <- top - bottom

      # Verified that curve is passed by value, not effect on
      # the value outside of this function's scope.
      curve$response <- curve$response - bottom

      bottom <- 0
    }

    # concentration logging
    if (apply_log_to_conc) {
      conc_mid <- log10(conc_mid)
      conc_min <- log10(conc_min)
      conc_max <- log10(conc_max)
    }

    is_decreasing <- sign(slope * (top - bottom)) < 0

    # prevent too large conc_mid (unless allow_big_mid),
    # decreasing curve (unless int_decreasing),
    # too low top asymptote (unless int_decreasing),
    # or invalid limits (DSS package did not check that).
    # Note: one might think that results can be different
    # from DSS package because this version accepts slope < 0, top < bottom.
    # But with bottom_corr=TRUE (DSS default), curve will be shifted
    # below 0 and DSS will be 0.
    if (FALSE ||
      # translated from DSS package, "ic50 >= Max.Conc"
      (conc_mid >= conc_max && !allow_big_mid) ||
      # translated from DSS package: "b < 0", "a = a - d" and "int_y < 0"
      (is_decreasing && !int_decreasing) ||
      # translated from DSS package: "a <= y"
      (resp_min >= top && !int_decreasing) ||
      # translated from DSS package: "int_y < 0", etc.
      (conc_min > conc_max)
    ) {
      dss_table[row] <- 0
      next
    }

    # set up inverse function
    inverse <- function(y,
                        a = top,
                        b = slope,
                        c = conc_mid,
                        d = bottom,
                        s = asymm) {
      if (y <= d || y >= a) {
        return(sign(y - a) * Inf)
      }

      if (s == 1 && force_symm) {
        x <- c - log10(((a - y) / (y - d))) / b
        return(x)
      }

      x <- c - log10(((a - d) / (y - d))^(1 / s) - 1) / b
      return(x)
    }

    # calculate x coordinate of intersection of curve with y = resp_min
    conc_inter <- inverse(resp_min)
    conc_inter <- max(conc_min, min(conc_inter, conc_max))

    # calculate integration and area limits
    conc_right <- max(conc_min, conc_max)
    conc_left <- max(conc_min, min(conc_inter, conc_right))

    # compute AUC integral
    x <- curve$dose
    if (apply_log_to_conc) {
      x <- log10(x)
    }

    y <- curve$response
    # convert decreasing curves into constant ones
    if (int_decreasing && is_decreasing) {
      curve_min <- y[length(y)]
      y[TRUE] <- curve_min
    }
    # shift curve and filter by min response
    y <- (y - resp_min) * (y > resp_min)

    integral <- area_under_curve(x, y)

    # compute maximal possible areas
    # do not clip conc_max to conc_right here, not used for CTA anyway
    max_tot_area <- (resp_max - resp_min) * (conc_max - conc_min)
    max_int_area <- (resp_max - resp_min) * (conc_right - conc_left)

    # compute "normalized" area
    # do not clip top to 100 here, likely overfitted anyway
    # do not use max(top, bottom) here, same reason
    dss1 <- 100 * integral / max_tot_area
    dss2 <- dss1 / log10(top)
    dss3 <- dss2 / max_tot_area * max_int_area * log10(100)
    # https://doi.org/10.1016/j.leukres.2017.11.008
    dssmod <- dss1 * log10(top / 10)

    dss <- switch(toString(score_type),
      "0" = integral,
      "1" = dss1,
      "2" = dss2,
      "3" = dss3,
      "mod" = dssmod,
    )

    if (!allow_nan) stopifnot(is.finite(dss[[1]]))

    # compute and export DSS
    dss_table[row] <- round(dss, digits = 8)
  }

  # Goodbye
  return(dss_table)
}
