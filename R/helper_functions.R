############################
## iTReX helper functions ##
## Author: Dina ElHarouni ##
############################

## Generate sequence of row IDs "A", "B", ...
EXCEL_COLUMN_LETTERS <- paste0(
  rep(c("", LETTERS), each = length(LETTERS)),
  rep(LETTERS, times = 1 + length(LETTERS))
)

row_sequence <- function(n) {
  EXCEL_COLUMN_LETTERS[seq_len(n)]
}

## Generate sequence of columns IDs "01", "02", ...
col_sequence <- function(n) {
  stringr::str_pad(seq_len(n), 2, pad = "0")
}

## Quality Control function to compute mean, SD, CV, fixsd and robust zprime
QCsummary <- function(screenData, PID) {
  # t(cbind(data.frame(c(1, 2)), data.frame(3, 4))) works
  #   rbind(data.frame(1, 2), data.frame(c(3, 4))) does not
  summary <- t(cbind(
    summarize_by_plate_and_well(screenData, mean, "mean"),
    summarize_by_plate_and_well(screenData, sd, "SD"),
    summarize_by_plate_and_well(screenData, cv, "CV"),
    summarize_by_plate(screenData, function(x) zprime(x, "fixsd"), "zprime"),
    summarize_by_plate(screenData, function(x) zprime(x, "robust"), "zprime_r"),
    screen_zprime = zprime(screenData, "fixsd"),
    screen_zprime_r = zprime(screenData, "robust"),
    analysis = PID
  ))

  rownames(summary) <- stringr::str_remove(rownames(summary), paste0(PID, "_"))

  return(summary)
}


## Helper functions to QCsummary
summarize_by_plate <- function(df, fun, col_suffix) {
  fun_split <- df %>%
    group_by(.data$Plate) %>%
    summarize(Readout = fun(.data)) %>%
    ungroup() %>%
    tibble::column_to_rownames("Plate") %>%
    t() %>%
    as.data.frame() %>%
    rename_with(function(x) paste(x, col_suffix, sep = "_"))

  return(fun_split)
}

summarize_by_plate_and_well <- function(df, fun, col_suffix) {
  fun_split <- df %>%
    group_by(.data$Plate, .data$WellType) %>%
    summarize(Readout = fun(.data$Readout), .groups = "drop") %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = "Plate", values_from = "Readout") %>%
    tibble::column_to_rownames("WellType") %>%
    rename_with(function(x) paste(x, col_suffix, sep = "_"))

  return(fun_split)
}

## Coefficient of variation in %
cv <- function(x) {
  100 * sd(x) / mean(x)
}

## z' between pos and neg
zprime <- function(x, method = c("fixsd", "robust")) {
  imageHTS::zprime(
    x$Readout[x$WellType == "pos"], x$Readout[x$WellType == "neg"],
    method = method
  )
}


## Helper functions for normalizations
normalize_by_plate <- function(df) {
  group_by(df, .data$Plate) %>%
    mutate(normVal = normalized_values(.data)) %>%
    ungroup()
}

## Normalized values using pos/neg medians
normalized_values <- function(x) {
  pos <- stats::median(x$Readout[x$WellType == "pos"])
  if (is.na(pos)) stop("No positive control wells found (or values are NA).")

  neg <- stats::median(x$Readout[x$WellType == "neg"])
  if (is.na(neg)) stop("No negative control wells found (or values are NA).")

  (x$Readout - pos) / (neg - pos)
}

## Wrappers around external functions
nplr_get_estimates <- function(...) {
  # Suppress nplr's "One (or more) of the values were
  # greater or equal to the estimated top asymptote"
  suppressWarnings(suppressMessages(nplr::getEstimates(...)))
}

rmarkdown_render <- function(rmd_stem, output_file) {
  rmarkdown::render(
    file.path("..", "rmd", paste0(rmd_stem, ".Rmd")),
    output_file = output_file,
    intermediates_dir = dirname(output_file),
    envir = parent.frame(),
    quiet = TRUE,
  )
}

## Create iTReX directories
iTReX.dirs <- function(project_dir, PID) {
  output_dir <- file.path(project_dir, PID)
  dirs <- c(
    file.path(project_dir, "Heatmap_data", c("QC", "mono", "combo")),
    file.path(output_dir, c("pre_process", "dose_response_curves"))
  )
  lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

  return(output_dir)
}


