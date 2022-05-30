source("../shinytest_helpers.R")
set_window()

app$setInputs(layout_and_readouts = "separate_files")
app$uploadFile(layout_table_file = demo_path("CRA_Layout.xlsx"))
app$uploadFile(readout_matrices_file = demo_path("CRA_1ReadoutXlsx_INF-R-1632.zip"))
app$setInputs(type_of_analysis = "StepA")

app$setInputs(iTReX = "QCN-mod")
app$setInputs(viewQCreport = "click", timeout_ = 1500e3)

tab2buttons <- c("MRA" = "nplranalyzeperreplica", "CRA" = "TCA")

for (module in names(tab2buttons)) {
  app$setInputs(iTReX = paste0(module, "-mod/Sample Screen"))

  args <- list("click", timeout_ = 300e3)
  names(args)[1] <- tab2buttons[module]
  do.call(app$setInputs, args)
  Sys.sleep(1)

  app$setInputs(iTReX = paste0(module, "-mod/Cohort Screen"), timeout_ = 10e3)
  app$snapshot(filename = paste0(module, "_01-Heatmap.json"))
}

shutdown()
