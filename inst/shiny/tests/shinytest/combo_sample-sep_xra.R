source("../shinytest_helpers.R")
set_window()

app$setInputs(layout_and_readouts = "separate_files")
app$uploadFile(layout_table_file = demo_path("CRA_Layout.xlsx"))
app$uploadFile(readout_matrices_file = demo_path("CRA_1ReadoutXlsx_INF-R-1632.zip"))
app$setInputs(start_oca = "click", timeout_ = 1500e3)

for (module in c("MRA", "CRA")) {
  app$setInputs(iTReX = paste0(module, "-mod/Sample Screen"))

  args <- list("click", timeout_ = 300e3)
  names(args)[1] <- paste0("vis_", module)
  do.call(app$setInputs, args)
  Sys.sleep(1)
  app$snapshot(filename = paste0(module, "_00_SampleScreen.json"))

  get_tab_snapshots(paste0(module, "_mod"))
}

shutdown()
