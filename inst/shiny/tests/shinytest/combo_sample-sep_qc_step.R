source("../shinytest_helpers.R")
set_window()

app$setInputs(layout_and_readouts = "separate_files")
app$uploadFile(layout_table_file = demo_path("CRA_Layout.xlsx"))
app$uploadFile(readout_matrices_file = demo_path("CRA_1ReadoutXlsx_INF-R-1632.zip"))
app$setInputs(type_of_analysis = "StepA")
app$snapshot(filename = "00_Home.json")

app$setInputs(iTReX = "QCN-mod")
app$setInputs(viewQCreport = "click", timeout_ = 300e3)

last_i <- get_tab_shapshots("QC_mod")

number <- formatC(last_i + 1, width = 2, flag = "0")
app$snapshotDownload("report", filename = paste0(number, "_Report.html"))
