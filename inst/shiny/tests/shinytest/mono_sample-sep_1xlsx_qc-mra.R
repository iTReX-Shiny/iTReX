source("../shinytest_helpers.R")
set_window()

app$setInputs(layout_and_readouts = "separate_files")
app$uploadFile(layout_table_file = demo_path("MRA_Layout-Imaging-1Plate_ST06.xlsx"))
app$uploadFile(readout_matrices_file = demo_path("MRA_Readout-Imaging_BT-40-V3-DS1_ST05.xlsx"))
app$setInputs(start_oca = "click", timeout_ = 300e3)

app$setInputs(iTReX = "QCN-mod")
app$setInputs(vis_QCN = "click", timeout_ = 60e3)

last_i <- get_tab_shapshots("QC_mod")

app$setInputs(iTReX = "MRA-mod/Sample Screen")
app$setInputs(vis_MRA = "click", timeout_ = 60e3)

get_tab_shapshots("MRA_mod", last_i = last_i)
