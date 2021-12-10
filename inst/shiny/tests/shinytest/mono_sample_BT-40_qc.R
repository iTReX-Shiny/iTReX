source("../shinytest_helpers.R")
set_window()

app$uploadFile(layout_table_file = demo_path("MRA_LayoutAndReadouts_BT-40_ST04.xlsx"))
app$setInputs(upload_reference_samples = TRUE)
app$uploadFile(reference_samples_file = demo_path("Controls_DKFZ_ST10-min.xlsx"))
app$setInputs(start_oca = "click", timeout_ = 300e3)

app$setInputs(iTReX = "QCN-mod")
app$setInputs(vis_QCN = "click", timeout_ = 60e3)

get_tab_shapshots("QC_mod")
