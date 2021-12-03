source("../shinytest_helpers.R")
set_window()

app$setInputs(number_of_samples = "cohort")
app$uploadFile(layout_table_file = demo_path("MRA_Layout.xlsx"))
app$uploadFile(readout_matrices_file = demo_path("MRA_2ReadoutsXlsx.zip"))
app$setInputs(start_oca = "click", timeout_ = 300e3)
app$setInputs(iTReX = "MRA-mod/Cohort Screen")
app$snapshot(filename = "01_Heatmap.json")
