source("../shinytest_helpers.R")
set_window()

app$setInputs(number_of_samples = "cohort")
app$uploadFile(layout_table_file = demo_path("MRA_Layout.xlsx"))
app$uploadFile(readout_matrices_file = demo_path("MRA_4ReadoutsXlsx.zip"))
app$setInputs(upload_reference_samples = TRUE)
app$uploadFile(reference_samples_file = demo_path("Controls_DKFZ_ST10-min.xlsx"))
app$setInputs(start_oca = "click", timeout_ = 1500e3)
app$setInputs(iTReX = "MRA-mod/Cohort Screen", timeout_ = 10e3)
app$snapshot(filename = "01_Heatmap_DSS.json")

app$setInputs(MRA_sdss = TRUE, timeout_ = 300e3)
app$snapshot(filename = "02_Heatmap_sDSS.json")

app$setInputs(MRA_filter_zp = TRUE, timeout_ = 300e3)
app$snapshot(filename = "03-Heatmap-sDSS-filter.json")

app$setInputs(MRA_threshold_zp = 0.6, timeout_ = 300e3)
app$snapshot(filename = "04-Heatmap-sDSS-threshold.json")

app$setInputs(MRA_filter_mode_zp = "any", timeout_ = 300e3)
app$snapshot(filename = "05-Heatmap-mode.json")

shutdown()
