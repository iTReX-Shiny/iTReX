source("../shinytest_helpers.R")
set_window()

app$uploadFile(layout_table_file = demo_path("MRA_LayoutAndReadouts_INF-R-153_ST12.xlsx"))
app$setInputs(upload_healthy_controls = TRUE)
app$uploadFile(healthy_controls_file = demo_path("Controls_DKFZ_ST10-min.xlsx"))
app$setInputs(type_of_analysis = "StepA")

app$setInputs(iTReX = "QCN-mod")
app$setInputs(viewQCreport = "click", timeout_ = 300e3)

app$setInputs(iTReX = "MRA-mod/Sample Screen")
app$setInputs(nplranalyzeperreplica = "click", timeout_ = 60e3)
Sys.sleep(1)
app$snapshot(filename = "MRA_00_SampleScreen.json")

get_tab_shapshots("MRA_mod", with_downloads = TRUE)
