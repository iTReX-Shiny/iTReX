source("../shinytest_helpers.R")
set_window()

app$setInputs(number_of_samples = "cohort")
app$uploadFile(layout_table_file = demo_path("MRA_Layout-Imaging-3Plates.xlsx"))
app$uploadFile(readout_matrices_file = demo_path("MRA_3ReadoutsMixed.zip"))
app$setInputs(upload_reference_samples = TRUE)
app$uploadFile(reference_samples_file = demo_path("Controls_DKFZ_ST10-min.xlsx"))
app$setInputs(start_oca = "click", timeout_ = 300e3)

for (variant in c("NA", "Full")) {
  sample <- paste0("BT-40_V1_DS1_", variant)
  prefix <- paste0(variant, "_")

  app$setInputs(iTReX = "QCN-mod")
  app$setInputs(sample = sample)
  app$setInputs(vis_QCN = "click", timeout_ = 60e3)

  last_i <- get_tab_shapshots("QC_mod", prefix = prefix)

  app$setInputs(iTReX = "MRA-mod/Sample Screen")
  app$setInputs(sample_MRA = sample)
  app$setInputs(vis_MRA = "click", timeout_ = 60e3)

  get_tab_shapshots("MRA_mod", prefix = prefix, last_i = last_i)
}

app$setInputs(iTReX = "MRA-mod/Cohort Screen")
app$snapshot(filename = "13_Heatmap.json")
