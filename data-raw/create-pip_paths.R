# Create PIP paths
pip_paths <- list(
  dlwdir    = "//wbgfscifs01/GPWG-GMD/Datalib/GMD-DLW/Support/Support_2005_CPI/",
  production_datadir   = "//w1wbgencifs01/pip/PIP-Data/",
  production_pipedir   = "//w1wbgencifs01/pip/pip_ingestion_pipeline/",
  staging_datadir  = "//w1wbgencifs01/pip/PIP-Data_QA/",
  testing_datadir  = "//w1wbgencifs01/pip/PIP-Data_Testing/",
  dsmfile   = "//w1wbgencifs01/pip/pip_ingestion_pipeline/dsm/deflated_svy_means.fst"
  #inventory = "//w1wbgencifs01/pip/PIP-Data/_inventory/inventory.fst"
)

usethis::use_data(pip_paths, overwrite = TRUE)
