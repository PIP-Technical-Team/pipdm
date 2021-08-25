pip_pipedir <- "//w1wbgencifs01/pip/pip_ingestion_pipeline/"
pipuax_default_options <- list(
  pip.dlwdir      = "//wbgfscifs01/GPWG-GMD/Datalib/GMD-DLW/Support/Support_2005_CPI/",
  pip.maindir     = "//w1wbgencifs01/pip/PIP-Data_QA/",
  pip.pipedir     = pip_pipedir,
  pip.cachedir.pc = paste0(pip_pipedir, "pc_data/cache/clean_survey_data/"),
  pip.cachedir.tb = paste0(pip_pipedir, "tb_data/cache/clean_survey_data/")
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(pipuax_default_options) %in% names(op))
  if (any(toset)) options(pipuax_default_options[toset])

  invisible()
}
