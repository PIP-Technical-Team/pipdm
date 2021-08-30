
# defined values
root_dir  <-  Sys.getenv("PIP_ROOT_DIR")
# root_dir  = Sys.getenv("PIP_ROOT_DIRfff")

if (root_dir != "") {
  # globals
  gls <- pipload::pip_create_globals(root_dir)
} else {
  delayedAssign("gls", pipload::pip_create_globals(root_dir))
}

# if you don't the official value in `Sys.getenv("PIP_ROOT_DIR")` you can
# provide the object `root_dir  <- "<you directory>"` before executing the first
# fucntion pipaux. In this way, object `gls`, which is a promise, will be
# created using with you `root_dir`. Otherwise, you can especify the complete
# directory path for each function.



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
