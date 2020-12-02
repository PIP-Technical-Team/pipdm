pipdm_default_options <- list(
  pip.dlwdir  = "//wbgfscifs01/GPWG-GMD/Datalib/GMD-DLW/Support/Support_2005_CPI/",
  pip.datadir = "//w1wbgencifs01/pip/PIP-Data/",
  pip.pipedir = "//w1wbgencifs01/pip/pip_ingestion_pipeline/"
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(pipdm_default_options) %in% names(op))
  if (any(toset)) options(pipdm_default_options[toset])

  invisible()
}
