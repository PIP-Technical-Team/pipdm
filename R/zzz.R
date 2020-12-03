pipdm_default_options <- list(
  pip.dlwdir    = "//wbgfscifs01/GPWG-GMD/Datalib/GMD-DLW/Support/Support_2005_CPI/",
  pip.datadir   = "//w1wbgencifs01/pip/PIP-Data/",
  pip.pipedir   = "//w1wbgencifs01/pip/pip_ingestion_pipeline/",
  pip.dsmfile   = "//w1wbgencifs01/pip/pip_ingestion_pipeline/dsm/deflated_svy_means.fst",
  pip.inventory = "//w1wbgencifs01/pip/PIP-Data/_inventory/inventory.fst"
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(pipdm_default_options) %in% names(op))
  if (any(toset)) options(pipdm_default_options[toset])

  invisible()
}
