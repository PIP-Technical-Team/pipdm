

pipdm_default_options <- list(
  pipdm.verbose = TRUE
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(pipdm_default_options) %in% names(op))
  if (any(toset)) options(pipdm_default_options[toset])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## defined values --------
  pipload::add_gls_to_env()

  invisible()
}
