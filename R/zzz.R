
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


pipdm_default_options <- list(
  pip.dm = ""
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(pipdm_default_options) %in% names(op))
  if (any(toset)) options(pipdm_default_options[toset])

  assign('gls', gls, envir = topenv())

  invisible()
}
