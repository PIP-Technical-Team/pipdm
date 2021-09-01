

pipdm_default_options <- list(
  pip.dm = ""
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(pipdm_default_options) %in% names(op))
  if (any(toset)) options(pipdm_default_options[toset])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## defined values --------
  # if you don't the official value in `Sys.getenv("PIP_ROOT_DIR")` you can
  # provide the object `root_dir  <- "<you directory>"` before executing the first
  # fucntion pipaux. In this way, object `gls`, which is a promise, will be
  # created using with you `root_dir`. Otherwise, you can especify the complete
  # directory path for each function.


  # current objects
  obj <-  ls(pos = ".GlobalEnv")

  # remove gls it  exists
  rm(list = obj[obj %in% c("gls")], pos = ".GlobalEnv")

  # If root_dir does not exist, create it
  if (!("root_dir" %in% obj)) {
    root_dir  <-  Sys.getenv("PIP_ROOT_DIR")
    # root_dir  <-  Sys.getenv("PIP_ROOT_DIRfff")

    # assign('root_dir', root_dir, envir = globalenv())

  } else {
    cli::cli_alert("object {.envvar root_dir} is already defined in
                   Global env to  {.field {root_dir}}. To get back to default
                   values, make sure you remove it from memory by typing
                   {.code rm(root_dir)}",
                   wrap = TRUE)
  }

  # create promises and assign to global env
  if (root_dir != "") {
    # globals
    gls <- pipload::pip_create_globals(root_dir)
    assign('gls', gls, envir = globalenv())

  } else {

    delayedAssign("gls",
                  pipload::pip_create_globals(root_dir),
                  assign.env =  globalenv(),
                  eval.env = globalenv())
  }



  invisible()
}
