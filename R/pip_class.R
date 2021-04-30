as_pipmd <- function(x) {
  class(x) <- pipmd_class
  x
}

as_pipgd <- function(x) {
  class(x) <- pipmgd_class
  x
}

as_pipid <- function(x) {

  class(x) <- pipid_class
  x

}

pipmd_class <- c("pipdm", "data.table", "data.frame")
pipgd_class <- c("pipgd", "data.table", "data.frame")
pipid_class <- c("pipid", "data.table", "data.frame")

