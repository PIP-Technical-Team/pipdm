#' Add pipmd class
#'
#' @param x data frame
#'
#' @return data frame with new class pipmd
#' @noRd
as_pipmd <- function(x) {
  class(x) <- pipmd_class
  x
}

#' Add pipgd class
#'
#' @param x data frame
#'
#' @return data frame with new class pipgd
#' @noRd
as_pipgd <- function(x) {
  class(x) <- pipgd_class
  x
}

#' Add pipid class
#'
#' @param x data frame
#'
#' @return data frame with new class pipid
#' @noRd
as_pipid <- function(x) {
  class(x) <- pipid_class
  x
}

pipmd_class <- c("pipmd", "data.table", "data.frame")
pipgd_class <- c("pipgd", "data.table", "data.frame")
pipid_class <- c("pipid", "data.table", "data.frame")
