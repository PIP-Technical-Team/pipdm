#' check_inputs_ref_years
#' @noRd
check_inputs_ref_years <- function(x) {
  if (!is.numeric(x)) {
    rlang::abort(c(
      "`ref_years` must be a numeric or integer vector: ",
      sprintf("You've supplied a %s vector.", class(x))
    ))
  }
}

#' check_inputs_pip_years
#' @noRd
check_inputs_pip_years <- function(x) {
  if (!is.numeric(x)) {
    rlang::abort(c(
      "`pip_years` must be a numeric or integer vector: ",
      sprintf("You've supplied a %s vector.", class(x))
    ))
  }
}

#' check_inputs_db_class
#' @noRd
check_inputs_db_class <- function(dt) {
  if (!any(class(dt) %in% "data.table")) {
    rlang::abort("`dt` must be of class `data.table`.")
  }
}

#' md_clean_data
#' Copied from wbpip to avoid notes in R CMD CHECK.
#' @noRd
md_clean_data <-
  utils::getFromNamespace("md_clean_data", "wbpip")

#' gd_clean_data
#' Copied from wbpip to avoid notes in R CMD CHECK.
#' @noRd
gd_clean_data <-
  utils::getFromNamespace("gd_clean_data", "wbpip")

#' md_compute_dist_stats
#' Copied from wbpip to avoid notes in R CMD CHECK.
#' @noRd
md_compute_dist_stats <-
  utils::getFromNamespace("md_compute_dist_stats", "wbpip")

#' gd_compute_dist_stats
#' Copied from wbpip to avoid notes in R CMD CHECK.
#' @noRd
gd_compute_dist_stats <-
  utils::getFromNamespace("gd_compute_dist_stats", "wbpip")

#' md_compute_lorenz
#' Copied from wbpip to avoid notes in R CMD CHECK.
#' @noRd
md_compute_lorenz <-
  utils::getFromNamespace("md_compute_lorenz", "wbpip")

#' compute_predicted_mean
#' Copied from wbpip to avoid notes in R CMD CHECK.
#' @noRd
compute_predicted_mean <-
  utils::getFromNamespace("compute_predicted_mean", "wbpip")
