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

#' convert variagles with unique values along the data set to attrbitus and then
#' remove those unique variables
#'
#' @param x data frame.
#' @param ex_pattern character: variable name pattern to exclude from
#'   single-value variables, so that they are not converted to attributes
#'
#' @return
#' @export
uniq_vars_to_attr <- function(x, ex_pattern = NULL) {

  if (!data.table::is.data.table(x)) {
    x <- as.data.table(x)
  } else {
    x <- data.table::copy(x)
  }

  N_vars   <- x[, lapply(.SD, uniqueN)]

  # if no pattern is selected
  if (is.null(ex_pattern)) {

    uni_vars <- names(N_vars)[N_vars == 1]
    mul_vars <- names(N_vars)[N_vars != 1]

  } else {

    uni_vars <- names(N_vars)[N_vars == 1 & !grepl(ex_pattern, names(N_vars))]
    mul_vars <- names(N_vars)[N_vars != 1 | grepl(ex_pattern, names(N_vars))]

  }


  for (i in seq_along(uni_vars)) {

    var <- uni_vars[i]
    value <- x[, unique(get(var))]
    attr(x, var) <- value

  }

  x <- x[, ..mul_vars]

  return(x)

}




#' Add attributes to dataset from single-value variables in another dataset.
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
add_simple_attr <- function(x, y) {

  if (!data.table::is.data.table(x)) {
    x <- as.data.table(x)
  } else {
    x <- data.table::copy(x)
  }

  vars <- names(y)

  for (i in seq_along(vars)) {

    var <- vars[i]
    value <- y[, get(var)]
    attr(x, var) <- value
  } # end of loop

  return(x)

} # end of function
