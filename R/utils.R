#' Create boolean column for surveys to include in the reference year table creation
#'
#' @param dt data.table A table with survey metadata information.
#' @return data.table
#' @seealso `db_create_dsm_table()`
#' @noRd
create_line_up_check <- function(dt){

  assertthat::assert_that(all(c("cache_id", "country_code", "reporting_level") %in% names(dt)))

  # Countries wo/ any national (reporting level) surveys
  cc <- check_no_national_survey(dt)

  # Create number of rows per cache_id
  dt[, n_rl := .N, by = cache_id]

  # Create check
  check <- (dt$reporting_level == "national" & dt$n_rl == 1) |  # Surveys w/ national reporting level and no split by U/R domain (e.g USA)
    (dt$reporting_level %in% c("urban", "rural") & dt$n_rl == 2) | # Surveys split by U/R domain (e.g. CHN, IND)
    dt$country_code %in% cc  # Countries wo/ any national surveys (e.g. ARG, SUR)

  # Add is_used_for_line_up
  dt$is_used_for_line_up <- ifelse(check, TRUE, FALSE)
  dt$n_rl <- NULL

  return(dt)
}

#' Find countries without national coverage
#'
#' @param df data.frame: A table with survey metadata information.
#'
#' @return character
#' @seealso `db_create_lkup_table()`
#' @noRd
check_no_national_survey <- function(df) {
  tmp <- table(df$country_code, df$reporting_level)
  tmp <- as.data.frame(tmp, stringsAsFactors = FALSE)
  names(tmp) <- c("country_code", "reporting_level", "freq")
  cc <- tmp[(tmp$reporting_level == "national" & tmp$freq == 0), ][["country_code"]]
  if (is.null(cc)) cc <- ""
  return(cc)
}

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

#' convert variables with unique values along the data set to attributes and
#' then remove those unique variables
#'
#' @param x data frame.
#'
#' @return same data.table without variables iwth unique values. Those variables
#'   become attributes of the data
#' @export
uniq_vars_to_attr <- function(x) {

  if (!data.table::is.data.table(x)) {
    x <- as.data.table(x)
  }

  N_vars   <- x[, lapply(.SD, uniqueN)]
  uni_vars <- names(N_vars)[N_vars == 1]
  mul_vars <- names(N_vars)[N_vars != 1]

  for (i in seq_along(uni_vars)) {

    var <- uni_vars[i]
    value <- x[, unique(get(var))]
    attr(x, var) <- value

  }

  x <- x[, ..mul_vars]

  return(x)

}


#' FIlter data.frame and convert it to list
#'
#' @param dt_ data.frame
#' @param condition condition in `i`  as in  `DT[i,j]`
#'
#' @return list
#' @export
#'
#' @examples
#' library(data.table)
#' dt <- data.table(x = c("a", "a", "b", "c", "c"),
#'  y = 1:5)
#'
#' filter2list(dt, y > 3)
filter2list <- function(dt_,...) {

  l <- list(...)
  cn <-
    purrr::imap(l,
                \(x, ix) {
                  if (is.character(x)) {
                    x <- shQuote(x)
                  }
                  left <- paste(ix, "==")
                  cnx  <- paste(left, x, collapse = " | ")
                  cnx  <- paste0("(", cnx, ")")
                }) |>
    paste(collapse = " & ") |>
    parse(text = _)

  dt_ |>
    fsubset(eval(cn)) |>
    as.list()
}




#' get weights from survey year to reference year
#'
#' @param ref_year numeric: reference year
#' @param svy_years numeric: survey years. Length either 1 or 2
#'
#' @return numeric vector with both weights that sum up to 1
#' @export
distance_weight <- function(ref_year, svy_years) {

  # early return
  l_sy <- length(svy_years)
  if (l_sy == 1) {
    return(1)
  }

  # check reference year is unique
  ref_year  <- unique(ref_year)
  stopifnot(exprs = {
    length(ref_year) == 1
    l_sy %% 2 == 0
  })

  x <- matrix(svy_years, nrow = 2)

  wt <- vector(mode   = "numeric",
               length = l_sy)

  a <- c(1,2)
  for (i in seq_along(x[1, ])) {
    svy_year1 <- x[1, i]
    svy_year2 <- x[2, i]

    weight1 <- (svy_year2 - ref_year)/(svy_year2 - svy_year1)
    weight2 <- 1 - weight1
    wt[a] <- c(weight1, weight2)
    a <-  a + 2
  }

  return(wt)

}








