#' Process survey data to cache file
#'
#' @param survey_id character: Original Survey ID
#' @param cache_id  character: cache id vector
#' @param pip_data_dir character: Input folder for the raw survey data.
#' @param cols character: vector of variables to keep. Default is NULL.
#' @param cache_svy_dir character: Output directory
#' @param compress numeric: Compression level used in `fst::write_fst()`.
#' @param cpi_dt data frame with CPI data to deflate welfare in TB data
#' @param ppp_dt data frame with PPP data to deflate welfare in TB data
#'
#' @return data frame with status of process
#' @export
process_svy_data_to_cache <- function(survey_id,
                                      cache_id,
                                      pip_data_dir,
                                      cache_svy_dir,
                                      compress,
                                      cols = NULL,
                                      cpi_dt = NULL,
                                      ppp_dt = NULL) {


  #--------- Load data ---------
  chh_filename <- fifelse(
    grepl("\\.fst$", cache_id),
    cache_id,
    paste0(cache_id, ".fst")
  )

  df <- tryCatch(
    expr = {
      # Load data
      pipload::pip_load_data(
        survey_id = survey_id,
        maindir   = pip_data_dir,
        noisy     = FALSE
      )
    }, # end of expr section

    error = function(e) {
      NULL
    }, # end of error section

    warning = function(w) {
      NULL
    }
  ) # End of trycatch

  if (is.null(df)) {
    ret <- data.table(
      id = survey_id,
      status = "error loading"
    )
    return(ret)
  }

  #--------- Clean Data ---------

  df <- tryCatch(
    expr = {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Clean data   ---------
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Standard cleaning --------

      df <- db_clean_data(df)

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## additional variables --------

      # make sure the right welfare type is in the microdata.
      wt <- gsub("(.+_)([A-Z]{3})(_[A-Z\\-]+)(\\.fst)?$", "\\2", chh_filename)
      wt <- fifelse(wt == "INC", "income", "consumption")

      df[, welfare_type := wt]

      # add max data level variable
      dl_var <- grep("data_level", names(df), value = TRUE) # data_level vars

      ordered_level <- purrr::map_dbl(dl_var, ~ get_ordered_level(df, .x))
      select_var <- dl_var[which.max(ordered_level)]

      df[, max_domain := get(select_var)]

      data.table::setorder(df, max_domain)

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Deflate data --------
      ppp_dt <- ppp_dt[ppp_default == TRUE]

      # Merge survey table with PPP (left join)
      df <- joyn::merge(df, ppp_dt,
        by         = c("country_code", "ppp_data_level"),
        match_type = "m:1",
        yvars      = "ppp",
        keep       = "left",
        reportvar  = FALSE,
        verbose    = FALSE
      )

      # Merge survey table with CPI (left join)
      df <- joyn::merge(df, cpi_dt,
        by = c(
          "country_code", "survey_year",
          "survey_acronym", "cpi_data_level"
        ),
        match_type = "m:1",
        yvars = "cpi",
        keep = "left",
        reportvar = FALSE,
        verbose = FALSE
      )

      df[
        ,
        welfare_lcu := welfare
      ][
        ,
        welfare_ppp := wbpip::deflate_welfare_mean(
          welfare_mean = welfare_lcu,
          ppp          = ppp,
          cpi          = cpi
        )
      ]
    }, # end of expr section

    error = function(e) {
      NULL
    }, # end of error section

    warning = function(w) {
      NULL
    }
  ) # End of trycatch

  if (is.null(df)) {
    ret <- data.table(
      id = survey_id,
      status = "error cleaning"
    )
    return(ret)
  }

  #--------- Saving data ---------
  df <- tryCatch(
    expr = {
      # Your code...
      if (!is.null(cols)) {
        df <- df[, ..cols]
      }

      df[, cache_id := (cache_id)]

      # Create paths

      svy_out_path <- paste(cache_svy_dir, chh_filename, sep = "/")

      fst::write_fst(
        x = df,
        path = svy_out_path,
        compress = compress
      )
      TRUE
    }, # end of expr section

    error = function(e) {
      NULL
    }, # end of error section

    warning = function(w) {
      NULL
    }
  ) # End of trycatch


  if (is.null(df)) {
    ret <- data.table(
      id = survey_id,
      status = "error saving"
    )
    return(ret)
  }

  ret <- data.table(
    id = survey_id,
    status = "success"
  )

  return(ret)
}



#' get ordered level of data_level variables
#'
#' @param dt cleaned dataframe
#' @param x data_level variable name
#'
#' @return integer
#' @noRd
get_ordered_level <- function(dt, x) {
  x_level <- unique(dt[[x]])
  d1 <- c("national")
  d2 <- c("rural", "urban")

  if (identical(x_level, d1)) {
    1
  } else if (identical(x_level, d2)) {
    2
  } else {
    3
  }
}
