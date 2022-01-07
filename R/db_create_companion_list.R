#' Create companion list of all information relavan for each survey data
#'
#' @param df
#' @param cpi_table
#' @param ppp_table
#' @param pfw_table
#' @param pop_table
#'
#' @return
#' @export
#'
#' @examples
db_create_companion_list <- function(df,
                                     cpi_table  = pipload::pip_load_aux("cpi"),
                                     ppp_table  = pipload::pip_load_aux("ppp"),
                                     pfw_table  = pipload::pip_load_aux("pfw"),
                                     pop_table  = pipload::pip_load_aux("pop")) {
  pipload::pip_load_all_aux(replace = TRUE)


  df <- pipload::pip_load_data("PRY", year = 2015)
  df <- db_clean_data(df)
  ld <- vector(mode = "list")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## add PFW info --------

  byvar <- c( "country_code", "surveyid_year", "survey_acronym")

  df <-   joyn::merge(df, pfw,
                      by          = byvar,
                      match_type  = "m:1",
                      keep        = "left",
                      reportvar   = FALSE)

  # get single-value variables as attributes

  ex_pattern <- "data_level"
  df      <- uniq_vars_to_attr(df, ex_pattern)
  df_attr <- attributes(df)

  # Add welfare data and attributes
  ld$df      <- df
  ld$df_attr <- df_attr


  c_c <- attr(df, "country_code")  # Country code
  s_y <- attr(df, "survey_year")  # survey_year
  s_a <- attr(df, "survey_acronym")  # survey_year


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Join CPI --------

  cpi_table <-
    cpi[
      country_code     == c_c
      & survey_year    == s_y
      & survey_acronym == s_a
    ][, .SD,
      .SDcols =c("cpi_data_level",
                 "cpi"
      )
    ]

  # add CPI data
  ld$cpi <- cpi_table

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Join PPP --------

  lppp       <- db_ppp_list(ppp)
  lppp_names <- names(lppp)


  ppp_ld <- purrr::map(.x = lppp_names,
                       .f = ~{

                         x <- lppp[[.x]]

                         y  <-
                           x[country_code == c_c,
                             .(ppp = unique(ppp)),
                             by = ppp_data_level]

                         ppp_attr <-
                           c("ppp_default",
                             "ppp_default_by_year")


                         for (i in seq_along(ppp_attr)) {
                           at <- ppp_attr[[i]]
                           attr(y, at) <- attr(x, at)
                         }

                         return(y)
                       })

  names(ppp_ld) <- lppp_names

  # Add PPP values
  ld$ppp <- ppp_ld


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## population table --------



  return(ld)
} # end of db_create_companion_list function
