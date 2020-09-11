#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter select group_by ungroup
NULL

# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= '2.15.1')
  utils::globalVariables(
    c('survey_year', 'is_ref_year', 'n_per_data_type', 'both_sides',
      'dist_from_ref_year', 'data_type')
  )

#' Get closest surveys to a reference year
#'
#' For any reference year, retrieves information about the closest surveys.
#'
#' @param svy_lkup data.frame: Look-up table for survey data.
#' @param ref_year numeric: The selected reference year.
#'
#' @return `data.frame`
#'
#' @keywords internal
get_closest_surveys <- function(svy_lkup, ref_year) {

  n <- nrow(svy_lkup)

  # Case 1: No survey found
  if (n == 0) {
    out <- svy_lkup
    return(out)
  }

  # Order table by survey year
  out <- svy_lkup[order(svy_lkup$survey_year), ]

  # Find interval
  i <- findInterval(ref_year, out[['survey_year']])

  # Case 2: reference year == survey year
  if (ref_year %in% out[['survey_year']]) {
    return(out[out[['survey_year']] == ref_year, ])
  }

  # Case 3: One-side - ref_year < min(survey_year)
  if (i == 0) {
    return(out[i + 1,])

    # Case 4: One-side - ref_year < max(survey_year)
  } else if (i == n) {
    return(out[i,])

    # Case 5: Both-side - min(survey_year) < ref_year < max(survey_year)
  } else {
    return(out[c(i, i + 1),])
  }
}

#' Select a single line-up survey
#'
#' Prioritize surveys to be used for the line-up. This is needed when multiple
#' choices of surveys are available. For instance, if two surveys are available
#' for the same year, one using consumption, the other one income.
#'
#' @param df data.frame: Subset of reference_year table.
#' @param ref_year numeric: Reference year.
#'
#' @return `data.frame`
#'
#' @keywords internal
select_lineup_survey <- function(df, ref_year) {

  # Return as is if there is only a single survey to choose from
  n_svy <- nrow(df)
  if (n_svy == 1) {return(as.data.frame(df))}
  if (length(unique(df[['data_type']])) == 1) {return(as.data.frame(df))}

  # Deal with cases where survey_year == ref_year
  tmp <- df %>%
    dplyr::mutate(
      is_ref_year = ref_year == survey_year
    ) %>%
    dplyr::filter(is_ref_year == TRUE) %>%
    dplyr::select(-is_ref_year) %>%
    as.data.frame()

  if (nrow(tmp) > 1) {return(tmp[tmp[['data_type']] == 'C', ])}
  if (nrow(tmp) == 1) {return(tmp)}

  # Deal with cases with at least one instance of both-sides surveys
  tmp <- df %>%
    dplyr::group_by(data_type) %>%
    dplyr::mutate(
      n_per_data_type = dplyr::n(),
      both_sides = length(n_per_data_type) == 2
    ) %>%
    dplyr::filter(both_sides == TRUE) %>%
    dplyr::select(-n_per_data_type, -both_sides) %>%
    as.data.frame()

  if (length(unique(tmp[['data_type']])) > 1) {return(tmp[tmp[['data_type']] == 'C', ]) }
  if (length(unique(tmp[['data_type']])) == 1) {return(tmp)}

  # Deal with cases with only one survey of each datatype
  # Choose survey closest to the ref_year
  if (nrow(df) == 2 & length(unique(df[['data_type']])) > 1) {
    tmp <- df %>%
      dplyr::group_by(data_type) %>%
      dplyr::mutate(
        dist_from_ref_year = abs(ref_year - survey_year)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(dist_from_ref_year == min(dist_from_ref_year, na.rm = TRUE)) %>%
      dplyr::select(-dist_from_ref_year) %>%
      as.data.frame()
    if (nrow(tmp) == 1) {
      return(tmp)
    } else {
      return(tmp[tmp[['data_type']] == 'C', ])
    }
  }

  # AE: No need to return anything unless above conditions are triggered?
  # return(df)

}


#' Get welfare adjustment method
#'
#' Find the correct extrapolation or interpolation method to use in
#' [adjust_welfare_mean()].
#'
#' @param svy_table data.frame: A table with information on surveys and
#'   national accounts data.
#' @param ref_year numeric: A value with the reference year.
#' @param ref_gdp numeric: GDP value for the reference year.
#' @param ref_pce numeric: PCE value for the reference year.
#'
#' @return numeric
#' @keywords internal
get_welfare_adjustment_method <- function(svy_table, ref_year, ref_gdp, ref_pce) {

  if (is.na(svy_table[['svy_year_mean']][1])) {
    return('missing_mean_a')
  }

  # Add CHECK for ref_year == survey_year
  if (ref_year %in% svy_table[['survey_year']]) {
    return('one_point_same_as_survey_year')
  }

  sides <- get_survey_side(svy_table)

  if (sides == 'one-side') {
    if (is_one_point_adjusted_with_pce(svy_table = svy_table,
                                       ref_pce = ref_pce)) {
      return('one_point_adjusted_with_pce')
    } else if (is_one_point_adjusted_with_gdp(svy_table = svy_table,
                                              ref_gdp = ref_gdp)) {
      return('one_point_adjusted_with_gdp')
    } else if (is.na(svy_table[['adjusted_svy_pce']])) {
      return('missing_pce_a')
    } else if (is.na(svy_table[['adjusted_svy_gdp']])) {
      return('missing_gdp_a')
    }

  } else if (sides == 'both-sides') {
    if (is.na(svy_table[['svy_year_mean']][2])) {
      return('missing_mean_b')
    } else if (is_non_monotonic_adjusted_with_pce(svy_table,
                                                  ref_pce)) {
      return('non_monotonic_adjusted_with_pce')
    } else if (is_same_direction_interpolated_with_pce(svy_table,
                                                       ref_pce)) {
      return('same_direction_interpolated_with_pce')
    } else if (is_non_monotonic_adjusted_with_gdp(svy_table,
                                                  ref_gdp)) {
      return('non_monotonic_adjusted_with_gdp')
    } else if (is_same_direction_interpolated_with_gdp(svy_table,
                                                       ref_gdp)) {
      return('same_direction_interpolated_with_gdp')
    } else {
      if (is.na(svy_table[['adjusted_svy_pce']][1])) {
        return('missing_pce_a')
      }
      if (is.na(svy_table[['adjusted_svy_pce']][2])) {
        return('missing_pce_b')
      }
      if (is.na(svy_table[['adjusted_svy_gdp']][1])) {
        return('missing_gdp_a')
      }
      if (is.na(svy_table[['adjusted_svy_gdp']][2])) {
        return('missing_gdp_b')
      }
    }
  }
}

#' get_survey_side
#' @noRd
get_survey_side <- function(df) {
  n <- nrow(df)
  # Add case when survey_year == ref_year
  if (n == 1) {
    return('one-side')
  } else if (n == 2) {
    return('both-sides')
  } else {
    return(NA)
  }
}

#' is_monotonic
#' @noRd
is_monotonic <- function(x1, x2, r) {((r - x1) * (x2 - r)) > 0}

#' is_same_direction
#' @noRd
is_same_direction <- function(x, y) {(x[2] - x[1]) * (y[2] - y[1]) > 0}

#' is_one_point_adjusted_with_pce
#' @noRd
is_one_point_adjusted_with_pce <- function(svy_table, ref_pce,
                                           pce_col = 'adjusted_svy_pce',
                                           region_col = 'wb_region_code') {
  if (!is.na(ref_pce) &
      !is.na(svy_table[[pce_col]]) &
      svy_table[[region_col]] != 'SSA' ) { # 'SSA to be changed to package level variable
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' is_one_point_adjusted_with_gdp
#' @noRd
is_one_point_adjusted_with_gdp <- function(svy_table, ref_gdp, gdp_col = 'adjusted_svy_gdp') {
  if (!is.na(ref_gdp) & !is.na(svy_table[[gdp_col]])) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' is_non_monotonic_adjusted_with_pce
#' @noRd
is_non_monotonic_adjusted_with_pce <- function(svy_table, ref_pce = NA,
                                               pce_col = 'adjusted_svy_pce',
                                               mean_col = 'svy_year_mean',
                                               region_col = 'wb_region_code') {
  sides <- get_survey_side(svy_table)
  if (sides != 'both-sides') {return(FALSE)}

  if (!is.na(ref_pce) &  all(!is.na(svy_table[[pce_col]])) & all(svy_table[[region_col]] != 'SSA' )) {
    if (is_monotonic(x1 = svy_table[[pce_col]][1], x2 = svy_table[[pce_col]][2],r = ref_pce)) {
      if (!is_same_direction(x = svy_table[[pce_col]], y = svy_table[[mean_col]])) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(TRUE)
    }
  }
  else {
    return(FALSE)
  }
}

#' is_same_direction_interpolated_with_pce
#' @noRd
is_same_direction_interpolated_with_pce <- function(svy_table, ref_pce,
                                                    pce_col = 'adjusted_svy_pce',
                                                    mean_col = 'svy_year_mean',
                                                    region_col = 'wb_region_code') {
  sides <- get_survey_side(svy_table)
  if (sides != 'both-sides') {return(FALSE)}

  if (!is.na(ref_pce) &
      all(!is.na(svy_table[[pce_col]])) &
      all(svy_table[[region_col]] != 'SSA' )) {
    if (is_monotonic(x1 = svy_table[[pce_col]][1],
                     x2 = svy_table[[pce_col]][2],
                     r = ref_pce)) {
      if (is_same_direction(x = svy_table[[pce_col]],
                            y = svy_table[[mean_col]])) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  }
  else {
    return(FALSE)
  }
}

#' is_non_monotonic_adjusted_with_gdp
#' @noRd
is_non_monotonic_adjusted_with_gdp <- function(svy_table, ref_gdp,
                                               gdp_col = 'adjusted_svy_gdp',
                                               mean_col = 'svy_year_mean',
                                               region_col = 'wb_region_code') {
  sides <- get_survey_side(svy_table)
  if (sides != 'both-sides') {return(FALSE)}

  if (!is.na(ref_gdp) &
      all(!is.na(svy_table[[gdp_col]]))) {
    if (is_monotonic(x1 = svy_table[[gdp_col]][1],
                     x2 = svy_table[[gdp_col]][2],
                     r = ref_gdp)) {
      if (!is_same_direction(x = svy_table[[gdp_col]],
                             y = svy_table[[mean_col]])) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(TRUE)
    }
  }
  else {
    return(FALSE)
  }
}

#' is_same_direction_interpolated_with_gdp
#' @noRd
is_same_direction_interpolated_with_gdp <- function(svy_table, ref_gdp,
                                                    gdp_col = 'adjusted_svy_gdp',
                                                    mean_col = 'svy_year_mean',
                                                    region_col = 'wb_region_code') {
  sides <- get_survey_side(svy_table)
  if (sides != 'both-sides') {return(FALSE)}

  if (!is.na(ref_gdp) &
      all(!is.na(svy_table[[gdp_col]]))) {
    if (is_monotonic(x1 = svy_table[[gdp_col]][1],
                     x2 = svy_table[[gdp_col]][2],
                     r = ref_gdp)) {
      if (is_same_direction(x = svy_table[[gdp_col]],
                            y = svy_table[[mean_col]])) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  }
  else {
    return(FALSE)
  }
}



