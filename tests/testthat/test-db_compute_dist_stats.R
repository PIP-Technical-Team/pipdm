library(data.table)

gdm       <- pipload::pip_load_aux("gdm")
pop       <- pipload::pip_load_aux("pop")
cch       <- pipload::pip_load_cache_inventory()

get_mean <- function(gdm, survey_id) {
  dtm <- gdm[survey_id == get("survey_id", envir = -2)]

  mean        <- dtm[, survey_mean_lcu]
  names(mean) <- dtm[, pop_data_level]
  return(mean)
}

test_that("national microdata", {
  dt        <- pipload::pip_load_cache("PRY", 2017)
  pipload::pip_load_data("PRY", 2017, tool = "pc")
  pipload::pip_load_data("CHN", 2015, tool = "pc")

  PHL_2018_FIES_D1_CON_GPWG
  names(pipload::pip_load_cache("PHL", 2018, welfare_type = "CON"))


  cache_id  <- dt[, unique(cache_id)]
  survey_id <- cch[cache_id == get("cache_id", envir = -2)
                   ][, survey_id]

  mean <- dt[, collapse::fmean(welfare, w = weight)]
  names(mean) <- "national"
  debugonce(compute_dist_stats)
  db_compute_dist_stats(dt, mean, pop, cache_id)

})
