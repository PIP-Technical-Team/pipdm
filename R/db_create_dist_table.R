#' Create distributional statistics table
#'
#' @param dl list: A list with distributional statistics datasets.
#' @param survey_id character: A vector with survey ids.
#'
#' @return data.table
#' @export
db_create_dist_table <- function(dl, survey_id) {

  assertthat::assert_that(
    length(dl) == length(survey_id),
    msg = '`dl` and `survey_id` must be of equal lengths.' )

  # Create data frame
  dl <- purrr::map(dl, .f = function(x) do.call('rbind', x))
  df <- do.call('rbind', dl) %>% as.data.frame()
  df$pop_data_level <- sub('[.].*', '', row.names(df))

  df[1:5] <- purrr::map_df(
    df[1:5], function(x) unname(unlist(x)))
  df$quantiles <- df$quantiles %>% unname()
  df$survey_id <- survey_id
  row.names(df) <- NULL

  # Convert quantiles vectors to matrix
  deciles <-
    data.frame(
      matrix(unlist(df$quantiles),
             nrow = nrow(df),
             ncol = 10))
  names(deciles) <- paste0('decile', 1:10)
  df <- cbind(df, deciles)
  df$quantiles <- NULL

  # Convert to data table
  dt <- df %>%
    data.table::as.data.table()

  # Order columns
  data.table::setcolorder(dt, c('survey_id'))

  return(dt)
}
