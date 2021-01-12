#' Create distributional statistics table
#'
#' @param dl list: A list with distributional statistics datasets.
#'
#' @return data.table
#' @export
db_create_dist_table <- function(dl) {

  # Create data frame
  df <- do.call('rbind', dl) %>%
    as.data.frame()
  df[1:5] <- purrr::map_df(
    df[1:5], function(x) unname(unlist(x)))
  df$quantiles <- df$quantiles %>% unname()
  df$survey_id <- row.names(df)
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
