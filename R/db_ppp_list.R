#' Create generic list of PPP version according the ppp year, release version
#' and adaptation version
#'
#' @param ppp data frame with ppp value. must have the same strcuture as in
#'   `pipload::pip_load_aux("ppp")`
#'
#' @return list of ppp data frames
#' @export
db_ppp_list <- function(ppp) {

  pt <- ppp[, unique(ppp_year)] # ppp_year


  y <- purrr::map(pt, ~{
    ppp[ppp_year == .x]
  })



  z <- purrr::map(y, ~{
    rv <- .x[, unique(release_version)] # release version
    av <- .x[, unique(adaptation_version)] # adaptation version


    va <- tidyr::expand_grid(rv, av) # versions available
    data.table::setDT(va)

    dd <- purrr::pmap(.l = va,
                      .f = ppp_filter,
                      ppp = .x)

    names(dd) <- va[,
                    paste(rv, "M", av, "A", sep = "_")]

    dd <-  purrr::compact(dd)

    return(dd)

  })

  names(z) <- paste0("y", pt)


  return(z)

}


#' Filter PPP data from using the structure of `pipload::pip_load_aux("ppp")`
#'
#' @inheritParams db_ppp_list
#' @param rv character: Release Version in the form "vX" where "X" refers to the
#'   version number
#' @param av character: Adaptation Version in the form "vX" where "X" refers to
#'   the version number
#'
#' @return data frmae
ppp_filter <- function(ppp, rv, av) {
  x <-
    ppp[release_version == rv
        & adaptation_version == av
    ]

  if (nrow(x) == 0)  {
    return(NULL)
  }

  attr(x, "ppp_year")           <- pt
  attr(x, "release_version")    <- rv
  attr(x, "adaptation_version") <- av

  x <- uniq_vars_to_attr(x)

  return(x)
}






