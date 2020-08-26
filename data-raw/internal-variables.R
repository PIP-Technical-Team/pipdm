# Internal package objects

# Reference years
reference_years <-  1981L:2019L

# Survey calender years
pip_years <-  1979L:2019L

# Save objects to R/sysdata.rda
usethis::use_data(pip_years,
                  reference_years,
                  internal = TRUE,
                  overwrite = TRUE)
