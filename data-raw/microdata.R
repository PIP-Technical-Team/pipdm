# Load list of synthetic test datasets
dl <- readRDS('tests/testdata/synthetic-microdata.RDS')

# Pick the first one
microdata <- dl[[1]]$data

# Order by increasing welfare values
microdata <- microdata[order(microdata$welfare), ]

# Reset rownames
microdata <- data.frame(microdata, row.names = NULL)

# Save .rda file to /data
usethis::use_data(microdata, version = 3, overwrite = TRUE)
