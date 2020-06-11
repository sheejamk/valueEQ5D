EQ5D5L_tariffs.df <- read.csv("data-raw/EQ5D5L_tariffs.csv", header = TRUE, stringsAsFactor = FALSE, row.names = 1)
usethis::use_data(EQ5D5L_tariffs.df, overwrite = TRUE, internal = TRUE)
