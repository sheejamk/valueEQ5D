
EQ5D3L_tariffs_TTO.df <- read.csv("data-raw/EQ5D3L_tariffs_TTO.csv", header = TRUE, stringsAsFactor = FALSE,row.names=1)
usethis::use_data(EQ5D3L_tariffs_TTO.df, overwrite = TRUE)

