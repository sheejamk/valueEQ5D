EQ5D5L_indexvalues.df <- read.csv("data-raw/EQ5D5L_indexvalues.csv", header = TRUE, stringsAsFactor = FALSE)
usethis::use_data(EQ5D5L_indexvalues.df, overwrite = TRUE)
