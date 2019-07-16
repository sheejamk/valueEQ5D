
EQ5D5L_Crosswalk_ValueSets.df <- read.csv("data-raw/EQ-5D-5L_Crosswalk_ValueSets.csv", header = TRUE, stringsAsFactor = FALSE)
usethis::use_data(EQ5D5L_Crosswalk_ValueSets.df, overwrite = TRUE)

