

# # # ###############################################################################################################
context("EQ5D5L crosswalk mapping")
test_that("EQ5D5L crosswalk mapping", {
  answers =EQ5D5L_crosswalk_indexvalues.df
  total_entries=seq(1,nrow(answers))
  end1=length(total_entries)
  total_countries=c("Denmark" ,"France","Germany","Japan", "Netherlands","Spain","Thailand",
                    "UK","USA","Zimbabwe")
  total<-length(total_countries)
  for(j in 1:total){
    this.country=total_countries[j]
    country.entry<-replaceSpaceUnderscore(total_countries[j])
    print(this.country)
    for (i in 1:nrow(answers)){
      the.result<-map5Lto3LInd(this.country,"CW",answers$state[total_entries[i]])
      this.col<-answers[[country.entry]]
      expect_equal(the.result,this.col[total_entries[i]],tolerance=9e-2)
    }
  }
})
# # # ###############################################################################################################
