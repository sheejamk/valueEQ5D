
################################################################################################################
# context("EQ5D5L scoring")
# test_that("EQ5D5L scoring", {
#   answers =EQ5D5L_indexvalues.df
#   total_entries=seq(1,nrow(answers))
#   end1=length(total_entries)
#   total_countries=c("Canada","China","England" ,"Germany","HongKong","Indonesia","Ireland",
#                      "Japan","Korea","Malaysia","Netherlands","Poland", "Portugal","Spain","Taiwan","Thailand","Uruguay")
#   
#   for(j in 1:length(total_countries)){
#     this.country=total_countries[j]
#     print(this.country)
#     for (i in 1:nrow(answers)){
#       the.result<-valueEQ5D5LIndscores(this.country,answers$state[total_entries[i]])
#       this.col<-answers[[this.country]]
#       expect_equal(the.result,this.col[total_entries[i]],tolerance=1e-3)
#     }
#   }
# })
# ###############################################################################################################
context("EQ5D3L scoring")
test_that("EQ5D3L scoring", {
  answers =EQ5D3L_indexvalues.df
  total_entries=seq(1,nrow(answers))
  end1=length(total_entries)
  VAS_countrylist=c("Argentina","Belgium","Denmark" ,"Europe","Finland","Germany", "Malaysia","NewZealand","Slovenia","Spain","UK")
  TTO_countrylist=c("Argentina","Australia","Brazil","Canada","Chile","China","Denmark" ,"Europe","France","Germany","Italy","Japan","Korea",
                    "Netherlands","Poland", "Portugal","Singapore","Spain","SriLanka","Sweden","Taiwan","Thailand","UK","USA","Zimbabwe","Trinidad_and_Tobago")
  common_countries<-Reduce(intersect, list(VAS_countrylist,TTO_countrylist))
  all_countries<-unique(c(VAS_countrylist,TTO_countrylist))
  total<-length(all_countries)
  for(j in 2:2){
    print(all_countries[j])
    if(all_countries[j]%in%common_countries){
      TTOcol=paste(all_countries[j],"TTO",sep="")
      VAScol=paste(all_countries[j],"VAS",sep="")
      for (i in 1:nrow(answers)){
        the.result.TTO<-valueEQ5D3LIndscores(all_countries[j],"TTO",answers$state[total_entries[i]])
        the.result.VAS<-valueEQ5D3LIndscores(all_countries[j],"VAS",answers$state[total_entries[i]])
        this.col.TTO<-answers[[TTOcol]]
        this.col.VAS<-answers[[VAScol]]
        expect_equal(the.result.TTO,this.col.TTO[total_entries[i]],tolerance=1e-2)
        expect_equal(the.result.VAS,this.col.VAS[total_entries[i]],tolerance=1e-2)
      }
    }else{
      col=which(colnames(answers)==all_countries[j])
      if(all_countries[j]%in%VAS_countrylist){
        method="VAS"
      }else{
        if(all_countries[j]%in%TTO_countrylist){
          method="TTO"
        }else{
          print("Associated method not found!!!")
          return(-1)
        }
      }
      for (i in 1:nrow(answers)){
        the.result<-valueEQ5D3LIndscores(all_countries[j],method,answers$state[total_entries[i]])
        this.col<-answers[[col]]
        expect_equal(the.result,as.numeric(this.col[total_entries[i]]),tolerance=1e-2)
      }
    }
  }
})
