
###########################################################################################################
#' Function to check the EQ-5D-3L scores
#' @param this.response  a must input,response for EQ-5D-3L mobility  or the 5 digit response, or the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param this.response2 response for EQ-5D-3L self care, or NA if the responses are given as this.response
#' @param this.response3  response for EQ-5D-3L usual activities,or NA if the responses are given as this.response
#' @param this.response4  response for EQ-5D-3L pain/discomfort, or NA if the responses are given as this.response
#' @param this.response5  response for EQ-5D-3L anxiety/depression, or NA if the responses are given as this.response
#' @examples checkScores3L(c(1,2,3,3,3))
#' @examples checkScores3L(1,2,3,3,3)
#' @examples checkScores3L(1,2,3,2,3)
#' @examples checkScores3L(12323)
#' @export
checkScores3L<-function(this.response, this.response2=NA, this.response3=NA, this.response4=NA, this.response5=NA){
  responses=c(this.response,this.response2,this.response3,this.response4,this.response5)
  if(sum(is.na(this.response))>0){ # first value should be not be a NA, do not contain NA
    this.score<-NA
    return(NA)
  }else{
    if(length(this.response)!=5 && length(this.response)!=1){
      message("Invalid EQ-5D-5L responses-check the responses to each question")
      return(-1)
    }else{
      if(length(this.response)==5){#first value a vector
        this.score <- paste(this.response,collapse = "")
        responses<-this.response
      }else{
        if(length(this.response)==1){#first value 5 digit number or actual response for mobility
          this.score <- paste(responses[!is.na(responses)],collapse="")
          responses<-convertNumberToIndividualDigits(this.score)
        }
      }
    }
  }
  if(!all(responses%in% 1:3)){
    message("Responses not valid for EQ-5D-3L scores")
    return(-1)
  }else{
    this.score<-as.numeric(this.score)
    if(this.score<11111 || this.score>33333){
      if(this.score<0 || this.score>33333){
        message("Responses not valid for EQ-5D-5L scores")
        return(-1)
      }else{
        message("Responses not valid for EQ-5D-5L scores or some missing")
        return(NA)
      }
    }else{
      return(responses)
      
    }
  }
}

###########################################################################################################
#' Function to check the EQ-5D-5L scores
#' @param this.response  a must input,response for EQ-5D-3L mobility  or the 5 digit response, or the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param this.response2 response for EQ-5D-5L  self care, or NA if the responses are given as this.response
#' @param this.response3  response for EQ-5D-5L  usual activities,or NA if the responses are given as this.response
#' @param this.response4  response for EQ-5D-5L  pain/discomfort, or NA if the responses are given as this.response
#' @param this.response5  response for EQ-5D-5L  anxiety/depression, or NA if the responses are given as this.response
#' @examples checkScores5L(c(1,2,3,5,3))
#' @examples checkScores5L(1,2,3,4,3)
#' @examples checkScores5L(1,2,3,7,3)
#' @examples checkScores5L(12323)
#' @export
checkScores5L<-function(this.response, this.response2=NA, this.response3=NA, this.response4=NA, this.response5=NA){
  responses=c(this.response,this.response2,this.response3,this.response4,this.response5)
  if(sum(is.na(this.response))>0){ # first value should be not be a NA, do not contain NA
    this.score<-NA
    return(NA)
  }else{
    if(length(this.response)!=5 && length(this.response)!=1){
      message("Invalid EQ-5D-5L responses-check the responses to each question")
      return(-1)
    }else{
      if(length(this.response)==5){#first value a vector
        this.score <- paste(this.response,collapse = "")
        responses<-this.response
      }else{
        if(length(this.response)==1){#first value 5 digit number or actual response for mobility
          this.score <- paste(responses[!is.na(responses)],collapse="")
          responses<-convertNumberToIndividualDigits(this.score)
        }
      }
    }
  }
  if(!all(responses%in% 1:5)){
    if(sum(is.na(responses)>0)){
      message("Responses not valid for EQ-5D-5L scores")
      return(NA)
    }else{
      message("Responses not valid for EQ-5D-5L scores")
      return(-1)
    }
   
  }else{
    this.score<-as.numeric(this.score)
    if(this.score<11111 || this.score>55555){
      if(this.score<0 || this.score>55555){
        message("Responses not valid for EQ-5D-5L scores")
        return(-1)
      }else{
        message("Responses not valid for EQ-5D-5L scores or some missing")
        return(NA)
      }
      
    }else{
      return(responses)
    }
  }
}
##########################################################################################################
#' Function to value EQ-5D-5L scores for various countries
#' @param country a country name from the list Canada,China,England,Germany,HongKong,Indonesia,Ireland,Japan,Korea,Malaysia,Netherlands,Poland,Spain,Taiwan,Thailand,and Uruguay
#' @param this.response  a must input,response for EQ-5D-5L mobility  or the 5 digit response, or the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param this.response2 response for EQ-5D-5L self care, or NA if the responses are given as this.response
#' @param this.response3  response for EQ-5D-5L usual activities,or NA if the responses are given as this.response
#' @param this.response4  response for EQ-5D-5L pain/discomfort, or NA if the responses are given as this.response
#' @param this.response5  response for EQ-5D-5L anxiety/depression, or NA if the responses are given as this.response
#' @return index values  if success, -1 if failure
#' @examples valueEQ5D5LIndscores("England",23434)
#' @examples valueEQ5D5LIndscores("China",2,3,4,3,4)
#' @examples valueEQ5D5LIndscores("Poland",c(1,2,3,4,3))
#' @export
### hint **whole values tested from paper * @references published values tested from paper
#' @references *Canada: Table 2 column 5 page 103 in Xie et al (2016) <doi: 10.1097/MLR.0000000000000447>
#' @references **China: Table 4 column 4 page 667 in Luo et al (2017) <doi: 10.1016/j.jval.2016.11.016>
#' @references **England: Table 2 column 2 page 17 in Devlin et al (2018) <doi: 10.1002/hec.3564>
#' @references **Germany: Table column 9 page 670 in Ludwig et al (2018) <doi: 10.1007/s40273-018-0615-8>
#' @references *Hong Kong: Table 3 column 8 page 244 in Wong et al (2018) <doi: 10.1007/s40271-017-0278-0>
#' @references *Indonesia: Table 3 column 8 page 1162 in Purba et al (2017) <doi: 10.1007/s40273-017-0538-9>
#' @references *Ireland: Table 2 column 2 page 1348 in Hobbins et al (2016) <doi: 10.1007/s40273-018-0690-x>
#' @references *Japan: Table 2 column 7 page 651 in Shiroiwa, et al (2016) <doi: 10.1016/j.jval.2016.03.1834>
#' @references *Korea: Table 5 column 6 page 1851 in Kim,  et al (2016) <doi: 10.1007/s11136-015-1205-2>
#' @references **Malaysia: Table 2 column 9 page 720 in Shafie  et al (2019) <doi: 10.1007/s40273-018-0758-7>
#' @references *Netherlands: Table 4 column 8 page 350 in Versteegh et al (2016) <doi: 10.1016/j.jval.2016.01.003>
#' @references **Poland: Table 2 column 7 in Golicki et al <doi: 10.1007/s40273-019-00811-7>
#' @references  *Portugal: Table 3 column 4 in Ferreira1 et al (2014) <doi:10.1007/s11136-019-02226-5>
#' @references *Spain: Table 1 column 9 in Ramos-Goñiet et al (2018) <https://doi.org/10.1016/j.jval.2017.10.023>
#' @references *Taiwan: Table 2 column 4 pge 9 in Lin et al (2018)  <https://doi.org/10.1371/journal.pone.0209344>
#' @references *Thailand: Table 3 column 6 page 4 in Pattanaphesaj et al (2018) <doi: 10.1080/14737167.2018>
#' @references *Uruguay: Table 2.3 column 5 page 29 in Augustovski et al (2016) <doi: 10.1007/s11136-015-1086-4>

valueEQ5D5LIndscores<-function(country,this.response,this.response2=NA, this.response3=NA, this.response4=NA, this.response5=NA){
  countrylist=c("Canada","China","England" ,"Germany","HongKong","Indonesia","Ireland",
                "Japan","Korea","Malaysia","Netherlands","Poland","Portugal","Spain","Taiwan","Thailand","Uruguay")
  if(country%in%countrylist){
    scores<-checkScores5L(this.response,this.response2, this.response3, this.response4, this.response5)
    if(sum(is.na(scores))>0){
      return(NA)
    }else{
      if(sum(scores)==-1){
        message("EQ-5D-5L scores are not valid")
        return(-1)
      }else{
        eq5d.valueset=EQ5D5L_tariffs.df
        names(scores)<-c("MO","SC","UA","PD","AD")
        rows=paste0(names(scores),scores)
        col=checkColumnExist(country,eq5d.valueset)
        rownum1=which(row.names(eq5d.valueset)==rows[1])
        rownum2=which(row.names(eq5d.valueset)==rows[2])
        rownum3=which(row.names(eq5d.valueset)==rows[3])
        rownum4=which(row.names(eq5d.valueset)==rows[4])
        rownum5=which(row.names(eq5d.valueset)==rows[5])
        rownumfh=which(row.names(eq5d.valueset)=="fullHealth")
        rownuminter=which(row.names(eq5d.valueset)=="intercept")
        rownumn4=which(row.names(eq5d.valueset)=="N4")
        rownumn45=which(row.names(eq5d.valueset)=="Num45sq")
        inter.value<-NA
        if(any(scores>1) && !is.na(eq5d.valueset[rownuminter,country])){
          inter.value<-eq5d.valueset[rownuminter,country]
        }
        n4value<-NA
        if(any(scores>=4) && !is.na(eq5d.valueset[rownumn4,country])){
          n4value<-eq5d.valueset[rownumn4,country]
        }
        n45<-which(scores%in%c(4,5))
        n45value<-NA
        if(length(n45)>=1 & !is.na(eq5d.valueset[rownumn45,country])){
          n45value<-(length(n45)-1)^2*eq5d.valueset[rownumn45,country]
        }
        n45sall=0
        if(length(n45)>=1){
          for(i in 1:length(n45)){
            names45row<-paste0(names(scores)[n45[i]],"45")
            rownumn45r=which(row.names(eq5d.valueset)==names45row)
            if(!is.na(eq5d.valueset[rownumn45r,country])){
              n45rvalue=eq5d.valueset[rownumn45r,country]
              n45sall=n45sall+n45rvalue
            }else{
              n45rvalue=0
              n45sall=n45sall+n45rvalue
            }
          }
        }
        dim.response=c(eq5d.valueset[rownum1,country],eq5d.valueset[rownum2,country],eq5d.valueset[rownum3,country],
                       eq5d.valueset[rownum4,country],eq5d.valueset[rownum5,country])
        sum.response=sum(dim.response,na.rm =TRUE)
        values<-c(eq5d.valueset[rownumfh,country],inter.value,sum.response,
                  n4value,n45value,n45sall)
        values.state<-sum(values,na.rm =TRUE)
      }
    }
    return(values.state)
  }else{
    message("No tariffs found for the country you specified for EQ-5D-5L. Please try later !!")
    return(-1)
  }
  
}
###########################################################################################################
#' Function to value EQ-5D-5L scores for any country and group by gender and age
#' @param eq5dresponse.data the data containing eq5d responses
#' @param mo  column name for EQ-5D-5L mobility
#' @param sc column name for response for EQ-5D-5L self care
#' @param ua  column name for response for EQ-5D-5L usual activities
#' @param pd  column name for response for EQ-5D-5L pain/discomfort
#' @param ad  column name for response for EQ-5D-5L anxiety/depression
#' @param country  country of interest, by default is England
#' @param groupby  male or female -grouping by gender, default NULL
#' @param agelimit  vector of ages to show upper and lower limits
#' @return index value  if success, -1 if failure
#' @examples 
#' data<-data.frame(age=c(10,20),sex=c("M","F"),mo=c(1,2),sc=c(1,2),ua=c(3,4),pd=c(3,4),ad=c(3,4))
#' valueEQ5D5L(data, "mo", "sc","ua", "pd", "ad","England",NULL,c(10,70))
#' @export
#' @description Function to value EQ-5D-5L descriptive system to index value.
valueEQ5D5L<-function(eq5dresponse.data,mo,sc,ua,pd,ad,country="England",groupby=NULL,agelimit=NULL){
  eq5d.colnames<-c(mo,sc,ua,pd,ad)
  ans.eq5d.colnames<-sapply(eq5d.colnames,checkColumnExist,eq5dresponse.data)
  if(all(ans.eq5d.colnames==0)){# if the eq5d column names match
    working.data=subsetGenderAgeToGroup(eq5dresponse.data,groupby,agelimit)
    scores=c()
    if(nrow(working.data)<1){
      message("No entries with the given criteria -Please check the contents or the criteria")
      return(-1)
    }else{
      for(j in 1:nrow(working.data)){
        res1=working.data[j,mo]
        res2=working.data[j,sc]
        res3=working.data[j,ua]
        res4=working.data[j,pd]
        res5=working.data[j,ad]
        this.score<-valueEQ5D5LIndscores(country,c(res1,res2,res3,res4,res5))
        if(this.score!=-1){
          scores=c(scores,this.score)
        }else{
          message("EQ-5D-5L responses not valid - 5L scores can not be valued")
          return(-1)
        }
      }
      names(scores)<-"EQ-5D-5Lscores"
      new.data=cbind(working.data,scores)
      colnames(new.data)<-c(colnames(working.data), "EQ-5D-5L scores")
      stats<-descriptiveStatDataColumn(scores,"EQ-5D-5L")
      freqtable<-getFrequencyTable(scores)
      first=is.null(groupby) || toupper(groupby)=="NA" || is.na(groupby)
      second=is.null(agelimit) || sum(toupper(agelimit)=="NA")!=0 || sum(is.na(agelimit))!=0
      if(first & second){
        title<-paste("Histogram of EQ-5D-5L index values", sep="")
      }else{
        if(first & !second){
          title<-paste("Histogram of EQ-5D-5L index values",
                       " with ages between ", agelimit[1], " and ",agelimit[2], sep="")
        }else{
          if(!second& second){
            title<-paste("Histogram of EQ-5D-5L index values for ",
                         groupby, sep="")
          }else{
            title<-paste("Histogram of EQ-5D-5L index values for ",
                         groupby, " with ages between ", agelimit[1], " and ",agelimit[2], sep="")
          }
        }
      }
      hist.plot <-graphics::hist(scores,main=title)
      results<- list("stats" = stats, "frequencyTable" = freqtable, "histogram"= hist.plot,"modifiedData"=new.data)
      return(results)
    }
  }else{# if the eq 5d column names do not match
    message("EQ-5D column names do not match")
    return(-1)
  }
}
##########################################################################################################
#' Function to value EQ-5D-3L scores for various countries 
#' @param country a country name from the list Belgium,Brazil,Canada,Chile,Denmark,Europe,Finland,France,Germany,Italy,Japan,Korea,Netherlands,NewZealand,Poland,Portugal,Slovenia,Spain,Taiwan,Thailand,UK,USA,and Zimbabwe
#' @param method method name either TTO or VAS
#' @param this.response  a must input,response for EQ-5D-5L mobility  or the 5 digit response, or the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param this.response2 response for EQ-5D-3L self care, or NA if the responses are given as this.response
#' @param this.response3  response for EQ-5D-3L usual activities,or NA if the responses are given as this.response
#' @param this.response4  response for EQ-5D-3L pain/discomfort, or NA if the responses are given as this.response
#' @param this.response5  response for EQ-5D-3L anxiety/depression, or NA if the responses are given as this.response
#' @return index value based if success, -1 if failure
#' @examples valueEQ5D3LIndscores("UK","TTO",23131)
#' @examples valueEQ5D3LIndscores("Spain","TTO",2,3,1,3,1)
#' @examples valueEQ5D3LIndscores("Denmark","VAS",c(1,2,3,1,3))
#' @export
#' @references Argentina (TTO and Vas), Brazil Canada Poland Portugal Taiwan Trinidad_and_Tobago (not in Szende book)
#' @references Belgium: Equation 2 page 208 in Cleemput, Irina. "A social preference valuations set for EQ-5D health states in Flanders, Belgium." The European Journal of Health Economics 11.2 (2010): 205-213.
#' @references ---Brazil:Santos, Marisa, et al. "Brazilian valuation of EQ-5D-3L health states: results from a saturation study." Medical Decision Making 36.2 (2016): 253-263.
#' @references Canada: Table 4 page 8 in Bansback, Nick, et al. "Canadian valuation of EQ-5D health states: preliminary value set and considerations for future valuation studies." PloS one 7.2 (2012): e31115.
#' @references Chile: Table 2 column 4 page 1137 in Zarate, Victor, et al. "Social valuation of EQ-5D health states: the Chilean case." Value in health 14.8 (2011): 1135-1141.
#' @references Denamrk: Table 4 page 463 in Wittrup-Jensen, Kim U., et al. "Generation of a Danish TTO value set for EQ-5D health states." Scandinavian Journal of Public Health 37.5 (2009): 459-466.
#' @references ---Europer: Greiner, Wolfgang, et al. "A single European currency for EQ-5D health states." The European Journal of Health Economics, formerly: HEPAC 4.3 (2003): 222-231.
#' @references Finland Table 6 column 4 page 68 in Ohinmaa, A., and H. Sintonen. "Inconsistencies and modelling of the Finnish EuroQol (EQ-5D) preference values In: Gainer, W, Graf von dem Schulenburg, J, Piercy, J (eds), EuroQol, Plenary Meeting, Hannover 1998, 1-2 October." Discussion Papers. Hannover: University of Hannover Centre of Health Economics and Health System Research (1999): 57-74.
#' @references ---Germany: Claes, C., et al. "An interview-based comparison of the TTO and VAS values given to EuroQol states of health by the general German population." Proceedings of the 15th Plenary Meeting of the EuroQol Group. Hannover, Germany: Centre for Health Economics and Health Systems Research, University of Hannover. 1999.
#' @references Italy: Table 4 coulmn 5, page 820 in Scalone, Luciana, et al. "Italian population-based values of EQ-5D health states." Value in Health 16.5 (2013): 814-822.
#' @references Japan: Table 4 column 1, page 41 in Tsuchiya, Aki, et al. "Estimating an EQ‐5D population value set: the case of Japan." Health economics 11.4 (2002): 341-353.
#' @references South Korea: Table 3 column 4, page 1191 in Lee, Yeon‐Kyeng, et al. "South Korean time trade‐off values for EQ‐5D health states: Modeling with observed values for 101 health states." Value in Health 12.8 (2009): 1187-1193.
#' @references Netherlands: Table 5 column 3, page 1128 in Lamers, Leida M., et al. "The Dutch tariff: results and arguments for an effective design for national EQ‐5D valuation studies." Health economics 15.10 (2006): 1121-1132.
#' @references New Zealand: Equation 2 page 541 in Devlin, Nancy J., et al. "Logical inconsistencies in survey respondents' health state valuations‐a methodological challenge for estimating social tariffs." Health economics 12.7 (2003): 529-544.
#' @references Poland: Table 5 column 2 page 293 in Golicki, Dominik, et al. "Valuation of EQ‐5D Health States in Poland: First TTO‐Based Social Value Set in Central and Eastern Europe." Value in Health 13.2 (2010): 289-297.
#' @references Portugal: Table 4 column 6 page 418 Ferreira, Lara N., et al. "The valuation of the EQ-5D in Portugal." Quality of Life Research 23.2 (2014): 413-423.
#' @references Solvenia: Table 6 column 2 page 30 (constant given in page 36) in Rupel, V. Prevolnik, and Matejka Rebolj. "The Slovenian VAS tariff based on valuations of EQ-5D health states from the general population." Discussion papers/17th Plenary Meeting of the Euroqol Group. Universidad Pública de Navarra, 2001.
#' @references Spain: Table 3 column 4 page 13 (TTO) in Badia, Xavier, et al. "A comparison of United Kingdom and Spanish general population time trade-off values for EQ-5D health states." Medical Decision Making 21.1 (2001): 7-16.
#' @references ---Spain: Badia, X., et al. "The Spanish VAS tariff based on valuation of EQ-5D health states from the general population." EuroQol Plenary Meeting Rotterdam. 1997.
#' @references Taiwan: Table 2 column 4 page 702 in Lee, Hsin-Yi, et al. "Estimating quality weights for EQ-5D (EuroQol-5 dimensions) health states with the time trade-off method in Taiwan." Journal of the Formosan Medical Association 112.11 (2013): 699-706.
#' @references Thailand: Table 1 column 2 page 1144 (parameters like MO3 are calculated) in Tongsiri, Sirinart, and John Cairns. "Estimating population-based values for EQ-5D health states in Thailand." Value in Health 14.8 (2011): 1142-1145.
#' @references UK: Table 1 column 2 page 1103 (TTO) in Dolan, Paul. "Modeling valuations for EuroQol health states." Medical care (1997): 1095-1108.
#' @references UK: Table 4A column 2 page 43 in MVH Group. "The measurement and valuation of health: Final report on the modelling of valuation tariffs." Centre for Health Economics, University of York (1995).
#' @references USA: Table 5 coulmn 2 page 214 in Shaw, James W., Jeffrey A. Johnson, and Stephen Joel Coons. "US valuation of the EQ-5D health states: development and testing of the D1 valuation model." Medical care (2005): 203-220.
#' @references Zimbabwe: Table 5 column 3 page 7 inJelsma, Jennifer, et al. "How do Zimbabweans value health states?." Population health metrics 1.1 (2003): 11.
#' @references Trinidad and Tobago: Table 4 page 65 in Bailey, Henry, Elly Stolk, and Paul Kind. "Toward explicit prioritization for the Caribbean: an EQ-5D value set for Trinidad and Tobago." Value in health regional issues 11 (2016): 60-67.
valueEQ5D3LIndscores<-function(country,method,this.response,this.response2=NA, this.response3=NA, this.response4=NA, this.response5=NA){
  countrylist=c("Belgium","Brazil","Canada","Chile","Denmark" ,"Europe","Finland","France","Germany","Italy","Japan","Korea",
                "Netherlands","NewZealand","Poland", "Portugal","Slovenia","Spain","Taiwan","Thailand","UK","USA","Zimbabwe","Trinidad_and_Tobago")
  	
  VAS_countrylist=c("Belgium","Denmark" ,"Europe","Finland","Germany","NewZealand","Slovenia","Spain","UK")
  TTO_countrylist=c("Brazil","Canada","Chile","Denmark" ,"Europe","France","Germany","Italy","Japan","Korea",
                "Netherlands","Poland", "Portugal","Spain","Taiwan","Thailand","UK","USA","Zimbabwe","Trinidad_and_Tobago")
  if(country%in%countrylist){
    scores<-checkScores3L(this.response,this.response2, this.response3, this.response4, this.response5)
    if(sum(is.na(scores))>0){
      return(NA)
    }else{
      if(sum(scores)==-1){
        message("EQ-5D-5L scores are not valid")
        return(-1)
      }else{
        if(method=="TTO" && country%in%TTO_countrylist){
          eq5d.valueset=EQ5D3L_tariffs_TTO.df
        }else{
          if(method=="VAS"&& country%in%VAS_countrylist){
            eq5d.valueset=EQ5D3L_tariffs_VAS.df
          }else{
            message("No method found!!")
            return(-1)
          }
        }
        names(scores)<-c("MO","SC","UA","PD","AD")
        rows=paste0(names(scores),scores)
        col=checkColumnExist(country,eq5d.valueset)
        if(col==0){
          min2or3<-which(scores%in%c(2,3))
          if(length(min2or3)==5){
            all.equals2or3<-1
          }else{
            all.equals2or3<-c()
          }
          which3<-which(scores%in%c(3))
          which2<-which(scores%in%c(2))
          rownums=c()
          dim.response=NA
          min3.value<-NA
          all.equals2or3.value<-NA
          min2or3.value<-NA
          c3sq.value<-NA
          d1.value<-NA
          I2.value<-NA
          I2_sq.value<-NA
          I3.value<-NA
          I3_sq.value<-NA
          rownumfh=which(row.names(eq5d.valueset)=="FullHealth")
          rownum_min2or3=which(row.names(eq5d.valueset)=="Constant")
          rownumn_min3=which(row.names(eq5d.valueset)=="N3")
          if(method=="TTO"){
            rownum_all.equals2or3=which(row.names(eq5d.valueset)=="X5")
            rownum_C3sq=which(row.names(eq5d.valueset)=="C3sq")
            rownumn_D1=which(row.names(eq5d.valueset)=="D1")
            rownumn_I2=which(row.names(eq5d.valueset)=="I2")
            rownumn_I2_sq=which(row.names(eq5d.valueset)=="I2_sq")
            rownumn_I3=which(row.names(eq5d.valueset)=="I3")
            rownumn_I3_sq=which(row.names(eq5d.valueset)=="I3_sq")
          }else{
            rownum_all.equals2or3=NA
            rownum_C3sq=NA
            rownumn_D1=NA
            rownumn_I2=NA
            rownumn_I2_sq=NA
            rownumn_I3=NA
            rownumn_I3_sq=NA
          }
          if(length(min2or3)>0){
            for(i in 1:length(min2or3)){
              rownams<-row.names(eq5d.valueset)
              ro=which(rownams==rows[min2or3[i]])
              rownums=cbind(rownums,ro)
            }
            dim.response=eq5d.valueset[rownums,country]
          }
          if(any(scores>=3) && !is.na(eq5d.valueset[rownumn_min3,country])){
            min3.value<-eq5d.valueset[rownumn_min3,country]
          }
          if(length(which3)>=1 & sum(is.na(rownum_C3sq)==0)){
            if( !is.na(eq5d.valueset[rownum_C3sq,country]))
              c3sq.value<-(length(which3))^2*eq5d.valueset[rownum_C3sq,country]
          }
          if(length(all.equals2or3)>=1 & sum(is.na(rownum_all.equals2or3)==0)){
            if(!is.na(eq5d.valueset[rownum_all.equals2or3,country]))
              all.equals2or3.value<-eq5d.valueset[rownum_all.equals2or3,country]
          }
          if(sum(scores)>5 & length(min2or3)>=1  &  sum(is.na(rownum_min2or3)==0)){
             if(!is.na(eq5d.valueset[rownum_min2or3,country]))
              min2or3.value<-eq5d.valueset[rownum_min2or3,country]
          }
          if(sum(scores)>5 & length(min2or3)>=1 &  sum(is.na(rownumn_D1)==0)){
             if(!is.na(eq5d.valueset[rownumn_D1,country]))
              d1.value<-(length(min2or3)-1)*eq5d.valueset[rownumn_D1,country]
          }
          if(sum(scores)>5 & length(which2)>=1 &  sum(is.na(rownumn_I2)==0)){
            if( !is.na(eq5d.valueset[rownumn_I2,country]))
            I2.value<-(length(which2)-1)*eq5d.valueset[rownumn_I2,country]
          }
          if(sum(scores)>5 & length(which2)>=1 &  sum(is.na(rownumn_I2_sq)==0)){
            if( !is.na(eq5d.valueset[rownumn_I2_sq,country]))
              I2_sq.value<-(length(which2)-1)^2*eq5d.valueset[rownumn_I2_sq,country]
          }
          if(sum(scores)>5 & length(which3)>=1 &  sum(is.na(rownumn_I3)==0)){
            if( !is.na(eq5d.valueset[rownumn_I3,country]))
              I3.value<-(length(which3)-1)*eq5d.valueset[rownumn_I3,country]
          }
          if(sum(scores)>5 & length(which3)>=1 &  sum(is.na(rownumn_I3_sq)==0)){
             if( !is.na(eq5d.valueset[rownumn_I3_sq,country]))
            I3_sq.value<-(length(which3)-1)^2*eq5d.valueset[rownumn_I3_sq,country]
          }
          if(country=="Germany" && method=="VAS"){
            prod.response=prod(dim.response,na.rm =TRUE)
            values<-c(eq5d.valueset[rownumfh,country],prod.response,min2or3.value,min3.value,all.equals2or3.value,c3sq.value,d1.value,I2.value,I2_sq.value,I3.value,I3_sq.value)
            values.state<-prod(values,na.rm =TRUE)
          }else{
            sum.response=sum(dim.response,na.rm =TRUE)
            values<-c(eq5d.valueset[rownumfh,country],sum.response,min2or3.value,min3.value,all.equals2or3.value,c3sq.value,d1.value,I2.value,I2_sq.value,I3.value,I3_sq.value)
            values.state<-sum(values,na.rm =TRUE)
          }
        }else{
          message("No country tariffs")
          return(-1)
        }
      }
    }
    return(values.state)
  }else{
    message("No tariffs found for the country you specified for EQ-5D-3L. Please try later !!")
    return(-1)
  }
}
###########################################################################################################
#' Function to value EQ-5D-3L columns to index values for any country and group by gender and age
#' @param eq5dresponse.data the data containing eq5d responses
#' @param mo  column name for EQ-5D-3L mobility
#' @param sc column name for response for EQ-5D-3L self care
#' @param ua  column name for response for EQ-5D-3L usual activities
#' @param pd  column name for response for EQ-5D-3L pain/discomfort
#' @param ad  column name for response for EQ-5D-3L anxiety/depression
#' @param country  country of interest, by default is UK, if groupby has to specify the country should be specified
#' @param method Either "TTO" or "VAS"
#' @param groupby  male or female -grouping by gender, default NULL
#' @param agelimit  vector of ages to show upper and lower limits
#' @return the descriptive statistics of index values, frequency table and the modified data where the last column will be the index values
#' data<-data.frame(age=c(10,20),sex=c("M","F"),mo=c(1,2),sc=c(1,2),ua=c(3,4),pd=c(3,1),ad=c(3,1))
#' valueEQ5D3L(data, "mo", "sc","ua", "pd", "ad","UK","TTO",NULL,c(10,70))
#' @export
#' @description Main function to value EQ-5D-5L descriptive system to 5L index values.
valueEQ5D3L<-function(eq5dresponse.data,mo,sc,ua,pd,ad,country,method,groupby,agelimit){
  eq5d.colnames<-c(mo,sc,ua,pd,ad)
  ans.eq5d.colnames<-sapply(eq5d.colnames,checkColumnExist,eq5dresponse.data)
  if(all(ans.eq5d.colnames==0)){# if the eq5d column names match
    working.data=subsetGenderAgeToGroup(eq5dresponse.data,groupby,agelimit)
    if(nrow(working.data)<1){
      message("No entries with the given criteria -Please check the contents or the criteria")
      return(-1)
    }else{
      scores=c()
      for(j in 1:nrow(working.data)){
        res1=working.data[j,mo]
        res2=working.data[j,sc]
        res3=working.data[j,ua]
        res4=working.data[j,pd]
        res5=working.data[j,ad]
        
        this.score<-valueEQ5D3LIndscores(country,method,res1,res2,res3,res4,res5)
        if(this.score!=-1){
          scores=c(scores,this.score)
        }else{
          message("responses not valid -3L scores can not be valued")
          return(-1)
        }
      }
      names(scores)<-"EQ-5D-3Lscores"
      new.data=cbind(working.data,scores)
      colnames(new.data)<-c(colnames(working.data), "EQ-5D-3L scores")
      stats<-descriptiveStatDataColumn(scores,"EQ-5D-3L")
      freqtable<-getFrequencyTable(scores)
      first=is.null(groupby) || toupper(groupby)=="NA" || is.na(groupby)
      second=is.null(agelimit) || sum(toupper(agelimit)=="NA")!=0  || sum(is.na(agelimit))!=0
      if(first & second){
        title<-paste("Histogram of EQ-5D-3L index values", sep="")
      }else{
        if(first & !second){
          title<-paste("Histogram of EQ-5D-3L index values",
                       " with ages between ", agelimit[1], " and ",agelimit[2], sep="")
        }else{
          if(!second& second){
            title<-paste("Histogram of EQ-5D-3L index values for ",
                         groupby, sep="")
          }else{
            title<-paste("Histogram of EQ-5D-3L index values for ",
                         groupby, " with ages between ", agelimit[1], " and ",agelimit[2], sep="")
          }
        }
      }
      hist.plot <-graphics::hist(scores,main=title)
      results<- list("stats" = stats, "frequencyTable" = freqtable, "histogram"= hist.plot,"modifiedData"=new.data)
      return(results)
    }
  }else{# if the eq 5d column names do not match
    message("EQ-5D column names do not match")
    return(-1)
  }
}
###########################################################################################################
#' Function to map EQ-5D-5L descriptive system to 3L index value
#' @param country  default is "UK", no other method currently currently implemented
#' @param method CW cross walk
#' @param this.response  response for EQ-5D-5L mobility  or the 5 digit response, or the vector of responses, e.g. 11111, c(1,1,1,1,1) or 1
#' @param this.response2 response for EQ-5D-5L self care, or NA if the responses are given as this.response
#' @param this.response3  response for EQ-5D-5L usual activities,or NA if the responses are given as this.response
#' @param this.response4  response for EQ-5D-5L pain/discomfort, or NA if the responses are given as this.response
#' @param this.response5  response for EQ-5D-5L anxiety/depression, or NA if the responses are given as this.response
#' @return index value of EQ-5D-3L, -1 if any error
#' @examples eq5dmap5Lto3LIndscores("UK","CW",11125)
#' @examples eq5dmap5Lto3LIndscores("UK","CW",c(1,1,1,2,5))
#' @examples eq5dmap5Lto3LIndscores("UK","CW",1,1,1,2,5)
#' @export
#' @description Function to map EQ-5D-5L descriptive system to 3L index value (ref:Van Hout et al 2012 and code inspired from https://github.com/brechtdv/eq5d-mapping)
eq5dmap5Lto3LIndscores<-function(country="UK",method="CW",this.response,this.response2=NA,this.response3=NA,this.response4=NA,this.response5=NA) {
  responses=c(this.response,this.response2,this.response3,this.response4,this.response5)
  if(sum(is.na(this.response))>0){ # first value should be not be a NA, do not contain NA
    this.score.5L<-NA
    values.state=NA
    return(values.state)
  }else{# check first value should be a vector containiing responses or a 5digit number
    if(length(this.response)!=5 && length(this.response)!=1){
      message("Invalid EQ-5D-5L responses-check the responses to each question")
      return(-1)
    }else{ #first value a vector or a 5 figit number
      if(length(this.response)==5){#first value a vector
        this.score.5L <- paste(this.response,collapse = "")
      }else{
        if(length(this.response)==1){#first value 5 digit number or actual response for mobility
          if(this.response>=11111 && this.response<=55555){ # valid 5 digit number
            this.score.5L <- this.response
          }else{ #first value might be valid-  a response to mobility
            if(this.response<=5 && this.response>0 ){ #valid response to mobility
              four.res=c(this.response2,this.response3,this.response4,this.response5)
              if(sum(is.na(four.res))==0){
                if(any(responses<=5)){
                  this.score.5L <- paste(responses,collapse = "")#all valid and generate the score
                }else{#error values
                  message("Invalid EQ-5D-5L responses-check the responses to each question")
                  return(-1)
                }
              }else{
                #missing values
                this.score.5L<-NA
                values.state=NA
                return(values.state)
              }
            }else{
              message("Invalid EQ-5D-5L responses-check the responses to each question")
              return(-1)
            }
          }
        }
      }
    }
  }
  if(this.score.5L<11111 || this.score.5L>55555){
    message("Invalid EQ-5D-5L responses")
    return(-1)
  }else{
    ## create a vector of all possible 3L index values (length == 3^5)
    index_3L <- numeric(243)
    ## create a dataframe of all possible 3L scores
    scores_3L <-
      expand.grid(AD = seq(3),
                  PD = seq(3),
                  UA = seq(3),
                  SC = seq(3),
                  MO = seq(3))
    ## calculate the index value for each score
    ## using function EQ5D_be based on Cleemput et al., 2010
    for (i in seq(243)) {
      index_3L[i] <-
        valueEQ5D3LIndscores("UK","TTO",scores_3L[i, "MO"],
                             scores_3L[i, "SC"],
                             scores_3L[i, "UA"],
                             scores_3L[i, "PD"],
                             scores_3L[i, "AD"])
    }
    ## create a dataframe of all possible 5L scores
    scores_5L <-
      expand.grid(AD = seq(5),
                  PD = seq(5),
                  UA = seq(5),
                  SC = seq(5),
                  MO = seq(5))
    ## 5L to 3L CROSSWALK
    ## load 'probability matrix' from 'EQ-5D-5L_Crosswalk_Value_Sets'
    ## this is saved as dataframe 'm'
    ## file <- system.file('extdata', "Probability_matrix_crosswalk.csv",package = 'EQ5Dmapping')
    ## prob.matrix=read.csv(file,header=FALSE)
    if(toupper(method)=="CW"){
      prob.matrix =Probability_matrix_crosswalk.df
      m<-prob.matrix
      rows_m=nrow(m)
      cols_m=ncol(m)
      if(rows_m!=3125 || cols_m!=243){
        message("Error in number of cols or rows of probability matrix")
        return(-1)
      }
      ## multiply each row of 't(m)' with 'index_3L'
      m_prod <- t(t(m) * index_3L)
      ## obtain sum per row
      ## crosswalked index value for each 5L score
      m_sums <- rowSums(m_prod)
      ## reorder columns and convert to matrix
      scores_5L <- with(scores_5L, cbind(MO, SC, UA, PD, AD))
      ## create 5L score labels
      scores_5L_chr <- apply(scores_5L, 1, paste, collapse = "")
      this_score <- which(scores_5L_chr == paste(this.score.5L, collapse = ""))
      return(m_sums[this_score])
    }else{
      message("The specified method is not implemented")
      return(-1)
    }
  }
}

###########################################################################################################
#' Function to map EQ-5D-5L scores to EQ-5D-3L index values as per the specific country and group by gender and age
#' @param eq5dresponse.data the data containing eq5d5L responses
#' @param mobility  column name for EQ-5D-5L mobility
#' @param self.care column name for response for EQ-5D-5L self care
#' @param usual.activities  column name for response for EQ-5D-5L usual activities
#' @param pain.discomfort  column name for response for EQ-5D-5L pain/discomfort
#' @param anxiety  column name for response for EQ-5D-5L anxiety/depression
#' @param country  country of interest, by default is UK, if groupby has to specify the country should be specified
#' @param method CW cross walk
#' @param groupby  male or female -grouping by gender, default NULL
#' @param agelimit  vector of ages to show upper and lower limits
#' @return index value  if success, -1 if failure
#' @examples eq5dmap5Lto3L(data, "Mobility", "SelfCare","UsualActivity", "Pain", "Anxiety",UK,NULL,c(10,70))
#' @export
#' @description Function to map EQ-5D-5L scores to EQ-5D-3L index values
eq5dmap5Lto3L<-function(eq5dresponse.data,mobility, self.care,usual.activities,pain.discomfort,anxiety,
                        country="UK",method="CW",groupby=NULL,agelimit=NULL){
  eq5d.colnames<-c(mobility, self.care,usual.activities,pain.discomfort,anxiety)
  ans.eq5d.colnames<-sapply(eq5d.colnames,checkColumnExist,eq5dresponse.data)
  if(all(ans.eq5d.colnames==0)){# if the eq5d column names match
    working.data=subsetGenderAgeToGroup(eq5dresponse.data,groupby,agelimit)
    scores=c()
    if(nrow(working.data)<1){
      message("No entries with the given criteria -Please check the contents or the criteria")
      return(-1)
    }else{
      for(j in 1:nrow(working.data)){
        res1=working.data[j,mobility]
        res2=working.data[j,self.care]
        res3=working.data[j,usual.activities]
        res4=working.data[j,pain.discomfort]
        res5=working.data[j,anxiety]
        this.score<-eq5dmap5Lto3LIndscores(country,method,c(res1,res2,res3,res4,res5))
        if(this.score!=-1){
          scores=c(scores,this.score)
        }else{
          message("EQ-5D-5L responses not valid - 5L scores can not be valued")
          return(-1)
        }
      }
      #names(scores)<-"Mapped EQ-5D-3Lscores"
      new.data=cbind(working.data,scores)
      colnames(new.data)<-c(colnames(working.data), "Mapped EQ-5D-3L scores")
      stats<-descriptiveStatDataColumn(scores,"EQ-5D-3L")
      freqtable<-getFrequencyTable(scores)
      first=is.null(groupby) || toupper(groupby)=="NA" || is.na(groupby)
      second=is.null(agelimit)|| sum(toupper(agelimit)=="NA")!=0 || sum(is.na(agelimit))!=0
      if(first & second){
        title<-paste("Histogram of EQ-5D-3L index values", sep="")
      }else{
        if(first & !second){
          title<-paste("Histogram of EQ-5D-3L index values",
                       " with ages between ", agelimit[1], " and ",agelimit[2], sep="")
        }else{
          if(!second& second){
            title<-paste("Histogram of EQ-5D-3L index values for ",
                         groupby, sep="")
          }else{
            title<-paste("Histogram of EQ-5D-3L index values for ",
                         groupby, " with ages between ", agelimit[1], " and ",agelimit[2], sep="")
          }
        }
      }
      hist.plot <-graphics::hist(scores,main=title)
      results<- list("stats" = stats, "frequencyTable" = freqtable, "histogram"= hist.plot,"modifiedData"=new.data)
      return(results)
    }
  }else{# if the eq 5d column names do not match
    message("EQ-5D column names do not match")
    return(-1)
  }
}

  
  