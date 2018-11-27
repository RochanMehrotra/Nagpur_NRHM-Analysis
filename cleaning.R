# In this file, I'm just extracting data from all nagpur excel files and
#then combining them to a single file further structuring is accomplised in filter_and_aggregate.R. 
setwd("/home/rochan/Downloads/DHIS (APRIL 15-MARCH 16)/")
library(readxl)
library(tidyr)
library(dplyr)
library(zoo)
# extracting maternal death cause of whole maharashtra:  
mortality_structure<-function(files){
  df<-data.frame()
  df<-as_tibble(df)
  for (i in files){
      #get district name from filename
      district<-sub('(^\\w+)\\s.+','\\1',i)
      #get relevant data
      data1<-read_xls(i,skip = 484,sheet = 1,col_names = F)
      phc<-read_excel(i,skip = 3,sheet = 1,col_names = F,range = "B4:C4",n_max = 1)
      phc<-phc[-1]
      colnames(phc)<-"facility"
      drop<-c("X__1","X__4","X__5","X__6","X__7","X__8","X__9","X__10","X__11","X__12","X__13","X__14","X__15")
      #filter useless columns
      data2<-data1[,!(names(data1) %in% drop)]
      data2<-as_tibble(data2)
      data3<-data2[1]%>%
        do(na.locf(.))
      data2<-data2[-1]
      data2<-cbind(data2,data3)
      data2<-data2%>%
        unite(newcol,X__2,X__3,sep = ".")
      #get total values of all death causing agents
      data2<-data2%>%filter((grepl(".Total",newcol)))
      data2<-cbind(phc,data2)
      final.row<-spread(data2,key =newcol,value = "X__16")
      final.row<-cbind(district,final.row)
      df<-rbind(final.row,df)
      }
  return(df)
}
all.files=list.files(recursive = T)
true.phc=grepl("PHC",all.files)
true.RH=grepl("/RH/",all.files) | grepl("/RH-SDH/",all.files) | grepl("/WH/",all.files)
true.SC=grepl(" SC_",all.files)
phc.files=all.files[true.phc]
rh.files=all.files[true.RH]
sc.files=all.files[true.SC]
mortality.phc<-mortality_structure(phc.files)
mortality.rh<-mortality_structure(rh.files)
mortality.sc<-mortality_structure(sc.files)
mortality.combined<-rbind(mortality.phc,mortality.rh,mortality.sc)
write.csv(mortality.phc,file="/home/rochan/socialcops/mortality/mortality_phc.csv")
write.csv(mortality.rh,file="/home/rochan/socialcops/mortality/mortality_rh.csv")
write.csv(mortality.sc,file="/home/rochan/socialcops/mortality/mortality_sc.csv")
write.csv(mortality.combined,file="/home/rochan/socialcops/mortality/mortality_combined.csv")
#*********************************************************************************************************
#extractig JSY,ANC,HB,IFA..etc
pregnant_structure<-function(files){
  df<-data.frame()
  df<-as_tibble(df)
  for(i in files){
    #get district name from filename
    district<-sub('(^\\w+)\\s.+','\\1',i)
    #get relevant data
    data1<-read_xls(i,skip = 10,sheet = 1,col_names = F)
    phc<-read_excel(i,skip = 3,sheet = 1,col_names = F,range = "B4:C4",n_max = 1)
    phc<-phc[-1]
    colnames(phc)<-"facility"
    drop<-c("X__1","X__3","X__4","X__5","X__6","X__7","X__8","X__9","X__10","X__11","X__12","X__13","X__14","X__15")
    #filter useless columns
    data2<-data1[,!(names(data1) %in% drop)]
    data2<-as_tibble(data2)
    data2<-data2[1:14,]
    final.row<-spread(data2,key ="X__2",value = "X__16")
    final.row<-final.row[c(-6,-9,-10)]
    final.row<-cbind(phc,final.row)
    df<-rbind(final.row,df)
  }
  return(df)
}
true.nagpur=grepl("NAGPUR",all.files)
nagpur.files=all.files[true.nagpur]
pregnant.nagpur<-pregnant_structure(nagpur.files)
write.csv(pregnant.nagpur,file="/home/rochan/socialcops/pregnant/pregnant_combined.csv")
#####################################################################################################
#extracting immunization,Vitamin A , infants disease data
immunization_disease_structure<-function(files){
  dfcause10.1<-data.frame()
  dfcause10.1.13<-data.frame()
  dfcause10.2<-data.frame()
  dfcause10.3.1<-data.frame()
  dfcause10.4<-data.frame()
  dfcauseM11<-data.frame()
  dfcauseM12<-data.frame()
  dfcause10.1<-as_tibble(dfcause10.1)
  dfcause10.1.13<-as_tibble(dfcause10.1.13)
  dfcause10.2<-as_tibble(dfcause10.2)
  dfcause10.3.1<-as_tibble(dfcause10.3.1)
  dfcause10.4<-as_tibble(dfcause10.4)
  dfcauseM11<-as_tibble(dfcauseM11)
  dfcauseM12<-as_tibble(dfcauseM12)
  for(i in files){
    #get relevant data
    data1<-read_xls(i,skip = 153,sheet = 1,col_names = F)
    phc<-read_excel(i,skip = 3,sheet = 1,col_names = F,range = "B4:C4",n_max = 1)
    phc<-phc[-1]
    colnames(phc)<-"facility"
    drop<-c("X__1","X__3")
    #filter useless columns
    data2<-data1[,!(names(data1) %in% drop)]
    data2<-as_tibble(data2)
    cause10.1<-data2[2:18,]
    cause10.1.13<-data2[22:23,]
    cause10.2<-data2[25:26,]
    cause10.3.1<-data2[32:35,]
    cause10.4<-data2[41:43,]
    causeM11<-data2[46:48,]
    causeM12<-data2[50:58,]
    colnames(cause10.1)<-c('vaccine',"Apr-2015","May-2015",	'Jun-2015',	'Jul-2015',	'Aug-2015',	'Sep-2015',	'Oct-2015',	'Nov-2015'	,'Dec-2015	','Jan-2016'	,'Feb-2016',	'Mar-2016	','Total')
    colnames(cause10.1.13)<-c('population',"Apr-2015","May-2015",	'Jun-2015',	'Jul-2015',	'Aug-2015',	'Sep-2015',	'Oct-2015',	'Nov-2015'	,'Dec-2015	','Jan-2016'	,'Feb-2016',	'Mar-2016	','Total')
    colnames(cause10.2)<-c('vaccine',"Apr-2015","May-2015",	'Jun-2015',	'Jul-2015',	'Aug-2015',	'Sep-2015',	'Oct-2015',	'Nov-2015'	,'Dec-2015	','Jan-2016'	,'Feb-2016',	'Mar-2016	','Total')
    colnames(cause10.3.1)<-c('population',"Apr-2015","May-2015",	'Jun-2015',	'Jul-2015',	'Aug-2015',	'Sep-2015',	'Oct-2015',	'Nov-2015'	,'Dec-2015	','Jan-2016'	,'Feb-2016',	'Mar-2016	','Total')
    colnames(cause10.4)<-c('schedule',"Apr-2015","May-2015",	'Jun-2015',	'Jul-2015',	'Aug-2015',	'Sep-2015',	'Oct-2015',	'Nov-2015'	,'Dec-2015	','Jan-2016'	,'Feb-2016',	'Mar-2016	','Total')
    colnames(causeM11)<-c('vaccine',"Apr-2015","May-2015",	'Jun-2015',	'Jul-2015',	'Aug-2015',	'Sep-2015',	'Oct-2015',	'Nov-2015'	,'Dec-2015	','Jan-2016'	,'Feb-2016',	'Mar-2016	','Total')
    colnames(causeM12)<-c('diseases',"Apr-2015","May-2015",	'Jun-2015',	'Jul-2015',	'Aug-2015',	'Sep-2015',	'Oct-2015',	'Nov-2015'	,'Dec-2015	','Jan-2016'	,'Feb-2016',	'Mar-2016	','Total')
    cause10.1<-cbind(phc,cause10.1)
    cause10.1.13<-cbind(phc,cause10.1.13)
    cause10.2<-cbind(phc,cause10.2)
    cause10.3.1<-cbind(phc,cause10.3.1)
    cause10.4<-cbind(phc,cause10.4)
    causeM11<-cbind(phc,causeM11)
    causeM12<-cbind(phc,causeM12)
    dfcause10.1<-rbind(cause10.1,dfcause10.1)
    dfcause10.1.13<-rbind(cause10.1.13,dfcause10.1.13)
    dfcause10.2<-rbind(cause10.2,dfcause10.2)
    dfcause10.3.1<-rbind(cause10.3.1,dfcause10.3.1)
    dfcause10.4<-rbind(cause10.4,dfcause10.4)
    dfcauseM11<-rbind(causeM11,dfcauseM11)
    dfcauseM12<-rbind(causeM12,dfcauseM12)
  }
  write.csv(dfcause10.1,file="/home/rochan/socialcops/CHILD IMMUNIZATION/Number of Infants 0 to 11 months old who received the following.csv")
  write.csv(dfcause10.1.13,file="/home/rochan/socialcops/CHILD IMMUNIZATION/Total number of children aged between 9 and 11 months who have been fully immunised (BCG+DPT123+OPV123+Measles) during the month.csv")
  write.csv(dfcause10.2,file="/home/rochan/socialcops/CHILD IMMUNIZATION/Number of children more than 16 months who received the following.csv")
  write.csv(dfcause10.3.1,file="/home/rochan/socialcops/Immunisation Status/Total number of children aged between 12 and 23 months who have been fully immunised (BCG+DPT123+OPV123+Measles) during the month.csv")
  write.csv(dfcause10.4,file="/home/rochan/socialcops/Immunisation Status/Number of Immunisation sessions during the month.csv")
  write.csv(dfcauseM11,file="/home/rochan/socialcops/vitaA/Number of Vitamin A doses.csv")
  write.csv(dfcauseM12,file="/home/rochan/socialcops/Disease/Number of cases of Childhood Diseases reported during the month0-5 years.csv")
  
}
immunization_disease_structure(nagpur.files)
###################################################################################################
#extracting infant and adult death case and districts
deaths_structure<-function(files){
  dfcause17.2<-data.frame()
  dfcause17.3<-data.frame()
  dfcause17.4<-data.frame()
  dfcause17.2<-as_tibble(dfcause17.2)
  dfcause17.3<-as_tibble(dfcause17.3)
  dfcause17.4<-as_tibble(dfcause17.4)
  for(i in files){
    #get relevant data
    data1<-read_xls(i,skip = 422,sheet = 1,col_names = F)
    phc<-read_excel(i,skip = 3,sheet = 1,col_names = F,range = "B4:C4",n_max = 1)
    phc<-phc[-1]
    colnames(phc)<-"facility"
    drop<-c("X__1")
    #filter useless columns
    data2<-data1[,!(names(data1) %in% drop)]
    data2<-as_tibble(data2)
    data3<-data2[1]%>%
      do(na.locf(.))
    data2<-data2[-1]
    data2<-cbind(data3,data2)
    cause17.2<-data2[1:12,]
    cause17.3<-data2[14:28,]
    cause17.4<-data2[30:61,]
    colnames(cause17.2)<-c('disease',"age","Apr-2015","May-2015",	'Jun-2015',	'Jul-2015',	'Aug-2015',	'Sep-2015',	'Oct-2015',	'Nov-2015'	,'Dec-2015	','Jan-2016'	,'Feb-2016',	'Mar-2016	','Total')
    colnames(cause17.3)<-c('disease',"age","Apr-2015","May-2015",	'Jun-2015',	'Jul-2015',	'Aug-2015',	'Sep-2015',	'Oct-2015',	'Nov-2015'	,'Dec-2015	','Jan-2016'	,'Feb-2016',	'Mar-2016	','Total')
    colnames(cause17.4)<-c('disease',"age","Apr-2015","May-2015",	'Jun-2015',	'Jul-2015',	'Aug-2015',	'Sep-2015',	'Oct-2015',	'Nov-2015'	,'Dec-2015	','Jan-2016'	,'Feb-2016',	'Mar-2016	','Total')
    cause17.2<-cbind(phc,cause17.2)
    cause17.3<-cbind(phc,cause17.3)
    cause17.4<-cbind(phc,cause17.4)
    dfcause17.2<-rbind(cause17.2,dfcause17.2)
    dfcause17.3<-rbind(cause17.3,dfcause17.3)
    dfcause17.4<-rbind(cause17.4,dfcause17.4)
  }
  dfcause17.2<-dfcause17.2%>%
    filter(age!="Total")
  dfcause17.3<-dfcause17.3%>%
    filter(age!="Total")
  dfcause17.4<-dfcause17.4%>%
    filter(age!="Total")
  write.csv(dfcause17.2,file="/home/rochan/Desktop/socialcops/Infant_deaths/Infant Deaths up to 4 weeks by cause.csv")
  write.csv(dfcause17.3,file="/home/rochan/Desktop/socialcops/Infant_deaths/Infant_Child Deaths up to 5 years by cause.csv")
  write.csv(dfcause17.4,file="/home/rochan/Desktop/socialcops/Adult_deaths/Adolescent_Adult deaths by cause.csv")
}
deaths_structure(nagpur.files)
#####################################################################################################
#extracting total births,infants weight and post-natal care
births_structure<-function(files){
  dfcause4.1<-data.frame()
  dfcause4.2<-data.frame()
  dfcauseM6<-data.frame()
  dfcause4.1<-as_tibble(dfcause4.1)
  dfcause4.2<-as_tibble(dfcause4.2)
  dfcauseM6<-as_tibble(dfcauseM6)
  for(i in files){
    #get relevant data
    data1<-read_xls(i,skip = 55,sheet = 1,col_names = F)
    phc<-read_excel(i,skip = 3,sheet = 1,col_names = F,range = "B4:C4",n_max = 1)
    phc<-phc[-1]
    colnames(phc)<-"facility"
    drop<-c("X__1","X__3")
    #filter useless columns
    data2<-data1[,!(names(data1) %in% drop)]
    data2<-as_tibble(data2)
    cause4.1<-data2[1:5,]
    cause4.2<-data2[7:8,]
    causeM6<-data2[24:25,]
    colnames(cause4.1)<-c("X__2","Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total')
    colnames(cause4.2)<-c("X__2","Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total')
    colnames(causeM6)<-c("X__2","Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total')
    cause4.1<-cbind(phc,cause4.1)
    cause4.2<-cbind(phc,cause4.2)
    causeM6<-cbind(phc,causeM6)
    dfcause4.1<-rbind(cause4.1,dfcause4.1)
    dfcause4.2<-rbind(cause4.2,dfcause4.2)
    dfcauseM6<-rbind(causeM6,dfcauseM6)
  }
  write.csv(dfcause4.1,file="/home/rochan/Desktop/socialcops/Birth/boygirl_count.csv")
  write.csv(dfcause4.2,file="/home/rochan/Desktop/socialcops/Birth/babyweight.csv")
  write.csv(dfcauseM6,file="/home/rochan/Desktop/socialcops/Birth/mother_checkup.csv")
}
births_structure(nagpur.files)
###################################################################################################
#jsy contribution data extraction
jsy.structure<-function(files){
  dffinal<-data.frame()
  dffinal<-as_tibble(dffinal)
  for(i in files){
    #get relevant data
    data1<-read_xls(i,skip = 27,sheet = 1,col_names = F)
    phc<-read_excel(i,skip = 3,sheet = 1,col_names = F,range = "B4:C4",n_max = 1)
    line<-data1[1:1,2]
    data1<-data1[-1:-1,]
    phc<-phc[-1]
    colnames(phc)<-"facility"
    drop<-c("X__1","X__3")
    #filter useless columns
    data2<-data1[,!(names(data1) %in% drop)]
    data2<-as_tibble(data2)
    cause4.1<-data2[1:7,]
    cause4.1<-cbind(line,cause4.1)
    colnames(cause4.1)<-c("join1","join2","Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total')
    cause4.1<-unite(cause4.1,cause,join1,join2,sep = " " )
    line2<-data2[8:8,1]
    cause4.2<-data2[9:11,]
    cause4.2<-cbind(line2,cause4.2)
    colnames(cause4.2)<-c("join1","join2","Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total')
    cause4.2<-unite(cause4.2,cause,join1,join2,sep = " " )
    line3<-data2[13:13,1]
    cause4.3<-data2[14:16,]
    cause4.3<-cbind(line3,cause4.3)
    colnames(cause4.3)<-c("join1","join2","Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total')
    cause4.3<-unite(cause4.3,cause,join1,join2,sep = " " )
    cause4.4<-data2[12:12,]
    colnames(cause4.4)<-c("cause","Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total')
    dffinal<-rbind(cause4.1,cause4.2,cause4.3,cause4.4,dffinal)
    #dffinal<-cbind(phc,dffinal)
  }
  return(dffinal)
}
jsy.nagpur<-jsy.structure(nagpur.files)
write.csv(jsy.nagpur,"/home/rochan/Desktop/socialcops/jsy/jsy.csv")