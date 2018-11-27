library(dplyr)
library(tidyr)
# this structure failed the visualization, i planned to plot trend.   
# Diarrhoeal.diseases<-adult.deaths%>%
#   filter(disease=="Diarrhoeal diseases",Total>0)
# 
# Respiratory.diseases<-adult.deaths%>%
#   filter(disease=="Respiratory diseases including infections (other than TB)",Total>0)
# 
# Malaria<-adult.deaths%>%
#   filter(disease=="Malaria",Total>0)
# 
# Other.Fever<-adult.deaths%>%
#   filter(disease=="Other Fever Related",Total>0)
# 
# HIV_AIDS<-adult.deaths%>%
#   filter(disease=="HIV/AIDS",Total>0)
# 
# Heart<-adult.deaths%>%
#   filter(disease=="Heart disease/Hypertension related",Total>0)
# 
# Neurological.disease<-adult.deaths%>%
#   filter(disease=="Neurological disease including strokes",Total>0)

# ignoring Diarrhoeal diseases,Malaria,HIV/AIDS because no cases found
# write.csv(Respiratory.diseases,"/home/rochan/Desktop/socialcops/Adult_deaths/filtered/Respiratorydiseases.csv")
# write.csv(Other.Fever,"/home/rochan/Desktop/socialcops/Adult_deaths/filtered/OtherFever.csv")
# write.csv(Heart,"/home/rochan/Desktop/socialcops/Adult_deaths/filtered/Heart.csv")
# write.csv(Neurological.disease,"/home/rochan/Desktop/socialcops/Adult_deaths/filtered/Neurologicaldisease.csv")
adultdeath.structure<-function(df,bimari,umar){
  newdf<-data.frame()
  for(x in bimari){
    for (y in umar) {
      disease<-adult.deaths%>%
        filter(disease==x,age==y)
      colnames(disease)
      ndisease<-summarise(disease,Apr=sum(Apr.2015),May=sum(May.2015),Jun=sum(Jun.2015),Jul=sum(Jul.2015),Aug=sum(Aug.2015),Sep=sum(Sep.2015),Oct=sum(Oct.2015),Nov=sum(Nov.2015),Dec=sum(Dec.2015.),Jan=sum(Jan.2016),Feb=sum(Feb.2016),Mar=sum(Mar.2016.),total=sum(Total))
      ndisease<-data.frame(disease[1:1,3:4],ndisease)
      newdf<-rbind(ndisease,newdf)
    }
  }
  return(newdf)
}

adult.deaths<-read.csv("/home/rochan/Desktop/socialcops/Adult_deaths/Adolescent_Adult deaths by cause.csv")
bimari<-levels(adult.deaths$disease)
umar<-levels(adult.deaths$age)
adult.deaths<-adultdeath.structure(adult.deaths,bimari,umar)
write.csv(adult.deaths,"/home/rochan/Desktop/socialcops/Adult_deaths/filtered/adultdeathscombined.csv")

################################################################################################
#Aggregating data "Number of children more than 16 months who received the following.csv" and "Number of Infants 0 to 11 months old who received the following.csv" 
#immunization<-read.csv("/home/rochan/Desktop/socialcops/CHILD IMMUNIZATION/Number of children more than 16 months who received the following.csv")
immunization<-read.csv("/home/rochan/Desktop/socialcops/CHILD IMMUNIZATION/Number of Infants 0 to 11 months old who received the following.csv")

wh<-immunization%>%
  filter(grepl("WH",facility))
wh.sum<-summarise(wh,Apr=sum(Apr.2015),May=sum(May.2015),Jun=sum(Jun.2015),Jul=sum(Jul.2015),Aug=sum(Aug.2015),Sep=sum(Sep.2015),Oct=sum(Oct.2015),Nov=sum(Nov.2015),Dec=sum(Dec.2015.),Jan=sum(Jan.2016),Feb=sum(Feb.2016),Mar=sum(Mar.2016.),total=sum(Total))
wh.sum[14]="WH"
wh.sum<-as_tibble(wh.sum)

sc<-immunization%>%
  filter(grepl("SC",facility))
sc.sum<-summarise(sc,Apr=sum(Apr.2015),May=sum(May.2015),Jun=sum(Jun.2015),Jul=sum(Jul.2015),Aug=sum(Aug.2015),Sep=sum(Sep.2015),Oct=sum(Oct.2015),Nov=sum(Nov.2015),Dec=sum(Dec.2015.),Jan=sum(Jan.2016),Feb=sum(Feb.2016),Mar=sum(Mar.2016.),total=sum(Total))
sc.sum[14]="SC"
sc.sum<-as_tibble(sc.sum)

phc<-immunization%>%
  filter(grepl("PHC",facility))
phc.sum<-summarise(phc,Apr=sum(Apr.2015),May=sum(May.2015),Jun=sum(Jun.2015),Jul=sum(Jul.2015),Aug=sum(Aug.2015),Sep=sum(Sep.2015),Oct=sum(Oct.2015),Nov=sum(Nov.2015),Dec=sum(Dec.2015.),Jan=sum(Jan.2016),Feb=sum(Feb.2016),Mar=sum(Mar.2016.),total=sum(Total))
phc.sum[14]="PHC"
phc.sum<-as_tibble(phc.sum)

new.aggregated<-rbind(wh.sum,sc.sum,phc.sum)
#write.csv(new.aggregated,"/home/rochan/Desktop/socialcops/CHILD IMMUNIZATION/aggregated/dpt_opv_morethan_16months.csv")
write.csv(new.aggregated,"/home/rochan/Desktop/socialcops/CHILD IMMUNIZATION/aggregated/dpt_opv_btw_0to11months.csv")
###############################################################################################################3
#Aggregating data Number of children more than 16 months who received the following.csv 
immunize.aggregate<-function(l,datadf,filteron){
  df<-data.frame()
  df<-as_tibble(df)
  for (x in l){
    bcg<-datadf%>%
      filter(filteron==x,Total>0)
    bcg.sum<-summarise(bcg,Apr=sum(Apr.2015),May=sum(May.2015),Jun=sum(Jun.2015),Jul=sum(Jul.2015),Aug=sum(Aug.2015),Sep=sum(Sep.2015),Oct=sum(Oct.2015),Nov=sum(Nov.2015),Dec=sum(Dec.2015.),Jan=sum(Jan.2016),Feb=sum(Feb.2016),Mar=sum(Mar.2016.))
    len<-length(bcg.sum)
    bcg.sum[len+1]=x
    bcg.sum<-as_tibble(bcg.sum)
    df<-rbind(df,bcg.sum)
  }
  return(df)
}
immunization<-read.csv("/home/rochan/Desktop/socialcops/CHILD IMMUNIZATION/Number of Infants 0 to 11 months old who received the following.csv")
vaccinenm<-levels(immunization$vaccine)
immunize.df<-immunize.aggregate(vaccinenm,immunization,immunization$vaccine)
#write.csv(immunize.df,"/home/rochan/Desktop/socialcops/CHILD IMMUNIZATION/aggregated/immunizebyvaccine.csv")
#some more structuring:
immunize.newstruct<-function(name,df){
  months<-c("Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	')
  newdf<-data.frame(months)
  for (x in row.names(vaccine.struct)){
    gdf<-gather(data = vaccine.struct[x:x,1:12],key = months,value =x)
    newdf<-cbind(newdf,gdf[2])
  } 
  return(newdf)
}
vaccine.struct<-read.csv("/home/rochan/Desktop/socialcops/CHILD IMMUNIZATION/aggregated/immunizebyvaccine.csv")
vaccine.struct<-vaccine.struct[-1]
colnames(vaccine.struct)<-c("Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Vaccine')
vaccine.name<-vaccine.struct$Vaccine
other.vaccines<-immunize.newstruct(vaccine.name,vaccine.struct)
months<-other.vaccines[1]
other.vaccines<-other.vaccines[-1]
colnames(other.vaccines)<-vaccine.name
other.vaccines<-cbind(months,other.vaccines)
write.csv(other.vaccines,"/home/rochan/Desktop/socialcops/CHILD IMMUNIZATION/aggregated/immunizebyvaccine2.csv")
#######################################################################################################3
disease<-read.csv("/home/rochan/Desktop/socialcops/Disease/Number of cases of Childhood Diseases reported during the month0-5 years.csv")
disease.names<-levels(disease$diseases)
disease.df<-immunize.aggregate(disease.names,disease,disease$diseases)
write.csv(disease.df,"/home/rochan/Desktop/socialcops/Disease/aggregation/count_diseases.csv")
###########################################################################################################
immune12to23<-function(l,datadf,filteron){
  df<-data.frame()
  df<-as_tibble(df)
  for (x in l){
    bcg<-datadf%>%
      filter(filteron==x,Total>0)
    bcg.sum<-summarise(bcg,category=x,total=sum(Total))
    bcg.sum<-as_tibble(bcg.sum)
    df<-rbind(df,bcg.sum)
  }
  return(df)
}
all.immune<-read.csv("/home/rochan/Desktop/socialcops/Immunisation Status/Total number of children aged between 12 and 23 months who have been fully immunised (BCG+DPT123+OPV123+Measles) during the month.csv")
all.names<-levels(all.immune$population)
all.names<-all.names[-4]
all.immune.df<-immune12to23(all.names,all.immune,all.immune$population)
write.csv(all.immune.df,"/home/rochan/Desktop/socialcops/Immunisation Status/aggregated/fullimmune.csv")
###############################################################################################################
deaths<-function(l,datadf,filteron){
  df<-data.frame()
  df<-as_tibble(df)
  for (x in l){
    bcg<-datadf%>%
      filter(filteron==x,Total>0)
    bcg.sum<-summarise(bcg,category=x,total=sum(Total))
    bcg.sum<-as_tibble(bcg.sum)
    df<-rbind(df,bcg.sum)
  }
  return(df)
}
deathupto5yr<-read.csv("/home/rochan/Desktop/socialcops/Infant_deaths/Infant_Child Deaths up to 5 years by cause.csv")
deathupto5yr.names<-levels(deathupto5yr$disease)
deaths.df<-deaths(deathupto5yr.names,deathupto5yr,deathupto5yr$disease)
write.csv(deaths.df,"/home/rochan/Desktop/socialcops/Infant_deaths/aggregated/deathsupto5.csv")

deathupto4weeks<-read.csv("/home/rochan/Desktop/socialcops/Infant_deaths/Infant Deaths up to 4 weeks by cause.csv")
deathupto4weeks.names<-levels(deathupto4weeks$disease)
deaths4.df<-deaths(deathupto4weeks.names,deathupto4weeks,deathupto4weeks$disease)
write.csv(deaths4.df,"/home/rochan/Desktop/socialcops/Infant_deaths/aggregated/deathsupto4weeks.csv")
####################################################################################################
dose<-read.csv("/home/rochan/Desktop/socialcops/vitaA/Number of Vitamin A doses.csv")
doses.names<-levels(dose$vaccine)
dose.df<-deaths(doses.names,dose,dose$vaccine)
write.csv(dose.df,"/home/rochan/Desktop/socialcops/vitaA/aggregated/doses.csv")
###########################################################################################
preg<-read.csv("/home/rochan/Desktop/socialcops/pregnant/pregnant_combined.csv")

preg.sum<-summarise(preg,JSY_new_registered=sum(New.women.registered.under.JSY),hb_below_11=sum(Number.having.Hb.level.11..tested.cases.),hb_below_7=sum(Number.having.severe.anaemia..Hb.7..treated.at.institution),
                    ANCcheckups_3=sum(Number.of.pregnant.women.received.3.ANC.check.ups),first_trimester=sum(Of.which.Number.registered.within.first.trimester),IFAtablets=sum(Total.number.of.pregnant.women.given.100.IFA.tablets),Registered_ANC=sum(Total.number.of.pregnant.women.Registered.for.ANC),TT1=sum(TT1),TT2orBooster=sum(TT2.or.Booster)
                    ,Eclampsia_cases=sum(Number.of.Eclampsia.cases.managed.during.delivery))
write.csv(preg.sum,"/home/rochan/Desktop/socialcops/pregnant/aggregated/preg.csv")
###########################################################################################
# structuring and aggregating nagpur maternal death data and other districts data
mortality<-read.csv("/home/rochan/Desktop/socialcops/mortality/mortality_combined.csv")
mortality.causes<-colnames(mortality)
filtered<-mortality%>%
  filter(mortality$district=="NAGPUR")

mortality.nagpur.sum<-summarise(filtered,Abortion=sum(Abortion.Total),AnimalBites=sum(Animal.bites.and.stings.Total),
                         Bleeding=sum(Bleeding.Total),UnknownCause=sum(Causes.not.known.Total),Highfever=sum(High.fever.Total),KnownAcuteDisease=sum(Known.Acute.Disease.Total),
                         KnownChronicDisease=sum(Known.Chronic.Disease.Total),Obstructedprolongedlabour=sum(Obstructed.prolonged.labour.Total),
                         OtherCauses=sum(Other.Causes..including.causes.not.known..Total),Severehypertesnionfits=sum(Severe.hypertesnion.fits.Total),
                         Suicide=sum(Suicide.Total),TraumaAccidentsBurncases=sum(Trauma.Accidents.Burn.cases.Total))

filtered<-mortality%>%
  filter(mortality$district!="NAGPUR")


mortality.all.sum<-summarise(filtered,Abortion=sum(Abortion.Total),AnimalBites=sum(Animal.bites.and.stings.Total),
                                Bleeding=sum(Bleeding.Total),UnknownCause=sum(Causes.not.known.Total),Highfever=sum(High.fever.Total),KnownAcuteDisease=sum(Known.Acute.Disease.Total),
                                KnownChronicDisease=sum(Known.Chronic.Disease.Total),Obstructedprolongedlabour=sum(Obstructed.prolonged.labour.Total),
                                OtherCauses=sum(Other.Causes..including.causes.not.known..Total),Severehypertesnionfits=sum(Severe.hypertesnion.fits.Total),
                                Suicide=sum(Suicide.Total),TraumaAccidentsBurncases=sum(Trauma.Accidents.Burn.cases.Total))
mortality.sum<-rbind(mortality.nagpur.sum,mortality.all.sum)
area<-data.frame(Area=c("NAGPUR","ALL"))
mortality.sum<-cbind(area,mortality.sum)
write.csv(mortality.sum,"/home/rochan/Desktop/socialcops/mortality/aggregated/compare_nag_all.csv")
########################################################################################################
#more structuring birth data
weightandmothercheckup<-function(df,type1,type2){
  
  newbirth<-df%>%
    filter(grepl(type1,X__2))
  newbirth.smm<-summarise(newbirth,Apr=sum(Apr.2015),May=sum(May.2015),Jun=sum(Jun.2015),Jul=sum(Jul.2015),Aug=sum(Aug.2015),Sep=sum(Sep.2015),Oct=sum(Oct.2015),Nov=sum(Nov.2015),Dec=sum(Dec.2015.),Jan=sum(Jan.2016),Feb=sum(Feb.2016),Mar=sum(Mar.2016.),total=sum(Total))
  
  newbirthbelow2.5<-df%>%
    filter(grepl(type2,X__2))
  newbirthbelow2.5.smm<-summarise(newbirthbelow2.5,Apr=sum(Apr.2015),May=sum(May.2015),Jun=sum(Jun.2015),Jul=sum(Jul.2015),Aug=sum(Aug.2015),Sep=sum(Sep.2015),Oct=sum(Oct.2015),Nov=sum(Nov.2015),Dec=sum(Dec.2015.),Jan=sum(Jan.2016),Feb=sum(Feb.2016),Mar=sum(Mar.2016.),total=sum(Total))
  
  final<-rbind(data.frame(newbirth.smm),
               data.frame(newbirthbelow2.5.smm))
  
  colnames(final)<-c("Apr/2015","May/2015","Jun/2015","Jul/2015","Aug/2015","Sep/2015","Oct/2015","Nov/2015","Dec/2015","Jan/2016","Feb/2016","Mar/2016","Total")
  
  cause4.2.1<-gather(data = final[1:1,1:13],key = c("Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total'),value = "X__2")
  cause4.2.2<-gather(data = final[2:2,1:13],key = c("Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total'),value = "X__2")
  final<-cbind(cause4.2.1,cause4.2.2)  
  final<-final[-3]
  colnames(final)<-c("Months",type1,type2)  
  return(final)
}
weight<-read.csv("/home/rochan/Desktop/socialcops/Birth/babyweight.csv")
weight<-weightandmothercheckup(weight,type1="Number of Newborns weighed at birth",type2="Number of Newborns having weight less than 2.5 kg")
percent<-cbind(data.frame(percentunderweight=(weight$`Number of Newborns having weight less than 2.5 kg`/weight$`Number of Newborns weighed at birth`)*100))
final<-cbind(weight,percent)
write.csv(final,"/home/rochan/Desktop/socialcops/Birth/aggregated/weight.csv")

checkup<-read.csv("/home/rochan/Desktop/socialcops/Birth/mother_checkup.csv")
checkup<-weightandmothercheckup(checkup,type1="Women receiving post partum check-up within 48 hours after delivery",type2="Women getting a post partum check up between 48 hours and 14 days")
write.csv(checkup,"/home/rochan/Desktop/socialcops/Birth/aggregated/checkup.csv")


cause4.1<-read.csv("/home/rochan/Desktop/socialcops/Birth/boygirl_count.csv")
cause4.1.male<-gather(data = cause4.1[1:1,4:16],key = c("Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total'),value = "X__2")
cause4.1.female<-gather(data = cause4.1[2:2,4:16],key = c("Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total'),value = "X__2")
cause4.1.mf<-gather(data = cause4.1[3:3,4:16],key = c("Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total'),value = "X__2")
cause4.1.still<-gather(data = cause4.1[4:4,4:16],key = c("Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total'),value = "X__2")
cause4.1.abort<-gather(data = cause4.1[5:5,4:16],key = c("Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total'),value = "X__2")
cause4.1<-cbind(cause4.1.male,cause4.1.female[2],cause4.1.mf[2],cause4.1.still[2],cause4.1.abort[2])
colnames(cause4.1)<-c("months","Boy","Girl","total_BG","Still_birth","Aborted")
BoytoGirl_ratio=(cause4.1$Boy/cause4.1$Girl)*1000
GirltoBoy_ratio=(cause4.1$Girl/cause4.1$Boy)*1000
cause4.1<-cbind(cause4.1,data.frame(round(BoytoGirl_ratio),round(GirltoBoy_ratio)))
write.csv(cause4.1,"/home/rochan/Desktop/socialcops/Birth/aggregated/BandGcounts.csv")
################################################################
#jsy aggregate and structuring
jsy.aggregate<-function(df,type1,filteron){
  newdf<-data.frame()
  for (x in type1){
    jsyhelp<-df%>%
      filter(filteron==x)
    jsy.summ<-summarise(jsyhelp,cause=x,Apr=sum(Apr.2015),May=sum(May.2015),Jun=sum(Jun.2015),Jul=sum(Jul.2015),Aug=sum(Aug.2015),Sep=sum(Sep.2015),Oct=sum(Oct.2015),Nov=sum(Nov.2015),Dec=sum(Dec.2015.),Jan=sum(Jan.2016),Feb=sum(Feb.2016),Mar=sum(Mar.2016.),total=sum(Total))
    jsy.summ<-data.frame(jsy.summ)
    # cause<-type1
    # cause<-as_data_frame(cause)
    # jsy.summ<-cbind(cause,jsy.summ)
    newdf<-rbind(jsy.summ,newdf)
  }
  return(newdf)
}
jsy<-read.csv("/home/rochan/Desktop/socialcops/jsy/jsy.csv")
cause<-levels(jsy$cause)
cause<-cause[-11]
jsy1<-jsy.aggregate(jsy,cause,jsy$cause)
colnames(jsy1)<-c("JSY contribution","Apr/2015","May/2015",	'Jun/2015',	'Jul/2015',	'Aug/2015',	'Sep/2015',	'Oct/2015',	'Nov/2015'	,'Dec/2015	','Jan/2016'	,'Feb/2016',	'Mar/2016	','Total')
jsy1[1:1,1]="JSY incentive paid to Mothers(Institutional delivery)"
jsy1[2:2,1]="JSY incentive paid to ASHA(Institutional delivery)"
jsy1[3:3,1]="JSY incentive paid to ANM or AWW (Institutional delivery)"
jsy1[4:4,1]="Home Deliveries by: SBA Trained"
jsy1[5:5,1]="discharged under 48 hours"
jsy1[6:6,1]="newborns visited within 24 hours of Home Delivery"
jsy1[7:7,1]="Number of mothers paid JSY incentive for Home deliveries"
jsy1[8:8,1]="Home Deliveries by: Non SBA"
jsy1[9:9,1]="Deliveries conducted at Public Institutions"
jsy1[10:10,1]=" Deliveries conducted at accredited Private Institutions"
jsy1[11:11,1]="cases where JSY incentive paid to Mothers"
jsy1[12:12,1]="cases where JSY incentive paid to Mothers"
jsy1[13:13,1]="cases where JSY incentive paid to ANM or AWW"

write.csv(jsy1,"/home/rochan/Desktop/socialcops/jsy/aggregated/jsy.csv")

