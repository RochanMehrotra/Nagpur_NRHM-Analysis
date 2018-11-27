#conclusions
#1.1
maternal_deaths<-read.csv("/home/rochan/Desktop/socialcops/mortality/aggregated/compare_nag_all.csv")
total=maternal_deaths[1:1,]+maternal_deaths[2:2,]
total[2]="Total"
#nagpur deaths in percent:
nagpur_accounts_to_percent_deaths<-(maternal_deaths[1:1,3:14]/total[1:1,3:14])*100

#1.2
preg<-read.csv("/home/rochan/Desktop/socialcops/pregnant/aggregated/preg.csv")
#perent of women regitered in trimester
preg$first_trimester/preg$Registered_ANC*100

#1.4
sex_ratio<-read.csv("/home/rochan/Desktop/socialcops/Birth/aggregated/BandGcounts.csv")
#sex ratios
sex_ratio[13:13,8:9]

#2.1
women_health<-read.csv("/home/rochan/Desktop/socialcops/pregnant/aggregated/preg.csv")
#Percentage of pregnant women having HB<11  
women_health$hb_below_11/women_health$Registered_ANC*100
# Percentage of pregnant women having HB<7  
women_health$hb_below_7/women_health$Registered_ANC*100
# Percentage of pregnant women had 3 ANC checkup  
women_health$ANCcheckups_3/women_health$Registered_ANC*100
# Percentage of pregnant women consuming IFA tablets   
women_health$IFAtablets/women_health$Registered_ANC*100
# Percentage of pregnant women vaccinated by TT1  
women_health$TT1/women_health$Registered_ANC*100
# Percentage of pregnant women vaccinated by TT2 or Booster 
women_health$TT2orBooster/women_health$Registered_ANC*100

postnatal<-read.csv("/home/rochan/Desktop/socialcops/Birth/aggregated/checkup.csv")
#percent of Women receiving post partum check-up within 48 hours after delivery
postnatal[13:13,3]/(sex_ratio[13:13,5]+151)*100
#percent of  Women getting a post partum check up between 48 hours and 14 days
postnatal[13:13,4]/(sex_ratio[13:13,5]+151)*100


weight<-read.csv("/home/rochan/Desktop/socialcops/Birth/aggregated/weight.csv")
#percentage of new born having weight <2.5kg
weight[13:13,5]
dose<-read.csv("/home/rochan/Desktop/socialcops/vitaA/aggregated/doses.csv")
#percent of infants getting Dose1 and dose2:
dose$total[2]/dose$total[1]*100
#percent of infants getting Dose1 dose2 and dose 3:
dose$total[3]/dose$total[1]*100
#percent of infants getting Dose2  and dose 3:
dose$total[3]/dose$total[2]*100
