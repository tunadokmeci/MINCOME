library(haven)
library ("readr")
library(dplyr)
library("tidyr")
library("DAMisc")
#increases in the number of children 
basepaypanel <- read_dta("Downloads/basepaypanel_revised.dta")
basepaypanel[basepaypanel == -9] <- NA
basepaypanel[basepaypanel == -7] <- NA
basepaypanel[basepaypanel == -1] <- NA
basepaypanel[basepaypanel == "."] <- NA
basepaypanel$individual <- 0 
basepaypanel$individual[basepaypanel$DH == 0 & basepaypanel$SH == 0] <- 1

#compute the number of children from previous period to see 
#if there has been an increase 

basepaypanel <- basepaypanel %>%
  group_by(FamNum) %>%
  arrange(month)  %>%
  mutate(prevnrch = dplyr::lag(CH, 1))  %>%
  ungroup()%>%
  arrange(FamNum, month)

basepaypanel$increase = 0 
basepaypanel$increase[basepaypanel$prevnrch < basepaypanel$CH] <- 1

#get the information on the treatment cell of the household

basepaypanel$AC <- as.character(basepaypanel$AC)
basepaypanel$plan <- substr(basepaypanel$AC, 1, 1)
basepaypanel$plan <- as.factor(basepaypanel$plan)

#by eliminating the NAs, we are removing the months for each household 
#where they were not enrolled in the experiment 
basepaypanel <- basepaypanel[!is.na(basepaypanel$plan),]

#month1 will then be 1 if that is the month where the family has begun 
#the experiment
basepaypanel <- basepaypanel %>%
  group_by(FamNum) %>%
  arrange(month)  %>%
  mutate(month1 = row_number())  %>%
  ungroup()%>%
  arrange(FamNum, month)


faminfo <- subset(basepaypanel, select=c("FamNum","FamSizex100", "month1", "CS", "CR"))
faminfo <- faminfo[which(faminfo$month1 ==1),]
faminfo <- faminfo %>% rename(FAMSI="FamSizex100", FAGE= "CS", MAGE = "CR")
faminfo <- faminfo[,-3]
#have a dummy that indicates if there has been any increases

basepaypanel$sum = 0

basepaypanel$FamNum <- as.factor(basepaypanel$FamNum)
for (i in levels(basepaypanel$FamNum)){
  s <- sum(basepaypanel[basepaypanel$FamNum == i, "increase"])
  basepaypanel$sum[basepaypanel$FamNum == i] <- s
}

basepaypanel$if_increase = 0   
basepaypanel$if_increase[basepaypanel$sum != 0] <- 1

#Plan 6 was merged with plan 7 at some point
basepaypanel$plan[basepaypanel$plan == 6] <- 7

#have a control dummy, 0 for treated, 1 for control group 
basepaypanel$control = 0 
basepaypanel$control[basepaypanel$plan == 9] <- 1


#there are families whose treament plan changes 
#and even whether they are in control group or not 
basepaypanel$changetreatment = 0 

basepaypanel <- basepaypanel %>%
  group_by(FamNum) %>%
  arrange(month)  %>%
  mutate(prevtreat = dplyr::lag(plan, 1))  %>%
  ungroup()%>%
  arrange(FamNum, month)

basepaypanel$changetreatment[basepaypanel$prevtreat !=basepaypanel$plan] = 1 

change <- basepaypanel[which(basepaypanel$changetreatment == 1),]


#let's remove these families 
basepaypanel$summ = 0

basepaypanel$FamNum <- as.factor(basepaypanel$FamNum)
for (i in levels(basepaypanel$FamNum)){
  s <- sum(basepaypanel[basepaypanel$FamNum == i, "changetreatment"])
  basepaypanel$summ[basepaypanel$FamNum == i] <- s
}

basepaypanel$if_change = 0   
basepaypanel$if_change[basepaypanel$summ != 0] <- 1

basepaypanel<-merge(basepaypanel, faminfo, by = "FamNum", all=T)

basepaypanel <- basepaypanel %>%
  group_by(FamNum) %>%
  arrange(month)  %>%
  mutate(prevtreat = dplyr::lag(plan, 1))  %>%
  ungroup()%>%
  arrange(FamNum, month)

basepaypanel$changetreatment[basepaypanel$prevtreat !=basepaypanel$plan] = 1 

basepaypanel_rem <- basepaypanel[which(basepaypanel$AC != 0),]

basepaypanel_rem <- basepaypanel_rem[which(basepaypanel_rem$if_change == 0),]

basepaypanel_winni <- basepaypanel_rem[which(basepaypanel_rem$SiteCode == 1),]

basepaypanel_winni$year <- 0 
basepaypanel_winni$year[basepaypanel_winni$month < 12] <- 1975
basepaypanel_winni$year[basepaypanel_winni$month >= 12 & basepaypanel_winni$month < 24] <- 1976
basepaypanel_winni$year[basepaypanel_winni$month >= 24 & basepaypanel_winni$month <= 37] <- 1977

#organising the data in women´s fertility histories with each row corresponding to a year between ages 15 and 50 
famdata_ind <- read_dta("Downloads/famdata_ind.dta")
famdata_ind[famdata_ind == -4] <- NA
famdata_ind <- famdata_ind[, c(1:6)]
famdata_ind <- famdata_ind[!(is.na(famdata_ind$OID)), ]
famdata_ind <- famdata_ind[famdata_ind$
                             RTH == 2 | famdata_ind$RTH == 4 | famdata_ind$RTH == 5, ]

famdata_ind$BIRTH <- as.character(famdata_ind$BIRTH)
famdata_ind$birthyear <- substr(famdata_ind$BIRTH, 1, 2)
famdata_ind$birthday <- substr(famdata_ind$BIRTH, 3, 6)
famdata_ind$birthyear <- paste("19", famdata_ind$birthyear, sep="")


famdata_ind$birthyear <- as.numeric(famdata_ind$birthyear)
famdata_ind <- famdata_ind %>% rename(FamNum = FAMNUM)
famdata_ind <- famdata_ind%>%
  group_by(FamNum) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_firstch = dplyr::lag(birthyear, 1) - birthyear)  %>%
  ungroup()%>%
  arrange(FamNum, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FamNum) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_firstch = dplyr::lag(birthyear, 1) - birthyear)  %>%
  ungroup()%>%
  arrange(FamNum, BIRTH)
famdata_ind <- famdata_ind%>%
  group_by(FamNum) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_secch = dplyr::lag(birthyear, 2) - birthyear)  %>%
  ungroup()%>%
  arrange(FamNum, birthyear)
famdata_ind <- famdata_ind%>%
  group_by(FamNum) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_thirdch = dplyr::lag(birthyear, 3) - birthyear)  %>%
  ungroup()%>%
  arrange(FamNum, birthyear)
famdata_ind <- famdata_ind%>%
  group_by(FamNum) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_fourthch = dplyr::lag(birthyear, 4) - birthyear)  %>%
  ungroup()%>%
  arrange(FamNum, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FamNum) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_fifthch = dplyr::lag(birthyear, 5) - birthyear)  %>%
  ungroup()%>%
  arrange(FamNum, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FamNum) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_sixthch = dplyr::lag(birthyear, 6) - birthyear)  %>%
  ungroup()%>%
  arrange(FamNum, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FamNum) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_seventhch = dplyr::lag(birthyear, 7) - birthyear)  %>%
  ungroup()%>%
  arrange(FamNum, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FamNum) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_eigthch = dplyr::lag(birthyear, 8) - birthyear)  %>%
  ungroup()%>%
  arrange(FamNum, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FamNum) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_ninthch = dplyr::lag(birthyear, 9) - birthyear)  %>%
  ungroup()%>%
  arrange(FamNum, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FamNum) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_tenthch = dplyr::lag(birthyear, 10) - birthyear)  %>%
  ungroup()%>%
  arrange(FamNum, birthyear)

famdata_ind <- famdata_ind[famdata_ind$RTH == 2, ]

age_femid15 <- subset(famdata_ind, select=c("FamNum","OID"))
age_femid15$age <- 15 
age_femid50 <- subset(famdata_ind, select=c("FamNum","OID"))
age_femid50$age <- 50 

age_femid <- rbind(age_femid15, age_femid50)

age_femid <- age_femid %>% dplyr::group_by(FamNum, OID) %>%
  mutate(age = as.numeric(age)) %>%
  complete(age = 1:max(age), fill = list())

famdata_ind<-merge(famdata_ind, age_femid, by = c("FamNum","OID"), all=T)

famdata_ind <- famdata_ind[famdata_ind$age > 14, ]

famdata_ind$event <- 0

famdata_ind$event[famdata_ind$age_firstch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_secch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_thirdch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_fourthch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_fifthch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_sixthch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_seventhch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_eigthch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_ninthch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_tenthch == famdata_ind$age] <- 1

#here we have the births that happened before the experiment start as well 
#as those that were recorded. we need to manually add the increases in the number of children 
#that we computed using baseline 
#payments data but that were absent from the family information data 

#information on basepay panel is monthly and we want to see if there has been an increase in that given year 

basepaypanel_winni$FamNum <- as.factor(basepaypanel_winni$FamNum)
basepaypanel_winni$year <- as.factor(basepaypanel_winni$year)
basepaypanel_winni$increase <- as.numeric(basepaypanel_winni$increase)

for (i in levels(basepaypanel_winni$FamNum)){
  for (j in levels(basepaypanel_winni$year)){
    s <- sum(basepaypanel_winni[basepaypanel_winni$FamNum == i & basepaypanel_winni$year == j, "increase"])
    basepaypanel_winni$summm[basepaypanel_winni$FamNum == i & basepaypanel_winni$year == j] <- s
  }
}

basepaypanel_winni$if_birth = 0   
basepaypanel_winni$if_birth[basepaypanel_winni$summm != 0] <- 1

basepaypanel_winni$plan <- as.numeric(as.character(basepaypanel_winni$plan))
basepaypanel_winni$control <- as.numeric(as.character(basepaypanel_winni$control))

basepaypanel_winni_year <- basepaypanel_winni %>%
  group_by(FamNum, year)%>%
  select(FamNum, if_birth, year, control, plan) %>%
  summarise(if_birth=mean(if_birth), control = mean(control), plan =mean(plan))

famdata_ind$FamNum <- as.factor(as.character(famdata_ind$FamNum))

famdata_ind$year = famdata_ind$birthyear + famdata_ind$age
famdata_ind <- merge(basepaypanel_winni_year, famdata_ind, by =c("FamNum", "year"), all=T)
famdata_ind$year = famdata_ind$birthyear + famdata_ind$age

famdata_ind$event[famdata_ind$if_birth == 1] <- 1
famdata_ind <- famdata_ind%>%
  arrange(OID, age) 

#for all women who reach the age 50 by the end of experiment, we have the full birth history
#for women who are under 50 by the end of experiment, the years after the experiment are censored 

famdata_ind$censored <-0
famdata_ind$censored[famdata_ind$year > 1977] <- 1


#have the control variable be 1 or 0 for all years 
famdata_ind$OID <- as.factor(famdata_ind$OID)

famdata_ind$control <- as.numeric(famdata_ind$control)

for (i in levels(famdata_ind$FamNum)){
  s <- sum(famdata_ind[famdata_ind$FamNum == i, "control"], na.rm=TRUE)
  famdata_ind$sumcontrol[famdata_ind$FamNum == i] <- s
}

famdata_ind$treated = 0   
famdata_ind$treated[famdata_ind$sumcontrol != 0] <- 1

#now we want treated to be 1 only during the experiment, so we create an experiment dummy that takes the value of 1 
#during 1975, 1976, 1977, which we then interact with treated 
famdata_ind$experiment = 0 
famdata_ind$experiment[famdata_ind$year == 1975 | famdata_ind$year == 1976 | famdata_ind$year == 1977] = 1 

#remove censored observations 

famdata_ind_nocens <- famdata_ind[famdata_ind$censored == 0, ]

#add intervals now 
famdata_ind_nocens$int1 = famdata_ind$age_firstch - 15 

famdata_ind_nocens$age1519 <- 0
famdata_ind_nocens$age1519[famdata_ind_nocens$age == 15 | famdata_ind_nocens$age == 16 | famdata_ind_nocens$age == 17 |
                             famdata_ind_nocens$age == 18 | famdata_ind_nocens$age == 19  ] <- 1


famdata_ind_nocens$age2024 <- 0 
famdata_ind_nocens$age1519[famdata_ind_nocens$age == 20 | famdata_ind_nocens$age == 21 | famdata_ind_nocens$age == 22 |
                             famdata_ind_nocens$age == 23 | famdata_ind_nocens$age == 24  ] <- 1


famdata_ind_nocens$age2429 <- 0 
famdata_ind_nocens$age2429[famdata_ind_nocens$age == 24 | famdata_ind_nocens$age == 25 | famdata_ind_nocens$age == 26 |
                             famdata_ind_nocens$age == 27 | famdata_ind_nocens$age == 28  ] <- 1

famdata_ind_nocens$age3034 <- 0 
famdata_ind_nocens$age3034[famdata_ind_nocens$age == 30 | famdata_ind_nocens$age == 31 | famdata_ind_nocens$age == 32 |
                             famdata_ind_nocens$age == 33 | famdata_ind_nocens$age == 34  ] <- 1



famdata_ind_nocens$age3539 <- 0
famdata_ind_nocens$age3539[famdata_ind_nocens$age == 35 | famdata_ind_nocens$age == 36 | famdata_ind_nocens$age == 37 |
                             famdata_ind_nocens$age == 38 | famdata_ind_nocens$age == 39  ] <- 1

famdata_ind_nocens$age4044 <- 0 
famdata_ind_nocens$age4044[famdata_ind_nocens$age == 40 | famdata_ind_nocens$age == 41 | famdata_ind_nocens$age == 42 |
                             famdata_ind_nocens$age == 43 | famdata_ind_nocens$age == 44  ] <- 1 


famdata_ind_nocens$age4550 <- 0 
famdata_ind_nocens$age4550[famdata_ind_nocens$age == 45 | famdata_ind_nocens$age == 46 | famdata_ind_nocens$age == 47 |
                             famdata_ind_nocens$age == 48 | famdata_ind_nocens$age == 49  ] <- 1 

#j is the birth interval, or the spell/episode 

famdata_ind_nocens <- btscs(df=famdata_ind_nocens, event="event", t_var="age", cs_unit="OID", pad_ts = FALSE)
famdata_ind_nocens$birthid <- NA 
famdata_ind_nocens$birthid <- ifelse(famdata_ind_nocens$event == 1, paste(famdata_ind_nocens$OID, famdata_ind_nocens$age, sep=""), NA)

famdata_ind_nocens$birthid <- as.factor(famdata_ind_nocens$birthid)

births <- famdata_ind_nocens[which(famdata_ind_nocens$event == 1),]

births <- births %>%
  group_by(OID) %>%
  arrange(age)  %>%
  mutate(j = row_number() + 1)  %>%
  ungroup()%>%
  arrange(OID, age) 

births <- births %>% select(OID, age, j) 

famdata_ind_nocens <- merge(famdata_ind_nocens, births, by = c("OID", "age"), all=TRUE)

spell = NA
woman = famdata_ind_nocens$OID[1]
for (i in 1:dim(famdata_ind_nocens)[1]){
  if (!is.na(famdata_ind_nocens$birthid[i])){
    spell = famdata_ind_nocens$j[i]
    woman = famdata_ind_nocens$OID[i]
  }
  else if (is.na(famdata_ind_nocens$birthid[i]) && famdata_ind_nocens$OID[i] == woman){
    famdata_ind_nocens$j[i] = spell
  }
}

famdata_ind_nocens$j[is.na(famdata_ind_nocens$j)] <- 1

data_personperiod <- famdata_ind_nocens 

#information on children out of household 

familydata <- read_excel("Downloads/familydata.xlsx")
familydata <- familydata %>%rename(FamNum = 'FAMNUM...1')

familydata <- familydata %>%
  dplyr::select("FamNum", 
                "clbegan", "mrgbegan", "mrgbegan2",
                "mrgstatchng1",  "mrgstatchng2", "mrgstatchng3", 
                "mrgstatchng4",  "mrgstatchng5", "mrgstatchng6", "mrgstatchng7",  
                "mrtstatus1", "mrtstatus2",  "mrtstatus3", 
                "mrtstatus4",  "mrtstatus5",  "mrtstatus6",  "mrtstatus7", 
                "mrtstatus8", "mrtstatus9", "prevmrg", "endprevmrg",  
                "lengthmrg", "lengthmrgprev", "chout", "chage", 
                "datesepcl", "datesepmrg")

familydata[familydata == -9] <- NA
familydata[familydata == -8] <- NA
familydata[familydata == -4] <- NA

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#marriage ends, beginnings, turn the date to a proper year

familydata$clbegan <- substrRight(familydata$clbegan, 2)
familydata$clbegan <-ifelse(!is.na(familydata$clbegan), paste("19", familydata$clbegan, sep=""), paste("NA"))

familydata$mrgbegan <- substrRight(familydata$mrgbegan, 2)
familydata$mrgbegan <-ifelse(!is.na(familydata$mrgbegan), paste("19", familydata$mrgbegan, sep=""), paste(NA))
familydata$mrgbegan2 <- substrRight(familydata$mrgbegan2, 2)
familydata$mrgbegan2 <-ifelse(!is.na(familydata$mrgbegan2), paste("19", familydata$mrgbegan2, sep=""), paste(NA))

familydata[familydata == "NA"] <- NA

#mrgbegan and mrgbegan2 are the same info, just asked to male or female, so let´s combine them 
familydata$mrgbegan <-
  ifelse(!is.na(familydata$mrgbegan), paste(familydata$mrgbegan), paste(familydata$mrgbegan2))

familydata[familydata == "NA"] <- NA

familydata$mrgstatchng1 <- substrRight(familydata$mrgstatchng1, 2)
familydata$mrgstatchng1 <-ifelse(!is.na(familydata$mrgstatchng1), paste("19", familydata$mrgstatchng1, sep=""), paste(NA))
familydata$mrgstatchng2 <- substrRight(familydata$mrgstatchng2, 2)
familydata$mrgstatchng2 <-ifelse(!is.na(familydata$mrgstatchng2), paste("19", familydata$mrgstatchng2, sep=""), paste(NA))
familydata$mrgstatchng3 <- substrRight(familydata$mrgstatchng3, 2)
familydata$mrgstatchng3 <-ifelse(!is.na(familydata$mrgstatchng3), paste("19", familydata$mrgstatchng2, sep=""), paste(NA))
familydata$mrgstatchng4 <- substrRight(familydata$mrgstatchng4, 2)
familydata$mrgstatchng4 <-ifelse(!is.na(familydata$mrgstatchng4), paste("19", familydata$mrgstatchng3, sep=""), paste(NA))
familydata$mrgstatchng5 <- substrRight(familydata$mrgstatchng5, 2)
familydata$mrgstatchng5 <-ifelse(!is.na(familydata$mrgstatchng5), paste("19", familydata$mrgstatchng5, sep=""), paste(NA))
familydata$mrgstatchng6 <- substrRight(familydata$mrgstatchng6, 2)
familydata$mrgstatchng6 <-ifelse(!is.na(familydata$mrgstatchng6), paste("19", familydata$mrgstatchng6, sep=""), paste(NA))
familydata$mrgstatchng7 <- substrRight(familydata$mrgstatchng7, 2)
familydata$mrgstatchng7 <-ifelse(!is.na(familydata$mrgstatchng7), paste("19", familydata$mrgstatchng7, sep=""), paste(NA))


familydata$endprevmrg <- substrRight(familydata$endprevmrg, 2)
familydata$endprevmrg <-ifelse(!is.na(familydata$endprevmrg), paste("19", familydata$endprevmrg, sep=""), paste(NA))
familydata$datesepcl <- substrRight(familydata$datesepcl, 2)
familydata$datesepcl <-ifelse(!is.na(familydata$datesepcl), paste("19", familydata$datesepcl, sep=""), paste(NA))
familydata$datesepmrg <- substrRight(familydata$datesepmrg, 2)
familydata$datesepmrg <-ifelse(!is.na(familydata$datesepmrg), paste("19", familydata$datesepmrg, sep=""), paste(NA))
familydata[familydata == "NA"] <- NA

data_personperiod <- merge(data_personperiod, familydata, by = "FamNum")
#construct marital history 

data_personperiod$married <- 0

data_personperiod$married[data_personperiod$year == data_personperiod$mrgbegan] <- 1
data_personperiod$prevmrgbegan <- NA
data_personperiod[data_personperiod == "NA"] <- NA

data_personperiod$endprevmrg<- as.numeric(data_personperiod$endprevmrg) 

data_personperiod$prevmrgbegan <- 
  data_personperiod$endprevmrg - data_personperiod$lengthmrgprev


data_personperiod$married[data_personperiod$year == data_personperiod$prevmrgbegan] <- 1


#merge the info on the date of change in marital status into one variable, this most likely happens only once 
#anyway 

data_personperiod$mrgstatchng1 <-
  ifelse(!is.na(data_personperiod$mrgstatchng1), paste(data_personperiod$mrgstatchng1), paste(data_personperiod$mrgstatchng2))
data_personperiod[data_personperiod == "NA"] <- NA
data_personperiod$mrgstatchng1 <-
  ifelse(!is.na(data_personperiod$mrgstatchng1), paste(data_personperiod$mrgstatchng1), paste(data_personperiod$mrgstatchng3))
data_personperiod[data_personperiod == "NA"] <- NA
data_personperiod$mrgstatchng1 <-
  ifelse(!is.na(data_personperiod$mrgstatchng1), paste(data_personperiod$mrgstatchng1), paste(data_personperiod$mrgstatchng4))
data_personperiod[data_personperiod == "NA"] <- NA
data_personperiod$mrgstatchng1 <-
  ifelse(!is.na(data_personperiod$mrgstatchng1), paste(data_personperiod$mrgstatchng1), paste(data_personperiod$mrgstatchng5))
data_personperiod[data_personperiod == "NA"] <- NA
data_personperiod$mrgstatchng1 <-
  ifelse(!is.na(data_personperiod$mrgstatchng1), paste(data_personperiod$mrgstatchng1), paste(data_personperiod$mrgstatchng6))
data_personperiod[data_personperiod == "NA"] <- NA
data_personperiod$mrgstatchng1 <-
  ifelse(!is.na(data_personperiod$mrgstatchng1), paste(data_personperiod$mrgstatchng1), paste(data_personperiod$mrgstatchng7))
data_personperiod[data_personperiod == "NA"] <- NA

data_personperiod <- data_personperiod %>%  rename(
  mrgstatchng = mrgstatchng1)


data_personperiod$separated <- 0
data_personperiod$separated[data_personperiod$year == data_personperiod$datesepcl |
                              data_personperiod$year == data_personperiod$datesepmrg |
                              data_personperiod$year == data_personperiod$endprevmrg] <- 1
data_personperiod$married[data_personperiod$separated == 1] <- 0
#create a marriage id 
data_personperiod$mrgid <- 
  ifelse(data_personperiod$married == 1, paste(data_personperiod$OID, data_personperiod$year, sep=""), NA)
#mrgid should change in the year of separation too 
data_personperiod$mrgid <- 
  ifelse(data_personperiod$separated == 1, paste(data_personperiod$OID, data_personperiod$year, sep=""), data_personperiod$mrgid)


data_personperiod[data_personperiod == "NA"] <- NA

#Adding the married wing dummy for all months until separation 

marie = NA
fem = data_personperiod$OID[1]
for (i in 1:dim(data_personperiod)[1]){
  if (!is.na(data_personperiod$mrgid[i])){
    marie = data_personperiod$married[i]
    fem = data_personperiod$OID[i]
  }
  else if (is.na(data_personperiod$mrgid[i]) && data_personperiod$OID[i] == fem){
    data_personperiod$married[i] = marie
  }
}


data_personperiod <- data_personperiod %>%  rename(
  birth = event)
#delete the observations we did not include in the basepay 

datapp <- data_personperiod[which(data_personperiod$FamNum %in% basepay$FamNum),]

#decades instead of years 
datapp$decade <- 0 
datapp$decade[datapp$year == 1930  |datapp$year ==1931 |datapp$year ==1932 |datapp$year ==1933
              |datapp$year == 1934  | datapp$year ==1935 |datapp$year ==1936
              |datapp$year ==1937 | datapp$year == 1938 |datapp$year == 1939] <- "30s" 
datapp$decade[datapp$year == 1940  |datapp$year ==1941 |datapp$year ==1942 |datapp$year ==1943
              |datapp$year == 1944  | datapp$year ==1945 |datapp$year ==1946
              |datapp$year ==1947 | datapp$year == 1948 |datapp$year == 1939] <- "40s" 
datapp$decade[datapp$year == 1950  |datapp$year ==1951  |datapp$year ==1952 |datapp$year ==1953
              |datapp$year == 1954  | datapp$year ==1955 |datapp$year ==1956
              |datapp$year ==1957 | datapp$year == 1958 |datapp$year == 1959] <- "50s" 
datapp$decade[datapp$year == 1960  |datapp$year ==1961  |datapp$year ==1962 |datapp$year ==1963
              |datapp$year == 1964  | datapp$year ==1965 |datapp$year ==1966
              |datapp$year ==1967 | datapp$year == 1968 |datapp$year == 1969] <- "60s" 
datapp$decade[datapp$year == 1970  |datapp$year ==1971 |datapp$year ==1972 |datapp$year ==1973
              |datapp$year == 1974  | datapp$year ==1975 |datapp$year ==1976
              |datapp$year ==1977] <- "70s"
