library("haven")
library("dplyr")
library("tidyr") 
library("tidyverse")
library("lubridate") 
library("data.table")
library("foreign")
library("haven")
library("quantmod") 
library("zoo")
library("plm")
library("gplots")
library("stargazer")
library("lfe")
library("Hmisc")
library("readxl")
library("naniar")
library("strex")

#basepaypanel_revised.dta is the data we obtained after turning the initial data to a panel data from cross section format
#on Stata 


basepaypanel <- read_dta("Downloads/basepaypanel_revised.dta")
basepaypanel[basepaypanel == -9] <- NA
basepaypanel[basepaypanel == -7] <- NA
basepaypanel[basepaypanel == -1] <- NA
basepaypanel[basepaypanel == "."] <- NA
basepaypanel$individual <- 0 
basepaypanel$individual[basepaypanel$DoubleHead1== 0 & basepaypanel$SingleHead1 == 0] <- 1

#only have Winnipeg 

basepaypanel <- basepaypanel[which(basepaypanel$SiteCode == 1),]

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
basepaypanel$inc <- substr(basepaypanel$AC, 2, 3)
basepaypanel$inc <- as.factor(basepaypanel$inc)

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

#exclude the birth that happened within the first nine months of the experiement 
basepaypanel$increase[basepaypanel$month1 < 9] <- 0

#CR and CS give the male and female householders´ age, also for households that were missing 
#at the beginning of the experiment 

basepaypanel$mage<- basepaypanel$CR
basepaypanel$fage<- basepaypanel$CS

faminfo <- subset(basepaypanel, select=c("FamNum","FSI", "FS", "month1", "mage", "fage", "AC"))
faminfo <- faminfo[which(faminfo$month1 ==1),]
faminfo <- faminfo %>% rename(FAMSI=FSI)
faminfo <- faminfo %>% rename(FAMS=FS)
faminfo <- faminfo %>% rename(FAGE=fage)
faminfo <- faminfo %>% rename(MAGE=mage)
faminfo <- faminfo %>% rename(asscell=AC)
faminfo <- faminfo[,-4]
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

basepaypanel$treated = 0 
basepaypanel$treated[basepaypanel$control == 0] <- 1

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

#46 families in Winnipeg, some sometimes in control and treatment  

#we remove these families 

basepaypanel$summ = 0

basepaypanel$FamNum <- as.factor(basepaypanel$FamNum)
for (i in levels(basepaypanel$FamNum)){
  s <- sum(basepaypanel[basepaypanel$FamNum == i, "changetreatment"])
  basepaypanel$summ[basepaypanel$FamNum == i] <- s
}

basepaypanel$if_change = 0   
basepaypanel$if_change[basepaypanel$summ != 0] <- 1

basepaypanel<-merge(basepaypanel, faminfo, by = "FamNum", all=T)

basepaypanel_rem <- basepaypanel[which(basepaypanel$AC != 0),]

basepaypanel_rem <- basepaypanel_rem[which(basepaypanel_rem$if_change == 0),]

#have it back to cross section for logit 

basepaypanel_rem$plan <- as.numeric(as.character(basepaypanel_rem$plan))
basepaypanel_rem$control <- as.numeric(as.character(basepaypanel_rem$control))
basepaypanel_rem$if_increase <- as.numeric(as.character(basepaypanel_rem$if_increase))
basepaypanel_rem$MAGE <- as.numeric(as.character(basepaypanel_rem$MAGE))
basepaypanel_rem$FAGE <- as.numeric(as.character(basepaypanel_rem$FAGE))
basepaypanel_rem$DoubleHead1 <- as.numeric(as.character(basepaypanel_rem$DoubleHead1))
basepaypanel_rem$individual <- as.numeric(as.character(basepaypanel_rem$individual))
basepaypanel_rem$SingleHead1 <- as.numeric(as.character(basepaypanel_rem$SingleHead1))
basepaypanel_rem$asscell <- as.numeric(as.character(basepaypanel_rem$asscell))
basepaypanel_rem$asscell <- as.numeric(as.character(basepaypanel_rem$asscell))

basepaypanel_rem$`Age Male Head...95` <- as.numeric(as.character(basepaycomp$`Age Male Head...95`))
basepaypanel_rem$`Age Female Head...96` <- as.numeric(as.character(basepaycomp$`Age Female Head...96`))
basepaypanel_rem$`Home Val` <- as.numeric(as.character(basepaycomp$`Home Val`))
basepaypanel_rem$Mortgae <- as.numeric(as.character(basepaycomp$Mortgae))
basepaypanel_rem$Rent <- as.numeric(as.character(basepaycomp$Rent))
basepaypanel_rem$`Val Vehic`<- as.numeric(as.character(basepaycomp$`Val Vehic`))
basepaypanel_rem$`Liq Assets` <- as.numeric(as.character(basepaycomp$`Liq Assets`))
basepaypanel_rem$`Durables Val` <- as.numeric(as.character(basepaycomp$`Durables Val`))
basepaypanel_rem$`Tot UIC 74` <- as.numeric(as.character(basepaycomp$`Tot UIC 74` ))
basepaypanel_rem$`Tot UIC 73` <- as.numeric(as.character(basepaycomp$`Tot UIC 73`))
basepaypanel_rem$`Tot Wel 74` <- as.numeric(as.character(basepaycomp$`Tot Wel 74`))
basepaypanel_rem$`Wel Mun_Prov 74`  <- as.numeric(as.character(basepaycomp$`Wel Mun_Prov 74`))
basepaypanel_rem$`Tot Oth Unear 73` <- as.numeric(as.character(basepaycomp$`Tot Oth Unear 73` ))
basepaypanel_rem$`Tot Fam Inc 74` <- as.numeric(as.character(basepaycomp$`Tot Fam Inc 74`))
basepaypanel_rem$mnumjob <- as.numeric(as.character(basepaycomp$mnumjob))
basepaypanel_rem$mhrspaid <- as.numeric(as.character(basepaycomp$mhrspaid))
basepaypanel_rem$mnumjob <- as.numeric(as.character(basepaycomp$mnumjob))
basepaypanel_rem$mwagerate <- as.numeric(as.character(basepaycomp$mwagerate))
basepaypanel_rem$mgrossearnings <- as.numeric(as.character(basepaycomp$mgrossearnings))
basepaypanel_rem$`mTot Earn` <- as.numeric(as.character(basepaycomp$`mTot Earn`))
basepaypanel_rem$`mTot Earn 74` <- as.numeric(as.character(basepaycomp$`mTot Earn 74`))
basepaypanel_rem$`mNum Weeks 74` <- as.numeric(as.character(basepaycomp$`mNum Weeks 74`))
basepaypanel_rem$`mNum Weeks 73` <- as.numeric(as.character(basepaycomp$`mNum Weeks 73`))
basepaypanel_rem$`mAvg week hrs x10` <- as.numeric(as.character(basepaycomp$`mAvg week hrs x10`))
basepaypanel_rem$myearsFT <- as.numeric(as.character(basepaycomp$myearsFT))
basepaypanel_rem$`fHours P  x10` <- as.numeric(as.character(basepaycomp$`fHours P  x10`))
basepaypanel_rem$`fWage Rate  x100` <- as.numeric(as.character(basepaycomp$`fWage Rate  x100`))
basepaypanel_rem$`fGross Earnings` <- as.numeric(as.character(basepaycomp$`fGross Earnings`))
basepaypanel_rem$`fTot Earn` <- as.numeric(as.character(basepaycomp$`fTot Earn`))
basepaypanel_rem$`fTot Earn 74` <- as.numeric(as.character(basepaycomp$`fTot Earn 74`))
basepaypanel_rem$`fNum Weeks 74` <- as.numeric(as.character(basepaycomp$`fNum Weeks 74`))
basepaypanel_rem$`fNum weeks 73`<- as.numeric(as.character(basepaycomp$`fNum weeks 73`))
basepaypanel_rem$`fAvg week hrs x10` <- as.numeric(as.character(basepaycomp$`fAvg week hrs x10`))
basepaypanel_rem$'fYears FT'<- as.numeric(as.character(basepaycomp$'fYears FT'))
basepaypanel_rem$yrschf<- as.numeric(as.character(basepaycomp$yrschf))
basepaypanel_rem$yrschm<- as.numeric(as.character(basepaycomp$yrschm))
basepaypanel_rem$fmotheduc<- as.numeric(as.character(basepaycomp$fmotheduc))
basepaypanel_rem$ffathereduc<- as.numeric(as.character(basepaycomp$ffathereduc))
basepaypanel_rem$mfathereduc<- as.numeric(as.character(basepaycomp$mfathereduc))
basepaypanel_rem$mmotheduc<- as.numeric(as.character(basepaycomp$mmotheduc))
basepaypanel_rem$`fChild care cost`<- as.numeric(as.character(basepaycomp$`fChild care cost`))
basepaypanel_rem$`mChild care cost`<- as.numeric(as.character(basepaycomp$`mChild care cost`))
basepaypanel_rem$ `fNum Job`<- as.numeric(as.character(basepaycomp$ `fNum Job`))

basepaypanel_rem <- basepaypanel_rem %>% rename(numvehic = `Num Vehic`, 
                                      valvehic = `Val Vehic`, 
                                      totfaminc = `Tot Fam Inc 74`, 
                                      fhrspaid = `fHours P  x10`, mtotearn = `mTot Earn`, ftotearn= `fTot Earn`,
                                      hmown = `Home Own`)



basepaycomp <- basepaycomp %>% select () 


basepay <- basepaypanel_rem %>%
  group_by(FamNum)%>%
  summarise(if_birth=mean(if_increase), control = mean(control), plan =mean(plan), 
            MAGE = mean(MAGE), FAGE = mean(FAGE), DH = mean(DoubleHead1), individual = mean(individual), 
            SH =mean(SingleHead1), AC = mean(asscell), numvehic = mean(numvehic), valvehic = mean(valvehic), mill = mean(mill), 
            fill = mean(fill), totfaminc = mean(totfaminc), minsch = mean(minsch), finsch = mean(finsch), 
            fhrspaid = mean(fhrspaid), mtotearn = mean(mtotearn),ftotearn = mean(ftotearn), 
            mhrspaid = mean(fhrspaid), fmotheduc = mean(fmotheduc),ffathereduc = mean(ffathereduc),
            hmown = mean(hmown)) %>% 
  ungroup()

 
#we want information on number of children, family size index etc. 
#we can get this info from the FSI, NUMCH from the first month households join 
bpinfo <- basepaypanel_rem %>%
  group_by(FamNum) %>%
  filter(month1 == 1) %>%
  ungroup()

bpinfo <- bpinfo %>%
  dplyr::select(FamSize, FamSizex100, FamNum, NumChild)

basepay <- merge(basepay, bpinfo, by = "FamNum", all = TRUE)
basepay$treated <- 0 
basepay$treated[basepay$control == 0] <- 1

#we want to exclude households with only males 
basepay <- basepay[-which(is.na(basepay$FAGE)), ]

names(basepay)[names(basepay) == 'FAGE'] <- "age"
basepay <- fastDummies::dummy_columns(basepay, select_columns = "age")
basepay <- fastDummies::dummy_columns(basepay, select_columns = "plan")


basepay$age1519 <- 0
basepay$age1519[basepay$age == 15 | basepay$age == 16 | basepay$age == 17 |
                  basepay$age == 18 | basepay$age == 19  ] <- 1


basepay$age2024 <- 0 
basepay$age2024[basepay$age == 20 | basepay$age == 21 | basepay$age == 22 |
                  basepay$age == 23 | basepay$age == 24  ] <- 1


basepay$age2429 <- 0 
basepay$age2429[basepay$age == 24 | basepay$age == 25 | basepay$age == 26 |
                  basepay$age == 27 | basepay$age == 28  ] <- 1

basepay$age3034 <- 0 
basepay$age3034[basepay$age == 30 | basepay$age == 31 | basepay$age == 32 |
                  basepay$age == 33 | basepay$age == 34  ] <- 1



basepay$age3539 <- 0
basepay$age3539[basepay$age == 35 | basepay$age == 36 | basepay$age == 37 |
                  basepay$age == 38 | basepay$age == 39  ] <- 1

basepay$age4044 <- 0 
basepay$age4044[basepay$age == 40 | basepay$age == 41 | basepay$age == 42 |
                  basepay$age == 43 | basepay$age == 44  ] <- 1 


basepay$age4550 <- 0 
basepay$age4550[basepay$age == 45 | basepay$age == 46 | basepay$age == 47 |
                  basepay$age == 48 | basepay$age == 49  ] <- 1 

basepay$age50plus <- 0 
basepay$age50plus[basepay$age > 50 ] <- 1 
names(basepay)[names(basepay) == 'FamSizex100'] <- "FSI"
names(basepay)[names(basepay) == 'NumChild'] <- "chnr"
names(basepay)[names(basepay) == 'TotFamInc74'] <- "TotInc74"
basepay$TotInc74 <- as.numeric(basepay$TotInc74)
basepay$age <- as.factor(basepay$age)

#using the family information data, which is available only for Winnipeg to obtain info on the children living outside of the 
#household

familydata <- read_excel("Downloads/familydata.xlsx")

familydata <- familydata %>%
  dplyr::select("FamNum", "chout")

names(familydata)

stata.merge <- function(x,y, by = intersect(names(x), names(y))){
  
  x[is.na(x)] <- Inf
  y[is.na(y)] <- Inf
  
  matched <- merge(x, y, by.x = by, by.y = by, all = TRUE)
  matched <- matched[complete.cases(matched),]
  matched$merge <- "matched"
  master <- merge(x, y, by.x = by, by.y = by, all.x = TRUE)
  master <- master[!complete.cases(master),]
  master$merge <- "master"
  using <- merge(x, y, by.x = by, by.y = by, all.y = TRUE)
  using <- using[!complete.cases(using),]
  using$merge <- "using"
  
  df <- rbind(matched, master,using)
  df[sapply(df, is.infinite)] <- NA
  df
}

basepay <- stata.merge(familydata, basepay, by = "FamNum")
basepay <- basepay[-which(basepay$merge == "master"), ]
basepay[basepay == -9] <- NA
basepay$chout[is.na(basepay$chout)] <- 0

basepay$AC <- as.character(basepay$AC)
basepay$incbracket <- substr(basepay$AC, 2, 3)
basepay$incbracket <- as.factor(basepay$incbracket)

basepay$valvehic[is.na(basepay$valvehic)] <- 0
basepay$mill <- as.factor(basepay$mill)
basepay$fill <- as.factor(basepay$fill)
basepay$minsch<- as.factor(basepay$minsch)
basepay$finsch<- as.factor(basepay$finsch)
basepay$if_birth9<- as.factor(basepay$if_birth9)
basepay$birth<- as.factor(basepay$birth)
basepay$changeDHSH<- as.factor(basepay$changeDHSH)
basepay$changeSHDH<- as.factor(basepay$changeSHDH)
basepay$hmown<- as.factor(basepay$hmown)
basepay$NumAdults<- as.numeric(as.character(basepay$NumAdults))
basepay$fmotheduc <- as.numeric(as.character(basepay$fmotheduc))

basepay <- basepay[which(!is.na(basepay$fill)), ]

mean(basepay$yrschf[basepay$highschf == 2], na.rm = TRUE)
basepay$yrschf[is.na(basepay$yrschf)] <- 8 


basepay$yrschf <- 0
basepay$yrschf[basepay$highschf == 1 & basepay$yrschf < 13] <- 1
basepay$yrschf[basepay$highschf == 1 & basepay$yrschf > 12] <- 2


basepay$fmotheduc <- 0
basepay$fmotheduc[basepay$highschm == 1 & basepay$yrschm < 13] <- 1
basepay$fmotheduc[basepay$highschm == 1 & basepay$yrschm > 12] <- 2

basepay$fmotheducoth <- 0
basepay$fmotheducoth[basepay$fmotheduc < 13 & basepay$fmotheduc > 8] <- 1
basepay$fmotheducoth[basepay$fmotheduc > 12] <- 2
basepay$fmotheducoth[is.na(basepay$fmotheduc)] <- NA


basepay$DH <- as.factor(basepay$DH)
basepay$age1519 <- as.factor(basepay$age1519)
basepay$age2024 <- as.factor(basepay$age2024)
basepay$age2429 <- as.factor(basepay$age2429)
basepay$age3034 <- as.factor(basepay$age3034)
basepay$age3539 <- as.factor(basepay$age3539)
basepay$age4044 <- as.factor(basepay$age4044)
basepay$age4550 <- as.factor(basepay$age4550)
basepay$age50plus <- as.factor(basepay$age50plus)
basepay$femhome <- as.factor(basepay$femhome)
basepay$plan_1 <- as.factor(basepay$plan_1)
basepay$plan_2 <- as.factor(basepay$plan_2)
basepay$plan_3 <- as.factor(basepay$plan_3)
basepay$plan_4 <- as.factor(basepay$plan_4)
basepay$plan_5 <- as.factor(basepay$plan_5)
basepay$plan_7 <- as.factor(basepay$plan_7)
basepay$plan_8 <- as.factor(basepay$plan_8)
basepay$plan_9 <- as.factor(basepay$plan_9)


basepay$if_birth[basepay$FAMNUM == 14324] <- 0 
basepay$costch <- as.numeric(basepay$costch)

basepay$edlevelmoth <- 0
basepay$edlevelmoth[basepay$fmotheduc < 9] <- 1
basepay$edlevelmoth[basepay$fmotheduc > 9] <- 2
basepay$edlevelmoth[is.na(basepay$fmotheduc)] <- NA



saveRDS(basepay, "basepay.rds")

