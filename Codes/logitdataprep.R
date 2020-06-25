library("haven")
library("dplyr")
library("tidyr") 
library("tidyverse")
library("foreign")
library("haven")
library("stargazer")
library("readxl")
library("fastDummies")

#basepaypanel_revised.dta is the data we obtained after turning the initial data to a panel data from cross section format
#on Stata 
basepaypanel <- "https://github.com/tunadokmeci/MINCOME/blob/master/Data/Raw%20Data/basepaypanel_revised.dta"
basepaypanel <- read_dta(basepaypanel)
basepaypanel[basepaypanel == -9] <- NA
basepaypanel[basepaypanel == -7] <- NA
basepaypanel[basepaypanel == -1] <- NA
basepaypanel[basepaypanel == "."] <- NA
basepaypanel$individual <- 0 
basepaypanel$individual[basepaypanel$DoubleHead1== 0 & basepaypanel$SingleHead1 == 0] <- 1

#only have Winnipeg 
basepaypanel <- basepaypanel[which(basepaypanel$SiteCode == 1),]
#get the information on the treatment cell of the household

basepaypanel$AC <- as.character(basepaypanel$AC)
basepaypanel$plan <- substr(basepaypanel$AC, 1, 1)
basepaypanel$incbracket <- substr(basepaypanel$AC, 2, 3)
basepaypanel$incbracket <- as.numeric(as.character(basepaypanel$incbracket)) 


#Plan 6 was merged with plan 7 at some point
basepaypanel$plan[basepaypanel$plan == 6] <- 7

#have a control dummy, 0 for treated, 1 for control group 
basepaypanel$control = 0 
basepaypanel$control[basepaypanel$plan == 9] <- 1

basepaypanel$treated = 0 
basepaypanel$treated[basepaypanel$control == 0] <- 1

#by eliminating the NAs, we are removing the months for each household 
#where they were not enrolled in the experiment 
basepaypanel <- basepaypanel[!is.na(basepaypanel$plan), ]

basepaypanel$plan <- as.numeric(basepaypanel$plan)

#month1 will then be 1 if that is the month where the family has begun 
#the experiment
basepaypanel <- basepaypanel %>%
  group_by(FamNum) %>%
  arrange(month)  %>%
  mutate(month1 = row_number())  %>%
  ungroup()%>%
  arrange(FamNum, month)

basepaypanel <- basepaypanel %>%
  group_by(FamNum) %>%
  arrange(month1)  %>%
  mutate(prevnrch = dplyr::lag(CH, 1))  %>%
  ungroup()%>%
  arrange(FamNum, month1)

basepaypanel$increase = 0 
basepaypanel$increase[basepaypanel$prevnrch < basepaypanel$CH] <- 1

#dummy if increase happened within the first nine months, to be used as a control later 
basepaypanel$birth9 <- 0
basepaypanel$birth9[basepaypanel$month1 < 9 & basepaypanel$increase == 1] <- 1

#exclude the birth that happened within the first nine months of the experiement 
basepaypanel$increase[basepaypanel$month1 < 9] <- 0

#have a dummy that indicates if there has been any increases

basepaypanel$sum = 0

basepaypanel$FamNum <- as.factor(basepaypanel$FamNum)
for (i in levels(basepaypanel$FamNum)){
  s <- sum(basepaypanel[basepaypanel$FamNum == i, "increase"])
  basepaypanel$sum[basepaypanel$FamNum == i] <- s
}

basepaypanel$if_increase = 0   
basepaypanel$if_increase[basepaypanel$sum != 0] <- 1

#same way to have an indicator if birth has happened within the first 9 months 

basepaypanel$tot = 0

basepaypanel$FamNum <- as.factor(basepaypanel$FamNum)
for (i in levels(basepaypanel$FamNum)){
  s <- sum(basepaypanel[basepaypanel$FamNum == i, "birth9"])
  basepaypanel$tot[basepaypanel$FamNum == i] <- s
}

basepaypanel$if_birth9 = 0   
basepaypanel$if_birth9[basepaypanel$tot != 0] <- 1

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

#46 families in Winnipeg, some sometimes in control and treatment, let's remove these families

basepaypanel$summ = 0

basepaypanel$FamNum <- as.factor(basepaypanel$FamNum)
for (i in levels(basepaypanel$FamNum)){
  s <- sum(basepaypanel[basepaypanel$FamNum == i, "changetreatment"])
  basepaypanel$summ[basepaypanel$FamNum == i] <- s
}

basepaypanel$if_change = 0   
basepaypanel$if_change[basepaypanel$summ != 0] <- 1
basepaypanel_rem <- basepaypanel[which(basepaypanel$AC != 0),]
basepaypanel_rem <- basepaypanel_rem[which(basepaypanel_rem$if_change == 0),]

#we take AC, inc, plan, treatment,
#and increase variables from the panel version and merge it with basepay 

basepaypanel_rem <- basepaypanel_rem %>%
  group_by(FamNum)%>%
  summarise(if_birth = mean(if_increase), if_birth9 = mean(if_birth9), 
            plan = mean(plan), incbracket = mean(incbracket), 
            treated = mean(treated)) %>% 
  ungroup()


#we take only FamNum and the increase variable, and merge it with the cross section data 
#some variable names are there twice, because they were asked at the baseline interview, 
#and then again at the beginning of the payments. Some households were joined later, so we take 
#second ones 
basepay <- read_excel("https://github.com/tunadokmeci/MINCOME/blob/master/Data/Raw%20Data/base_pay.data_revised.xlsx", 
                      col_types = c("numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric"))

basepay[basepay == -9] <- NA
basepay[basepay == -7] <- NA
basepay[basepay == -1] <- NA
basepay[basepay == "."] <- NA

basepay$FAMNUM = as.factor(basepay$FAMNUM)
basepaypanel_rem$FAMNUM = as.factor(basepaypanel_rem$FamNum)

basepay <- merge(basepay, basepaypanel_rem, by = "FAMNUM", all = F)
#remove all those where no female householder was present at the beginning of the experiment 
basepay <- basepay[-which(is.na(basepay$`Age Female Head...8`)), ]
basepay <- basepay[-which(is.na(basepay$`Age Female Head...97`)), ]
basepay$incbracket <- as.factor(basepay$incbracket) 


#rename the variables we will use to shorten them 
names(basepay)

basepay <- basepay %>% rename(FSI = `Fam Size (x100)`,
                              NumChild = `Num Child`,
                              NumAdults = `Num Adults`,
                              numvehic = `Num Vehic`, valvehic = `Val Vehic`, hmown = `Home Own`, 
                              age = `Age Female Head...97`, femhome =`Labor participation (female)`,
                              yrschf = `Years of schooling (female)`, fill = `Ill Disabled? (female)`, 
                              finsch = `In School (female)`,
                              MAGE = `Age Male Head...96`, mill = `Ill Disabled? (male)`,
                              minsch = `In School (male)`, yrschm = `Years of schooling (male)`,
                              fmotheduc = `Years Sch M (female)...91`, 
                               'Finished high school (male)')  


#get the family composition data for the number of children living out of the household 

familydata <- read_excel("https://github.com/tunadokmeci/MINCOME/blob/master/Data/Raw%20Data/familydata-1.xlsx")
familydata  <- familydata %>% rename(FAMNUM = FAMNUM...1)  
familydata <- familydata %>%
  dplyr::select("FAMNUM", "chout")

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

basepay <- stata.merge(familydata, basepay, by = "FAMNUM")
basepay <- basepay[-which(basepay$merge == "master"), ]
basepay[basepay == -9] <- NA


basepay$hmown <- as.factor(basepay$hmown)
basepay$mill <- as.factor(basepay$mill)
basepay$minsch <- as.factor(basepay$minsch)
basepay$femhome <- as.factor(basepay$femhome)
basepay$fill <- as.factor(basepay$fill)
basepay$finsch <- as.factor(basepay$finsch)
basepay$DH <- as.factor(basepay$'Double Head = 1...94')
basepay$SH <- as.factor(basepay$'Single Head = 1...95')
basepay$NumChild <- as.factor(basepay$NumChild)
basepay$NumAdults <- as.factor(basepay$NumAdults)
basepay$FSI <- as.factor(basepay$FSI)

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


#dummies indicating change from two householders to one, and vice versa 
basepay$DH <- basepay$`Double Head = 1...94`
basepay$SH <- basepay$`Single Head = 1...95` == 1

basepay$changeDHSH = 0
basepay$changeDHSH[basepay$`Double Head = 1...5` == 1 | basepay$`Single Head = 1...95` == 1] <- 1 
basepay$changeSHDH = 0
basepay$changeSHDH[basepay$`Single Head = 1...6` == 1 | basepay$`Double Head = 1...94` == 1] <- 1 

basepay <- fastDummies::dummy_columns(basepay, select_columns = "plan")


#dummies for female householder's mother's education 
basepay$edlevelmoth <- 0
basepay$edlevelmoth[basepay$fmotheduc < 9] <- 1
basepay$edlevelmoth[basepay$fmotheduc > 9] <- 2
basepay$edlevelmoth[is.na(basepay$fmotheduc)] <- NA



