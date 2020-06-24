#Descriptives 
library(stargazer)
library("xtable")
library(dplyr)
library("compareGroups")
library("haven")
library(dplyr)

#Table with birth rates by assignment to treatment plans, Table 2 in the main text 
pt <- length(basepay[which(basepay$treated == 1)])
p1 <- length(basepay[which(basepay$plan == 1)])
p2 <- length(basepay[which(basepay$plan == 2)])
p3 <- length(basepay[which(basepay$plan == 3)])
p4 <- length(basepay[which(basepay$plan == 4)])
p5 <- length(basepay[which(basepay$plan == 5)])
p7 <- length(basepay[which(basepay$plan == 7)])
p8 <- length(basepay[which(basepay$plan == 8)])
p9 <- length(basepay[which(basepay$plan == 9)])

hh_plans <- c(pt, p1, p2, p3, p4, p5, p7, p8, p9)
hh_plans <- as.data.frame(hh_plans) 

ct <- length(basepay[which(basepay$treated == 1 & basepay$if_birth == 1 )])
c1 <- length(basepay[which(basepay$plan == 1 & basepay$if_birth == 1 )])
c2 <- length(basepay[which(basepay$plan == 2 & basepay$if_birth == 1)])
c3 <- length(basepay[which(basepay$plan == 3 & basepay$if_birth == 1)])
c4 <- length(basepay[which(basepay$plan == 4 & basepay$if_birth == 1)])
c5 <- length(basepay[which(basepay$plan == 5 & basepay$if_birth == 1)])
c7 <- length(basepay[which(basepay$plan == 7 & basepay$if_birth == 1)])
c8 <- length(basepay[which(basepay$plan == 8 & basepay$if_birth == 1)])
c9 <- length(basepay[which(basepay$plan == 9 & basepay$if_birth == 1)])

hh_plans_ch <- c(ct, c1, c2, c3, c4, c5, c7, c8, c9)
hh_plans_ch <- as.data.frame(hh_plans_ch) 

plans <- c('Treated', 'Plan 1', 'Plan 2', 'Plan 3', 'Plan 4', 'Plan 5', 'Plan 7', 'Plan 8', 'Control')
plans <- as.data.frame(plans) 

table_br <- cbind(plans, hh_plans, hh_plans_ch)

table_br$'Birth Rate' <- table_br$hh_plans_ch/table_br$hh_plans
table_br <- table_br %>% rename('Treatment status' = plans, 'Number of households'  = hh_plans,
                                'Number of births' = hh_plans_ch)


xtable(table_br)

#Table 6 in the Appendix, we compare househols in treatment and control groups in all observables 

#we only want the treatment status and household characteristics, and for those that are double
#get the second one 

basepay <- basepay %>%rename ("Years SCh F (male)...62" = "Years SCh F (female)...62",
                              "Years Sch M (male)...63" = "Years Sch M (female)...63") 

data_desc <- basepay[, c(2,11,12, 14, 15, 16, 18:29, 32: 41, 43, 45: 60, 62:69, 71: 92,95,96,97,98,589,592)]

#have them as correct types 

data_desc$hmown <- as.factor(data_desc$hmown)
data_desc$'Oth Prop' <- as.factor(data_desc$'Oth Prop')
data_desc$'Labor market participation (male)' <- as.factor(data_desc$'Labor market participation (male)')
data_desc$'Flex Hrs Dum (male)' <- as.factor(data_desc$'Flex Hrs Dum (male)')
data_desc$'Job Sat (male)' <- as.factor(data_desc$'Job Sat (male)')
data_desc$'Ever Unemp? (male)' <- as.factor(data_desc$'Ever Unemp? (male)')
data_desc$mill <- as.factor(data_desc$mill)
data_desc$'Ethnic (male)' <- as.factor(data_desc$'Ethnic (male)')
data_desc$'First Lang (male)' <- as.factor(data_desc$'First Lang (male)')
data_desc$'Finished high school (male)' <- as.factor(data_desc$'Finished high school (male)')
data_desc$minsch <- as.factor(data_desc$minsch)
data_desc$femhome <- as.factor(data_desc$femhome)
data_desc$'Flex Hrs Dum (female)' <- as.factor(data_desc$'Flex Hrs Dum (female)')
data_desc$'Job Sat (female)' <- as.factor(data_desc$'Job Sat (female)')
data_desc$'Ever Unemp? (female)' <- as.factor(data_desc$'Ever Unemp? (female)')
data_desc$fill <- as.factor(data_desc$fill)
data_desc$'Ethnic (female)' <- as.factor(data_desc$'Ethnic (female)')
data_desc$'First Lang (female)' <- as.factor(data_desc$'First Lang (female)')
data_desc$'Finished high school (female)' <- as.factor(data_desc$'Finished high school (female)')
data_desc$finsch <- as.factor(data_desc$finsch)
data_desc$'Double Head = 1...94' <- as.factor(data_desc$'Double Head = 1...94')
data_desc$'Single Head = 1...95' <- as.factor(data_desc$'Single Head = 1...95')
data_desc$NumChild <- as.factor(data_desc$NumChild)
data_desc$NumAdults <- as.factor(data_desc$NumAdults)
data_desc$chout <- as.factor(data_desc$chout)


data_desc$Individual = 0 
data_desc$Individual[data_desc$'Single Head = 1...95' == 0 & data_desc$'Double Head = 1...94' == 0] <- 1
          

write.xlsx(data_desc, 'data_desc.xlsx')
names(data_desc)
label(data_desc$chout) <- "Number of children out of hh"
label(data_desc$MAGE) <- "Age (male)"
label(data_desc$NumChild) <- "Number of children"
label(data_desc$NumAdults) <- "Number of adults (non-householders)"
label(data_desc$hmown) <- "Owns the house"
label(data_desc$'Home Val') <- "Value of the house owned"
label(data_desc$Mortgage) <- "Value of mortgage, if any"
label(data_desc$Rent) <- "Rent paid"
label(data_desc$'Oth Prop') <- "Owns other property"
label(data_desc$'Sell Price Oth Prop') <- "Estimated selling price, other property"
label(data_desc$numvehic) <- "Number of vehicles owned"
label(data_desc$valvehic) <- "Value of vehicles owned"
label(data_desc$'Liq Assets') <- "Liquid assets"
label(data_desc$'Durables Val') <- "Value of durable goods owned"
label(data_desc$'Tot UIC 74') <- "Total unemployment insurance received in 1974"
label(data_desc$'Tot UIC 73') <- "Total unemployment insurance received in 1973"
label(data_desc$'Tot Wel 74') <- "Total welfare received in 1974"
label(data_desc$'Tot Wel 73') <- "Total welfare received in 1973"
label(data_desc$'Tot Oth Unear 74') <- "Other unearned income, 1974"
label(data_desc$'Tot Oth Unear 73') <- "Other unearned income, 1973"
label(data_desc$'Tot Fam Inc 74') <- "Total family income, 1974"
label(data_desc$'Num Job (male)') <- "Number of jobs held last week (male)"
label(data_desc$'Num Job (female)') <- "Number of jobs held last week (female)"
label(data_desc$'Hours P  x10 (male)') <- "Hours paid last week (male)"
label(data_desc$'Hours P  x10 (female)') <- "Hours paid last week (female)"
label(data_desc$'Flex Hrs Dum (male)') <- "Flexible hours (male)"
label(data_desc$'Flex Hrs Dum (female)') <- "Flexible hours (female)"
label(data_desc$'Job Sat (male)') <- "Job satisfaction (male)"
label(data_desc$'Job Sat (female)') <- "Job satisfaction (female)"
label(data_desc$'Child care cost male)') <- "Child care cost (male)"
label(data_desc$'Num jobs 74 (male)') <- "Number of jobs held in 1974 (male)"
label(data_desc$'Num jobs 74 (female)') <- "Number of jobs held in 1974 (female)"
label(data_desc$'Ever Unemp? (male)') <- "Ever unemployed? (male)"
label(data_desc$'Ever Unemp? (female)') <- "Ever unemployed? (female)"
label(data_desc$'Tot Earn (male)') <- "Total Earings (male)"
label(data_desc$'Tot Earn (female)') <- "Total Earings (female)"
label(data_desc$'Tips+ (male)') <- "Tips, bonuses, commissions (male)"
label(data_desc$'Tips+ (female)') <- "Tips, bonuses, commissions (female)"
label(data_desc$'Tot Earn 74 (male)') <- "Total earnings in 1974 (male)"
label(data_desc$'Tot Earn 74 (female)') <- "Total earnings in 1974 (female)"
label(data_desc$'Num Weeks 74 (male)') <- "Number of weeks worked in 1974 (male)"
label(data_desc$'Num Weeks 74 (female)') <- "Number of weeks worked in 1974 (female)"
label(data_desc$'Num Weeks 73 (male)') <- "Number of weeks worked in 1973 (male)"
label(data_desc$'Num Weeks 73 (female)') <- "Number of weeks worked in 1973 (female)"
label(data_desc$'Num Weeks 73 (male)') <- "Number of weeks worked in 1973 (male)"
label(data_desc$'Avg week hrs x10 (male)') <- "Average weekly hours (male)"
label(data_desc$'Avg week hrs x10 (female)') <- "Average weekly hours (female)"
label(data_desc$'Years FT (male)') <- "Years worked full-time (male)"
label(data_desc$'Years FT (female)') <- "Years worked full-time (female)"
label(data_desc$'Ethnic (male)') <- "Ethnic background (male)"
label(data_desc$'Ethnic (female)') <- "Ethnic background (female)"
label(data_desc$'First Lang (male)') <- "First language (male)"
label(data_desc$'First Lang (female)') <- "First language (female)"
label(data_desc$'Years SCh F (male)...62') <- "Years of schooling, father (male)"
label(data_desc$'Years SCh M (male)...62') <- "Years of schooling, mother (male)"
label(data_desc$'Years SCh F (female)...62') <- "Years of schooling, father (female)"
label(data_desc$'Years SCh M (female)...62') <- "Years of schooling, mother (female)"
label(data_desc$minsch) <- "In school (male)"
label(data_desc$finsch) <- "In school (female)"
label(data_desc$if_birth9) <- "If there has been birth in the first nine months"

compare = compareGroups::compareGroups(treated ~ ., data = data_desc,
                                       max.ylev = 50 )

