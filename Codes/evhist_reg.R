library("lme4")
library("stargazer")
library(dplyr)
library(openxlsx)

# only with women born after 1934 and women who have been divorced before and are now single

datapp2 <- datapp[which(datapp$birthyear > 1934), ]
datapp2 <- datapp2[-which(!is.na(datapp2$datesepcl) & is.na(datapp2$lengthmrgprev)), ]
datapp2 <- datapp2[-which(!is.na(datapp2$datesepmrg) & is.na(datapp2$lengthmrgprev)), ]

m1 <- glmer(formula = birth ~ treated*experiment + factor(age) 
            + (1|OID) + factor(j) + factor(year) + factor(married), 
            data = datapp2)
stargazer(m1)

m2 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
           + plan_3*experiment + plan_4*experiment + plan_5*experiment 
           + plan_7*experiment + plan_8*experiment + factor(age) 
           + (1|OID) + factor(year) + factor(j) + factor(married), 
           data = datapp2)
stargazer(m2)

##look only at married couples 

bp2 <- basepay[which(!is.na(basepay$MAGE)), ]
bp2 <- bp2[which(!is.na(bp2$yrschm)), ]
bpid2 <- bp2[, 1]

bpid2 <- as.data.frame(bpid2)

bpid2 <- bpid2 %>% rename(FamNum=bpid2)

datapp3 <- datapp2[which(datapp2$FamNum %in% bpid2$FamNum),]

m3 <- glmer(formula = birth ~ treated*experiment + factor(age) 
            + (1|OID) + factor(year) + factor(j) + factor(married), 
            data = datapp3)
stargazer(m3)

m4 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
           + plan_3*experiment + plan_4*experiment + plan_5*experiment 
           + plan_7*experiment + plan_8*experiment + factor(age) 
           + (1|OID) + factor(year) + factor(j) + factor(married), 
           data = datapp3)
stargazer(m4)

##third set of people 

bp3 <- bp2[which(!is.na(bp2$fmotheduc)), ]
bpid3 <- bp3[, 1]

bpid3 <- as.data.frame(bpid3)

bpid3 <- bpid3 %>% rename(FamNum=bpid3)

datapp4 <- datapp2[which(datapp2$FamNum %in% bpid3$FamNum),]

m5 <- glmer(formula = birth ~ treated*experiment + factor(age) 
            + (1|OID) + factor(year) + factor(j) + factor(married), 
            data = datapp4)

m6 <- lmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
           + plan_3*experiment + plan_4*experiment + plan_5*experiment 
           + plan_7*experiment + plan_8*experiment + factor(age) 
           + (1|OID) + factor(year) + factor(j) + factor(married), 
           data = datapp4)

stargazer(m1, m2, m3, m4, m5, m6)
