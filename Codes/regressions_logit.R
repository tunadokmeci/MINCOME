#formulas for the six different specifications 
formula1 = if_birth ~ treated + DH + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
  chout + if_birth9 + femhome + changeDHSH + changeSHDH + NumAdults + yrschf + fill + hmown

formula2 = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
  plan_5 + plan_7 + plan_8 + DH + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
  chout + if_birth9 + femhome + changeDHSH + changeSHDH + NumAdults + yrschf + fill + hmown

formula3 = if_birth ~ treated  + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
  chout + if_birth9 + femhome + NumAdults + yrschf + fill + hmown + MAGE + mill + minsch + yrschm 

formula4 = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
  plan_5 + plan_7 + plan_8  + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
  chout + if_birth9 + femhome + NumAdults + yrschf + fill + hmown + MAGE + mill + minsch + yrschm

formula5 =  if_birth ~ treated  + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
  chout + if_birth9 + femhome + NumAdults + yrschf + fill + hmown + MAGE + mill + minsch + yrschm + edlevelmoth

formula6 =   if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
  plan_5 + plan_7 + plan_8  + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
  chout + if_birth9 + femhome + NumAdults + yrschf + fill + hmown + MAGE + mill + minsch + yrschm + fmotheduc


reg1 <- glm(formula = formula1, 
            family = binomial(link = "logit"), data = basepay)
stargazer(reg1)

reg2 <- glm(formula = formula2, 
            family = binomial(link = "logit"), data = basepay)
stargazer(reg2)


#adding controls for the male householder 

reg3 <- glm(formula = formula3, 
            family = binomial(link = "logit"), data = basepay)
stargazer(reg3)


reg4 <- glm(formula = formula4, family = binomial(link = "logit"), data = basepay)
stargazer(reg4)
#adding control for mother´s education

reg5 <- glm(formula = formula5, 
            family = binomial(link = "logit"), data = basepay)

summary(reg5, apply.coef = exp)


reg6 <- glm(formula = formula6, 
            family = binomial(link = "logit"), data = basepay)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, apply.coef=exp, t.auto=F, p.auto=F, report = "vc*s")

#Marginal effects 

mef1 <- logitmfx(reg1, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef2 <- logitmfx(reg2, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef3 <- logitmfx(formula = formula3, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef4 <- logitmfx(formula = formula4, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef5 <- logitmfx(formula = formula5, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef6 <- logitmfx(formula = formula6, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
texreg(list(mef1, mef2, mef3, mef4, mef5, mef6))
