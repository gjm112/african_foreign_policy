#Note original Hirschman ideas was the sqaure root of the sum of squres of market share. 
#HHI is now just the sum of squares.  
library(tidyverse)
library(readxl)

setwd("/Users/gregorymatthews/Dropbox/african_foreign_policy_git/")

#masterset = read_csv("data/Master 08-28-2025 AFP excel dataset-3.csv")
masterset = read_csv("data2026/MASTER DATASET 3.26.2026.csv")

#restofworldold = read_excel("data/Rest of the World.xlsx")
#restofworld = read_excel("data2026/Rest of the World - 5.6.2026.xlsx")


## Cleaning data ##
masterset = masterset %>%
  mutate(across(where(is.numeric), ~ na_if(., -99)),
         across(where(is.numeric), ~ na_if(., -88)),
         across(where(is.numeric), ~ na_if(., -77)),
         across(where(is.numeric), ~ na_if(., -66)),
         COUNTRY = case_when(COUNTRY == "seychelles" ~ "Seychelles",
                             COUNTRY == "sao tome & Principe" ~ "Sao Tome & Principe",
                             .default = COUNTRY))

#Convert commas to periods. 
#it looks like commas are being used as the decimal.  
#Fix the nonsense formatting from excel. 
masterset$GNI <- gsub(",",".",masterset$GNI)
masterset$GNI <- gsub(" ","",masterset$GNI)%>% as.numeric()

masterset$GNI_CAP <- gsub(",",".",masterset$GNI_CAP)
masterset$GNI_CAP <- gsub(" ","",masterset$GNI_CAP)%>% as.numeric()

masterset$POPULATN <- gsub(",",".",masterset$POPULATN)
masterset$POPULATN <- gsub(" ","",masterset$POPULATN) %>% as.numeric()

#Manually fixing names
names(masterset)[which(names(masterset) == "TRDX352b")] <- "TRDEX352b" #These years are getting dropped.  
names(masterset)[which(names(masterset) == "TRDX200h")] <- "TRDEX200h"
names(masterset)[which(names(masterset) == "TRDX200m")] <- "TRDEX200m"
names(masterset)[which(names(masterset) == "TDX321sr")] <- "TRDEX321sr"
names(masterset)[which(names(masterset) == "TDX321sm")] <- "TRDEX321sm"
names(masterset)[which(names(masterset) == "TDX321yr")] <- "TRDEX321yr"

names(masterset)[which(names(masterset) == "TRDI200h")] <- "TRDIM200h"
names(masterset)[which(names(masterset) == "TRDI200m")] <- "TRDIM200m"
names(masterset)[which(names(masterset) == "TDI321sr")] <- "TRDIM321sr"
names(masterset)[which(names(masterset) == "TDI321sm")] <- "TRDIM321sm"
names(masterset)[which(names(masterset) == "TDI321yr")] <- "TRDIM321yr"
names(masterset)[which(names(masterset) == "TDI316SU")] <- "TRDIM316su"

masterset <- masterset %>% mutate(ODAG404 = as.numeric(ODAG404))

HHI <- masterset %>%
  mutate(across(ODAG201:ODAG501, ~ replace_na(.x, 0))) %>% #replace NA with 0
  mutate(across(ODAG201:ODAG501, ~ pmax(.x, 0))) %>% #no negative numbers
  mutate(across(ODAG201:ODAG501, ~ as.numeric(.x))) %>% #make these variables numeric
  rowwise() %>% 
  mutate(ODAGTOT200_501 = rowSums(across(ODAG201:ODAG501))) %>% 
  mutate(HHI = rowSums((across(ODAG201:ODAG501) / rowSums(across(ODAG201:ODAG501)))^2)) %>% 
  mutate(HHI = ifelse(ODAGTOT200_501 == 0, NA, HHI))


#HHI values over time
#HHI %>% select(COUNTRY, YEAR, HHI, ODAGTOT200_500, ODAG201:ODAG416) %>% write.csv(file = "./HHI.csv", row.names = FALSE)

HHI %>% ggplot(aes(x = YEAR, y = HHI)) + geom_line() +  facet_wrap(~COUNTRY) + theme_bw()


## Prepare data for analysis. 
HHI <- HHI %>% mutate(across(TRDEX351:TRDEX203, ~ as.numeric(.x))) %>% 
  mutate(across(TRDIM351:TRDIM203, ~ as.numeric(.x))) %>% 
  mutate(TRDEXtot = rowSums(pick(TRDEX351:TRDEX250, 
                                 TRDEX300:TRDEX285, 
                                 TRDEX280:TRDEX415,
                                 TRDEX100:TRDEX147,
                                 TRDEX577:TRDEX551,
                                 TRDEX563:TRDEX203),na.rm = TRUE),
         TRDIMtot = rowSums(pick(TRDIM351:TRDIM250,
                                 TRDIM300:TRDIM285,
                                 TRDIM280:TRDIM415,
                                 TRDIM100:TRDIM147,
                                 TRDIM577:TRDIM551,
                                 TRDIM563:TRDIM203),na.rm = TRUE)) %>% 
  mutate(ODAGtot = rowSums(pick(ODAG201:ODAG501), na.rm = T)) %>% 
  mutate(POPULATN = ifelse(POPULATN == -99, NA, POPULATN),
         logGNI = log(GNI, 10),
         logPOP = log(POPULATN,10),
         logTRDIMtot = log(TRDIMtot + 1,10),
         logTRDEXtot = log(TRDEXtot + 1,10), 
         logODAGtot = log(ODAGtot + 1, 10)) 

#Now create the lag variables
HHI <- HHI %>% arrange(CCODE, YEAR) %>% group_by(CCODE) %>% 
  mutate(TOTLIB1_lag1 = lag(TOTLIB1,1),
         TOTLIB1_lag2 = lag(TOTLIB1,2),
         TOTLIB1_lag3 = lag(TOTLIB1,3),
         POLITY_lag1 = lag(POLITY,1),
         POLITY_lag2 = lag(POLITY,2),
         POLITY_lag3 = lag(POLITY,3),
         POLITY2_lag1 = lag(POLITY2,1),
         POLITY2_lag2 = lag(POLITY2,2),
         POLITY2_lag3 = lag(POLITY2,3)
  ) %>% 
  ungroup() %>% mutate(logitHHI = log(ifelse(HHI == 1,0.999,HHI)/(1-ifelse(HHI == 1,0.999,HHI))))
  

#

#Summary stuff
HHI %>% filter(YEAR >= 1960) %>%group_by(YEAR) %>% summarize(meanHHI = mean(HHI, na.rm = T)) 
HHI %>% filter(YEAR >= 1960) %>%group_by(YEAR) %>% summarize(meanHHI = mean(HHI, na.rm = T))  %>% ggplot(aes(x = YEAR, y = meanHHI)) + geom_point() + geom_line() + theme_bw()
HHI %>% ggplot(aes(x = YEAR, y = log(HHI))) + geom_point() + geom_smooth(se = F)  + theme_bw()
HHI %>% ggplot(aes(x = HHI)) + geom_density() + theme_bw()


#
HHI %>% ggplot(aes(x = POLITY2, y = log(HHI), color = YEAR, group = YEAR)) + geom_point() + geom_smooth(se = F)  + theme_bw()
HHI %>% ggplot(aes(x = POLITY, y = log(HHI), color = YEAR, group = YEAR)) + geom_point() + geom_smooth(se = F)  + theme_bw()
HHI %>% ggplot(aes(x = TOTLIB1, y = log(HHI), color = YEAR, group = YEAR)) + geom_point() + geom_smooth(se = F)  + theme_bw()
HHI %>% ggplot(aes(x = logODAGtot, y = (HHI))) + geom_point() + geom_smooth(se = F)  + theme_bw()
HHI %>% ggplot(aes(x = logGNI, y = (HHI))) + geom_point() + geom_smooth(se = F)  + theme_bw()
HHI %>% ggplot(aes(x = logPOP, y = (HHI))) + geom_point() + geom_smooth(se = F)  + theme_bw()


#Build some models
#HHI = 1 if there is a monopoly
#Smaller HHI 
library(lme4)
library(lmerTest)
library(splines)
mod0_POLITY2 <- lmer(log(HHI) ~ POLITY2  + (1|CCODE) + (1|YEAR), data = HHI)
mod1_POLITY2 <- lmer(log(HHI) ~  POLITY2  + logPOP + I(logPOP^2) + I(logPOP^3) + logGNI + I(logGNI^2)  + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)

mod0_POLITY <- lmer(log(HHI) ~ POLITY  + (1|CCODE) + (1|YEAR), data = HHI)
mod1_POLITY <- lmer(log(HHI) ~  POLITY  + logPOP + I(logPOP^2) + I(logPOP^3) + logGNI + I(logGNI^2)  + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)

mod0_TOTLIB1 <- lmer(log(HHI) ~ TOTLIB1  + (1|CCODE) + (1|YEAR), data = HHI)
mod1_TOTLIB1 <- lmer(log(HHI) ~  TOTLIB1 + logPOP + I(logPOP^2) + I(logPOP^3) + logGNI  + I(logGNI^2) + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)

save(mod0_POLITY2, file = "./HHImodels/mod0_POLITY2.RData")
save(mod1_POLITY2, file = "./HHImodels/mod1_POLITY2.RData")

save(mod0_POLITY, file = "./HHImodels/mod0_POLITY.RData")
save(mod1_POLITY, file = "./HHImodels/mod1_POLITY.RData")

save(mod0_TOTLIB1, file = "./HHImodels/mod0_TOTLIB1.RData")
save(mod1_TOTLIB1, file = "./HHImodels/mod1_TOTLIB1.RData")


#############################################
#Appendix
#############################################
#Build some models
#HHI = 1 if there is a monopoly
#Smaller HHI 
library(lme4)
library(lmerTest)
library(splines)
mod0_POLITY2 <- lmer(log(HHI) ~ POLITY2  + (1|CCODE) + (1|YEAR), data = HHI)
mod1_POLITY2_1 <- lmer(log(HHI) ~  POLITY2  + logPOP  + logGNI + I(logGNI^2)  + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)
mod1_POLITY2_2 <- lmer(log(HHI) ~  POLITY2  + logPOP + I(logPOP^2)  + logGNI + I(logGNI^2)  + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)
mod1_POLITY2_3 <- lmer(log(HHI) ~  POLITY2  + logPOP + I(logPOP^2) + I(logPOP^3) + logGNI + I(logGNI^2)  + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)
AIC(mod0_POLITY2,mod1_POLITY2_1, mod1_POLITY2_2, mod1_POLITY2_3)
summary(mod1_POLITY2)
plot(mod1_POLITY2@frame$POLITY2, predict(mod1_POLITY2))

mod0_POLITY <- lmer(log(HHI) ~ POLITY  + (1|CCODE) + (1|YEAR), data = HHI)
mod1_POLITY <- lmer(log(HHI) ~  POLITY + logPOP + I(logPOP^2) + I(logPOP^3) + logGNI  + I(logGNI^2)  + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)
mod1_POLITY_1 <- lmer(log(HHI) ~  POLITY  + logPOP  + logGNI + I(logGNI^2)  + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)
mod1_POLITY_2 <- lmer(log(HHI) ~  POLITY  + logPOP + I(logPOP^2)  + logGNI + I(logGNI^2)  + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)
mod1_POLITY_3 <- lmer(log(HHI) ~  POLITY  + logPOP + I(logPOP^2) + I(logPOP^3) + logGNI + I(logGNI^2)  + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)
AIC(mod0_POLITY,mod1_POLITY_1, mod1_POLITY_2, mod1_POLITY_3)
summary(mod1_POLITY)

mod0_TOTLIB1 <- lmer(log(HHI) ~ TOTLIB1  + (1|CCODE) + (1|YEAR), data = HHI)
mod1_TOTLIB1_1 <- lmer(log(HHI) ~  TOTLIB1 + logPOP  + logGNI  + I(logGNI^2) + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)
mod1_TOTLIB1_2 <- lmer(log(HHI) ~  TOTLIB1 + logPOP + I(logPOP^2) + logGNI  + I(logGNI^2) + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)
mod1_TOTLIB1_3 <- lmer(log(HHI) ~  TOTLIB1 + logPOP + I(logPOP^2) + I(logPOP^3) + logGNI  + I(logGNI^2) + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)
AIC(mod0_TOTLIB1,mod1_TOTLIB1_1, mod1_TOTLIB1_2, mod1_TOTLIB1_3)
summary(mod1_TOTLIB1)
plot(mod1_TOTLIB1)

data.frame(x = mod1_TOTLIB1@frame$`log(HHI)`, resid = residuals(mod1_TOTLIB1)) %>% 
  ggplot(aes(x = x, y = resid)) + geom_point() + geom_smooth(se = F)

test <- lmer(log(HHI) ~  TOTLIB1 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR) , data = HHI)
#The plot of y vs resid
data.frame(x = test@frame$`log(HHI)`, resid = residuals(test)) %>% 
  ggplot(aes(x = x, y = resid)) + geom_point() + geom_smooth(se = F)

data.frame(x = test@frame$logGNI, resid = residuals(test)) %>% 
  ggplot(aes(x = x, y = resid)) + geom_point() + geom_smooth(method = "lm", formula =  "y~x + I(x^2)", se = F)
data.frame(x = test@frame$logPOP, resid = residuals(test)) %>% 
  ggplot(aes(x = x, y = resid)) + geom_point() + geom_smooth(method = "lm", formula =  "y~x + I(x^2)",se = F)
data.frame(x = test@frame$logTRDIMtot, resid = residuals(test)) %>% 
  ggplot(aes(x = x, y = resid)) + geom_point() + geom_smooth(method = "lm", formula =  "y~x + I(x^2)",se = F)
data.frame(x = test@frame$logTRDEXtot, resid = residuals(test)) %>% 
  ggplot(aes(x = x, y = resid)) + geom_point() + geom_smooth(method = "lm", formula =  "y~x + I(x^2)",se = F)
#This looks quadratic
data.frame(x = test@frame$logODAGtot, resid = residuals(test)) %>% 
  ggplot(aes(x = x, y = resid)) + geom_point() + geom_smooth(method = "lm", formula =  "y~x + I(x^2)",se = F)

data.frame(x = predict(test), resid = residuals(test)) %>% 
  ggplot(aes(x = x, y = resid)) + geom_point() + geom_smooth(method = "lm", formula =  "y~x + I(x^2)",se = F)


test <- lmer(log(HHI) ~ POLITY2 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + logODAGtot + I(logODAGtot^2) + (1|CCODE) + (1|YEAR), data = HHI)
summary(test)


test <- lmer(logitHHI ~ POLITY2 + (1|CCODE) + (1|YEAR), data = HHI)
test2 <- lmer(log(HHI) ~ POLITY2 + I(POLITY^2) + (1|CCODE) + (1|YEAR) , data = HHI)
summary(test2)
plot(test2)
qqnorm(residuals(test2))
qqnorm(residuals(test))
  
  
HHI %>% ggplot(aes(x = logitHHI)) + geom_density()
HHI %>% ggplot(aes(x = log(HHI))) + geom_density()




