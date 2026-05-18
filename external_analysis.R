# Connection ~ Totlib (of external country) +
#   population + wealth + 
#   Imports + Exports + foreign aid + (Coldwar / post cold war?)
# (1|Africa Country) + (1|Year) + (1|Country?)
## Load in packages and data sets ##
library(tidyverse)
library(readxl)

#1. Total number and diversification of African embassies maintained abroad.
#2. Total amount and diversification of African trade imports and trade exports
#3. Total amount and diversification of foreign aid received by African countries
#setwd("schrader")
setwd("/Users/gregorymatthews/Dropbox/african_foreign_policy_git/")

# masterset = read_csv("data/Master 08-28-2025 AFP excel dataset-3.csv")
masterset = read_csv("data2026/MASTER DATASET 3.26.2026.csv")

# cbind(masterset$POLITY, masterset$POLITY2) %>% view()
# out <- masterset %>% select(COUNTRY, YEAR, POLITY, POLITY2) %>% filter(POLITY != POLITY2) 
# write.csv(out, file = "/Users/gregorymatthews/polity_vs_polity2.csv",row.names = TRUE)

#diplomaticrep = read_csv("data/Master 08-28-2025 AFP excel dataset-3_diplomatic_represetations.csv")

# restofworld = read_excel("data/Rest of the World.xlsx")
restofworld = read_excel("data2026/Rest of the World - 5.6.2026.xlsx")


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

##############
#Add lags!!!
##############

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


restofworld <- restofworld %>% 
  rename(v2x_LIBDEM = V2x_Libdem) %>%
  mutate(GNI = as.numeric(GNI),
         GNI_CAP = as.numeric(GNI_CAP),
         AVLIFEEX = as.numeric(AVLIFEEX),
         ENRGUSE = as.numeric(ENRGUSE)) %>% 
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
restofworld$GNI <- gsub(",",".",restofworld$GNI)
restofworld$GNI <- gsub(" ","",restofworld$GNI)%>% as.numeric()

restofworld$GNI_CAP <- gsub(",",".",restofworld$GNI_CAP)
restofworld$GNI_CAP <- gsub(" ","",restofworld$GNI_CAP)%>% as.numeric()

restofworld$POPULATN <- gsub(",",".",restofworld$POPULATN)
restofworld$POPULATN <- gsub(" ","",restofworld$POPULATN) %>% as.numeric()


## WORK INVOLVING NOTES FROM 3-27-26 ##

library(data.table)
#Get all the countries
countrycodes <- list(
  world = restofworld %>%
    select(CCODE:COUNTRY) %>%
    unique(),
  africa = masterset %>%
    select(CCODE:COUNTRY) %>%
    mutate(CCODE = paste0("C", CCODE)) %>%
    unique()
) %>%
  rbindlist()

#Merge on  TOTLIB1, POLITY, v2x_LIBDEM for external country
#Rest of world is Missing v2x_LIBDEM
#external democracy, GNI, GNI_CAP, POPULATN
external_democracy <- list(
  world = restofworld %>%
    select(CCODE,YEAR,TOTLIB1, POLITY, POLITY2,GNI, GNI_CAP, POPULATN) %>%
    unique(),
  africa = masterset %>%
    select(CCODE,YEAR,TOTLIB1, POLITY, POLITY2,GNI, GNI_CAP, POPULATN) %>%
    mutate(CCODE = paste0("C", CCODE)) %>%
    unique()
) %>% rbindlist() 
  

#Make South Sudan Correct
#Should be C099 instead of C99
countrycodes <- countrycodes %>% 
  mutate(CCODE = ifelse(CCODE == "C99","C099",CCODE))

external_democracy <- external_democracy %>% 
  mutate(CCODE = ifelse(CCODE == "C99","C099",CCODE)) %>%
  rename_with(
    .fn = ~ paste0(.x, "_EXT"),
    .cols = -YEAR
  ) %>%
  arrange(CCODE_EXT, YEAR) %>% 
  group_by(CCODE_EXT)  %>% 
  mutate(TOTLIB1_EXT_lag1 = lag(TOTLIB1_EXT,1),
         TOTLIB1_EXT_lag2 = lag(TOTLIB1_EXT,2),
         TOTLIB1_EXT_lag3 = lag(TOTLIB1_EXT,3),
         POLITY_EXT_lag1 = lag(POLITY_EXT,1),
         POLITY_EXT_lag2 = lag(POLITY_EXT,2),
         POLITY_EXT_lag3 = lag(POLITY_EXT,3),
         POLITY2_EXT_lag1 = lag(POLITY2_EXT,1),
         POLITY2_EXT_lag2 = lag(POLITY2_EXT,2),
         POLITY2_EXT_lag3 = lag(POLITY2_EXT,3)
  ) %>% 
  ungroup()

#TOTLIB1, POLITY, v2x_LIBDEM 
#Merge these on for the EXTERNAL country
newdata <- masterset %>%
  select(CCODE_INT = CCODE, COUNTRY_INT = COUNTRY, YEAR, C099:C572) %>%
  mutate(CCODE_INT = paste0("C", CCODE_INT)) %>%
  pivot_longer(!CCODE_INT:YEAR, names_to = "CCODE_EXT", values_to = "CONNECTION") %>%
  left_join(countrycodes, by = c("CCODE_EXT" = "CCODE"), relationship = "many-to-many") %>%
  select(CONNECTION, CCODE_INT, COUNTRY_INT, CCODE_EXT, COUNTRY_EXT = COUNTRY, 
         everything()) %>%
  filter(CONNECTION %in% c(0, 1), CCODE_INT != CCODE_EXT) %>% 
  left_join(external_democracy, by = c("CCODE_EXT" = "CCODE_EXT","YEAR" = "YEAR")) %>% 
  mutate(GNI_EXT = as.numeric(GNI_EXT),
         GNI_CAP_EXT = as.numeric(GNI_CAP_EXT))

#create odag data
odag_data <- masterset %>% rename(ODAG251 = odag251) %>% 
  select(CCODE_INT = CCODE, COUNTRY_INT = COUNTRY, YEAR, ODAG201:ODAG643, ODAG251) %>% 
  mutate(across(c(ODAG201:ODAG643,ODAG251), ~ gsub(",", ".", .x))) %>% 
  mutate(across(c(ODAG201:ODAG643,ODAG251), as.numeric)) %>% 
  pivot_longer(cols = c(ODAG201:ODAG643,ODAG251), names_to ="CCODE_EXT", values_to = "ODAG") %>% 
  mutate(CCODE_EXT = gsub( "ODAG","",CCODE_EXT)) %>%
  mutate(CCODE_EXT = paste0("C", CCODE_EXT),
         CCODE_INT = paste0("C", CCODE_INT)) %>% 
  filter(CCODE_INT != "CNA" & ODAG != -99) 

#create trade exports 
trdex_data <- masterset %>% 
  select(CCODE_INT = CCODE, 
         COUNTRY_INT = COUNTRY, 
         YEAR, 
         matches("^TRDEX\\d{3}")) %>% 
  mutate(across(TRDEX351:TRDEX203, ~ gsub(",", ".", .x))) %>% 
  mutate(across(TRDEX351:TRDEX203, as.numeric)) %>% 
  mutate(TRDEX200 = ifelse((is.na(TRDEX200) &
                              is.na(TRDEX200h) &
                              is.na(TRDEX200h)),
                           NA,
                           rowSums(across(c(TRDEX200, TRDEX200h, TRDEX200m)), na.rm = TRUE)
  )) %>%
  mutate(TRDEX321 = ifelse((is.na(TRDEX321sm) &
                              is.na(TRDEX321sr) &
                              is.na(TRDEX321yr)),
                           NA,
                           rowSums(across(c(TRDEX321sm, TRDEX321sr, TRDEX321yr)), na.rm = TRUE)
  )) %>% #combining 321 yr, sm, se
  pivot_longer(cols = TRDEX351:TRDEX321, names_to ="CCODE_EXT", values_to = "TRDEX") %>% 
  mutate(CCODE_EXT = gsub( "TRDEX","",CCODE_EXT)) %>% 
  mutate(CCODE_EXT = paste0("C", CCODE_EXT),
         CCODE_INT = paste0("C", CCODE_INT)) %>% 
  filter(CCODE_INT != "CNA")

#create trade imports
trdim_data <- masterset %>% 
  select(CCODE_INT = CCODE, 
         COUNTRY_INT = COUNTRY, 
         YEAR, 
         matches("^TRDIM\\d{3}")) %>% 
  mutate(across(TRDIM351:TRDIM203, ~ gsub(",", ".", .x))) %>% 
  mutate(across(TRDIM351:TRDIM203, as.numeric)) %>% 
  mutate(TRDIM200 = ifelse((is.na(TRDIM200) &
                              is.na(TRDIM200h) &
                              is.na(TRDIM200h)),
                           NA,
                           rowSums(across(c(TRDIM200, TRDIM200h, TRDIM200m)), na.rm = TRUE)
  )) %>% #summing 200, 200m, 200h to combine China
  mutate(TRDIM321 = ifelse((is.na(TRDIM321sm) &
                              is.na(TRDIM321sr) &
                              is.na(TRDIM321yr)),
                           NA,
                           rowSums(across(c(TRDIM321sm, TRDIM321sr, TRDIM321yr)), na.rm = TRUE)
  )) %>% #combining 321 yr, sm, se
  mutate(TRDIM316 = ifelse((is.na(TRDIM316) &
                              is.na(TRDIM316su)),
                           NA,
                           rowSums(across(c(TRDIM316,TRDIM316su)), na.rm = TRUE)
  )) %>% #combining 316 and 316su (i.e. Russia and Soviet Union)
  pivot_longer(cols = TRDIM351:TRDIM321, names_to ="CCODE_EXT", values_to = "TRDIM") %>% 
  mutate(CCODE_EXT = gsub( "TRDIM","",CCODE_EXT)) %>%
  mutate(CCODE_EXT = paste0("C", CCODE_EXT),
         CCODE_INT = paste0("C", CCODE_INT)) %>% 
  filter(CCODE_INT != "CNA") %>% filter(!(CCODE_INT == "C114" & CCODE_EXT == "C148" & YEAR == 1972))
  
#merge on odag, trdex, trdim
cleandata <- newdata %>% 
  left_join(odag_data %>% select(CCODE_INT,YEAR,CCODE_EXT,ODAG), 
                              by = c("CCODE_INT","CCODE_EXT","YEAR")) %>%
  left_join(trdex_data %>% select(CCODE_INT,YEAR,CCODE_EXT,TRDEX), 
            by = c("CCODE_INT","CCODE_EXT","YEAR")) %>%
  left_join(trdim_data %>% select(CCODE_INT,YEAR,CCODE_EXT,TRDIM), 
            by = c("CCODE_INT","CCODE_EXT","YEAR")) %>% 
  mutate( ODAG = ifelse(ODAG == -99, NA, ODAG),
          TRDIM = ifelse(TRDIM == -99, NA, TRDIM),
          TRDEX = ifelse(TRDEX == -99, NA, TRDEX),
          GNI_EXT = ifelse(GNI_EXT == -99, NA, GNI_EXT),
          POPULATN_EXT = ifelse(POPULATN_EXT == -99, NA, POPULATN_EXT),
          logGNI_EXT = log(GNI_EXT, 10),
          logPOP_EXT = log(POPULATN_EXT, 10),
          logODAG = log(ODAG+1,10),
          logTRDIM = log(TRDIM+1,10),
          logTRDEX = log(TRDEX+1,10)
  ) %>% ungroup() 


#Data Viz
# ggplot(aes(y = logODAG), data = cleandata) + geom_boxplot()
# ggplot(aes(y = logTRDEX), data = cleandata) + geom_boxplot()
# ggplot(aes(y = logTRDIM), data = cleandata) + geom_boxplot()
# ggplot(aes(y = logGNI_EXT), data = cleandata) + geom_boxplot()
# ggplot(aes(y = logPOP_EXT), data = cleandata) + geom_boxplot()
# cleandata_scaled <- cleandata %>%  mutate(across(TOTLIB1_EXT:logTRDEX, scale))

#Add lags 

# cleandata <- cleandata %>% arrange(COUNTRY_INT,COUNTRY_EXT, YEAR) %>% group_by(COUNTRY_INT, COUNTRY_EXT)  %>% 
#   mutate(TOTLIB1_EXT_lag1 = lag(TOTLIB1_EXT,1),
#          TOTLIB1_EXT_lag2 = lag(TOTLIB1_EXT,2),
#          TOTLIB1_EXT_lag3 = lag(TOTLIB1_EXT,3),
#          POLITY_EXT_lag1 = lag(POLITY_EXT,1),
#          POLITY_EXT_lag2 = lag(POLITY_EXT,2),
#          POLITY_EXT_lag3 = lag(POLITY_EXT,3),
#          POLITY2_EXT_lag1 = lag(POLITY2_EXT,1),
#          POLITY2_EXT_lag2 = lag(POLITY2_EXT,2),
#          POLITY2_EXT_lag3 = lag(POLITY2_EXT,3)
#   ) %>%  ungroup()


library(lme4)
#Base models 

library(lme4)
mod0_TOTLIB1 <- glmer(CONNECTION ~ TOTLIB1_EXT + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))
mod0_TOTLIB1_lag1 <- glmer(CONNECTION ~ TOTLIB1_EXT_lag1  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT)  + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))
mod0_TOTLIB1_lag2 <- glmer(CONNECTION ~ TOTLIB1_EXT_lag2  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT)  + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))
mod0_TOTLIB1_lag3 <- glmer(CONNECTION ~ TOTLIB1_EXT_lag3  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT)  + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))

save(mod0_TOTLIB1, file = "./externalmodels/mod0_TOTLIB1.RData")
save(mod0_TOTLIB1_lag1, file = "./externalmodels/mod0_TOTLIB1_lag1.RData")
save(mod0_TOTLIB1_lag2, file = "./externalmodels/mod0_TOTLIB1_lag2.RData")
save(mod0_TOTLIB1_lag3, file = "./externalmodels/mod0_TOTLIB1_lag3.RData")

mod0_POLITY <- glmer(CONNECTION ~ POLITY_EXT  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) +   (1|COUNTRY_INT:COUNTRY_EXT)  + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))
mod0_POLITY_lag1 <- glmer(CONNECTION ~ POLITY_EXT_lag1  + (1|COUNTRY_INT) + (1|COUNTRY_EXT) +   (1|COUNTRY_INT:COUNTRY_EXT)   + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))
mod0_POLITY_lag2 <- glmer(CONNECTION ~ POLITY_EXT_lag2  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) +   (1|COUNTRY_INT:COUNTRY_EXT)   + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))
mod0_POLITY_lag3 <- glmer(CONNECTION ~ POLITY_EXT_lag3  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT)   + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))

save(mod0_POLITY, file = "./externalmodels/mod0_POLITY.RData")
save(mod0_POLITY_lag1, file = "./externalmodels/mod0_POLITY_lag1.RData")
save(mod0_POLITY_lag2, file = "./externalmodels/mod0_POLITY_lag2.RData")
save(mod0_POLITY_lag3, file = "./externalmodels/mod0_POLITY_lag3.RData")

mod0_POLITY2 <- glmer(CONNECTION ~ POLITY2_EXT  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) +   (1|COUNTRY_INT:COUNTRY_EXT)  + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))
mod0_POLITY2_lag1 <- glmer(CONNECTION ~ POLITY2_EXT_lag1  + (1|COUNTRY_INT) + (1|COUNTRY_EXT) +   (1|COUNTRY_INT:COUNTRY_EXT)   + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))
mod0_POLITY2_lag2 <- glmer(CONNECTION ~ POLITY2_EXT_lag2  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) +   (1|COUNTRY_INT:COUNTRY_EXT)   + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))
mod0_POLITY2_lag3 <- glmer(CONNECTION ~ POLITY2_EXT_lag3  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT)   + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))

save(mod0_POLITY2, file = "./externalmodels/mod0_POLITY2.RData")
save(mod0_POLITY2_lag1, file = "./externalmodels/mod0_POLITY2_lag1.RData")
save(mod0_POLITY2_lag2, file = "./externalmodels/mod0_POLITY2_lag2.RData")
save(mod0_POLITY2_lag3, file = "./externalmodels/mod0_POLITY2_lag3.RData")

mod0_v2x_LIBDEM <- glmer(CONNECTION ~ v2x_LIBDEM_EXT  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) +   (1|COUNTRY_INT:COUNTRY_EXT)  + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))
mod0_v2x_LIBDEM_lag1 <- glmer(CONNECTION ~ v2x_LIBDEM_EXT_lag1  + (1|COUNTRY_INT) + (1|COUNTRY_EXT) +   (1|COUNTRY_INT:COUNTRY_EXT)   + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))
mod0_v2x_LIBDEM_lag2 <- glmer(CONNECTION ~ v2x_LIBDEM_EXT_lag2  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) +   (1|COUNTRY_INT:COUNTRY_EXT)   + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))
mod0_v2x_LIBDEM_lag3 <- glmer(CONNECTION ~ v2x_LIBDEM_EXT_lag3  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT)   + (1|YEAR), family = "binomial", data = cleandata, control = glmerControl(optimizer = "bobyqa"))

save(mod0_v2x_LIBDEM, file = "./externalmodels/mod0_v2x_LIBDEM.RData")
save(mod0_v2x_LIBDEM_lag1, file = "./externalmodels/mod0_v2x_LIBDEM_lag1.RData")
save(mod0_v2x_LIBDEM_lag2, file = "./externalmodels/mod0_v2x_LIBDEM_lag2.RData")
save(mod0_v2x_LIBDEM_lag3, file = "./externalmodels/mod0_POLITY2_lag3.RData")

#Full models 
mod1_TOTLIB1 <- glmer(CONNECTION ~ TOTLIB1_EXT + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                      family = "binomial", 
                      data = cleandata, 
                      control = glmerControl(optimizer = "bobyqa"))

mod1_TOTLIB1_lag1 <- glmer(CONNECTION ~ TOTLIB1_EXT_lag1 + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

mod1_TOTLIB1_lag2 <- glmer(CONNECTION ~ TOTLIB1_EXT_lag2 + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

mod1_TOTLIB1_lag3 <- glmer(CONNECTION ~ TOTLIB1_EXT_lag3 + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

save(mod1_TOTLIB1, file = "./externalmodels/mod1_TOTLIB1.RData")
save(mod1_TOTLIB1_lag1, file = "./externalmodels/mod1_TOTLIB1_lag1.RData")
save(mod1_TOTLIB1_lag2, file = "./externalmodels/mod1_TOTLIB1_lag2.RData")
save(mod1_TOTLIB1_lag3, file = "./externalmodels/mod1_TOTLIB1_lag3.RData")


mod1_POLITY <- glmer(CONNECTION ~ POLITY_EXT + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                     family = "binomial", 
                     data = cleandata, 
                     control = glmerControl(optimizer = "bobyqa"))

mod1_POLITY_lag1 <- glmer(CONNECTION ~ POLITY_EXT_lag1 + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                          family = "binomial", 
                          data = cleandata, 
                          control = glmerControl(optimizer = "bobyqa"))

mod1_POLITY_lag2 <- glmer(CONNECTION ~ POLITY_EXT_lag2 + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                          family = "binomial", 
                          data = cleandata, 
                          control = glmerControl(optimizer = "bobyqa"))

mod1_POLITY_lag3 <- glmer(CONNECTION ~ POLITY_EXT_lag3 + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                          family = "binomial", 
                          data = cleandata, 
                          control = glmerControl(optimizer = "bobyqa"))

save(mod1_POLITY, file = "./externalmodels/mod1_POLITY.RData")
save(mod1_POLITY_lag1, file = "./externalmodels/mod1_POLITY_lag1.RData")
save(mod1_POLITY_lag2, file = "./externalmodels/mod1_POLITY_lag2.RData")
save(mod1_POLITY_lag3, file = "./externalmodels/mod1_POLITY_lag3.RData")

mod1_POLITY2 <- glmer(CONNECTION ~ POLITY2_EXT + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                      family = "binomial", 
                      data = cleandata, 
                      control = glmerControl(optimizer = "bobyqa"))

mod1_POLITY2_lag1 <- glmer(CONNECTION ~ POLITY2_EXT_lag1 + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

mod1_POLITY2_lag2 <- glmer(CONNECTION ~ POLITY2_EXT_lag2 + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

mod1_POLITY2_lag3 <- glmer(CONNECTION ~ POLITY2_EXT_lag3 + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

save(mod1_POLITY2, file = "./externalmodels/mod1_POLITY2.RData")
save(mod1_POLITY2_lag1, file = "./externalmodels/mod1_POLITY2_lag1.RData")
save(mod1_POLITY2_lag2, file = "./externalmodels/mod1_POLITY2_lag2.RData")
save(mod1_POLITY2_lag3, file = "./externalmodels/mod1_POLITY2_lag3.RData")

mod1_v2x_LIBDEM <- glmer(CONNECTION ~ v2x_LIBDEM_EXT + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                      family = "binomial", 
                      data = cleandata, 
                      control = glmerControl(optimizer = "bobyqa"))

mod1_v2x_LIBDEM_lag1 <- glmer(CONNECTION ~ v2x_LIBDEM_EXT_lag1 + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

mod1_v2x_LIBDEM_lag2 <- glmer(CONNECTION ~ v2x_LIBDEM_EXT_lag2 + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

mod1_v2x_LIBDEM_lag3 <- glmer(CONNECTION ~ v2x_LIBDEM_EXT_lag3 + logGNI_EXT + logPOP_EXT + logODAG + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

save(mod1_v2x_LIBDEM, file = "./externalmodels/mod1_v2x_LIBDEM.RData")
save(mod1_v2x_LIBDEM_lag1, file = "./externalmodels/mod1_v2x_LIBDEM_lag1.RData")
save(mod1_v2x_LIBDEM_lag2, file = "./externalmodels/mod1_v2x_LIBDEM_lag2.RData")
save(mod1_v2x_LIBDEM_lag3, file = "./externalmodels/mod1_v2x_LIBDEM_lag3.RData")

#Full models without ODAG
mod1_TOTLIB1_noODAG <- glmer(CONNECTION ~ TOTLIB1_EXT + 
                        logGNI_EXT + 
                        logPOP_EXT + 
                        logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                      family = "binomial", 
                      data = cleandata, 
                      control = glmerControl(optimizer = "bobyqa"))

mod1_TOTLIB1_lag1_noODAG <- glmer(CONNECTION ~ TOTLIB1_EXT_lag1 + logGNI_EXT + logPOP_EXT  + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

mod1_TOTLIB1_lag2_noODAG <- glmer(CONNECTION ~ TOTLIB1_EXT_lag2 + logGNI_EXT + logPOP_EXT  + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

mod1_TOTLIB1_lag3_noODAG <- glmer(CONNECTION ~ TOTLIB1_EXT_lag3 + logGNI_EXT + logPOP_EXT + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

save(mod1_TOTLIB1_noODAG, file = "./externalmodels/mod1_TOTLIB1_noODAG.RData")
save(mod1_TOTLIB1_lag1_noODAG, file = "./externalmodels/mod1_TOTLIB1_lag1_noODAG.RData")
save(mod1_TOTLIB1_lag2_noODAG, file = "./externalmodels/mod1_TOTLIB1_lag2_noODAG.RData")
save(mod1_TOTLIB1_lag3_noODAG, file = "./externalmodels/mod1_TOTLIB1_lag3_noODAG.RData")


mod1_POLITY_noODAG <- glmer(CONNECTION ~ POLITY_EXT + logGNI_EXT + logPOP_EXT +  logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                     family = "binomial", 
                     data = cleandata, 
                     control = glmerControl(optimizer = "bobyqa"))

mod1_POLITY_lag1_noODAG <- glmer(CONNECTION ~ POLITY_EXT_lag1 + logGNI_EXT + logPOP_EXT + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                          family = "binomial", 
                          data = cleandata, 
                          control = glmerControl(optimizer = "bobyqa"))

mod1_POLITY_lag2_noODAG <- glmer(CONNECTION ~ POLITY_EXT_lag2 + logGNI_EXT + logPOP_EXT  + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                          family = "binomial", 
                          data = cleandata, 
                          control = glmerControl(optimizer = "bobyqa"))

mod1_POLITY_lag3_noODAG <- glmer(CONNECTION ~ POLITY_EXT_lag3 + logGNI_EXT + logPOP_EXT  + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                          family = "binomial", 
                          data = cleandata, 
                          control = glmerControl(optimizer = "bobyqa"))

save(mod1_POLITY_noODAG, file = "./externalmodels/mod1_POLITY_noODAG.RData")
save(mod1_POLITY_lag1_noODAG, file = "./externalmodels/mod1_POLITY_lag1_noODAG.RData")
save(mod1_POLITY_lag2_noODAG, file = "./externalmodels/mod1_POLITY_lag2_noODAG.RData")
save(mod1_POLITY_lag3_noODAG, file = "./externalmodels/mod1_POLITY_lag3_noODAG.RData")



mod1_POLITY2_noODAG <- glmer(CONNECTION ~ POLITY2_EXT + logGNI_EXT + logPOP_EXT + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                      family = "binomial", 
                      data = cleandata, 
                      control = glmerControl(optimizer = "bobyqa"))

mod1_POLITY2_lag1_noODAG <- glmer(CONNECTION ~ POLITY2_EXT_lag1 + logGNI_EXT + logPOP_EXT  + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

mod1_POLITY2_lag2_noODAG <- glmer(CONNECTION ~ POLITY2_EXT_lag2 + logGNI_EXT + logPOP_EXT + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

mod1_POLITY2_lag3_noODAG <- glmer(CONNECTION ~ POLITY2_EXT_lag3 + logGNI_EXT + logPOP_EXT  + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                           family = "binomial", 
                           data = cleandata, 
                           control = glmerControl(optimizer = "bobyqa"))

save(mod1_POLITY2_noODAG, file = "./externalmodels/mod1_POLITY2_noODAG.RData")
save(mod1_POLITY2_lag1_noODAG, file = "./externalmodels/mod1_POLITY2_lag1_noODAG.RData")
save(mod1_POLITY2_lag2_noODAG, file = "./externalmodels/mod1_POLITY2_lag2_noODAG.RData")
save(mod1_POLITY2_lag3_noODAG, file = "./externalmodels/mod1_POLITY2_lag3_noODAG.RData")

mod1_v2x_LIBDEM_noODAG <- glmer(CONNECTION ~ v2x_LIBDEM_EXT + logGNI_EXT + logPOP_EXT + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                             family = "binomial", 
                             data = cleandata, 
                             control = glmerControl(optimizer = "bobyqa"))

mod1_v2x_LIBDEM_lag1_noODAG <- glmer(CONNECTION ~ v2x_LIBDEM_EXT_lag1 + logGNI_EXT + logPOP_EXT  + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                                  family = "binomial", 
                                  data = cleandata, 
                                  control = glmerControl(optimizer = "bobyqa"))

mod1_v2x_LIBDEM_lag2_noODAG <- glmer(CONNECTION ~ v2x_LIBDEM_EXT_lag2 + logGNI_EXT + logPOP_EXT + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                                  family = "binomial", 
                                  data = cleandata, 
                                  control = glmerControl(optimizer = "bobyqa"))

mod1_v2x_LIBDEM_lag3_noODAG <- glmer(CONNECTION ~ v2x_LIBDEM_EXT_lag3 + logGNI_EXT + logPOP_EXT  + logTRDIM + (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|COUNTRY_INT:COUNTRY_EXT) + (1|YEAR), 
                                  family = "binomial", 
                                  data = cleandata, 
                                  control = glmerControl(optimizer = "bobyqa"))

save(mod1_v2x_LIBDEM_noODAG, file = "./externalmodels/mod1_v2x_LIBDEM_noODAG.RData")
save(mod1_v2x_LIBDEM_lag1_noODAG, file = "./externalmodels/mod1_v2x_LIBDEM_lag1_noODAG.RData")
save(mod1_v2x_LIBDEM_lag2_noODAG, file = "./externalmodels/mod1_v2x_LIBDEM_lag2_noODAG.RData")
save(mod1_v2x_LIBDEM_lag3_noODAG, file = "./externalmodels/mod1_v2x_LIBDEM_lag3_noODAG.RData")

## ---- Testing New Data ----

view(read_excel("data2026/Rest of the World - 5.6.2026.xlsx"))


