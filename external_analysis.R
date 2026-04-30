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
setwd("/Users/gregorymatthews/Dropbox/CDSCpappasGit/schrader/")

masterset = read_csv("data/Master 08-28-2025 AFP excel dataset-3.csv")

# cbind(masterset$POLITY, masterset$POLITY2) %>% view()
# out <- masterset %>% select(COUNTRY, YEAR, POLITY, POLITY2) %>% filter(POLITY != POLITY2) 
# write.csv(out, file = "/Users/gregorymatthews/polity_vs_polity2.csv",row.names = TRUE)

diplomaticrep = read_csv("data/Master 08-28-2025 AFP excel dataset-3_diplomatic_represetations.csv")

restofworld = read_excel("data/Rest of the World.xlsx")


## Cleaning data ##
masterset = masterset %>%
  mutate(across(where(is.numeric), ~ na_if(., -99)),
         across(where(is.numeric), ~ na_if(., -88)),
         across(where(is.numeric), ~ na_if(., -77)),
         across(where(is.numeric), ~ na_if(., -66)),
         COUNTRY = case_when(COUNTRY == "seychelles" ~ "Seychelles",
                             COUNTRY == "sao tome & Principe" ~ "Sao Tome & Principe",
                             .default = COUNTRY))


#Manuallty fixing names
names(masterset)[which(names(masterset) == "TRDX352b")] <- "TRDEX352b"
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
    select(CCODE,YEAR,TOTLIB1, POLITY2,GNI, GNI_CAP, POPULATN) %>%
    unique(),
  africa = masterset %>%
    select(CCODE,YEAR,TOTLIB1, POLITY2,GNI, GNI_CAP, POPULATN) %>%
    mutate(CCODE = paste0("C", CCODE)) %>%
    unique()
) %>%
  rbindlist()

#Make South Sudan Correct
#Should be C099 instead of C99
countrycodes <- countrycodes %>% 
  mutate(CCODE = ifelse(CCODE == "C99","C099",CCODE))

external_democracy <- external_democracy %>% 
  mutate(CCODE = ifelse(CCODE == "C99","C099",CCODE))

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
  left_join(external_democracy, by = c("CCODE_EXT" = "CCODE","YEAR" = "YEAR")) %>% 
  rename(POLITY2_EXT = POLITY2, 
         TOTLIB1_EXT = TOTLIB1, 
         GNI_EXT = GNI,
         GNI_CAP_EXT = GNI_CAP, 
         POPULATN_EXT = POPULATN) %>% 
  mutate(GNI_EXT = as.numeric(GNI_EXT),
         GNI_CAP_EXT = as.numeric(GNI_CAP_EXT))

#create odag data
odag_data <- masterset %>% rename(ODAG251 = odag251) %>% 
  select(CCODE_INT = CCODE, COUNTRY_INT = COUNTRY, YEAR, ODAG201:ODAG643) %>% 
  mutate(across(ODAG201:ODAG643, as.numeric)) %>% 
  pivot_longer(cols = ODAG201:ODAG643, names_to ="CCODE_EXT", values_to = "ODAG") %>% 
  mutate(CCODE_EXT = gsub( "ODAG","",CCODE_EXT)) %>%
  mutate(CCODE_EXT = paste0("C", CCODE_EXT),
         CCODE_INT = paste0("C", CCODE_INT)) %>% 
  filter(CCODE_INT != "CNA")

#create trade exports 
trdex_data <- masterset %>% 
  select(CCODE_INT = CCODE, 
         COUNTRY_INT = COUNTRY, 
         YEAR, 
         matches("^TRDEX\\d{3}")) %>% 
  mutate(across(TRDEX351:TRDEX203, as.numeric)) %>% 
  pivot_longer(cols = TRDEX351:TRDEX203, names_to ="CCODE_EXT", values_to = "TRDEX") %>% 
  mutate(CCODE_EXT = gsub( "TRDEX","",CCODE_EXT)) %>% 
  mutate(CCODE_EXT = paste0("C", CCODE_EXT),
         CCODE_INT = paste0("C", CCODE_INT)) %>% 
  filter(CCODE_INT != "CNA")

#create trade imports
trdim_data <- masterset %>% 
  select(CCODE_INT = CCODE, 
         COUNTRY_INT = COUNTRY, 
         YEAR, 
         matches("^TRDIM\\d{3}$")) %>% 
  mutate(across(TRDIM351:TRDIM203, as.numeric)) %>% 
  pivot_longer(cols = TRDIM351:TRDIM203, names_to ="CCODE_EXT", values_to = "TRDIM") %>% 
  mutate(CCODE_EXT = gsub( "TRDIM","",CCODE_EXT)) %>%
  mutate(CCODE_EXT = paste0("C", CCODE_EXT),
         CCODE_INT = paste0("C", CCODE_INT)) %>% 
  filter(CCODE_INT != "CNA")
  
#merge on odag, trdex, trdim
cleandata <- newdata %>% 
  left_join(odag_data %>% select(CCODE_INT,YEAR,CCODE_EXT,ODAG), 
                              by = c("CCODE_INT","CCODE_EXT","YEAR")) %>%
  left_join(trdex_data %>% select(CCODE_INT,YEAR,CCODE_EXT,TRDEX), 
            by = c("CCODE_INT","CCODE_EXT","YEAR")) %>%
  left_join(trdim_data %>% select(CCODE_INT,YEAR,CCODE_EXT,TRDIM), 
            by = c("CCODE_INT","CCODE_EXT","YEAR")) %>% 
  mutate(logGNI_EXT = log(GNI_EXT, 10),
         logPOP_EXT = log(POPULATN_EXT, 10),
         logODAG = log(ODAG+1,10),
         logTRDIM = log(TRDIM+1,10),
         logTRDEX = log(TRDEX+1,10),
         ODAG = ifelse(ODAG == -99, NA, ODAG),
         TRDIM = ifelse(TRDIM == -99, NA, TRDIM)) %>% 
  filter(ODAG >= 0) %>% ungroup() %>% group_by(COUNTRY_INT, COUNTRY_EXT) %>%
  arrange(-YEAR,.by_group = TRUE) %>%
  mutate(CONNECTIONlag1 = lag(CONNECTION))


cleandata %>% view()


#Data Viz
ggplot(aes(y = logODAG), data = cleandata) + geom_boxplot()
ggplot(aes(y = logTRDEX), data = cleandata) + geom_boxplot()
ggplot(aes(y = logTRDIM), data = cleandata) + geom_boxplot()
ggplot(aes(y = logGNI_EXT), data = cleandata) + geom_boxplot()
ggplot(aes(y = logPOP_EXT), data = cleandata) + geom_boxplot()

cleandata_scaled <- cleandata %>%  mutate(across(TOTLIB1_EXT:logTRDEX, scale))

library(lme4)
mod0_TOTLIB1 <- glmer(CONNECTION ~ TOTLIB1_EXT  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT)  + (1|YEAR), family = "binomial", data = cleandata_scaled, control = glmerControl(optimizer = "bobyqa"))
mod1_TOTLIB1 <- glmer(CONNECTION ~ TOTLIB1_EXT  +  logGNI_EXT + logPOP_EXT + logODAG + logTRDEX + logTRDIM +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|YEAR), family = "binomial", data = cleandata_scaled, control = glmerControl(optimizer = "bobyqa"))
mod2_TOTLIB1 <- glmer(CONNECTION ~ TOTLIB1_EXT  + logGNI_EXT + logPOP_EXT + logODAG + logTRDEX + logTRDIM +  (1|COUNTRY_INT) + (1|COUNTRY_EXT)  + (1|YEAR), family = "binomial", data = cleandata_scaled, control = glmerControl(optimizer = "bobyqa"))

mod0_POLITY <- glmer(CONNECTION ~ POLITY2_EXT  +  (1|COUNTRY_INT) + (1|COUNTRY_EXT)  + (1|YEAR), family = "binomial", data = cleandata_scaled, control = glmerControl(optimizer = "bobyqa"))
mod1_POLITY <- glmer(CONNECTION ~ POLITY2_EXT  + logGNI_EXT + logPOP_EXT + logODAG + logTRDEX + logTRDIM +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|YEAR), family = "binomial", data = cleandata_scaled, control = glmerControl(optimizer = "bobyqa"))
mod2_POLITY <- glmer(CONNECTION ~ POLITY2_EXT  + logGNI_EXT + logPOP_EXT + logODAG + logTRDEX + logTRDIM +  (1|COUNTRY_INT) + (1|COUNTRY_EXT) + (1|YEAR), family = "binomial", data = cleandata_scaled, control = glmerControl(optimizer = "bobyqa"))

summary(mod0_TOTLIB1)
summary(mod1_TOTLIB1)
summary(mod2_TOTLIB1)

summary(mod0_POLITY)
summary(mod1_POLITY)
summary(mod2_POLITY)









