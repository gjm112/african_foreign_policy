
library(tidyverse)
library(readxl)

masterset = read_excel("data/MASTER DATASET - 9.10.2026.xlsx")

masterset = read_csv("data/MASTER DATASET - 9.10.2026.csv") %>%
  rename_with(toupper)

odagkey = read_csv("data/odag_key.csv", skip = 1) %>%
  na.omit() %>%
  mutate(`Variable Name` = str_to_upper(`Variable Name`),
         `Variable Name` = gsub("[^A-Z0-9]", "", x = `Variable Name`))

## Cleaning data ##

masterset = masterset %>%
  mutate(across(where(is.numeric), ~ na_if(., -99)),
         across(where(is.numeric), ~ na_if(., -88)),
         across(where(is.numeric), ~ na_if(., -77)),
         across(where(is.numeric), ~ na_if(., -66)),
         COUNTRY = case_when(COUNTRY == "seychelles" ~ "Seychelles",
                             COUNTRY == "sao tome & Principe" ~ "Sao Tome & Principe",
                             .default = COUNTRY),
         COLD_WAR = ifelse(YEAR >= 1992, 0, 1), 
         COLPAST1 = as.factor(COLPAST1),
         REGION = as.factor(REGION),
         IDEOLOGY = as.factor(IDEOLOGY))

# Convert commas to periods. 
# It looks like commas are being used as the decimal.  
# Fix the nonsense formatting from excel. 

masterset$GNI <- gsub(",",".",masterset$GNI)
masterset$GNI <- gsub(" ","",masterset$GNI)%>% as.numeric()

masterset$GNI_CAP <- gsub(",",".",masterset$GNI_CAP)
masterset$GNI_CAP <- gsub(" ","",masterset$GNI_CAP)%>% as.numeric()

masterset$POPULATN <- gsub(",",".",masterset$POPULATN)
masterset$POPULATN <- gsub(" ","",masterset$POPULATN) %>% as.numeric()

masterset <- masterset %>% 
  mutate(N_EMBASSY = select(., C099:C572) %>% rowSums(na.rm = T))

# Manually fixing names

names(masterset)[which(names(masterset) == "TRDX352b")] <- "TRDEX352b" # These years are getting dropped.  
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
masterset <- masterset %>% mutate(ODAG600 = as.numeric(ODAG600))

masterset <- masterset %>% 
  mutate(across(c(ODAG600, ODAG601, ODAG609, ODAG610, ODAG613:ODAGARBC, ODAGARBA:ODAGIAEA, ODAGRDBTOT, 
                  ODAGFAO:ODAGWBTOT, ODAGAF:ODAGGGGI), ~ as.numeric(.x))) %>% # Make these variables numeric
  mutate(across(c(ODAG600, ODAG601, ODAG609, ODAG610, ODAG613:ODAGARBC, ODAGARBA:ODAGIAEA, ODAGRDBTOT, 
                  ODAGFAO:ODAGWBTOT, ODAGAF:ODAGGGGI), ~ replace_na(.x, 0))) %>% # Replace NA with 0
  mutate(across(c(ODAG600, ODAG601, ODAG609, ODAG610, ODAG613:ODAGARBC, ODAGARBA:ODAGIAEA, ODAGRDBTOT, 
                  ODAGFAO:ODAGWBTOT, ODAGAF:ODAGGGGI), ~ pmax(.x, 0))) %>% # No negative numbers
  rowwise() %>% 
  mutate(ODAGTOT200_501 = rowSums(across(ODAG201:ODAG501))) %>% 
  mutate(HHI = rowSums((across(ODAG201:ODAG501) / rowSums(across(ODAG201:ODAG501)))^2)) %>% 
  mutate(HHI = ifelse(ODAGTOT200_501 == 0, NA, HHI))

odag_countries = odagkey %>%
  filter(Classification == "Countries") %>%
  pull(`Variable Name`)

odag_io = odagkey %>%
  filter(Classification == "IOs") %>%
  mutate(`Variable Name` = case_when(`Variable Name` == "ODAG615" ~ "ODAG615 IMF", .default = `Variable Name`)) %>%
  pull(`Variable Name`)

odag_total = odagkey %>%
  filter(Classification == "Total") %>%
  pull(`Variable Name`)

masterset = masterset %>%
  mutate(ODAG_COUNTRIES = rowSums(across(all_of(odag_countries)), na.rm = T),
         ODAG_IO = rowSums(across(all_of(odag_io)), na.rm = T),
         ODAG_TOT = ODAG_COUNTRIES + ODAG_IO)

# HHI values over time

# HHI %>% select(COUNTRY, YEAR, HHI, ODAGTOT200_500, ODAG201:ODAG416) %>% write.csv(file = "./HHI.csv", row.names = FALSE)

#HHI %>% ggplot(aes(x = YEAR, y = HHI)) + geom_line() +  facet_wrap(~COUNTRY) + theme_bw()

## Prepare data for analysis

masterset <- masterset %>% mutate(across(TRDEX351:TRDEX203, ~ as.numeric(.x))) %>% 
  mutate(across(TRDIM351:TRDIM203, ~ as.numeric(.x))) %>% 
  mutate(TRDEXTOT = rowSums(pick(TRDEX351:TRDEX250, 
                                 TRDEX300:TRDEX285, 
                                 TRDEX280:TRDEX415,
                                 TRDEX100:TRDEX147,
                                 TRDEX577:TRDEX551,
                                 TRDEX563:TRDEX203),na.rm = TRUE),
         TRDIMTOT = rowSums(pick(TRDIM351:TRDIM250,
                                 TRDIM300:TRDIM285,
                                 TRDIM280:TRDIM415,
                                 TRDIM100:TRDIM147,
                                 TRDIM577:TRDIM551,
                                 TRDIM563:TRDIM203),na.rm = TRUE)) %>% 
  mutate(ODAGTOT = rowSums(pick(ODAG201:ODAG501), na.rm = T),
         ODAGTOT_private = rowSums(pick(c(ODAG600, ODAG601, ODAG609, ODAG610, ODAG613:ODAGARBC, ODAGARBA:ODAGIAEA, ODAGRDBTOT, 
                                          ODAGFAO:ODAGWBTOT, ODAGAF:ODAGGGGI)), na.rm = T)) %>% 
  filter(ODAGTOT_private >= 0,
         ODAG_IO >= 0,
         ODAG_COUNTRIES >= 0,
         ODAG_TOT >= 0) %>%
  mutate(POPULATN = ifelse(POPULATN == -99, NA, POPULATN),
         logGNI = log(GNI, 10),
         logPOP = log(POPULATN,10),
         logTRDIMtot = log(TRDIMTOT + 1,10),
         logTRDEXtot = log(TRDEXTOT + 1,10), 
         logODAGtot = log(ODAGTOT + 1, 10)) 

# Now create the lag variables

masterset <- masterset %>% arrange(CCODE, YEAR) %>% group_by(CCODE) %>% 
  mutate(TOTLIB1_lag1 = lag(TOTLIB1,1),
         TOTLIB1_lag2 = lag(TOTLIB1,2),
         TOTLIB1_lag3 = lag(TOTLIB1,3),
         POLITY_lag1 = lag(POLITY,1),
         POLITY_lag2 = lag(POLITY,2),
         POLITY_lag3 = lag(POLITY,3),
         POLITY2_lag1 = lag(POLITY2,1),
         POLITY2_lag2 = lag(POLITY2,2),
         POLITY2_lag3 = lag(POLITY2,3),
         V2X_LIBDEM_lag1 = lag(V2X_LIBDEM,1),
         V2X_LIBDEM_lag2 = lag(V2X_LIBDEM,2),
         V2X_LIBDEM_lag3 = lag(V2X_LIBDEM,3)
  ) %>% 
  ungroup() 

# EDA
#
# masterset %>% ggplot(aes(x = YEAR, y = N_EMBASSY, group = COUNTRY)) + geom_line() + geom_point() + theme_bw() 
# 
# masterset %>% ggplot(aes(x = v2x_LIBDEM, y = N_EMBASSY)) + facet_wrap(~YEAR) +geom_point() + theme_bw() + geom_smooth()
# masterset %>% ggplot(aes(x = v2x_LIBDEM_lag1, y = N_EMBASSY)) + facet_wrap(~YEAR) +geom_point() + theme_bw() + geom_smooth()
# masterset %>% ggplot(aes(x = v2x_LIBDEM_lag2, y = N_EMBASSY)) + facet_wrap(~YEAR) +geom_point() + theme_bw() + geom_smooth()
# masterset %>% ggplot(aes(x = v2x_LIBDEM_lag3, y = N_EMBASSY)) + facet_wrap(~YEAR) +geom_point() + theme_bw() + geom_smooth()
# 
# masterset %>% ggplot(aes(x = DEMOC, y = N_EMBASSY)) + facet_wrap(~YEAR) +geom_point() + theme_bw() + geom_smooth()
# masterset %>% ggplot(aes(x = DEMOC_lag1, y = N_EMBASSY)) + facet_wrap(~YEAR) +geom_point() + theme_bw() + geom_smooth()
# masterset %>% ggplot(aes(x = DEMOC_lag2, y = N_EMBASSY)) + facet_wrap(~YEAR) +geom_point() + theme_bw() + geom_smooth()
# masterset %>% ggplot(aes(x = DEMOC_lag3, y = N_EMBASSY)) + facet_wrap(~YEAR) +geom_point() + theme_bw() + geom_smooth()
# 
# masterset %>% ggplot(aes(x = TOTLIB1, y = N_EMBASSY)) + facet_wrap(~YEAR) +geom_point() + theme_bw() + geom_smooth()
# masterset %>% ggplot(aes(x = TOTLIB1_lag1, y = N_EMBASSY)) + facet_wrap(~YEAR) +geom_point() + theme_bw() + geom_smooth()
# masterset %>% ggplot(aes(x = TOTLIB1_lag2, y = N_EMBASSY)) + facet_wrap(~YEAR) +geom_point() + theme_bw() + geom_smooth()
# masterset %>% ggplot(aes(x = TOTLIB1_lag3, y = N_EMBASSY)) + facet_wrap(~YEAR) +geom_point() + theme_bw() + geom_smooth()
# 
# Correlations
#
# masterset %>% group_by(YEAR) %>% summarize(cor = cor(N_EMBASSY,DEMOC, use = "pairwise.complete.obs") , pval = cor.test(N_EMBASSY,v2x_LIBDEM)$p.val)
# masterset %>% group_by(YEAR) %>% summarize(cor = cor(N_EMBASSY,DEMOC_lag1, use = "pairwise.complete.obs") , pval = cor.test(N_EMBASSY,v2x_LIBDEM_lag1)$p.val)
# masterset %>% group_by(YEAR) %>% summarize(cor = cor(N_EMBASSY,DEMOC_lag2, use = "pairwise.complete.obs") , pval = cor.test(N_EMBASSY,v2x_LIBDEM_lag2)$p.val)
# masterset %>% group_by(YEAR) %>% summarize(cor = cor(N_EMBASSY,DEMOC_lag3, use = "pairwise.complete.obs") , pval = cor.test(N_EMBASSY,v2x_LIBDEM_lag3)$p.val)
# 
# masterset %>% group_by(YEAR) %>% summarize(cor = cor(N_EMBASSY,v2x_LIBDEM, use = "pairwise.complete.obs") , pval = cor.test(N_EMBASSY,v2x_LIBDEM)$p.val)
# masterset %>% group_by(YEAR) %>% summarize(cor = cor(N_EMBASSY,v2x_LIBDEM_lag1, use = "pairwise.complete.obs") , pval = cor.test(N_EMBASSY,v2x_LIBDEM_lag1)$p.val)
# masterset %>% group_by(YEAR) %>% summarize(cor = cor(N_EMBASSY,v2x_LIBDEM_lag2, use = "pairwise.complete.obs") , pval = cor.test(N_EMBASSY,v2x_LIBDEM_lag2)$p.val)
# masterset %>% group_by(YEAR) %>% summarize(cor = cor(N_EMBASSY,v2x_LIBDEM_lag3, use = "pairwise.complete.obs") , pval = cor.test(N_EMBASSY,v2x_LIBDEM_lag3)$p.val)
# 
# masterset %>% filter(YEAR > 1965 & YEAR < 2024) %>% group_by(YEAR) %>% summarize(cor = cor(N_EMBASSY,TOTLIB1, use = "pairwise.complete.obs") , pval = cor.test(N_EMBASSY,TOTLIB1)$p.val)
# masterset %>% filter(YEAR > 1965 & YEAR < 2024) %>% group_by(YEAR) %>% summarize(cor = cor(N_EMBASSY,TOTLIB1_lag1, use = "pairwise.complete.obs") , pval = cor.test(N_EMBASSY,TOTLIB1_lag1)$p.val)
# masterset %>% filter(YEAR > 1965 & YEAR < 2024) %>% group_by(YEAR) %>% summarize(cor = cor(N_EMBASSY,TOTLIB1_lag2, use = "pairwise.complete.obs") , pval = cor.test(N_EMBASSY,TOTLIB1_lag2)$p.val)
# masterset %>% filter(YEAR > 1965 & YEAR < 2024) %>% group_by(YEAR) %>% summarize(cor = cor(N_EMBASSY,TOTLIB1_lag3, use = "pairwise.complete.obs") , pval = cor.test(N_EMBASSY,TOTLIB1_lag3)$p.val)

library(lme4)
library(lmerTest)
library(splines)

# Polity2

mod0_POLITY2 <- glmer(N_EMBASSY ~ POLITY2 + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod1_POLITY2 <- glmer(N_EMBASSY ~  POLITY2 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))

mod0_POLITY2_lag1 <- glmer(N_EMBASSY ~ POLITY2_lag1 + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod0_POLITY2_lag2 <- glmer(N_EMBASSY ~ POLITY2_lag2 + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod0_POLITY2_lag3 <- glmer(N_EMBASSY ~ POLITY2_lag3 + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))

mod1_POLITY2_lag1 <- glmer(N_EMBASSY ~  POLITY2_lag1 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod1_POLITY2_lag2 <- glmer(N_EMBASSY ~  POLITY2_lag2 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod1_POLITY2_lag3 <- glmer(N_EMBASSY ~  POLITY2_lag3 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))

# TOTLIB1

mod0_TOTLIB1 <- glmer(N_EMBASSY ~ TOTLIB1 + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod1_TOTLIB1 <- glmer(N_EMBASSY ~ TOTLIB1 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))

mod0_TOTLIB1_lag1 <- glmer(N_EMBASSY ~ TOTLIB1_lag1 + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod0_TOTLIB1_lag2 <- glmer(N_EMBASSY ~ TOTLIB1_lag2 + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod0_TOTLIB1_lag3 <- glmer(N_EMBASSY ~ TOTLIB1_lag3 + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))

mod1_TOTLIB1_lag1 <- glmer(N_EMBASSY ~ TOTLIB1_lag1 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod1_TOTLIB1_lag2 <- glmer(N_EMBASSY ~ TOTLIB1_lag2 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod1_TOTLIB1_lag3 <- glmer(N_EMBASSY ~ TOTLIB1_lag3 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))

# V2X_LIBDEM

mod0_V2X_LIBDEM <- glmer(N_EMBASSY ~ V2X_LIBDEM + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod1_V2X_LIBDEM <- glmer(N_EMBASSY ~ V2X_LIBDEM + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))

mod0_V2X_LIBDEM_lag1 <- glmer(N_EMBASSY ~ V2X_LIBDEM_lag1  + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod0_V2X_LIBDEM_lag2 <- glmer(N_EMBASSY ~ V2X_LIBDEM_lag2  + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod0_V2X_LIBDEM_lag3 <- glmer(N_EMBASSY ~ V2X_LIBDEM_lag3  + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))

mod1_V2X_LIBDEM_lag1 <- glmer(N_EMBASSY ~ V2X_LIBDEM_lag1 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod1_V2X_LIBDEM_lag2 <- glmer(N_EMBASSY ~ V2X_LIBDEM_lag2 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
mod1_V2X_LIBDEM_lag3 <- glmer(N_EMBASSY ~ V2X_LIBDEM_lag3 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))

save(mod0_POLITY2, file = "internal_count/models/mod0_POLITY2.RData")
save(mod1_POLITY2, file = "internal_count/models/mod1_POLITY2.RData")
save(mod0_POLITY2_lag1, file = "internal_count/models/mod0_POLITY2_lag1.RData")
save(mod0_POLITY2_lag2, file = "internal_count/models/mod0_POLITY2_lag2.RData")
save(mod0_POLITY2_lag3, file = "internal_count/models/mod0_POLITY2_lag3.RData")
save(mod1_POLITY2_lag1, file = "internal_count/models/mod1_POLITY2_lag1.RData")
save(mod1_POLITY2_lag2, file = "internal_count/models/mod1_POLITY2_lag2.RData")
save(mod1_POLITY2_lag3, file = "internal_count/models/mod1_POLITY2_lag3.RData")

save(mod0_TOTLIB1, file = "internal_count/models/mod0_TOTLIB1.RData")
save(mod1_TOTLIB1, file = "internal_count/models/mod1_TOTLIB1.RData")
save(mod0_TOTLIB1_lag1, file = "internal_count/models/mod0_TOTLIB1_lag1.RData")
save(mod0_TOTLIB1_lag2, file = "internal_count/models/mod0_TOTLIB1_lag2.RData")
save(mod0_TOTLIB1_lag3, file = "internal_count/models/mod0_TOTLIB1_lag3.RData")
save(mod1_TOTLIB1_lag1, file = "internal_count/models/mod1_TOTLIB1_lag1.RData")
save(mod1_TOTLIB1_lag2, file = "internal_count/models/mod1_TOTLIB1_lag2.RData")
save(mod1_TOTLIB1_lag3, file = "internal_count/models/mod1_TOTLIB1_lag3.RData")

save(mod0_V2X_LIBDEM, file = "internal_count/models/mod0_V2X_LIBDEM.RData")
save(mod1_V2X_LIBDEM, file = "internal_count/models/mod1_V2X_LIBDEM.RData")
save(mod0_V2X_LIBDEM_lag1, file = "internal_count/models/mod0_V2X_LIBDEM_lag1.RData")
save(mod0_V2X_LIBDEM_lag2, file = "internal_count/models/mod0_V2X_LIBDEM_lag2.RData")
save(mod0_V2X_LIBDEM_lag3, file = "internal_count/models/mod0_V2X_LIBDEM_lag3.RData")
save(mod1_V2X_LIBDEM_lag1, file = "internal_count/models/mod1_V2X_LIBDEM_lag1.RData")
save(mod1_V2X_LIBDEM_lag2, file = "internal_count/models/mod1_V2X_LIBDEM_lag2.RData")
save(mod1_V2X_LIBDEM_lag3, file = "internal_count/models/mod1_V2X_LIBDEM_lag3.RData")

# mod0_DEMOC <- glmer(N_EMBASSY ~ DEMOC + (1|COUNTRY) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
# mod0_DEMOC_lag1 <- glmer(N_EMBASSY ~ DEMOC_lag1 + (1|COUNTRY) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
# mod0_DEMOC_lag2 <- glmer(N_EMBASSY ~ DEMOC_lag2 + (1|COUNTRY)+ (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
# mod0_DEMOC_lag3 <- glmer(N_EMBASSY ~ DEMOC_lag3 + (1|COUNTRY)+ (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
# 
# summary(mod0_DEMOC)
# summary(mod0_DEMOC_lag1)
# summary(mod0_DEMOC_lag2)
# summary(mod0_DEMOC_lag3)
# 
# mod0_DEMOC_lag123 <- glmer(N_EMBASSY ~ DEMOC + DEMOC_lag1  + DEMOC_lag2  + DEMOC_lag3 +  (1|COUNTRY) + (1|YEAR), data = masterset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
# summary(mod0_DEMOC_lag123)
# 
# # Perform the overdispersion test
# library(performance)
# check_overdispersion(mod0_DEMOC)
# check_overdispersion(mod0_DEMOC_lag1)
# check_overdispersion(mod0_DEMOC_lag2)
# check_overdispersion(mod0_DEMOC_lag3)
# 
# #We see a lot of over disperion so we instead fit a negastive binomial model
# mod0_nb_DEMOC <- glmer.nb(N_EMBASSY ~ DEMOC + (1|COUNTRY) + (1|YEAR), data = masterset, control = glmerControl(optimizer = "bobyqa"))
# mod0_nb_DEMOC_lag1 <- glmer.nb(N_EMBASSY ~ DEMOC_lag1 + (1|COUNTRY)+ (1|YEAR), data = masterset, control = glmerControl(optimizer = "bobyqa"))
# mod0_nb_DEMOC_lag2 <- glmer.nb(N_EMBASSY ~ DEMOC_lag2 + (1|COUNTRY)+ (1|YEAR), data = masterset, control = glmerControl(optimizer = "bobyqa"))
# mod0_nb_DEMOC_lag3 <- glmer.nb(N_EMBASSY ~ DEMOC_lag3 + (1|COUNTRY)+ (1|YEAR), data = masterset, control = glmerControl(optimizer = "bobyqa"))
# summary(mod0_nb_DEMOC)
# summary(mod0_nb_DEMOC_lag1)
# summary(mod0_nb_DEMOC_lag2)
# summary(mod0_nb_DEMOC_lag3)
# getME(mod0_nb_DEMOC, "glmer.nb.theta")
# check_overdispersion(mod0_nb_DEMOC)
# check_overdispersion(mod0_nb_DEMOC_lag1)
# check_overdispersion(mod0_nb_DEMOC_lag2)
# check_overdispersion(mod0_nb_DEMOC_lag3)
# 
# 
# ################################################
# #v2x_LIBDEM
# ################################################
# mod0_v2x_LIBDEM <- glmer(N_EMBASSY ~ v2x_LIBDEM + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
# mod0_v2x_LIBDEM_lag1 <- glmer(N_EMBASSY ~ v2x_LIBDEM_lag1 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
# mod0_v2x_LIBDEM_lag2 <- glmer(N_EMBASSY ~ v2x_LIBDEM_lag2 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
# mod0_v2x_LIBDEM_lag3 <- glmer(N_EMBASSY ~ v2x_LIBDEM_lag3 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
# 
# summary(mod0_v2x_LIBDEM)
# summary(mod0_v2x_LIBDEM_lag1)
# summary(mod0_v2x_LIBDEM_lag2)
# summary(mod0_v2x_LIBDEM_lag3)
# 
# # Perform the overdispersion test
# library(performance)
# check_overdispersion(mod0_v2x_LIBDEM)
# check_overdispersion(mod0_v2x_LIBDEM_lag1)
# check_overdispersion(mod0_v2x_LIBDEM_lag2)
# check_overdispersion(mod0_v2x_LIBDEM_lag3)
# 
# #Overdispersion detected
# mod0_nb_v2x_LIBDEM <- glmer.nb(N_EMBASSY ~ v2x_LIBDEM + (1|COUNTRY)+ (1|YEAR), data = modelset, control = glmerControl(optimizer = "bobyqa"))
# mod0_nb_v2x_LIBDEM_lag1 <- glmer.nb(N_EMBASSY ~ v2x_LIBDEM_lag1 + (1|COUNTRY)+ (1|YEAR), data = modelset, control = glmerControl(optimizer = "bobyqa"))
# mod0_nb_v2x_LIBDEM_lag2 <- glmer.nb(N_EMBASSY ~ v2x_LIBDEM_lag2 + (1|COUNTRY)+ (1|YEAR), data = modelset, control = glmerControl(optimizer = "bobyqa"))
# mod0_nb_v2x_LIBDEM_lag3 <- glmer.nb(N_EMBASSY ~ v2x_LIBDEM_lag3 + (1|COUNTRY)+ (1|YEAR), data = modelset, control = glmerControl(optimizer = "bobyqa"))
# summary(mod0_nb_v2x_LIBDEM)
# summary(mod0_nb_v2x_LIBDEM_lag1)
# summary(mod0_nb_v2x_LIBDEM_lag2)
# summary(mod0_nb_v2x_LIBDEM_lag3)
# getME(mod0_nb_v2x_LIBDEM, "glmer.nb.theta")
# check_overdispersion(mod0_nb_v2x_LIBDEM)
# check_overdispersion(mod0_nb_v2x_LIBDEM_lag1)
# check_overdispersion(mod0_nb_v2x_LIBDEM_lag2)
# check_overdispersion(mod0_nb_v2x_LIBDEM_lag3)
# 
# 
# ################################################
# #TOTLIB
# ################################################
# mod0_TOTLIB1 <- glmer(N_EMBASSY ~ TOTLIB1 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
# mod0_TOTLIB1_lag1 <- glmer(N_EMBASSY ~ TOTLIB1_lag1 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
# mod0_TOTLIB1_lag2 <- glmer(N_EMBASSY ~ TOTLIB1_lag2 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
# mod0_TOTLIB1_lag3 <- glmer(N_EMBASSY ~ TOTLIB1_lag3 + (1|COUNTRY)+ (1|YEAR), data = modelset, family = "poisson", control = glmerControl(optimizer = "bobyqa"))
# 
# summary(mod0_TOTLIB1)
# summary(mod0_TOTLIB1_lag1)
# summary(mod0_TOTLIB1_lag2)
# summary(mod0_TOTLIB1_lag3)
# 
# # Perform the overdispersion test
# library(performance)
# check_overdispersion(mod0_TOTLIB1)
# check_overdispersion(mod0_TOTLIB1_lag1)
# check_overdispersion(mod0_TOTLIB1_lag2)
# check_overdispersion(mod0_TOTLIB1_lag3)
# 
# #Overdispersion detected
# mod0_nb_TOTLIB1 <- glmer.nb(N_EMBASSY ~ TOTLIB1 + (1|COUNTRY)+ (1|YEAR), data = modelset, control = glmerControl(optimizer = "bobyqa"))
# mod0_nb_TOTLIB1_lag1 <- glmer.nb(N_EMBASSY ~ TOTLIB1_lag1 + (1|COUNTRY)+ (1|YEAR), data = modelset, control = glmerControl(optimizer = "bobyqa"))
# mod0_nb_TOTLIB1_lag2 <- glmer.nb(N_EMBASSY ~ TOTLIB1_lag2 + (1|COUNTRY)+ (1|YEAR), data = modelset, control = glmerControl(optimizer = "bobyqa"))
# mod0_nb_TOTLIB1_lag3 <- glmer.nb(N_EMBASSY ~ TOTLIB1_lag3 + (1|COUNTRY)+ (1|YEAR), data = modelset, control = glmerControl(optimizer = "bobyqa"))
# summary(mod0_nb_TOTLIB1)
# summary(mod0_nb_TOTLIB1_lag1)
# summary(mod0_nb_TOTLIB1_lag2)
# summary(mod0_nb_TOTLIB1_lag3)
# getME(mod0_nb_TOTLIB1, "glmer.nb.theta")
# check_overdispersion(mod0_nb_TOTLIB1)
# check_overdispersion(mod0_nb_TOTLIB1_lag1)
# check_overdispersion(mod0_nb_TOTLIB1_lag2)
# check_overdispersion(mod0_nb_TOTLIB1_lag3)
# 
# #Multiple regression
# ################################################
# #DEMOC
# ################################################
# library(lmerTest)
# mod1_DEMOC <- glmer(N_EMBASSY ~ DEMOC  + logGNI + 
#                             logPOPULATN  + REGION + COL2 + 
#                             factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + 
#                             (1|COUNTRY) + (1|YEAR), data = modelset, 
#                           control = glmerControl(optimizer = "bobyqa"), family = "poisson")
# 
# mod1_nb_DEMOC <- glmer.nb(N_EMBASSY ~ DEMOC  + logGNI + 
#                             logPOPULATN  + REGION + COL2 + 
#                             factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + 
#                             (1|COUNTRY) + (1|YEAR), data = modelset, 
#                           control = glmerControl(optimizer = "bobyqa"))
# 
# summary(mod1_nb_DEMOC)
# summary(mod1_DEMOC)
# 
# 
# mod1_v2x_LIBDEM <- glmer(N_EMBASSY ~ v2x_LIBDEM + logGNI + 
#                                  logPOPULATN  + REGION + COL2 + 
#                                  factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + 
#                                  (1|COUNTRY) + (1|YEAR), data = modelset, 
#                                control = glmerControl(optimizer = "bobyqa"), family = "poisson")
# 
# 
# mod1_nb_v2x_LIBDEM <- glmer.nb(N_EMBASSY ~ v2x_LIBDEM + logGNI + 
#                             logPOPULATN  + REGION + COL2 + 
#                             factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + 
#                             (1|COUNTRY) + (1|YEAR), data = modelset, 
#                           control = glmerControl(optimizer = "bobyqa"))
# summary(mod1_v2x_LIBDEM)
# summary(mod1_nb_v2x_LIBDEM)
# 
# 
# mod1_TOTLIB1 <- glmer(N_EMBASSY ~ TOTLIB1  + logGNI + 
#                               logPOPULATN  + REGION + COL2 + 
#                               factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + 
#                               (1|COUNTRY) + (1|YEAR), data = modelset, 
#                             control = glmerControl(optimizer = "bobyqa"), family = "poisson")
# 
# 
# mod1_nb_TOTLIB1 <- glmer.nb(N_EMBASSY ~ TOTLIB1  + logGNI + 
#                                  logPOPULATN  + REGION + COL2 + 
#                                  factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + 
#                                  (1|COUNTRY)+ (1|YEAR), data = modelset, 
#                                control = glmerControl(optimizer = "bobyqa"))
# summary(mod1_TOTLIB1)
# summary(mod1_nb_TOTLIB1)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#  
# modelset %>% ungroup() %>% select(N_EMBASSY, DEMOC, YEAR1965, 
#                       logGNI ,logPOPULATN) %>% cor(use = "complete.obs")
# 
# ggplot(aes(y = N_EMBASSY, x = DEMOC), data = modelset) + geom_point()
# ggplot(aes(y = N_EMBASSY, x = YEAR1965), data = modelset) + geom_point()
# ggplot(aes(y = N_EMBASSY, x = logGNI), data = modelset) + geom_point()
# ggplot(aes(y = N_EMBASSY, x = logPOPULATN), data = modelset) + geom_point()
# ggplot(aes(y = N_EMBASSY, x = REGION), data = modelset) + geom_boxplot()
# ggplot(aes(y = N_EMBASSY, x = COL2), data = modelset) + geom_boxplot()
# ggplot(aes(y = N_EMBASSY, x = factor(IDEOLOGY)), data = modelset) + geom_boxplot()
# ggplot(aes(y = N_EMBASSY, x = factor(AFR_HIST_PERIOD)), data = modelset) + geom_boxplot()
# 
# 
# summary(mod1_nb_DEMOC)
# 
# 
# mod0_v2x_LIBDEM <- glmer(N_EMBASSY ~ v2x_LIBDEM + logGNI + logPOPULATN + REGION + COL2 + factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + (1|COUNTRY), data = modelset, family = "poisson")
# mod0_nb_v2x_LIBDEM <- glmer.nb(N_EMBASSY ~ v2x_LIBDEM + logGNI + logPOPULATN + REGION + COL2 + factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + (1|COUNTRY), data = modelset)
# summary(mod0_v2x_LIBDEM)
# summary(mod0_nb_v2x_LIBDEM)
# 
# mod0_TOTLIB1 <- glmer(N_EMBASSY ~ TOTLIB1 + logGNI + logPOPULATN + REGION + COL2 + factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + (1|COUNTRY), data = modelset, family = "poisson")
# mod0_nb_TOTLIB1 <- glmer.nb(N_EMBASSY ~ TOTLIB1  + logGNI + logPOPULATN + REGION + COL2 + factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + (1|COUNTRY), data = modelset)
# summary(mod0_TOTLIB1)
# summary(mod0_nb_TOTLIB1)
# 
# 
# 
# #Change in embassy
# 
# mod0_delta_DEMOC_PER_YEAR <- lmer(DELTA_N_EMBASSY_PER_YEAR ~ DELTA_DEMOC_PER_YEAR + (1|COUNTRY), data = modelset)
# mod1_delta_DEMOC_PER_YEAR <- lmer(DELTA_N_EMBASSY_PER_YEAR ~ DELTA_DEMOC_PER_YEAR + DELTA_logGNI + DELTA_logPOPULATN + REGION + COL2 + factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + (1|COUNTRY), 
#                                   data = modelset)
# 
# summary(mod0_delta_DEMOC_PER_YEAR)
# summary(mod1_delta_DEMOC_PER_YEAR) 
# 
# mod0_delta_DEMOC <- lmer(DELTA_N_EMBASSY_PER_YEAR ~ DEMOC + (1|COUNTRY), data = modelset)
# mod1_delta_DEMOC <- lmer(DELTA_N_EMBASSY_PER_YEAR ~ DEMOC + logGNI + logPOPULATN + REGION + COL2 + factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + (1|COUNTRY), 
#                          data = modelset)
# 
# summary(mod0_delta_DEMOC)
# summary(mod1_delta_DEMOC)
# 
# mod0_delta_v2x_LIBDEM <- lmer(DELTA_N_EMBASSY_PER_YEAR ~ v2x_LIBDEM + (1|COUNTRY), data = modelset)
# mod1_delta_v2x_LIBDEM <- lmer(DELTA_N_EMBASSY_PER_YEAR ~ v2x_LIBDEM + logGNI + logPOPULATN + REGION + COL2 + factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + (1|COUNTRY), 
#                               data = modelset)
# 
# summary(mod0_delta_v2x_LIBDEM)
# summary(mod1_delta_v2x_LIBDEM)
# 
# mod0_delta_TOTLIB1 <- lmer(DELTA_N_EMBASSY_PER_YEAR ~ TOTLIB1 + (1|COUNTRY), data = modelset)
# mod1_delta_TOTLIB1 <- lmer(DELTA_N_EMBASSY_PER_YEAR ~ TOTLIB1 + logGNI + logPOPULATN + REGION + COL2 + factor(IDEOLOGY) + factor(AFR_HIST_PERIOD) + (1|COUNTRY), 
#                            data = modelset)
# 
# summary(mod0_delta_TOTLIB1) 
# summary(mod1_delta_TOTLIB1)
