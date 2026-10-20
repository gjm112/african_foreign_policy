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

# Summary stuff

# HHI %>% filter(YEAR >= 1960) %>%group_by(YEAR) %>% summarize(meanHHI = mean(HHI, na.rm = T)) 
# HHI %>% filter(YEAR >= 1960) %>%group_by(YEAR) %>% summarize(meanHHI = mean(HHI, na.rm = T))  %>% ggplot(aes(x = YEAR, y = meanHHI)) + geom_point() + geom_line() + theme_bw()
# HHI %>% ggplot(aes(x = YEAR, y = log(HHI))) + geom_point() + geom_smooth(se = F)  + theme_bw()
# HHI %>% ggplot(aes(x = HHI)) + geom_density() + theme_bw()


#
# HHI %>% ggplot(aes(x = POLITY2, y = log(HHI), color = YEAR, group = YEAR)) + geom_point() + geom_smooth(se = F)  + theme_bw()
# HHI %>% ggplot(aes(x = POLITY, y = log(HHI), color = YEAR, group = YEAR)) + geom_point() + geom_smooth(se = F)  + theme_bw()
# HHI %>% ggplot(aes(x = TOTLIB1, y = log(HHI), color = YEAR, group = YEAR)) + geom_point() + geom_smooth(se = F)  + theme_bw()
# HHI %>% ggplot(aes(x = V2x_Libdem, y = log(HHI), color = YEAR, group = YEAR)) + geom_point() + geom_smooth(se = F)  + theme_bw()
# HHI %>% ggplot(aes(x = logODAGtot, y = (HHI))) + geom_point() + geom_smooth(se = F)  + theme_bw()
# HHI %>% ggplot(aes(x = logGNI, y = (HHI))) + geom_point() + geom_smooth(se = F)  + theme_bw()
# HHI %>% ggplot(aes(x = logPOP, y = (HHI))) + geom_point() + geom_smooth(se = F)  + theme_bw()

library(lme4)
library(lmerTest)
library(splines)

# Polity2

mod0_COUNTRIES_POLITY2 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ POLITY2 + (1|CCODE) + (1|YEAR), data = masterset)
mod1_COUNTRIES_POLITY2 <- lmer(log(ODAG_COUNTRIES + 1,10) ~  POLITY2 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset)

mod0_COUNTRIES_POLITY2_lag1 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ POLITY2_lag1 + (1|CCODE) + (1|YEAR), data = masterset)
mod0_COUNTRIES_POLITY2_lag2 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ POLITY2_lag2 + (1|CCODE) + (1|YEAR), data = masterset)
mod0_COUNTRIES_POLITY2_lag3 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ POLITY2_lag3 + (1|CCODE) + (1|YEAR), data = masterset)

mod1_COUNTRIES_POLITY2_lag1 <- lmer(log(ODAG_COUNTRIES + 1,10) ~  POLITY2_lag1 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset)
mod1_COUNTRIES_POLITY2_lag2 <- lmer(log(ODAG_COUNTRIES + 1,10) ~  POLITY2_lag2 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset)
mod1_COUNTRIES_POLITY2_lag3 <- lmer(log(ODAG_COUNTRIES + 1,10) ~  POLITY2_lag3 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset)

# TOTLIB1

mod0_COUNTRIES_TOTLIB1 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ TOTLIB1 + (1|CCODE) + (1|YEAR), data = masterset)
mod1_COUNTRIES_TOTLIB1 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ TOTLIB1 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset)

mod0_COUNTRIES_TOTLIB1_lag1 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ TOTLIB1_lag1 + (1|CCODE) + (1|YEAR), data = masterset)
mod0_COUNTRIES_TOTLIB1_lag2 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ TOTLIB1_lag2 + (1|CCODE) + (1|YEAR), data = masterset)
mod0_COUNTRIES_TOTLIB1_lag3 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ TOTLIB1_lag3 + (1|CCODE) + (1|YEAR), data = masterset)

mod1_COUNTRIES_TOTLIB1_lag1 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ TOTLIB1_lag1 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset)
mod1_COUNTRIES_TOTLIB1_lag2 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ TOTLIB1_lag2 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset)
mod1_COUNTRIES_TOTLIB1_lag3 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ TOTLIB1_lag3 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset)

# V2X_LIBDEM

mod0_COUNTRIES_V2X_LIBDEM <- lmer(log(ODAG_COUNTRIES + 1,10) ~ V2X_LIBDEM + (1|CCODE) + (1|YEAR), data = masterset)
mod1_COUNTRIES_V2X_LIBDEM <- lmer(log(ODAG_COUNTRIES + 1,10) ~ V2X_LIBDEM + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset)

mod0_COUNTRIES_V2X_LIBDEM_lag1 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ V2X_LIBDEM_lag1  + (1|CCODE) + (1|YEAR), data = masterset)
mod0_COUNTRIES_V2X_LIBDEM_lag2 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ V2X_LIBDEM_lag2  + (1|CCODE) + (1|YEAR), data = masterset)
mod0_COUNTRIES_V2X_LIBDEM_lag3 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ V2X_LIBDEM_lag3  + (1|CCODE) + (1|YEAR), data = masterset)

mod1_COUNTRIES_V2X_LIBDEM_lag1 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ V2X_LIBDEM_lag1 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset)
mod1_COUNTRIES_V2X_LIBDEM_lag2 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ V2X_LIBDEM_lag2 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset)
mod1_COUNTRIES_V2X_LIBDEM_lag3 <- lmer(log(ODAG_COUNTRIES + 1,10) ~ V2X_LIBDEM_lag3 + logPOP + logGNI + logTRDIMtot + logTRDEXtot + COLPAST1 + REGION + COLD_WAR + COLD_WAR:IDEOLOGY + (1|CCODE) + (1|YEAR), data = masterset)

library(modelsummary)

modelsummary(
  list(
    "Model 1" = mod0_COUNTRIES_V2X_LIBDEM,
    "Model 2" = mod0_COUNTRIES_V2X_LIBDEM_lag1,
    "Model 3" = mod0_COUNTRIES_V2X_LIBDEM_lag2,
    "Model 4" = mod0_COUNTRIES_V2X_LIBDEM_lag3
  ),
  stars = TRUE, 
  gof_omit = "."
)

save(mod0_COUNTRIES_POLITY2, file = "models/ODAG_COUNTRIES/mod0_COUNTRIES_POLITY2.RData")
save(mod1_COUNTRIES_POLITY2, file = "models/ODAG_COUNTRIES/mod1_COUNTRIES_POLITY2.RData")
save(mod0_COUNTRIES_POLITY2_lag1, file = "models/ODAG_COUNTRIES/mod0_COUNTRIES_POLITY2_lag1.RData")
save(mod0_COUNTRIES_POLITY2_lag2, file = "models/ODAG_COUNTRIES/mod0_COUNTRIES_POLITY2_lag2.RData")
save(mod0_COUNTRIES_POLITY2_lag3, file = "models/ODAG_COUNTRIES/mod0_COUNTRIES_POLITY2_lag3.RData")
save(mod1_COUNTRIES_POLITY2_lag1, file = "models/ODAG_COUNTRIES/mod1_COUNTRIES_POLITY2_lag1.RData")
save(mod1_COUNTRIES_POLITY2_lag2, file = "models/ODAG_COUNTRIES/mod1_COUNTRIES_POLITY2_lag2.RData")
save(mod1_COUNTRIES_POLITY2_lag3, file = "models/ODAG_COUNTRIES/mod1_COUNTRIES_POLITY2_lag3.RData")

save(mod0_COUNTRIES_TOTLIB1, file = "models/ODAG_COUNTRIES/mod0_COUNTRIES_TOTLIB1.RData")
save(mod1_COUNTRIES_TOTLIB1, file = "models/ODAG_COUNTRIES/mod1_COUNTRIES_TOTLIB1.RData")
save(mod0_COUNTRIES_TOTLIB1_lag1, file = "models/ODAG_COUNTRIES/mod0_COUNTRIES_TOTLIB1_lag1.RData")
save(mod0_COUNTRIES_TOTLIB1_lag2, file = "models/ODAG_COUNTRIES/mod0_COUNTRIES_TOTLIB1_lag2.RData")
save(mod0_COUNTRIES_TOTLIB1_lag3, file = "models/ODAG_COUNTRIES/mod0_COUNTRIES_TOTLIB1_lag3.RData")
save(mod1_COUNTRIES_TOTLIB1_lag1, file = "models/ODAG_COUNTRIES/mod1_COUNTRIES_TOTLIB1_lag1.RData")
save(mod1_COUNTRIES_TOTLIB1_lag2, file = "models/ODAG_COUNTRIES/mod1_COUNTRIES_TOTLIB1_lag2.RData")
save(mod1_COUNTRIES_TOTLIB1_lag3, file = "models/ODAG_COUNTRIES/mod1_COUNTRIES_TOTLIB1_lag3.RData")

save(mod0_COUNTRIES_V2X_LIBDEM, file = "models/ODAG_COUNTRIES/mod0_COUNTRIES_V2X_LIBDEM.RData")
save(mod1_COUNTRIES_V2X_LIBDEM, file = "models/ODAG_COUNTRIES/mod1_COUNTRIES_V2X_LIBDEM.RData")
save(mod0_COUNTRIES_V2X_LIBDEM_lag1, file = "models/ODAG_COUNTRIES/mod0_COUNTRIES_V2X_LIBDEM_lag1.RData")
save(mod0_COUNTRIES_V2X_LIBDEM_lag2, file = "models/ODAG_COUNTRIES/mod0_COUNTRIES_V2X_LIBDEM_lag2.RData")
save(mod0_COUNTRIES_V2X_LIBDEM_lag3, file = "models/ODAG_COUNTRIES/mod0_COUNTRIES_V2X_LIBDEM_lag3.RData")
save(mod1_COUNTRIES_V2X_LIBDEM_lag1, file = "models/ODAG_COUNTRIES/mod1_COUNTRIES_V2X_LIBDEM_lag1.RData")
save(mod1_COUNTRIES_V2X_LIBDEM_lag2, file = "models/ODAG_COUNTRIES/mod1_COUNTRIES_V2X_LIBDEM_lag2.RData")
save(mod1_COUNTRIES_V2X_LIBDEM_lag3, file = "models/ODAG_COUNTRIES/mod1_COUNTRIES_V2X_LIBDEM_lag3.RData")

