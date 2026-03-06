####################################
#Notes
####################################
# Statistics – African Foreign Policy 2/27/2026
# 
# BROAD QUESTION
# Broad question: Does the process of democratization within African
# countries affect the formulation and implementation of African foreign 
# policies.
# 
# THERE ARE THREE DIFFERENT SETS OF STATISTICALLY BASED QUESTIONS:
#   1.	African democracy and African embassies. Do greater levels of democratization of African countries lead to larger and more diversified embassy networks maintained by African countries overseas? Dependent variable: embassies maintained abroad (yes/no by country) in 1965, 1975, 1985, 1995, 2000, 2005, 2010, 2015, 2020, 2025 (use 2024 for 2025).
# 2.	African democracy and African trade. Do greater levels of democratization within African countries lead to larger and more diversified levels of trade imports and trade exports by African countries? Dependent variable: $ amount of imports and exports annually for 1960 to 2022)
# 3.	African democracy and aid to Africa. Do greater levels of democratization within African countries lead to larger and more diversified levels of foreign aid to African countries? (Dependent variable: $ amount of aid annually for every country in the world 1960 to 2021).
# 
# THUS, THREE DIFFERENT DEPENDENT VARIABLES:
#   1.	Total number and diversification of African embassies maintained abroad.
# 2.	Total amount and diversification of African trade imports and trade exports
# 3.	Total amount and diversification of foreign aid received by African countries
# 
# Question: can the Herfindahl-Hirschman Index (HHI) be used for all 3 questions, or only questions #2 and #3 (what I was told before)
# 
# THREE DIFFERENT VARIABLES OF AFRICAN DEMOCRACY AS INDEPENDENT VARIABLE
# Three different variables of democracy (each with different inclusive dates) to use separately in the analyses as the independent variable:
#   1.	V-Dem democracy variable (1951-2024)
# •	Variable name in the dataset: v2x_LIBDEM. It ranges from 0 (worst) to 1 (best).
# 2.	Polity IV democracy variable (1948-2018)
# •	Variable name in the dataset: DEMOC. It ranges from -10 (worst) to +10 (best).
# 3.	Freedom House democracy variable (1972 - 2024)
# •	Variable name in the dataset: TOTLIB. It ranges from 1 (best) to 14 (worst). It is a combination of the following two variables, which people sometimes run separately:
#   o	Civil liberties - Variable name in the dataset: CIVLIB. It ranges from 1 (best) to 7 (worst).
# o	Political rights: Variable name in the dataset: POLLIB. It ranges from 1 (best) to 7 (worst).
# 
# For each of the three democracy variables, will do a series of 4 tests with lags.
# Test 1. Change in democracy score (no lag)
# Test 2. Change in democracy score (1 year lag)
# Test 3. Change in democracy score (2 year lag)
# Test 4. Change in democracy score (3 year lag)
# 
# CONTROL VARIABLES RELATED FOR AFRICAN COUNTRIES (AFRICAN FACTORS)
# •	African GNI OR GNI per capita (logged?)
# •	African Population (size in millions)
# •	Region of Africa
# o	1= North Africa
# o	2 = East Africa
# o	3 = Southern Africa
# o	4= Central Africa
# o	5 = West Africa
# •	African Colonial Past (Colpast2)
# o	1 = French
# o	2 = British
# o	3= Portuguese
# o	4= Spanish
# o	5 = Italian
# o	6 = Belgian
# o	7 = German
# o	8 = United States
# o	9 = Not colony
# o	10 = unique
# •	African Ideology
# o	1 = capitalist
# o	2 = socialist
# o	3 = Marxist
# •	African Historical Period
# o	Entire independence era
# o	Cold War era (1945-1989)
# o	Post-Cold War era (1990-present)
# 
# 
# (PULL FACTORS—CHARACTERISTICS OF OTHER NON-AFRICAN COUNTRIES OF THE WORLD—JUST FOR THE EMBASSSY QUESTION)
# Level of democracy (Freedom House)
# Level of democracy (Polity IV)
# Level of democracy (V-dem) These are in the data set for the African countries, but need to be added for the rest of the world.
# GNI 
# GNI per capita
# Population
# Imports
# Exports
# Foreign Aid
# 
# FOR THE THREE SETS OF STATISTICALLY BASED QUESTIONS, LET’S START WITH THE EMBASSIES (ABOVE QUESTION #1 FIRST)
#                                                                                      See attached results that I received in 2018 when initial work was done on an earlier data set.
#                                                                                      
                                                                                     
                                                                                    
## Load in packages and data sets ##
library(tidyverse)
library(readxl)

masterset = read_csv("./data/Master 08-28-2025 AFP excel dataset-3.xlsx - AFRSdata.csv")

restofworld = read_excel("./old_data/Rest of the World.xlsx")


#Data cleaning
masterset = masterset %>%
  mutate(across(where(is.numeric), ~ na_if(., -99)),
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


#Count the embassies
names(masterset)
masterset <- masterset %>% mutate(n_embassy = select(., C099:C572) %>% rowSums(na.rm = T))


#Preparing data for modeling
modelset = list(
  diplomaticrep,
  masterset
) %>% reduce(merge, by = c("COUNTRY", "CCODE", "YEAR", "REGION"), all = T) %>%
  filter(!is.na(COUNT), COUNTRY != "South Sudan") %>%
  mutate(YEAR = as.numeric(YEAR),
         YEAR1965 = YEAR - 1965)

modelset %>% select(COUNTRY, YEAR, COUNT, v2x_LIBDEM, DEMOC, TOTLIB1,CIVLIB, POLLIB, 
                    GNI_CAP, REGION, COLPAST2, IDEOLOGY, ) %>% View()



