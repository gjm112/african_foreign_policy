## Load in packages and data sets ##
library(tidyverse)
library(readxl)

#1. Total number and diversification of African embassies maintained abroad.
#2. Total amount and diversification of African trade imports and trade exports
#3. Total amount and diversification of foreign aid received by African countries

#setwd("schrader")
setwd("/Users/gregorymatthews/Dropbox/CDSCpappasGit/schrader/")

masterset = read_csv("data/Master 08-28-2025 AFP excel dataset-3.csv")
masterset %>% select(COUNTRY, YEAR, v2x_LIBDEM, DEMOC, TOTLIB1,CIVLIB, POLLIB, 
                     GNI_CAP, REGION, COLPAST2, IDEOLOGY ) %>% View()
#Which variable is historical period? 

diplomaticrep = read_csv("data/Master 08-28-2025 AFP excel dataset-3_diplomatic_represetations.csv")

restofworld = read_excel("data/Rest of the World.xlsx")

Model: 
EmbassyCount ~ (democracyvariables with lags) + covariates ()

## Transforming data sets ##

masterset_adj = masterset %>%
  mutate(across(where(is.numeric), ~ na_if(., -99)),
         COUNTRY = case_when(COUNTRY == "seychelles" ~ "Seychelles",
                             COUNTRY == "sao tome & Principe" ~ "Sao Tome & Principe",
                             .default = COUNTRY))

sort(unique(masterset_adj$COUNTRY))

diplomaticrep_adj = list(
  diplomaticrep %>%
    select(-contains("...")) %>%
    pivot_longer(cols = -c(CCODE, COUNTRY),
                 names_to = "YEAR",
                 values_to = "COUNT"),
  masterset_adj %>%
    select(COUNTRY, CCODE, REGION) %>%
    filter(!is.na(REGION)) %>%
    distinct()
) %>% 
  reduce(merge, by = c("COUNTRY", "CCODE"))

countrygrowth = masterset_adj %>%
  select(COUNTRY, YEAR, IDEOLOGY, COLPAST2, AVLIFEEX, GNI_CAP, ODAGGTOT) %>%
  mutate(IDEOLOGY = as.character(IDEOLOGY), ODAGGTOT = as.numeric(ODAGGTOT),
         COL2 = ifelse(COLPAST2 != 9, "Colony", "Not Colony"))

#Missingness
countrygrowth %>%
  reframe(across(everything(), ~ sum(is.na(.))))

## Exploratory plots ##
countrygrowth %>%
  filter(!is.na(COL2)) %>%
  ggplot(aes(x = AVLIFEEX, y = IDEOLOGY)) +
  geom_boxplot(outliers = T) +
  stat_boxplot(geom = "errorbar", width = 0.25)

countrygrowth %>% # Bar chart for ideology by year #
  filter(!is.na(IDEOLOGY)) %>%
  ggplot(aes(x = YEAR, fill = IDEOLOGY)) +
  geom_bar(position = "stack", color = "black") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1950, 2020, by = 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 55), breaks = seq(0, 50, by = 10)) +
  scale_fill_manual(values = c("red", "blue", "green"), labels = c("Capitalist", "Socialist", "Marxist")) +
  labs(title = "Progression of Ideology by Year in All African Countries",
       x = "Year",
       y = "Number of Measured African Countries",
       fill = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top", legend.text = element_text(face = "bold"))

embassyplot = function(name) { # Progression of embassies by each country #
  
  library(tidyverse)
  
  plottitle = paste0("Number of Embassies in ", name, " by Year")

  diplomaticrep_adj %>%
    filter(COUNTRY == name) %>%
    ggplot(aes(x = YEAR, y = COUNT, group = COUNTRY)) +
    geom_point(size = 3, shape = 21, color = "black", fill = "white") +
    geom_line(color = "red") +
    labs(title = plottitle,
         x = "Year",
         y = "Number of Measured Embassies") +
    theme_bw()
  
}

embassyplot("Angola")
embassyplot("Algeria")
embassyplot("Botswana")
embassyplot("Zimbabwe")

countrygrowth %>% # Scatter plot for progression of ideology in each country #
  filter(!is.na(IDEOLOGY)) %>%
  ggplot(x = YEAR, y = COUNTRY, fill = IDEOLOGY) +
  geom_point(aes(x = YEAR, y = COUNTRY, fill = IDEOLOGY), size = 3, shape = 21, color = "black") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1950, 2020, by = 10), limits = c(1950, 2026)) +
  scale_fill_manual(values = c("red", "blue", "green"), labels = c("Capitalist", "Socialist", "Marxist")) +
  labs(title = "Progression of Ideology by Year in Different African Countries",
       x = "Year",
       y = "African Country",
       fill = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top", legend.text = element_text(face = "bold"))

## Modeling ##

library(zoo)
library(lme4)

modelset = list(
  diplomaticrep_adj,
  masterset_adj
) %>% reduce(merge, by = c("COUNTRY", "CCODE", "YEAR", "REGION"), all = T) %>%
  filter(!is.na(COUNT), COUNTRY != "South Sudan") %>%
  mutate(YEAR = as.numeric(YEAR),
         YEAR1965 = YEAR - 1965)

modelset %>% select(COUNTRY, YEAR, COUNT, v2x_LIBDEM, DEMOC, TOTLIB1,CIVLIB, POLLIB, 
                    GNI_CAP, REGION, COLPAST2, IDEOLOGY) %>% View()

mod1 = glmer(COUNT ~ as.factor(IDEOLOGY) + (1|COUNTRY) + GNI + POPULATN + YEAR1965, 
             modelset, family = "poisson",
             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000)))

mod1 = glmer(COUNT ~ as.factor(IDEOLOGY) + (1|COUNTRY) + GNI + POPULATN + YEAR1965, 
             modelset, family = "poisson")
summary(mod1)

plot(mod1)

# group_by(COUNTRY, CCODE) %>%
# mutate(COUNT2 = na.approx(COUNT, x = YEAR)) %>%
# ungroup()

modelset %>%
  filter(COUNTRY == "Algeria", CCODE == "148") %>%
  reframe(sum(is.na(COUNT)))
