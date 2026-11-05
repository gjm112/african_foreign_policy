# Summary

# BROAD QUESTION

Broad question: Does the process of democratization within African countries affect the formulation and implementation of African foreign policies.

## THERE ARE THREE DIFFERENT SETS OF STATISTICALLY BASED QUESTIONS (We address only question 1 here):

1.  African democracy and African embassies. Do greater levels of democratization of African countries lead to larger and more diversified embassy networks maintained by African countries overseas? Dependent variable: embassies maintained abroad (yes/no by country) in 1965, 1975, 1985, 1995, 2000, 2005, 2010, 2015, 2020, 2025 (use 2024 for 2025).
2.  African democracy and African trade. Do greater levels of democratization within African countries lead to larger and more diversified levels of trade imports and trade exports by African countries? Dependent variable: \$ amount of imports and exports annually for 1960 to 2022)
3.  African democracy and aid to Africa. Do greater levels of democratization within African countries lead to larger and more diversified levels of foreign aid to African countries? (Dependent variable:\$ amount of aid annually for every country in the world 1960 to 2021).

THUS, THREE DIFFERENT DEPENDENT VARIABLES:

1.  Total number and diversification of African embassies maintained abroad.
2.  Total amount and diversification of African trade imports and trade exports
3.  Total amount and diversification of foreign aid received by African countries

## THREE DIFFERENT VARIABLES OF AFRICAN POLITY2RACY AS INDEPENDENT VARIABLE

Three different variables of democracy (each with different inclusive dates) to use separately in the analyses as the independent variable:

1.  V-Dem democracy variable (1951-2024) Variable name in the dataset: v2x_LIBDEM. It ranges from 0 (worst) to 1 (best).
2.  Polity IV democracy variable (1948-2018) Variable name in the dataset: POLITY2. It ranges from -10 (worst) to +10 (best).
3.  Freedom House democracy variable (1972 - 2024) Variable name in the dataset: TOTLIB. It ranges from 1 (best) to 14 (worst). It is a combination of the following two variables, which people sometimes run separately: Civil liberties - Variable name in the dataset: -CIVLIB. It ranges from 1 (best) to 7 (worst). -Political rights: Variable name in the dataset: POLLIB. It ranges from 1 (best) to 7 (worst).

For each of the three democracy variables, will do a series of 4 tests with lags. -Test 1. Change in democracy score (no lag) -Test 2. Change in democracy score (1 year lag) -Test 3. Change in democracy score (2 year lag) -Test 4. Change in democracy score (3 year lag)

## CONTROL VARIABLES RELATED FOR AFRICAN COUNTRIES (AFRICAN FACTORS)

-African GNI OR GNI per capita (logged?) -African Population (size in millions) - Region of Africa (1= North Africa, 2 = East Africa, 3 = Southern Africa, 4= Central Africa, 5 = West Africa) - African Colonial Past (Colpast2) (1 = French, 2 = British, 3 = Portuguese, 4 = Spanish, 5 = Italian, 6 = Belgian, 7 = German, 8 = United States, 9 = Not colony, 10 = unique) - African Ideology (1 = capitalist, 2 = socialist, 3 = Marxist) - African Historical Period - Entire independence era - Cold War era (1945-1989) - Post-Cold War era (1990-present)

## PULL FACTORS—CHARACTERISTICS OF OTHER NON-AFRICAN COUNTRIES OF THE WORLD—JUST FOR THE EMBASSSY QUESTION

- Level of democracy (Freedom House)
- Level of democracy (Polity IV)
- Level of democracy (V-dem) These are in the data set for the African countries, but need to be added for the rest of the world.
- GNI
- GNI per capita
- Population
- Imports
- Exports
- Foreign Aid

FOR THE THREE SETS OF STATISTICALLY BASED QUESTIONS, LET’S START WITH THE EMBASSIES (ABOVE QUESTION #1 FIRST)

## ANSWERING QUESTION #1

To evaluate this question we first created three sets of reduced and full models. Our reduced models sought to evaluate the relationship and the significance of the relationship between a country's embassy count and its democracy score from Freedom House, Polity IV, and V-dem. In these models, democracy score was the only explanatory variable after controlling for the country and year with random effects. The model equation is given below, where $\gamma_{0k}$ represents the random effect for country $k$ and $\gamma_{0t}$ is the random effect for year $t$. We follow the same setup for the lagged models, which go back as far as three years for each democracy score.
