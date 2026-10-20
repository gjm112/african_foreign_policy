
library(tidyverse)
library(sf)
library(rnaturalearth)

africa <- ne_countries(
  continent = "Africa",
  scale = "medium",
  returnclass = "sf"
)

region_data <- masterset %>%
  filter(YEAR == 2024) %>%
  mutate(COUNTRY = case_when(COUNTRY == "Tanzania" ~ "United Republic of Tanzania",
                             COUNTRY == "CongoDR Kinshasa" ~ "Democratic Republic of the Congo",
                             COUNTRY == "CongoR Brazzaville" ~ "Republic of the Congo",
                             COUNTRY == "Cote d'Ivoire" ~ "Ivory Coast",
                             COUNTRY == "Eswatini" ~ "eSwatini",
                             COUNTRY == "Cape Verde" ~ "Cabo Verde",
                             COUNTRY == "Sao Tome & Principe" ~ "São Tomé and Principe",
                             .default = COUNTRY),
         REGION = case_when(REGION == 1 ~ "North Africa",
                            REGION == 2 ~ "East Africa",
                            REGION == 3 ~ "Southern Africa",
                            REGION == 4 ~ "Central Africa",
                            REGION == 5 ~ "West Africa",
                            .default = REGION)) %>%
  select(COUNTRY, REGION) %>%
  add_row(
    COUNTRY = c("Somaliland", "Western Sahara"),
    REGION = c("East Africa", "North Africa")
  )

africa_map <- africa %>%
  left_join(
    region_data,
    by = c("admin" = "COUNTRY")
  )

ggplot(africa_map) +
  geom_sf(
    aes(fill = REGION),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_brewer(
    palette = "Set2",
    na.value = "grey90"
  ) +
  labs(
    title = "African Countries by Region",
    fill = "Region"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    legend.position = "bottom"
  )
