# loading packages
library(tidyverse)
library(skimr)
library(eurostat)
library(WDI)
library(WHO)
library(Rilostat)
library(knitr)
library(reactable)
library(reactablefmtr)
library(htmltools)
library(ggtext)
library(sf)
library(giscoR)
library(countrycode)
library(rvest)


# loading data
data <- read_csv("2_Tidy data/data.csv")


# data summary
skim(data)


# adding geography data for map plotting
# loading Europe map data
europe_map <- gisco_get_countries(region = "Europe", resolution = 20) %>%
  bind_rows(gisco_get_countries(country = c("CYP", "TUR"), resolution = 20))

# merging data
data_map <- left_join(x = data,
                      y = europe_map %>%
                        select(iso3c = ISO3_CODE, geometry),
                      by = "iso3c")


# plotting regions
ggplot(data_map) +
  geom_sf(data = europe_map,
          aes(geometry = geometry)) +
  geom_sf(aes(geometry = geometry, fill = region)) +
  scale_x_continuous(limits = c(-31, 42)) +
  scale_y_continuous(limits = c(28, 71)) +
  scale_fill_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  labs(title = "European regions.",
       subtitle = "Regions: <span style=color:#a50026>**Baltic states**</span>, 
       <span style=color:#f46d43>**Eastern**</span>, <span style=color:#fdae61>**Northern**</span>, 
       <span style=color:#4393c3>**Southern**</span>, and
       <span style=color:#053061>**Western**</span> Europe.",
       caption = "Modified from the UN geoscheme.") +
  theme_minimal(base_size = 11,
                base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "none") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank())


# plotting map - Gini index for the year 2023
ggplot(data_map %>%
         filter(year == 2023,
                !is.na(gini_index))) +
  geom_sf(data = europe_map,
          aes(geometry = geometry)) +
  geom_sf(aes(geometry = geometry, fill = gini_index)) +
  scale_x_continuous(limits = c(-31, 42)) +
  scale_y_continuous(limits = c(28, 71)) +
  scale_fill_gradient2(low = "#004529", mid = "yellow", high = "darkred",
                       midpoint = mean(data_map$gini_index, na.rm = TRUE)) +
  labs(title = "Gini index - Year 2023",
       x = element_blank(),
       y = element_blank(),
       caption = "Data source: Eurostat",
       fill = "Gini index") +
  theme_minimal(base_size = 11,
                base_family = "sans") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold"))


# correlation analysis
ggplot(data %>%
         select(where(is.numeric), -year) %>%
         filter(!is.na(gini_index)) %>%
         summarise(across(everything(), ~cor(gini_index, .x, use = "complete.obs"))) %>%
         pivot_longer(cols = everything(), names_to = "variable", values_to = "corr_coeff") %>%
         arrange(desc(corr_coeff)) %>%
         slice(-1),
       aes(x = corr_coeff, y = fct_reorder(variable, corr_coeff, .desc = FALSE), fill = corr_coeff)) +
  geom_col(colour = "black") +
  scale_fill_gradient2(low = "darkblue", mid = "#ffffbf", high = "darkred") +
  scale_y_discrete(expand = expansion(mult = c(0.025, 0.025)),
                   labels = c("s80s20_ratio" = "Income S80/S20 ratio",
                              "at_risk_poverty" = "Pop. at risk of poverty",
                              "in_work_risk_poverty" = "In-work pop. at risk of poverty",
                              "mat_soc_depr_perc" = "Pop. exp. material and social deprivation",
                              "avg_ann_unempl_rate" = "Unemployment rate",
                              "lt_unempl_rate" = "Long-term unemployment",
                              "avg_youth_unempl_rate_ann" = "Youth unemployment rate",
                              "vlt_unempl_rate" = "Very long-term unemployment",
                              "income_real_terms" = "Income in real terms",
                              "housing_development_exp" = "Gov. expend. in housing development",
                              "ann_avg_wage" = "Average wage",
                              "poors_cov" = "Poors covered by protection systems",
                              "eys" = "Expected years of schooling",
                              "social_protection_exp" = "Gov. expend. in social protection",
                              "pop_cov" = "Pop. covered by protection systems",
                              "literacy_rate" = "Literacy rate",
                              "gdp_capita_pps" = "GDP per capita (PPS)",
                              "gdp_per_capita" = "GDP per capita",
                              "education_exp" = "Gov. expend. in education",
                              "mean_income_pps" = "Mean income (PPS)",
                              "mys" = "Mean years of schooling",
                              "ed_index" = "Education index",
                              "health_exp" = "Gov. expend. in healthcare",
                              "uhc_cov_index" = "UHC coverage index",
                              "median_income_pps" = "Median income (PPS)",
                              "ihdi" = "Inequality-adjusted Human Development Index")) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title = "Correlation with the Gini index",
       subtitle = "Pearson correlation coefficient",
       x = "Pearson correlation coefficient",
       y = element_blank(),
       fill = element_blank(),
       caption = "Data source: Eurostat, World Bank, WHO, ILO, UNHDR.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "bold"),
        legend.position = "none")


# pairwise correlation plots
data %>%
  select(where(is.numeric), -year) %>%
  pivot_longer(cols = -gini_index, names_to = "variable", values_to = "value") %>%
  filter(!is.na(value), !is.na(gini_index)) %>%
  ggplot(aes(x = gini_index, y = value)) +
  geom_point(alpha = 0.5) +
  facet_wrap(vars(variable),
             ncol = 5,
             scales = "free_y", 
             labeller = labeller(variable = c("s80s20_ratio" = "Inc. S80/S20",
                                              "at_risk_poverty" = "At risk poverty",
                                              "in_work_risk_poverty" = "In-work risk pov.",
                                              "mat_soc_depr_perc" = "Mat. & soc. depr.",
                                              "avg_ann_unempl_rate" = "Unempl. rate",
                                              "lt_unempl_rate" = "LT unempl.",
                                              "avg_youth_unempl_rate_ann" = "Youth unempl.",
                                              "vlt_unempl_rate" = "Very LT unempl.",
                                              "income_real_terms" = "Inc. real terms",
                                              "housing_development_exp" = "Gov. housing",
                                              "ann_avg_wage" = "Average wage",
                                              "poors_cov" = "Poors cov. prot.",
                                              "eys" = "Exp. y. school.",
                                              "social_protection_exp" = "Gov. soc. prot.",
                                              "pop_cov" = "Pop. cov. prot.",
                                              "literacy_rate" = "Literacy rate",
                                              "gdp_capita_pps" = "GDP capita (PPS)",
                                              "gdp_per_capita" = "GDP capita",
                                              "education_exp" = "Gov. education",
                                              "mean_income_pps" = "Mean inc. (PPS)",
                                              "mys" = "Mean y. school.",
                                              "ed_index" = "Educ. index",
                                              "health_exp" = "Gov. healthcare",
                                              "uhc_cov_index" = "UHC cov. index",
                                              "median_income_pps" = "Med. inc. (PPS)",
                                              "hdi" = "HDI"))) +
  labs(title = "Pairwise correlation plots",
       subtitle = "Pearson correlation coefficients",
       x = "Gini index",
       y = "Value",
       caption = "Data: Eurostat, World Bank, WHO, ILO, UNHDR.") +
  theme_classic(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "bold"))


# Gini index over time
# evolution of the Gini index over time by region
ggplot(data %>%
         filter(!is.na(gini_index)) %>%
         group_by(region, year) %>%
         summarise(gini_index = mean(gini_index, na.rm = TRUE)),
       aes(x = year, y = gini_index, colour = region, shape = region)) +
  geom_point(size = 3, alpha = 0.3) +
  geom_smooth(se = FALSE, linewidth = 1) +
  scale_x_continuous(breaks = seq(1970, 2024, 5)) +
  scale_colour_manual(values = c("#b2182b", "#f4a582", "#4393c3", "#2166ac", "#053061")) +
  scale_shape_manual(values = c(15:18, 8)) +
  labs(title = "Evolution of the Gini index over time",
       x = "Year",
       y = "Gini index",
       caption = "Data source: Eurostat, World Bank.",
       colour = "Region",
       shape = "Region") +
  theme_classic(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))


# Gini index change by country
ggplot(data %>%
         filter(!is.na(gini_index)),
       aes(x = year, y = gini_index, colour = region)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = c(20, 30, 40)) +
  scale_colour_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  labs(title = "Evolution of the Gini index by country over time",
       subtitle = "Regions: <span style=color:#a50026>**Baltic states**</span>, 
       <span style=color:#f46d43>**Eastern**</span>, <span style=color:#fdae61>**Northern**</span>, 
       <span style=color:#4393c3>**Southern**</span>, and
       <span style=color:#053061>**Western**</span> Europe.",
       x = "Year",
       y = "Gini index",
       caption = "Data source: Eurostat, World Bank") +
  theme_classic(base_size = 11,
                base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "none") +
  facet_wrap(vars(country))


# Gini index evolution in Denmark, Finland and Sweden
ggplot(data %>%
         filter(country %in% c("Denmark", "Finland", "Sweden"),
                !is.na(gini_index)),
       aes(x = year, y = gini_index, colour = gini_index)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_colour_gradient2(low = "#004529", mid = "yellow", high = "darkred",
                         midpoint = mean(data_map$gini_index, na.rm = TRUE)) +
  scale_y_continuous(limits = c(22, 31)) +
  labs(title = "Evolution of the income inequality in Denmark, Finland and Sweden.",
       x = "Year",
       y = "Gini index",
       colour = "Gini index",
       caption = "Data source: Eurostat, World Bank.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold")) +
  facet_wrap(vars(country), ncol = 1)


# Gini index evolution in Malta
ggplot(data %>%
         filter(country == "Malta",
                !is.na(gini_index)),
       aes(x = year, y = gini_index, colour = gini_index)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_colour_gradient2(low = "#004529", mid = "yellow", high = "darkred",
                         midpoint = mean(data_map$gini_index, na.rm = TRUE)) +
  labs(title = "Evolution of the income inequality in Malta.",
       x = "Year",
       y = "Gini index",
       colour = "Gini index",
       caption = "Data source: Eurostat, World Bank.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))


# Gini index evolution in North Macedonia, Poland, and Serbia
ggplot(data %>%
         filter(country %in% c("North Macedonia", "Poland", "Serbia"),
                !is.na(gini_index)),
       aes(x = year, y = gini_index, colour = gini_index)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_colour_gradient2(low = "#004529", mid = "yellow", high = "darkred",
                         midpoint = mean(data_map$gini_index, na.rm = TRUE)) +
  scale_y_continuous(limits = c(26, 46)) +
  labs(title = "Evolution of the income inequality in North Macedonia, Poland and Serbia",
       x = "Year",
       y = "Gini index",
       colour = "Gini index",
       caption = "Data source: Eurostat, World Bank.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold")) +
  facet_wrap(vars(country), ncol = 1)


# Income quintile share ratio S80/S20 vs. Gini index
data %>%
  filter(!is.na(s80s20_ratio)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            s80s20_ratio = first(s80s20_ratio),
            gini_index = first(gini_index),
            region = first(region)) %>%
  arrange(s80s20_ratio) %>%
  ggplot(aes(x = s80s20_ratio, y = gini_index, colour = region, shape = region)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_shape_manual(values = c(15:18, 8)) +
  labs(title = "Relationship between the Gini index and the S80/S20 ratio.",
       subtitle = "Regions: <span style=color:#a50026>**Baltic states**</span>, 
       <span style=color:#f46d43>**Eastern**</span>, <span style=color:#fdae61>**Northern**</span>, 
       <span style=color:#4393c3>**Southern**</span>, and
       <span style=color:#053061>**Western**</span> Europe.",
       x = "Income quintile share ratio S80/S20.",
       y = "Gini index",
       caption = "Data for the year 2023 (except for ISL: 2019, GBR: 2018, MKD: 2020, CHE: 2022, TUR: 2022).\nData source: Eurostat.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "none")


# Income quintile share ratio S80/S20 by country
data %>%
  filter(!is.na(s80s20_ratio)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            s80s20_ratio = first(s80s20_ratio),
            gini_index = first(gini_index),
            region = first(region)) %>%
  arrange(s80s20_ratio) %>%
  ggplot(aes(x = s80s20_ratio, y = fct_reorder(iso3c, s80s20_ratio, .desc = FALSE),
             fill = region)) +
  geom_col() +
  scale_fill_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.025))) +
  scale_y_discrete(expand = expansion(mult = c(0.025, 0.025))) +
  labs(title = "Income quintile share ratio S80/S20",
       subtitle = "Data for the year 2023 (except for ISL: 2019, GBR: 2018, MKD: 2020, CHE: 2022, TUR: 2022).",
       x = "Income quintile share ratio S80/S20",
       y = element_blank(),
       caption = "Data source: Eurostat.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(size = 10),
        axis.text.y = element_text(size = 8),
        legend.position = "none")


# income quintile share ratio S80/S20 over time for selected countries
ggplot(data %>%
         filter(country %in% c("Denmark", "Sweden", "Malta", "North Macedonia", "Poland", "Serbia"),
                !is.na(s80s20_ratio)) %>%
         mutate(country = factor(country,
                                 levels = c("Denmark", "Sweden", "Malta",
                                            "North Macedonia", "Poland", "Serbia"))),
       aes(x = year, y = s80s20_ratio, colour = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  labs(title = "Income quintile share ratio S80/S20 over time",
       x = "Year",
       y = "Income quintile share ratio S80/S20",
       caption = "Data source: Eurostat.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none") +
  facet_wrap(vars(country))


# People at risk of poverty
data %>%
  filter(!is.na(at_risk_poverty)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            at_risk_poverty = first(at_risk_poverty),
            gini_index = first(gini_index),
            region = first(region)) %>%
  ggplot(aes(x = at_risk_poverty, y = fct_reorder(iso3c, at_risk_poverty, .desc = FALSE),
             fill = region)) +
  geom_col() +
  scale_fill_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.025))) +
  scale_y_discrete(expand = expansion(mult = c(0.025, 0.025))) +
  labs(title = "% population at risk of poverty",
       subtitle = "Data for the year 2023 (except for ISL: 2019, GBR: 2018, MKD: 2020, CHE: 2022, TUR: 2022).",
       x = "% population",
       y = element_blank(),
       caption = "Data source: Eurostat.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(size = 10),
        axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "black"),
        legend.position = c(0.9, 0.2))


# Gini index and % population at risk of poverty
data %>%
  filter(!is.na(at_risk_poverty)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            at_risk_poverty = first(at_risk_poverty),
            gini_index = first(gini_index),
            region = first(region)) %>%
  ggplot(aes(x = at_risk_poverty, y = gini_index, colour = region, shape = region)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_shape_manual(values = c(15:18, 8)) +
  labs(title = "Relationship between the Gini index and the % pop. at risk of poverty.",
       subtitle = "Regions: <span style=color:#a50026>**Baltic states**</span>, 
       <span style=color:#f46d43>**Eastern**</span>, <span style=color:#fdae61>**Northern**</span>, 
       <span style=color:#4393c3>**Southern**</span>, and
       <span style=color:#053061>**Western**</span> Europe.",
       x = "% pop. at risk of poverty",
       y = "Gini index",
       caption = "Data for the year 2023 (except for ISL: 2019, GBR: 2018, MKD: 2020, CHE: 2022, TUR: 2022).\nData source: Eurostat.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "none")


# governement expenditure in healthcare (% GDP)
data %>%
  filter(!is.na(health_exp)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            health_exp = first(health_exp),
            gini_index = first(gini_index),
            region = first(region)) %>%
  ggplot(aes(x = health_exp, y = fct_reorder(iso3c, health_exp, .desc = FALSE),
             fill = region)) +
  geom_col() +
  scale_fill_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.025))) +
  scale_y_discrete(expand = expansion(mult = c(0.025, 0.025))) +
  labs(title = "Government expenditure in healthcare (% GDP)",
       subtitle = "Data for the year 2022.",
       x = "% GDP",
       y = element_blank(),
       caption = "Data source: Eurostat.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(size = 10),
        axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "black"),
        legend.position = c(0.9, 0.2))


# Government expenditure in healthcare and Gini index
data %>%
  filter(!is.na(health_exp), !is.na(gini_index)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            health_exp = first(health_exp),
            gini_index = first(gini_index),
            region = first(region)) %>%
  ggplot(aes(x = health_exp, y = gini_index, colour = region, shape = region)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_shape_manual(values = c(15:18, 8)) +
  labs(title = "Relationship between the Gini index and the government expenditure in healthcare.",
       subtitle = "Regions: <span style=color:#a50026>**Baltic states**</span>, 
       <span style=color:#f46d43>**Eastern**</span>, <span style=color:#fdae61>**Northern**</span>, 
       <span style=color:#4393c3>**Southern**</span>, and
       <span style=color:#053061>**Western**</span> Europe.",
       x = "% GDP",
       y = "Gini index",
       caption = "Data for the year 2022 (except for ISL: 2020).\nData source: Eurostat.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "none")


# UHC coverage index and Gini index
data %>%
  filter(!is.na(uhc_cov_index), !is.na(gini_index)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            uhc_cov_index = first(uhc_cov_index),
            gini_index = first(gini_index),
            region = first(region)) %>%
  ggplot(aes(x = uhc_cov_index, y = gini_index, colour = region, shape = region)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_shape_manual(values = c(15:18, 8)) +
  labs(title = "Relationship between the Gini index and the UHC coverage index.",
       subtitle = "Regions: <span style=color:#a50026>**Baltic states**</span>, 
       <span style=color:#f46d43>**Eastern**</span>, <span style=color:#fdae61>**Northern**</span>, 
       <span style=color:#4393c3>**Southern**</span>, and
       <span style=color:#053061>**Western**</span> Europe.",
       x = "UHC coverage index",
       y = "Gini index",
       caption = "Data for the year 2021 (except for ISL: 2019, GBR: 2017, MKD: 2019).\nData source: Eurostat, WHO.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "none")


# Education index and Gini index
data %>%
  filter(!is.na(ed_index), !is.na(gini_index)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            ed_index = first(ed_index),
            gini_index = first(gini_index),
            region = first(region)) %>%
  ggplot(aes(x = ed_index, y = gini_index, colour = region, shape = region)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_shape_manual(values = c(15:18, 8)) +
  labs(title = "Relationship between the Gini index and the education index.",
       subtitle = "Regions: <span style=color:#a50026>**Baltic states**</span>, 
       <span style=color:#f46d43>**Eastern**</span>, <span style=color:#fdae61>**Northern**</span>, 
       <span style=color:#4393c3>**Southern**</span>, and
       <span style=color:#053061>**Western**</span> Europe.",
       x = "Education index",
       y = "Gini index",
       caption = "Data for the year 2022 (except for ISL: 2020, GBR: 2018, MKD: 2020).\nData source: Eurostat, UNHDR.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "none")


# Mean years of schooling and Gini index
data %>%
  filter(!is.na(mys), !is.na(gini_index)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            mys = first(mys),
            gini_index = first(gini_index),
            region = first(region)) %>%
  ggplot(aes(x = mys, y = gini_index, colour = region, shape = region)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_shape_manual(values = c(15:18, 8)) +
  labs(title = "Relationship between the Gini index and mean years of schooling.",
       subtitle = "Regions: <span style=color:#a50026>**Baltic states**</span>, 
       <span style=color:#f46d43>**Eastern**</span>, <span style=color:#fdae61>**Northern**</span>, 
       <span style=color:#4393c3>**Southern**</span>, and
       <span style=color:#053061>**Western**</span> Europe.",
       x = "Mean years of schooling",
       y = "Gini index",
       caption = "Data for the year 2022 (except for ISL: 2020, GBR: 2018, MKD: 2020).\nData source: Eurostat, UNHDR.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "none")


# Government expenditure in education
data %>%
  filter(!is.na(education_exp)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            education_exp = first(education_exp),
            gini_index = first(gini_index),
            region = first(region)) %>%
  ggplot(aes(x = education_exp, y = fct_reorder(iso3c, education_exp, .desc = FALSE),
             fill = region)) +
  geom_col() +
  scale_fill_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.025))) +
  scale_y_discrete(expand = expansion(mult = c(0.025, 0.025))) +
  labs(title = "Government expenditure in education (% GDP)",
       subtitle = "Data for the year 2022.",
       x = "% GDP",
       y = element_blank(),
       caption = "Data source: Eurostat.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(size = 10),
        axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "black"),
        legend.position = c(0.9, 0.2))


# Government expenditure in education vs. Gini index
data %>%
  filter(!is.na(education_exp), !is.na(gini_index)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            education_exp = first(education_exp),
            gini_index = first(gini_index),
            region = first(region)) %>%
  ggplot(aes(x = education_exp, y = gini_index, colour = region, shape = region)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_shape_manual(values = c(15:18, 8)) +
  labs(title = "Gini index vs. government expenditure in education.",
       subtitle = "Regions: <span style=color:#a50026>**Baltic states**</span>, 
       <span style=color:#f46d43>**Eastern**</span>, <span style=color:#fdae61>**Northern**</span>, 
       <span style=color:#4393c3>**Southern**</span>, and
       <span style=color:#053061>**Western**</span> Europe.",
       x = "% GDP",
       y = "Gini index",
       caption = "Data for the year 2022 (except for ISL: 2020).\nData source: Eurostat, UNHDR.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "none")


# Median income and income inequality
data %>%
  filter(!is.na(mean_income_pps), !is.na(median_income_pps), !is.na(gini_index)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            mean_income_pps = first(mean_income_pps),
            median_income_pps = first(median_income_pps),
            gini_index = first(gini_index),
            region = first(region)) %>%
  ggplot(aes(x = median_income_pps, y = gini_index, colour = region, shape = region, group = region)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_shape_manual(values = c(15:18, 8)) +
  labs(title = "Relationship between the Gini index and median income.",
       subtitle = "Regions: <span style=color:#a50026>**Baltic states**</span>, 
       <span style=color:#f46d43>**Eastern**</span>, <span style=color:#fdae61>**Northern**</span>, 
       <span style=color:#4393c3>**Southern**</span>, and
       <span style=color:#053061>**Western**</span> Europe.",
       x = "Median income (PPS)",
       y = "Gini index",
       caption = "Data for the year 2023 (except for ISL: 2019, GBR: 2018, MKD: 2020, CHE: 2022, TUR: 2022).\nData source: Eurostat, UNHDR.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "none")


# Income and income inequality
data %>%
  filter(!is.na(mean_income_pps), !is.na(median_income_pps), !is.na(gini_index)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            mean_income_pps = first(mean_income_pps),
            median_income_pps = first(median_income_pps),
            gini_index = first(gini_index),
            region = first(region)) %>%
  ggplot(aes(y = fct_reorder(iso3c, gini_index, .desc = TRUE))) +
  geom_segment(aes(x = median_income_pps, xend = mean_income_pps, colour = gini_index), linewidth = 2) +
  geom_point(aes(x = median_income_pps), colour = "#313695", size = 4) +
  geom_point(aes(x = mean_income_pps), colour = "#f46d43", size = 4) +
  scale_colour_gradient2(low = "#004529", mid = "yellow", high = "darkred",
                         midpoint = 33.5) +
  labs(title = "<span style=color:#313695>Median</span> and <span style=color:#f46d43>mean</span>
       income",
       subtitle = "Countries ordered from lower (up) to greater (low) income inequality.",
       x = "PPS",
       y = element_blank(),
       colour = "Gini index",
       caption = "Data for the year 2023 (except for ISL: 2019, GBR: 2018, MKD: 2020, CHE: 2022, TUR: 2022).\nData source: Eurostat.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_markdown(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.grid.major.y = element_blank())


# % population covered for at least one social protection benefit
data %>%
  filter(!is.na(pop_cov), !is.na(gini_index)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            pop_cov = first(pop_cov),
            gini_index = first(gini_index),
            region = first(region)) %>%
  ggplot(aes(x = pop_cov, y = fct_reorder(iso3c, pop_cov, .desc = FALSE),
             fill = region)) +
  geom_col() +
  scale_fill_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.025))) +
  scale_y_discrete(expand = expansion(mult = c(0.025, 0.025))) +
  labs(title = "% population covered by at least one social protection benefit",
       subtitle = "Regions: <span style=color:#a50026>**Baltic states**</span>, 
       <span style=color:#f46d43>**Eastern**</span>, <span style=color:#fdae61>**Northern**</span>, 
       <span style=color:#4393c3>**Southern**</span>, and
       <span style=color:#053061>**Western**</span> Europe.",
       x = "% population",
       y = element_blank(),
       caption = "Data for the year 2022 (DNK, GRC, NOR, POL: 2023; CHE, FRA, LUX, MLT: 2021; MDK: 2020; ISL: 2019; GBR: 2018).\nData source: ILO.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(size = 10),
        axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_blank(),
        legend.position = "none")


# Income inequality and iHDI
data %>%
  filter(!is.na(ihdi), !is.na(gini_index)) %>%
  group_by(iso3c, country) %>%
  filter(year == max(year)) %>%
  arrange(iso3c, country, desc(year)) %>%
  summarise(iso3c = first(iso3c),
            country = first(country),
            year = first(year),
            ihdi = first(ihdi),
            gini_index = first(gini_index),
            region = first(region)) %>%
  ggplot(aes(x = ihdi, y = gini_index, colour = region, shape = region, group = region)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#a50026", "#f46d43", "#fdae61", "#4393c3", "#053061")) +
  scale_shape_manual(values = c(15:18, 8)) +
  labs(title = "Relationship between the Gini index and the iHDI.",
       subtitle = "Regions: <span style=color:#a50026>**Baltic states**</span>, 
       <span style=color:#f46d43>**Eastern**</span>, <span style=color:#fdae61>**Northern**</span>, 
       <span style=color:#4393c3>**Southern**</span>, and
       <span style=color:#053061>**Western**</span> Europe.",
       x = "Inequality-adjusted HDI",
       y = "Gini index",
       caption = "Data for the year 2022 (except for ISL: 2020, GBR: 2018, MKD: 2020, CHE: 2022, TUR: 2022).\nData source: Eurostat, UNHDR.") +
  theme_bw(base_size = 11,
           base_family = "sans") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "none")
































