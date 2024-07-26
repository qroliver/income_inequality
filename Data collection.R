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


# Eurostat codes list
eurostat_codes <- list(
  gini = "ilc_di12",  # Gini index
  gdp_capita = "nama_10_pc",  # GDP per capita
  unemployment = "une_rt_m",   # unemployment
  long_term_unempl = "une_ltu_a",  # long-term unemployment
  mean_median_income = "ilc_di03",  # mean and median income
  income_s80s20_ratio = "ilc_di11",  # Income quintile share ratio S80/S20 for disposable income
  income_real_terms = "ilc_di18",  # income in real terms
  risk_of_poverty = "ilc_li02",  # at risk of poverty
  in_work_risk_of_poverty = "ilc_iw01",  # in-work at risk of poverty
  material_social_deprivation = "ilc_mdsd07",  # material and social deprivation
  government_expenditure = "gov_10a_exp"  # government expenditure by function
)


# World Bank data
wb_code_list = list(
  literacy_rate = "SE.ADT.LITR.ZS",
  gini_index = "SI.POV.GINI"
)


# WHO data
uhc_coverage_index_code = "UHC_INDEX_REPORTED"


# ILO data code list
ilo_codes = list(
  social_protection = "SDG_0131_SEX_SOC_RT_A",  # percentage of population covered by social protection systems
  annual_avg_wage = "EAR_4MTH_SEX_ECO_CUR_NB_A"   # annual average wage (in 2017 PPS USD)
)


# Europe geographic data
europe_map <- gisco_get_countries(region = "Europe", resolution = 20) %>%
  bind_rows(gisco_get_countries(country = "Cyprus", resolution = 20))


# fetching Eurostat data and preliminary data processing
# defining a function to do this
fetch_eurostat_data <- function(indicator_id){
  get_eurostat(id = indicator_id, stringsAsFactors = TRUE, type = "label") %>%
    filter(!str_detect(geo, "^Euro|New Member States")) %>%
    mutate(geo = if_else(str_detect(geo, "Kosovo"),
                         "Kosovo",
                         geo),
           year = year(TIME_PERIOD),
           iso3c = if_else(str_detect(geo, "Kosovo"),
                           "XKX",
                           countrycode(sourcevar = geo,
                                       origin = "country.name",
                                       destination = "iso3c"))) %>%
    rename(country = geo)
}

# fetching Eurostat data
eurostat_data_list <- map(eurostat_codes, ~fetch_eurostat_data(.x))


# Eurostat data extra processing
data_list <- list()  # creation of an empty list to store processed data

# Gini index
data_list[["gini"]] <- eurostat_data_list[["gini"]] %>%
  filter(age == "Total") %>%
  select(iso3c, country, year, gini_index = values)

# GDP per capita
data_list[["gdp_capita"]] <- eurostat_data_list[["gdp_capita"]] %>%
  filter(unit %in% c("Current prices, euro per capita",
                     "Current prices, purchasing power standard (PPS, EU27 from 2020) per capita"),
         na_item == "Gross domestic product at market prices") %>%
  mutate(unit = if_else(unit == "Current prices, euro per capita",
                        "gdp_per_capita",
                        "gdp_capita_pps")) %>%
  select(iso3c, country, year, variable = unit, values) %>%
  pivot_wider(names_from = variable,
              values_from = values)

# unemployment (annual average)
data_list[["unemployment_ann"]] <- eurostat_data_list[["unemployment"]] %>%
  filter(str_detect(s_adj, "Unadjusted data"),
         age == "Total",
         unit == "Percentage of population in the labour force",
         sex == "Total") %>%
  mutate(month = month(TIME_PERIOD)) %>%
  select(iso3c, country, year, month, ann_unempl_rate = values) %>%
  group_by(iso3c, country, year) %>%
  summarise(avg_ann_unempl_rate = mean(ann_unempl_rate, na.rm = TRUE))


# youth unemployment (annual average)
data_list[["youth_unemployment_ann"]] <- eurostat_data_list[["unemployment"]] %>%
  filter(str_detect(s_adj, "Unadjusted data"),
         age == "Less than 25 years",
         unit == "Percentage of population in the labour force",
         sex == "Total") %>%
  mutate(month = month(TIME_PERIOD)) %>%
  select(iso3c, country, year, month, youth_unempl_rate = values) %>%
  group_by(iso3c, country, year) %>%
  summarise(avg_youth_unempl_rate_ann = mean(youth_unempl_rate, na.rm = TRUE))


# long-term unemployment rate (> 12 months)
data_list[["long_term_unempl"]] <- eurostat_data_list[["long_term_unempl"]] %>%
  filter(indic_em == "Long-term unemployment",
         age == "From 15 to 74 years",
         sex == "Total",
         unit == "Percentage of population in the labour force") %>%
  select(iso3c, country, year, lt_unempl_rate = values)


# very long-term (> 24 months)
data_list[["very_lt_unempl"]] <- eurostat_data_list[["long_term_unempl"]] %>%
  filter(indic_em == "Very long-term unemployment",
         age == "From 15 to 74 years",
         sex == "Total",
         unit == "Percentage of population in the labour force") %>%
  select(iso3c, country, year, vlt_unempl_rate = values)


# mean and median income
# mean income
data_list[["mean_income_pps"]] <- eurostat_data_list[["mean_median_income"]] %>%
  filter(age == "Total",
         sex == "Total",
         indic_il == "Mean equivalised net income",
         unit == "Purchasing power standard (PPS)") %>%
  select(iso3c, country, year, mean_income_pps = values)

# median income
data_list[["median_income_pps"]] <- eurostat_data_list[["mean_median_income"]] %>%
  filter(age == "Total",
         sex == "Total",
         indic_il == "Median equivalised net income",
         unit == "Purchasing power standard (PPS)") %>%
  select(iso3c, country, year, median_income_pps = values)


# Income quintile share ratio S80/S20 for disposable income
data_list[["income_s80s20_ratio"]] <- eurostat_data_list[["income_s80s20_ratio"]] %>%
  filter(age == "Total",
         sex == "Total") %>%
  select(iso3c, country, year, s80s20_ratio = values)


# income in real terms (2010 = 100)
data_list[["income_real_terms"]] <- eurostat_data_list[["income_real_terms"]] %>%
  select(iso3c, country, year, income_real_terms = values)


# people at risk of poverty
data_list[["at_risk_poverty"]] <- eurostat_data_list[["risk_of_poverty"]] %>%
  filter(unit == "Percentage",
         str_detect(indic_il, "60% of median equivalised income"),
         sex == "Total",
         age == "Total") %>%
  select(iso3c, country, year, at_risk_poverty = values)


# in-work at-risk-of-poverty rate
data_list[["in_work_at_risk_poverty"]] <- eurostat_data_list[["in_work_risk_of_poverty"]] %>%
  filter(wstatus == "Employed persons",
         sex == "Total",
         age == "18 years or over",
         unit == "Percentage") %>%
  select(iso3c, country, year, in_work_risk_poverty = values)


# material and social deprivation (% people)
data_list[["material_social_deprivation"]] <- eurostat_data_list[["material_social_deprivation"]] %>%
  filter(age == "Total",
         sex == "Total",
         unit == "Percentage") %>%
  select(iso3c, country, year, mat_soc_depr_perc = values)


# government expenditure (social protection, housing development, health, education; % GDP)
data_list[["government_expenditure"]] <- eurostat_data_list[["government_expenditure"]] %>%
  filter(str_detect(unit, "GDP"),
         sector == "General government",
         cofog99 %in% c("Housing development", "Health", "Education", "Social protection"),
         na_item == "Total general government expenditure") %>%
  mutate(cofog99 = paste(str_replace_all(str_to_lower(cofog99), " ", "_"), "exp", sep = "_")) %>%
  select(iso3c, country, year, cofog99, values) %>%
  pivot_wider(names_from = cofog99, values_from = values)


# creating a vector with countries included in Eurostat data to filter World Bank, ILO and WHO data
eurostat_iso3c <- unique(eurostat_data_list[[1]]$iso3c)


# creating a function to fetch World Bank data
fetch_wb_data <- function(indicator_id, name){
  WDI(indicator = indicator_id, extra = TRUE) %>%
    rename(!!name := !!indicator_id) %>%
    mutate(country = case_when(iso3c == "XKX" ~ "Kosovo",
                               iso3c == "TUR" ~ "Türkiye",
                               .default = country)) %>%
    filter(iso3c %in% eurostat_iso3c)
}


# fetching World Bank data
wb_data_list <- map(wb_code_list, ~fetch_wb_data(.x, names(wb_code_list)[wb_code_list == .x]))


# adding Gini index data from the World Bank to the data list
data_list[["gini"]] <- data_list[["gini"]] %>%
  bind_rows(wb_data_list[["gini_index"]] %>%
              filter(year < 2014)) %>%
  filter(!is.na(gini_index)) %>%
  distinct() %>%
  select(iso3c, country, year, gini_index)


# adding literacy data
data_list[["literacy_rate"]] <- wb_data_list[["literacy_rate"]] %>%
  filter(!is.na(literacy_rate)) %>%
  select(iso3c, country, year, literacy_rate)


# fetching WHO data (Universal Healthcare Coverage index)
data_list[["uhc_coverage_index"]] <- get_data("UHC_INDEX_REPORTED") %>% 
  mutate(iso3c = countrycode(sourcevar = country,
                             origin = "country.name",
                             destination = "iso3c"),
         country = case_when(iso3c == "XKX" ~ "Kosovo",
                             iso3c == "TUR" ~ "Türkiye",
                             iso3c == "GBR" ~ "United Kingdom",
                             iso3c == "NLD" ~ "Netherlands",
                             .default = country)) %>%
  filter(iso3c %in% eurostat_iso3c) %>%
  select(iso3c, country, year, uhc_cov_index = value)


# ILO data: percentage population covered by social protection floors/systems
data_list[["social_protection"]] <- get_ilostat(id = "SDG_0131_SEX_SOC_RT_A", type = "both") %>%
  filter(classif1.label %in% c("Contingency: Population covered by at least one social protection benefit",
                               "Contingency: Poor persons covered by social protection systems"),
         str_detect(sex.label, "Total")) %>%
  mutate(year = as.integer(time),
         classif1.label = if_else(str_detect(classif1.label, "one social protection"),
                                  "pop_cov",
                                  "poors_cov"),
         ref_area.label = if_else(ref_area == "GBR",
                                  "United Kingdom",
                                  ref_area.label)) %>%
  select(iso3c = ref_area, country = ref_area.label, year, variable = classif1.label, obs_value) %>%
  pivot_wider(names_from = variable,
              values_from = obs_value) %>%
  filter(iso3c %in% eurostat_iso3c)


# ILO data: annual average wage (2017 PPS USD)
data_list[["avg_wage"]] <- get_ilostat("EAR_4MTH_SEX_ECO_CUR_NB_A", type = "both") %>%
  filter(sex == "SEX_T",
         classif1.label == "Economic activity (Aggregate): Total",
         classif2.label == "Currency: 2021 PPP $") %>%
  mutate(year = as.integer(time),
         ref_area.label = if_else(ref_area == "GBR",
                                  "United Kingdom",
                                  ref_area.label)) %>%
  select(iso3c = ref_area, country = ref_area.label, year, ann_avg_wage = obs_value) %>%
  filter(iso3c %in% eurostat_iso3c)


# fetching Human Development Index and years of schooling data (UNHDR)
data_list[["hdi"]] <- read_csv("https://hdr.undp.org/sites/default/files/2023-24_HDR/HDR23-24_Composite_indices_complete_time_series.csv") %>%
  pivot_longer(cols = !c("iso3", "country", "hdicode", "region"),
               names_to = "variable",
               values_to = "value") %>%
  separate(col = variable,
           into = c("variable", "year"),
           sep = "_(?=\\d{4}$)") %>%
  mutate(year = as.integer(year),
         country = case_when(iso3 == "XKX" ~ "Kosovo*",
                             iso3 == "TUR" ~ "Türkiye",
                             .default = country)) %>%
  rename(iso3c = iso3) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  select(iso3c, country, year, ihdi, eys, mys) %>%
  mutate(ed_index = round(((eys / 18 + mys / 15)/2), 2)) %>%
  filter(iso3c %in% eurostat_iso3c)


# merging data
data <- reduce(data_list, full_join, by = c("iso3c", "country", "year")) %>%
  mutate(country = case_when(country == "Kosovo*" ~ "Kosovo",
                             country == "Slovak Republic" ~ "Slovakia",
                             str_detect(country, "Great Britain and Northern") ~ "United Kingdom",
                             str_detect(country, "Netherlands") ~ "Netherlands",
                             .default = country)) %>%
  filter(iso3c %in% eurostat_iso3c)


# adding an extra column to classify countries as part of the EU27, EFTA and others
data$group <- case_when(data$iso3c %in% c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "EST", "GRC",
                                          "ESP", "FIN", "FRA", "HRV", "HUN", "IRL", "ITA", "LTU",
                                          "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SWE",
                                          "SVN", "SVK") ~ "EU27",
                        data$iso3c %in% c("CHE", "ISL", "NOR") ~ "EFTA",
                        .default = "Other")


# adding another column to classify countries at a sub-regional level within Europe (adapted from UN geoschemes)
UN_codes <- read_html("https://unstats.un.org/unsd/methodology/m49/overview/") %>%   # fetching UN geoscheme
  html_table() %>%
  .[[1]] %>%
  setNames(str_replace_all(.[1, ], "-| ", "_")) %>%
  slice(-1) %>%
  select(country = Country_or_Area, iso3c = ISO_alpha3_Code, region = Sub_region_Name)


data <- data %>%
  left_join(y = UN_codes,
            by = c("iso3c", "country")) %>%
  mutate(region = case_when(iso3c %in% c("EST", "LVA", "LTU") ~ "Baltic states",
                            iso3c %in% c("NLD", "GBR", "IRL") ~ "Western Europe",
                            iso3c %in% c("CYP", "TUR") ~ "Southern Europe",
                            .default = region))

# exporting data as csv
write_csv(data, "2_Tidy data/data.csv")


# cleaning the environment
rm(list = setdiff(ls(), c("data")))





































