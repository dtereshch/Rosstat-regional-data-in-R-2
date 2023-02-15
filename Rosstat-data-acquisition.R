library(tidyverse)
library(readxl)

rm(list = ls())

regions_data_path <- "~/Documents/eco archives/data/Регионы России/приложения/pril2021/"

# Rosstat data =================================================================
setwd(regions_data_path)

## Innovations data ------------------------------------------------------------

### R&D expenditures
RD_exp <- read_excel("Раздел 18 - Наука и инновации.xlsx", 
                     sheet = "18.5.", col_types = c("text", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric"), 
                     na = "-", skip = 5)

### Patents for inventions
pat_inv <- read_excel("Раздел 18 - Наука и инновации.xlsx", 
                      sheet = "18.9.3.", col_types = c("text", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric"), 
                      na = "-", skip = 6)


### Patents for models
pat_mod <- read_excel("Раздел 18 - Наука и инновации.xlsx", 
                      sheet = "18.9.4.", col_types = c("text", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric"), 
                      na = "-", skip = 6)

### Value of innovative goods, ... 
vig <- read_excel("Раздел 18 - Наука и инновации.xlsx", 
                  sheet = "18.4.1.", col_types = c("text", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric"), 
                  na = "-", skip = 6)

## Inflation data --------------------------------------------------------------
# cpi <- read_excel("Раздел 20 - Цены и тарифы.xlsx", 
#                   sheet = "20.1.", na = "…", skip = 6)

basket <- read_excel("Раздел 20 - Цены и тарифы.xlsx", 
                     sheet = "20.3.1.", col_types = c("text", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric"), 
                     na = "…", skip = 7)

basket_Russia <- read_excel("Раздел 20 - Цены и тарифы.xlsx", 
                            sheet = "20.3.1.", range = "A8:B9") %>%
  as.matrix()

basket_Russia_base <- basket_Russia[1, 2] %>% as.numeric()


## Control variables -----------------------------------------------------------

### Population size
pop <- read_excel("Раздел 1 - Население.xlsx", 
                  sheet = "1.2.", skip = 5)

### NUMBER /Share of students enrolled in bachelor’s, specialist’s and master’s programmes
### in the total population
stud <- read_excel("Раздел 4 - Образование.xlsx", 
                   sheet = "4.18.", col_types = c("text", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric"), 
                   skip = 5)

### Unemployment rate
unem <- read_excel("Раздел 2 - Труд.xlsx", 
                   sheet = "2.10.1.", na = "…", skip = 7)

### Gender
gender <- read_excel("Раздел 1 - Население.xlsx", 
                     sheet = "1.5.", skip = 5)

### Age
middle <- read_excel("Раздел 1 - Население.xlsx", 
                     sheet = "1.6.2.", skip = 6)

old <- read_excel("Раздел 1 - Население.xlsx", 
                  sheet = "1.6.3.", skip = 6)

### GRP
grp <- read_excel("Раздел 8 - Валовой региональный продукт.xlsx", 
                  sheet = "8.1.", na = "…", skip = 5)

### Investments in fixed assets
invest <- read_excel("Раздел 10 - Инвестиции.xlsx", 
                     sheet = "10.1.", skip = 6)

### Depreciation of fixed assets
depr <- read_excel("Раздел 9 - Основные фонды.xlsx", 
                   sheet = "9.3.", na = "…", skip = 5)

### VOLUME / Share of manufacturing output in gross value added
manuf <- read_excel("Раздел 12 - Промышленное производство.xlsx", 
                    sheet = "12.2.3.", na = "…", skip = 6)

### VOLUME / Share of mining
mining <- read_excel("Раздел 12 - Промышленное производство.xlsx", 
                     sheet = "12.2.1.", col_types = c("text", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric"), 
                     na = "…", skip = 6)


## Creating long panel ---------------------------------------------------------

### List of all Rosstat datasets
regions_datasets_names <- ls()[sapply(ls(), function(x) any(is.data.frame(get(x))))]

regions_datasets <- lapply(regions_datasets_names, 
                           function(x) get(x))

names(regions_datasets) <- regions_datasets_names

### Manipulations
regions_datasets <- lapply(regions_datasets, rename, "region_rus" = "...1")

regions_datasets <- lapply(regions_datasets, filter, !if_all(-region_rus, is.na))

regions_datasets <- lapply(regions_datasets, filter, 
                            (!str_detect(region_rus, "едера")) 
                            & (!str_detect(region_rus, "Северо")))

regions_datasets <- lapply(regions_datasets, mutate, region_rus = str_replace_all(region_rus, "\t", " "))
regions_datasets <- lapply(regions_datasets, mutate, region_rus = str_replace_all(region_rus, "–", "-"))
regions_datasets <- lapply(regions_datasets, mutate, region_rus = str_replace_all(region_rus, "-Алания", " - Алания"))
regions_datasets <- lapply(regions_datasets, mutate, region_rus = replace(region_rus, str_detect(region_rus, "Калинин"), "Калининградская область"))
regions_datasets <- lapply(regions_datasets, mutate, region_rus = str_squish(region_rus))

### Reshape
regions_datasets <- regions_datasets %>%
  names(.) %>%
  #walk(~ pivot_longer(regions_datasets[[.]], -region_rus, names_to = "year", values_to = .)) # Invisible output
  map(~ pivot_longer(regions_datasets[[.]], -region_rus, names_to = "year", values_to = .)) # Prints output to console

#regions_datasets[[1]]
#regions_datasets[[14]]

regions_datasets <- lapply(regions_datasets, mutate, 
                           year = str_replace_all(year, "/20..", ""))

### Joining
regions_rosstat <- regions_datasets %>% reduce(full_join, by = c("region_rus", "year"))


# Saving dataset ===============================================================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
regions_rosstat %>% write_csv("data/regions_rosstat.csv")
rm(list = ls())
