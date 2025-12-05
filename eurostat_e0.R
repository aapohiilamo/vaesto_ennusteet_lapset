library(dplyr)
library(tidyr)
library(countrycode)
library(lubridate)

tpl_main <- system.file("extdata", "my_e0_template.txt", package = "bayesLife")
tpl_subn <- system.file("extdata", "subnational_e0_template.txt", package = "bayesLife")

my_e0_template <- read.table(
  tpl_main,
  header = TRUE,
  sep = "",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
dataset_id <- "demo_mlexpec"
df <- get_eurostat(
  id = dataset_id,
  time_format = "date",   # convert time to Date (year-end); you can change to "num" if preferred
  type = "code"           # keep codes first; we’ll add labels next
)

df_birth_total <- df %>%
  filter(
    unit == "YR",
    age %in% c("Y0", "Y_LT1"),
    sex == "F"                     # <-- if you don't have total in le_birth, re-pull from Eurostat with sex = T
  )

df_birth_total <- df_birth_total %>%
  mutate(year = year(TIME_PERIOD))

df_birth_total <- df_birth_total %>%
  mutate(
    country_code = countrycode(geo, origin = "eurostat", destination = "un",
                               custom_match = c("XK" = 383)),                # Kosovo
    name         = countrycode(geo, origin = "eurostat", destination = "country.name",
                               custom_match = c("XK" = "Kosovo"))
  ) %>%
  filter(!is.na(country_code))

e0_template_single_years <- df_birth_total %>%
  select(name, country_code, year, values) %>%
  arrange(country_code, year) %>%
  filter(year==2024) %>%
  distinct(name, country_code, year, .keep_all = TRUE) %>%
  pivot_wider(
    names_from  = year,
    values_from = values,
    names_sort  = TRUE
  ) %>%
  arrange(name, country_code) %>%
  filter(country_code!=383, country_code!=674) 

data(e0F1)
e0_template_single_years<-e0F1 %>% left_join(e0_template_single_years) %>%
  filter(country_code<900)

write.table(
  e0_template_single_years,
  file = "data/my_e0_single_years.txt",
  sep = "\t",              # tab-delimited
  quote = FALSE,           # no quotes around text
  row.names = FALSE,       # drop row numbers
  na = ""                  # leave missing cells blank
)

###########################
tpl_main <- system.file("extdata", "my_e0_template.txt", package = "bayesLife")
tpl_subn <- system.file("extdata", "subnational_e0_template.txt", package = "bayesLife")

my_e0_template <- read.table(
  tpl_main,
  header = TRUE,
  sep = "",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

df_birth_total <- df %>%
  filter(
    unit == "YR",
    age %in% c("Y0", "Y_LT1"),
    sex == "M"                     # <-- if you don't have total in le_birth, re-pull from Eurostat with sex = T
  )

df_birth_total <- df_birth_total %>%
  mutate(year = year(TIME_PERIOD))

df_birth_total <- df_birth_total %>%
  mutate(
    country_code = countrycode(geo, origin = "eurostat", destination = "un",
                               custom_match = c("XK" = 383)),                # Kosovo
    name         = countrycode(geo, origin = "eurostat", destination = "country.name",
                               custom_match = c("XK" = "Kosovo"))
  ) %>%
  filter(!is.na(country_code))

e0_template_single_years <- df_birth_total %>%
  select(name, country_code, year, values) %>%
  arrange(country_code, year) %>%
  filter(year==2024) %>%
  distinct(name, country_code, year, .keep_all = TRUE) %>%
  pivot_wider(
    names_from  = year,
    values_from = values,
    names_sort  = TRUE
  ) %>%
  arrange(name, country_code) %>%
  filter(country_code!=383, country_code!=674)

data(e0M1)
e0_template_single_years_men<-e0M1 %>% left_join(e0_template_single_years) %>%
  filter(country_code<900)

write.table(
  e0_template_single_years_men,
  file = "data/my_eM0_single_years.txt",
  sep = "\t",              # tab-delimited
  quote = FALSE,           # no quotes around text
  row.names = FALSE,       # drop row numbers
  na = ""                  # leave missing cells blank
)
