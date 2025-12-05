library(eurostat)
library(readr)
library(countrycode)
library(bayesPop)
library(bayesMig)
library(bayesLife)
library(wpp2019)
library(tidyverse)
library(writexl)
library(pxweb)
library(readxl)
#population counts

meta <- read_excel("meta_data/kunnat_ja_kuntapohjaiset_alueet_2025_suomeksi_ruotsiksi_englanniksi (1).xlsx", 
                   range = "A2:AJ310") |> 
  distinct()

pxweb_query_list <- 
  list("Alue"=c("SSS"),
       "Ikä"=c("SSS","000","001","002","003","004","005","006","007","008","009","010","011","012","013","014","015","016","017","018","019","020","021","022","023","024","025","026","027","028","029","030","031","032","033","034","035","036","037","038","039","040","041","042","043","044","045","046","047","048","049","050","051","052","053","054","055","056","057","058","059","060","061","062","063","064","065","066","067","068","069","070","071","072","073","074","075","076","077","078","079","080","081","082","083","084","085","086","087","088","089","090","091","092","093","094","095","096","097","098","099","100-"),
       "Sukupuoli"=c("SSS","1","2"),
       "Vuosi"=c("1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024"),
       "Tiedot"=c("vaesto"))
px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/vaerak/statfin_vaerak_pxt_11re.px",
            query = pxweb_query_list)

px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

vaesto <- px_data_frame|> 
  filter(Ikä!="Yhteensä") |>
  rename(Kunta=Alue,
         pop='Väestö 31.12.',
         age=Ikä)|>
  mutate(age=case_when(age=="100 -"~100, TRUE~as.numeric(age))) |>
  mutate(name="Finland") |>
  mutate(country_code=246)

popF <- vaesto |>
  filter(Sukupuoli == "Naiset") |>
  select(country_code, name, age, year = Vuosi, value = pop) |>
  pivot_wider(names_from = year, values_from = value) |>
  arrange(country_code, age) |> filter(!is.na(age)) |>
  arrange(-country_code)

write_delim(popF, "data/popF_koko.txt", delim = "\t", na = "", quote_escape = "none")

popM <- vaesto |>
  filter(Sukupuoli == "Miehet") |>
  select(country_code, name, age, year = Vuosi, value = pop) |>
  pivot_wider(names_from = year, values_from = value) |>
  arrange(country_code, age) |> filter(!is.na(age)) |>
  arrange(-country_code)

write_delim(popM, "data/popM_koko.txt", delim = "\t", na = "", quote_escape = "none")


#########################################
#migration
######################################
net_mig <- get_eurostat("migr_netmigr", time_format = "num")

net_mig_filtered <- net_mig |>
  filter(sex == "T", agedef == "COMPLET", age == "TOTAL") |>
  select(iso2 = geo, year = TIME_PERIOD, migration = values) |>
  mutate(code = as.character(countrycode(iso2, origin = "iso2c", destination = "un")))

pop_data <- get_eurostat("demo_pjan", time_format = "num")

pop_filtered <- pop_data |>
  filter(sex == "T", age == "TOTAL") |>
  select(iso2 = geo, year = TIME_PERIOD, population = values) |>
  mutate(code = as.character(countrycode(iso2, origin = "iso2c", destination = "un")))

fin_mig_2024 <- tibble(code = "246", year = 2024, migration = 47051)
fin_pop_2025 <- tibble(code = "246", year = 2025, population = 5635971)

fin_mig_2025 <- tibble(code = "246", year = 2025, migration = 34000)
fin_pop_2026 <- tibble(code = "246", year = 2026, population = 5635971)

net_mig_filtered <- bind_rows(net_mig_filtered |> select(code, year, migration), fin_mig_2024,fin_mig_2025)
pop_filtered <- bind_rows(pop_filtered |> select(code, year, population), fin_pop_2025, fin_pop_2026)

pop_shifted <- pop_filtered |>
  rename(year_plus1 = year, population_te = population)

mig_with_pop <- net_mig_filtered |>
  left_join(pop_shifted, by = c("code", "year" = "year_plus1"))

mig_rates <- mig_with_pop |>
  mutate(rate = migration / (population_te - migration)) |>
  select(code, year, rate)

mig_rates_named <- mig_rates |>
  mutate(name = countrycode(as.numeric(code), origin = "un", destination = "country.name"))

mig_wide <- mig_rates_named |>
  filter(!is.na(code)) |>
  select(code, name, year, rate) |>
  pivot_wider(names_from = year, values_from = rate) |>
  arrange(code)

write_delim(mig_wide, "data/my_net_migration_rates.txt", delim = "\t", na = "", quote_escape = "none")



####### MUUTTOLIIKE  counts

muutto<- muutto   |>
  mutate(reg_code=case_when(is.na(reg_code)~100, TRUE~reg_code))   |>
  mutate(name=case_when(is.na(name)~"Koko maa", TRUE~name))   |>
  bind_rows(muutto_crude2024)

mig <- muutto |>
  mutate(
    country_code = 100,
    location_type = 4,  
    Vuosi = as.character(Vuosi)  
  ) |>
  select(name, country_code, reg_code, location_type, year = Vuosi, value = counts) |>
  pivot_wider(names_from = year, values_from = value) |>
  arrange(reg_code) |>
  mutate(reg_code=as.numeric(reg_code)) 

write_delim(mig, "data/mig_counts.txt", delim = "\t", na = "", quote_escape = "none")

