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
meta <- read_excel("data/meta_data/kunnat_ja_kuntapohjaiset_alueet_2025_suomeksi_ruotsiksi_englanniksi (1).xlsx", 
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

#väestön ennakkotiedot

pxweb_query_list <- 
  list("Kuukausi"=c("2025M12"),
       "Alue"=c("SSS"),
       "Sukupuoli"=c("SSS","1","2"),
       "Ikä"=c("SSS","000","001","002","003","004","005","006","007","008","009","010","011","012","013","014","015","016","017","018","019","020","021","022","023","024","025","026","027","028","029","030","031","032","033","034","035","036","037","038","039","040","041","042","043","044","045","046","047","048","049","050","051","052","053","054","055","056","057","058","059","060","061","062","063","064","065","066","067","068","069","070","071","072","073","074","075","076","077","078","079","080","081","082","083","084","085","086","087","088","089","090","091","092","093","094","095","096","097","098","099","100-"),
       "Tiedot"=c("vaesto"))
px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/vamuu/statfin_vamuu_pxt_11lj.px",
            query = pxweb_query_list)
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") |>
  mutate(Vuosi="2025") |> 
  rename(pop=Väkiluku)|>
  bind_rows(px_data_frame |> 
              rename(pop='Väestö 31.12.')) 

vaesto <- px_data_frame|> 
  filter(Ikä!="Yhteensä") |>
  rename(Kunta=Alue,
         age=Ikä)|>
  mutate(age=case_when(age=="100 -"~100, TRUE~as.numeric(age))) |>
  mutate(name="Finland") |>
  mutate(country_code=246) |>
  arrange(Vuosi)

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
fin_pop_2025 <- tibble(code = "246", year = 2025, population = 5656900)

fin_mig_2025 <- tibble(code = "246", year = 2025, migration = 34852)
fin_pop_2026 <- tibble(code = "246", year = 2026, population = 5656900)

net_mig_filtered <- bind_rows(net_mig_filtered |> select(code, year, migration), fin_mig_2025)
pop_filtered <- bind_rows(pop_filtered |> select(code, year, population),  fin_pop_2026)

pop_shifted <- pop_filtered |>
  rename(year_plus1 = year, population_te = population)

mig_with_pop <- net_mig_filtered |>
  left_join(pop_shifted, by = c("code", "year" = "year_plus1"))

mig_rates <- mig_with_pop |>
  mutate(rate = migration / (population_te )) |>
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

pxweb_query_list <- 
  list("Vuosi"=c("1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024"),
       "Alue"=c("SSS","KU020","KU005","KU009","KU010","KU016","KU018","KU019","KU035","KU043","KU046","KU047","KU049","KU050","KU051","KU052","KU060","KU061","KU062","KU065","KU069","KU071","KU072","KU074","KU075","KU076","KU077","KU078","KU079","KU081","KU082","KU086","KU111","KU090","KU091","KU097","KU098","KU102","KU103","KU105","KU106","KU108","KU109","KU139","KU140","KU142","KU143","KU145","KU146","KU153","KU148","KU149","KU151","KU152","KU165","KU167","KU169","KU170","KU171","KU172","KU176","KU177","KU178","KU179","KU181","KU182","KU186","KU202","KU204","KU205","KU208","KU211","KU213","KU214","KU216","KU217","KU218","KU224","KU226","KU230","KU231","KU232","KU233","KU235","KU236","KU239","KU240","KU320","KU241","KU322","KU244","KU245","KU249","KU250","KU256","KU257","KU260","KU261","KU263","KU265","KU271","KU272","KU273","KU275","KU276","KU280","KU284","KU285","KU286","KU287","KU288","KU290","KU291","KU295","KU297","KU300","KU301","KU304","KU305","KU312","KU316","KU317","KU318","KU398","KU399","KU400","KU407","KU402","KU403","KU405","KU408","KU410","KU416","KU417","KU418","KU420","KU421","KU422","KU423","KU425","KU426","KU444","KU430","KU433","KU434","KU435","KU436","KU438","KU440","KU441","KU475","KU478","KU480","KU481","KU483","KU484","KU489","KU491","KU494","KU495","KU498","KU499","KU500","KU503","KU504","KU505","KU508","KU507","KU529","KU531","KU535","KU536","KU538","KU541","KU543","KU545","KU560","KU561","KU562","KU563","KU564","KU309","KU576","KU577","KU578","KU445","KU580","KU581","KU599","KU583","KU854","KU584","KU592","KU593","KU595","KU598","KU601","KU604","KU607","KU608","KU609","KU611","KU638","KU614","KU615","KU616","KU619","KU620","KU623","KU624","KU625","KU626","KU630","KU631","KU635","KU636","KU678","KU710","KU680","KU681","KU683","KU684","KU686","KU687","KU689","KU691","KU694","KU697","KU698","KU700","KU702","KU704","KU707","KU729","KU732","KU734","KU736","KU790","KU738","KU739","KU740","KU742","KU743","KU746","KU747","KU748","KU791","KU749","KU751","KU753","KU755","KU758","KU759","KU761","KU762","KU765","KU766","KU768","KU771","KU777","KU778","KU781","KU783","KU831","KU832","KU833","KU834","KU837","KU844","KU845","KU846","KU848","KU849","KU850","KU851","KU853","KU857","KU858","KU859","KU886","KU887","KU889","KU890","KU892","KU893","KU895","KU785","KU905","KU908","KU092","KU915","KU918","KU921","KU922","KU924","KU925","KU927","KU931","KU934","KU935","KU936","KU941","KU946","KU976","KU977","KU980","KU981","KU989","KU992"),
       "Tiedot"=c("koknetmuutto"))

px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/muutl/statfin_muutl_pxt_11ae.px",
            query = pxweb_query_list)

mig_data_raw <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

saveRDS(mig_data_raw,file = "data/alue/mig_raw.rds")

mig_data_raw <- readRDS("data/alue/mig_raw.rds")


muutto_crude<- mig_data_raw |> rename(year=Vuosi) |>
  filter(Alue=="KOKO MAA")

mig <- muutto_crude |>
  mutate(
    country_code = 246,
    reg_code=246,
    name="Finland",
    location_type =  case_when(name=="Finland" ~4, TRUE~4),  
    year = as.character(year) 
  ) |>
  select(name, country_code, reg_code, location_type, year,counts=Kokonaisnettomuutto) |>
  pivot_wider(names_from = year, values_from = counts) |>
  arrange(reg_code) |>
  mutate(reg_code=as.numeric(reg_code)) |>
  filter(country_code==246)

write_delim(mig, "data/mig_counts_Finland.txt", delim = "\t", na = "", quote_escape = "none")


