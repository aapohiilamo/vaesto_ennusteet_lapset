library(pxweb)
library(readxl)
library(dplyr)
library(readr)
library(sotkanet)
library(tidyverse)
library(DemoTools)

#seuraavat datat
#1. meta tiedot (kuntien ja maakuntien linkit)
#2. väestö
#3. syntyvyys
#4. elinajanodote (tämä sotkanetistä) (muista ajaa my;s koodi jolla jaetaan nama ikakohtaiseksi)
#5. muuttoliike
#6. tk:n ennuste
#7. PASFR
#8. Mx
#9. Age and sex specifix migration

#####################################
#1. municipality_2_regiontiedot (kuntien ja maakuntien linkit)
# https://stat.fi/fi/luokitukset/tupa here is the link https://media.stat.fi/A7H6ohk0S8qafyCM4bfDaz/DICwBPn5Q8uifsM6dwow
###################################

municipality_2_region <- read_excel(
  "data/meta_data/kunnat_ja_kuntapohjaiset_alueet_2025_suomeksi_ruotsiksi_englanniksi (1).xlsx", 
  range = "A2:AJ310") |> 
  distinct()  |> 
  mutate(name =Maakunta)  |>
  mutate(reg_code =`Maakunnan koodi`) |>
  mutate(reg_code=as.numeric(reg_code)) |>
  bind_rows(
    tibble(reg_code = 246, Kunta="KOKO MAA", name = "Finland")
  )

region_codes <- municipality_2_region |>
  select(reg_code, Maakunta,name) |>
  distinct() 

wafips <- region_codes |>
  mutate(
    country_code = 246,
    location_type = case_when(
      reg_code == 246 ~ 0,
      TRUE ~ 4
    )  ) |>
  select(country_code, reg_code, name, location_type) |>
  arrange(reg_code)

write_delim(wafips, "data/alue/locations.txt", delim = "\t", na = "", quote_escape = "none")

#####################################
#2.väestö / pop counts
###################################
#population counts
pxweb_query_list <- 
  list("Alue"=c("SSS","KU020","KU005","KU009","KU010","KU016","KU018","KU019","KU035","KU043","KU046","KU047","KU049","KU050","KU051","KU052","KU060","KU061","KU062","KU065","KU069","KU071","KU072","KU074","KU075","KU076","KU077","KU078","KU079","KU081","KU082","KU086","KU111","KU090","KU091","KU097","KU098","KU102","KU103","KU105","KU106","KU108","KU109","KU139","KU140","KU142","KU143","KU145","KU146","KU153","KU148","KU149","KU151","KU152","KU165","KU167","KU169","KU170","KU171","KU172","KU176","KU177","KU178","KU179","KU181","KU182","KU186","KU202","KU204","KU205","KU208","KU211","KU213","KU214","KU216","KU217","KU218","KU224","KU226","KU230","KU231","KU232","KU233","KU235","KU236","KU239","KU240","KU320","KU241","KU322","KU244","KU245","KU249","KU250","KU256","KU257","KU260","KU261","KU263","KU265","KU271","KU272","KU273","KU275","KU276","KU280","KU284","KU285","KU286","KU287","KU288","KU290","KU291","KU295","KU297","KU300","KU301","KU304","KU305","KU312","KU316","KU317","KU318","KU398","KU399","KU400","KU407","KU402","KU403","KU405","KU408","KU410","KU416","KU417","KU418","KU420","KU421","KU422","KU423","KU425","KU426","KU444","KU430","KU433","KU434","KU435","KU436","KU438","KU440","KU441","KU475","KU478","KU480","KU481","KU483","KU484","KU489","KU491","KU494","KU495","KU498","KU499","KU500","KU503","KU504","KU505","KU508","KU507","KU529","KU531","KU535","KU536","KU538","KU541","KU543","KU545","KU560","KU561","KU562","KU563","KU564","KU309","KU576","KU577","KU578","KU445","KU580","KU581","KU599","KU583","KU854","KU584","KU592","KU593","KU595","KU598","KU601","KU604","KU607","KU608","KU609","KU611","KU638","KU614","KU615","KU616","KU619","KU620","KU623","KU624","KU625","KU626","KU630","KU631","KU635","KU636","KU678","KU710","KU680","KU681","KU683","KU684","KU686","KU687","KU689","KU691","KU694","KU697","KU698","KU700","KU702","KU704","KU707","KU729","KU732","KU734","KU736","KU790","KU738","KU739","KU740","KU742","KU743","KU746","KU747","KU748","KU791","KU749","KU751","KU753","KU755","KU758","KU759","KU761","KU762","KU765","KU766","KU768","KU771","KU777","KU778","KU781","KU783","KU831","KU832","KU833","KU834","KU837","KU844","KU845","KU846","KU848","KU849","KU850","KU851","KU853","KU857","KU858","KU859","KU886","KU887","KU889","KU890","KU892","KU893","KU895","KU785","KU905","KU908","KU092","KU915","KU918","KU921","KU922","KU924","KU925","KU927","KU931","KU934","KU935","KU936","KU941","KU946","KU976","KU977","KU980","KU981","KU989","KU992"),
       "Ikä"=c("SSS","000","001","002","003","004","005","006","007","008","009","010","011","012","013","014","015","016","017","018","019","020","021","022","023","024","025","026","027","028","029","030","031","032","033","034","035","036","037","038","039","040","041","042","043","044","045","046","047","048","049","050","051","052","053","054","055","056","057","058","059","060","061","062","063","064","065","066","067","068","069","070","071","072","073","074","075","076","077","078","079","080","081","082","083","084","085","086","087","088","089","090","091","092","093","094","095","096","097","098","099","100-"),
       "Sukupuoli"=c("SSS","1","2"),
       "Vuosi"=c("1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024"),
       "Tiedot"=c("vaesto"))
px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/vaerak/statfin_vaerak_pxt_11re.px",
            query = pxweb_query_list)

population_raw <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
##väestönennakkotiedot
pxweb_query_list <- 
  list("Kuukausi"=c("2025M12"),
       "Alue"=c("SSS","KU020","KU005","KU009","KU010","KU016","KU018","KU019","KU035","KU043","KU046","KU047","KU049","KU050","KU051","KU052","KU060","KU061","KU062","KU065","KU069","KU071","KU072","KU074","KU075","KU076","KU077","KU078","KU079","KU081","KU082","KU086","KU111","KU090","KU091","KU097","KU098","KU102","KU103","KU105","KU106","KU108","KU109","KU139","KU140","KU142","KU143","KU145","KU146","KU153","KU148","KU149","KU151","KU152","KU165","KU167","KU169","KU170","KU171","KU172","KU176","KU177","KU178","KU179","KU181","KU182","KU186","KU202","KU204","KU205","KU208","KU211","KU213","KU214","KU216","KU217","KU218","KU224","KU226","KU230","KU231","KU232","KU233","KU235","KU236","KU239","KU240","KU320","KU241","KU322","KU244","KU245","KU249","KU250","KU256","KU257","KU260","KU261","KU263","KU265","KU271","KU272","KU273","KU275","KU276","KU280","KU284","KU285","KU286","KU287","KU288","KU290","KU291","KU295","KU297","KU300","KU301","KU304","KU305","KU312","KU316","KU317","KU318","KU398","KU399","KU400","KU407","KU402","KU403","KU405","KU408","KU410","KU416","KU417","KU418","KU420","KU421","KU422","KU423","KU425","KU426","KU444","KU430","KU433","KU434","KU435","KU436","KU438","KU440","KU441","KU475","KU478","KU480","KU481","KU483","KU484","KU489","KU491","KU494","KU495","KU498","KU499","KU500","KU503","KU504","KU505","KU508","KU507","KU529","KU531","KU535","KU536","KU538","KU541","KU543","KU545","KU560","KU561","KU562","KU563","KU564","KU309","KU576","KU577","KU578","KU445","KU580","KU581","KU599","KU583","KU854","KU584","KU592","KU593","KU595","KU598","KU601","KU604","KU607","KU608","KU609","KU611","KU638","KU614","KU615","KU616","KU619","KU620","KU623","KU624","KU625","KU626","KU630","KU631","KU635","KU636","KU678","KU710","KU680","KU681","KU683","KU684","KU686","KU687","KU689","KU691","KU694","KU697","KU698","KU700","KU702","KU704","KU707","KU729","KU732","KU734","KU736","KU790","KU738","KU739","KU740","KU742","KU743","KU746","KU747","KU748","KU791","KU749","KU751","KU753","KU755","KU758","KU759","KU761","KU762","KU765","KU766","KU768","KU771","KU777","KU778","KU781","KU783","KU831","KU832","KU833","KU834","KU837","KU844","KU845","KU846","KU848","KU849","KU850","KU851","KU853","KU857","KU858","KU859","KU886","KU887","KU889","KU890","KU892","KU893","KU895","KU785","KU905","KU908","KU092","KU915","KU918","KU921","KU922","KU924","KU925","KU927","KU931","KU934","KU935","KU936","KU941","KU946","KU976","KU977","KU980","KU981","KU989","KU992"),
            "Sukupuoli"=c("SSS","1","2"),
       "Ikä"=c("SSS","000","001","002","003","004","005","006","007","008","009","010","011","012","013","014","015","016","017","018","019","020","021","022","023","024","025","026","027","028","029","030","031","032","033","034","035","036","037","038","039","040","041","042","043","044","045","046","047","048","049","050","051","052","053","054","055","056","057","058","059","060","061","062","063","064","065","066","067","068","069","070","071","072","073","074","075","076","077","078","079","080","081","082","083","084","085","086","087","088","089","090","091","092","093","094","095","096","097","098","099","100-"),
       "Tiedot"=c("vaesto"))
px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/vamuu/statfin_vamuu_pxt_11lj.px",
            query = pxweb_query_list)
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") |>
  mutate(Vuosi="2025") |> 
  rename(pop=Väkiluku)|>
  bind_rows(population_raw |> 
              rename(pop='Väestö 31.12.')) 


saveRDS(px_data_frame,file = "data/alue/population_raw.rds")

population_raw <- readRDS("data/alue/population_raw.rds")

vaesto <- population_raw |> 
  rename(Kunta=Alue,
         age=Ikä,
         year=Vuosi, 
         sex=Sukupuoli) |>
  mutate(age=case_when(age=="100 -"~100, TRUE~as.numeric(age))) |>
  left_join(municipality_2_region) |>
  group_by(name ,reg_code,year,age, sex) |>
  summarise(pop=sum(pop)) |>
  ungroup() |>
  arrange(year)

pop <- vaesto |>
  select(reg_code,sex, name, age, year,pop) |>
  pivot_wider(names_from = year, 
              values_from = pop) |>
  arrange(reg_code, age) |> filter(!is.na(age)) |>
  mutate(reg_code=as.numeric(reg_code)) |>
  arrange(as.numeric(reg_code))

popF <- pop |>
  filter(sex == "Naiset") |>
  select(!sex) 

write_delim(popF, "data/alue/popF.txt", delim = "\t", na = "", quote_escape = "none")

popM <- pop |>
  filter(sex == "Miehet") |>
  select(!sex)

write_delim(popM, "data/alue/popM.txt", delim = "\t", na = "", quote_escape = "none")

#####################################
#3. syntyvyys /tfr
#####################################

pxweb_query_list <- 
  list("Maakunta"=c("SSS","MA1","MK01","MK02","MK04","MK05","MK06","MK07","MK08","MK09","MK10","MK11","MK12","MK13","MK14","MK15","MK16","MK17","MK18","MK19","MA2","MK21"),
       "Tiedot"=c("tfr"),
       "Vuosi"=c("1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024"))

px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/synt/statfin_synt_pxt_12du.px",
            query = pxweb_query_list)

trf_raw <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

saveRDS(trf_raw,file = "data/alue/trf_raw.rds")

trf_raw <- readRDS("data/alue/trf_raw.rds")

tfr <- trf_raw |>
  filter(!str_starts(Maakunta, "MA")) |>  
  mutate(
    Maakunta = str_remove(Maakunta, "^MK\\d+\\s*"),  
    country_code = 246,
    Maakunta = case_when(Maakunta=="Koko maa" ~ "Finland", TRUE~ Maakunta), 
  )  |>
  left_join(region_codes, by = c("Maakunta")) |>  
  select(country_code, reg_code, name, year = Vuosi, value = Kokonaishedelmällisyysluku) |>
  pivot_wider(names_from = year, values_from = value) |>
  mutate(reg_code=as.numeric(reg_code), 
         reg_code = case_when(is.na(reg_code) ~ 246, TRUE~ reg_code),  
         include_code = case_when(reg_code==246 ~ 0, TRUE~ 2),  
         name = case_when(is.na(name) ~  "Finland", TRUE~ name),  
  ) |>
  select(country_code, reg_code, name, include_code, everything()) |>
  arrange(reg_code)

write_delim(tfr, "data/alue/tfr.txt", delim = "\t", na = "", quote_escape = "none")

#####################################
#4. elinajanodote / e0
#using sotkanet package for convinience
#####################################

dat <- get_sotkanet(indicators = 4011) |> 
  filter(region.category!="NUTS1") |>  
  rename(reg_code=region.code)

e0 <- dat|>
  arrange(year,gender) |>
  mutate(country_code = 246) |>
  mutate(reg_code=as.numeric(reg_code)) |>
  select(country_code, reg_code,gender, year, primary.value) |>
  left_join(region_codes) |>
  select(country_code, reg_code,gender, name, year, primary.value) |>
  pivot_wider(names_from = year, values_from = primary.value) |>
  mutate(name=case_when(is.na(name)~"Finland", TRUE~name))|>
  mutate(reg_code=case_when(reg_code==358~246, TRUE~reg_code), 
         include_code = case_when(reg_code==246 ~ 0, TRUE~ 2),  
  ) |>
  select(country_code, reg_code, name, include_code, everything()) |>
  arrange(reg_code)

e0F <- e0|> filter(gender=="female")|> select(!gender)

write_delim(e0F, "data/alue/e0F.txt", delim = "\t", na = "", quote_escape = "none")

e0M <- e0|> filter(gender=="male")|> select(!gender)

write_delim(e0M, "data/alue/e0M.txt", delim = "\t", na = "", quote_escape = "none")

#####################################
#5. MUUTTOLIIKE / migration
#these are muncipality level but summed to regions
#####################################

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


##crude immigration rate
pop <- vaesto |>
  filter(sex == "Yhteensä") |>
  select(reg_code, name, age, year, pop) |>
  arrange(reg_code, age) |> 
  filter(is.na(age))  |>
  mutate(name=case_when(name=="Koko maa"~"Finland", TRUE~name), 
         reg_code=case_when(reg_code==100~246, TRUE~reg_code))

muutto_crude<- mig_data_raw |> rename(Kunta=Alue, year=Vuosi) |>
  left_join(municipality_2_region) |>
  group_by(name ,reg_code,year) |>
  summarise(counts=sum(Kokonaisnettomuutto)) |>
  ungroup() |>
  left_join(pop) |>
  mutate(crude_migration=counts/pop)

mig_rates <- muutto_crude |> 
  select(country_code=reg_code, name,  year, crude_migration) |>
  pivot_wider(names_from = year, values_from = crude_migration) |>
  arrange(country_code) |>
  mutate(country_code=as.numeric(country_code)) |>
  arrange(country_code) |>
  mutate(include_code=case_when(country_code==246~0, TRUE~2))  |>
  select(country_code,  name, include_code, everything()) |>
  arrange(country_code)

write_delim(mig_rates, "data/alue/mig_rates.txt", delim = "\t", na = "", quote_escape = "none")

# migration counts  counts

mig <- muutto_crude |>
  mutate(
    country_code = 246,
    location_type =  case_when(name=="Finland" ~0, TRUE~4),  
    year = as.character(year) 
  ) |>
  select(name, country_code, reg_code, location_type, year,counts) |>
  pivot_wider(names_from = year, values_from = counts) |>
  arrange(reg_code) |>
  mutate(reg_code=as.numeric(reg_code)) 

write_delim(mig, "data/alue/mig_counts.txt", delim = "\t", na = "", quote_escape = "none")

#####################################
#6. TK ENNUSTE / StatFin 
#these are muncipality level but summed to regions
#####################################

###tk;n ennuste

pxweb_query_list <- 
  list("Alue"=c("SSS","KU020","KU005","KU009","KU010","KU016","KU018","KU019","KU035","KU043","KU046","KU047","KU049","KU050","KU051","KU052","KU060","KU061","KU062","KU065","KU069","KU071","KU072","KU074","KU075","KU076","KU077","KU078","KU079","KU081","KU082","KU086","KU111","KU090","KU091","KU097","KU098","KU102","KU103","KU105","KU106","KU108","KU109","KU139","KU140","KU142","KU143","KU145","KU146","KU153","KU148","KU149","KU151","KU152","KU165","KU167","KU169","KU170","KU171","KU172","KU176","KU177","KU178","KU179","KU181","KU182","KU186","KU202","KU204","KU205","KU208","KU211","KU213","KU214","KU216","KU217","KU218","KU224","KU226","KU230","KU231","KU232","KU233","KU235","KU236","KU239","KU240","KU320","KU241","KU322","KU244","KU245","KU249","KU250","KU256","KU257","KU260","KU261","KU263","KU265","KU271","KU272","KU273","KU275","KU276","KU280","KU284","KU285","KU286","KU287","KU288","KU290","KU291","KU295","KU297","KU300","KU301","KU304","KU305","KU312","KU316","KU317","KU318","KU398","KU399","KU400","KU407","KU402","KU403","KU405","KU408","KU410","KU416","KU417","KU418","KU420","KU421","KU422","KU423","KU425","KU426","KU444","KU430","KU433","KU434","KU435","KU436","KU438","KU440","KU441","KU475","KU478","KU480","KU481","KU483","KU484","KU489","KU491","KU494","KU495","KU498","KU499","KU500","KU503","KU504","KU505","KU508","KU507","KU529","KU531","KU535","KU536","KU538","KU541","KU543","KU545","KU560","KU561","KU562","KU563","KU564","KU309","KU576","KU577","KU578","KU445","KU580","KU581","KU599","KU583","KU854","KU584","KU588","KU592","KU593","KU595","KU598","KU601","KU604","KU607","KU608","KU609","KU611","KU638","KU614","KU615","KU616","KU619","KU620","KU623","KU624","KU625","KU626","KU630","KU631","KU635","KU636","KU678","KU710","KU680","KU681","KU683","KU684","KU686","KU687","KU689","KU691","KU694","KU697","KU698","KU700","KU702","KU704","KU707","KU729","KU732","KU734","KU736","KU790","KU738","KU739","KU740","KU742","KU743","KU746","KU747","KU748","KU791","KU749","KU751","KU753","KU755","KU758","KU759","KU761","KU762","KU765","KU766","KU768","KU771","KU777","KU778","KU781","KU783","KU831","KU832","KU833","KU834","KU837","KU844","KU845","KU846","KU848","KU849","KU850","KU851","KU853","KU857","KU858","KU859","KU886","KU887","KU889","KU890","KU892","KU893","KU895","KU785","KU905","KU908","KU092","KU915","KU918","KU921","KU922","KU924","KU925","KU927","KU931","KU934","KU935","KU936","KU941","KU946","KU976","KU977","KU980","KU981","KU989","KU992"),
       "Vuosi"=c("2024","2025","2026","2027","2028","2029","2030","2031","2032","2033","2034","2035","2036","2037","2038","2039","2040","2041","2042","2043","2044","2045"),
       "Sukupuoli"=c("SSS","1","2"),
       "Ikä"=c("SSS","000","001","002","003","004","005","006","007","008","009","010","011","012","013","014","015","016","017","018","019","020","021","022","023","024","025","026","027","028","029","030","031","032","033","034","035","036","037","038","039","040","041","042","043","044","045","046","047","048","049","050","051","052","053","054","055","056","057","058","059","060","061","062","063","064","065","066","067","068","069","070","071","072","073","074","075","076","077","078","079","080","081","082","083","084","085","086","087","088","089","090","091","092","093","094","095","096","097","098","099","100-"),
       "Tiedot"=c("vaesto_e24"))

px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/vaenn/statfin_vaenn_pxt_14wx.px",
            query = pxweb_query_list)

StatFin_forecast <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

StatFin_forecast <- StatFin_forecast |>
  rename(Kunta=Alue) |>
  mutate(
    count=as.numeric(`Väestö 31.12. (ennuste 2024)`)
  ) |>
  left_join(municipality_2_region) |>
  filter(Sukupuoli=="Yhteensä", Ikä !="Yhteensä") |>
  mutate(Ikä=as.numeric(Ikä)) |>
  filter(Ikä<18) |>
  group_by(Vuosi, Maakunta) |>
  summarise(median=sum(count)) |>
  ungroup() |>
  select(year=Vuosi, name=Maakunta,median) |>
  mutate(ennuste="Tilastokeskus")

saveRDS(StatFin_forecast, file = "data/alue/tk_ennuste.rds")


#####################################
#7. PASFR
#####################################

pxweb_query_list <- 
  list("Alue"=c("SSS","KU020","KU005","KU009","KU010","KU016","KU018","KU019","KU035","KU043","KU046","KU047","KU049","KU050","KU051","KU052","KU060","KU061","KU062","KU065","KU069","KU071","KU072","KU074","KU075","KU076","KU077","KU078","KU079","KU081","KU082","KU086","KU111","KU090","KU091","KU097","KU098","KU102","KU103","KU105","KU106","KU108","KU109","KU139","KU140","KU142","KU143","KU145","KU146","KU153","KU148","KU149","KU151","KU152","KU165","KU167","KU169","KU170","KU171","KU172","KU176","KU177","KU178","KU179","KU181","KU182","KU186","KU202","KU204","KU205","KU208","KU211","KU213","KU214","KU216","KU217","KU218","KU224","KU226","KU230","KU231","KU232","KU233","KU235","KU236","KU239","KU240","KU320","KU241","KU322","KU244","KU245","KU249","KU250","KU256","KU257","KU260","KU261","KU263","KU265","KU271","KU272","KU273","KU275","KU276","KU280","KU284","KU285","KU286","KU287","KU288","KU290","KU291","KU295","KU297","KU300","KU301","KU304","KU305","KU312","KU316","KU317","KU318","KU398","KU399","KU400","KU407","KU402","KU403","KU405","KU408","KU410","KU416","KU417","KU418","KU420","KU421","KU422","KU423","KU425","KU426","KU444","KU430","KU433","KU434","KU435","KU436","KU438","KU440","KU441","KU475","KU478","KU480","KU481","KU483","KU484","KU489","KU491","KU494","KU495","KU498","KU499","KU500","KU503","KU504","KU505","KU508","KU507","KU529","KU531","KU535","KU536","KU538","KU541","KU543","KU545","KU560","KU561","KU562","KU563","KU564","KU309","KU576","KU577","KU578","KU445","KU580","KU581","KU599","KU583","KU854","KU584","KU592","KU593","KU595","KU598","KU601","KU604","KU607","KU608","KU609","KU611","KU638","KU614","KU615","KU616","KU619","KU620","KU623","KU624","KU625","KU626","KU630","KU631","KU635","KU636","KU678","KU710","KU680","KU681","KU683","KU684","KU686","KU687","KU689","KU691","KU694","KU697","KU698","KU700","KU702","KU704","KU707","KU729","KU732","KU734","KU736","KU790","KU738","KU739","KU740","KU742","KU743","KU746","KU747","KU748","KU791","KU749","KU751","KU753","KU755","KU758","KU759","KU761","KU762","KU765","KU766","KU768","KU771","KU777","KU778","KU781","KU783","KU831","KU832","KU833","KU834","KU837","KU844","KU845","KU846","KU848","KU849","KU850","KU851","KU853","KU857","KU858","KU859","KU886","KU887","KU889","KU890","KU892","KU893","KU895","KU785","KU905","KU908","KU092","KU915","KU918","KU921","KU922","KU924","KU925","KU927","KU931","KU934","KU935","KU936","KU941","KU946","KU976","KU977","KU980","KU981","KU989","KU992"),
       "Äidin ikä"=c("SSS","0-19","20-24","25-29","30-34","35-39","40-44","45-"),
       "Vuosi"=c("1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024"),
       "Sukupuoli"=c("SSS"),
       "Tiedot"=c("vm01"))

px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/synt/statfin_synt_pxt_12dq.px",
            query = pxweb_query_list)

births_5age <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

saveRDS(births_5age,file = "data/alue/births_raw.rds")

births_5age <- readRDS("data/alue/births_raw.rds")

numerators <- births_5age |>
  select(Kunta=Alue, 
         age_g='Äidin ikä', 
         year=Vuosi, 
         births='Elävänä syntyneet') |>
  filter(age_g!="Yhteensä")|>
  left_join(municipality_2_region) |>
  group_by(year, name, age_g) |>
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")  |>
  ungroup()

denominators <- vaesto |>
  filter(sex == "Naiset", !is.na(age), year>1988) |>
  select(reg_code, name, age, year, pop)   |> 
  mutate(
    age_g = case_when(
      age >= 10  &  age <= 19       ~ "0 - 19",
      age >= 20  & age <= 24       ~ "20 - 24",
      age >= 25  & age <= 29       ~ "25 - 29",
      age >= 30  & age <= 34       ~ "30 - 34",
      age >= 35  & age <= 39       ~ "35 - 39",
      age >= 40  & age <= 44       ~ "40 - 44",
      age >= 45  & age <= 54       ~ "45 -",
      TRUE ~ NA_character_
    )
  )  |>
  group_by(reg_code,year, name, age_g) |>
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")  |>
  arrange(name, age_g, year) |>
  group_by(name, age_g) |>
  mutate(
    population_prev = lag(pop),                       
    mid_year_population = (population_prev + pop) / 2 
  ) |>
  ungroup() |>
  filter(!is.na(mid_year_population))      

ASFR <- numerators |> 
  left_join(denominators) |>
  mutate(
    births = coalesce(births, 0L),                      
    asfr   = if_else(mid_year_population > 0,
                     births / mid_year_population, 0)   
  ) |>
  select(year, name,reg_code,age_g,asfr) 

PASFR <- ASFR |>
  mutate(
    age = case_when(
      age_g == "45 -" ~ "45-54",
      age_g == "0 - 19" ~ "10-19",
      TRUE ~ gsub(" ", "", age_g)          
    )
  ) |>
  separate(age, into = c("a0","a1"), sep = "-", convert = TRUE) |>
  uncount(a1 - a0 + 1, .remove = FALSE) |>
  group_by(year, name, age_g) |>
  mutate(age = a0 + row_number() - 1) |>
  ungroup() |>
  select(year, name,reg_code, age, asfr) |>
  group_by(name, year) |>
  mutate(
    TFR = sum(asfr, na.rm = TRUE), 
    PASFR = 100* asfr /TFR) |>
  ungroup()



ggplot(PASFR, aes(x = age, y = PASFR, group = year, colour = as.numeric(year))) +
  geom_line(alpha = 0.5, linewidth = 0.3) +
  facet_wrap(~ name) +
  scale_colour_viridis_c(option = "D", guide = "none") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

PASFR_wide <- PASFR |>
  mutate(year = as.character(year), 
         reg_code=as.numeric(reg_code)) |>  
  select(reg_code, name, age, year, PASFR) |>
  pivot_wider(
    names_from  = year,
    values_from = PASFR
  ) |>
  arrange(reg_code, age)

write.table(
  PASFR_wide,
  file = "data/alue/pasfr.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE,
  fileEncoding = "UTF-8"
)




births_5age <- readRDS("data/alue/births_raw.rds")

numerators <- births_5age |>
  select(Kunta=Alue, 
         age_g='Äidin ikä', 
         year=Vuosi, 
         births='Elävänä syntyneet') |>
  filter(age_g!="Yhteensä")|>
  left_join(municipality_2_region) |>
  group_by(year, name, age_g) |>
  summarise(births = sum(births, na.rm = TRUE), .groups = "drop")  |>
  ungroup()

numerators <- numerators %>%
  mutate(
    age_g = case_when(
      age_g == "45 -"   ~ "45-54",
      age_g == "0 - 19" ~ "10-19",
      TRUE ~ gsub(" ", "", age_g)
    ),
    lo = as.integer(str_extract(age_g, "^\\d+")),
    hi = as.integer(str_extract(age_g, "\\d+$")),
    AgeInt = hi - lo + 1L
  ) %>%
  arrange(name, year, lo) %>%
  group_by(name, year) %>%
  group_modify(~{
    df <- .x
    g <- DemoTools::graduate(
      Value  = df$births,
      Age    = df$lo,
      AgeInt = df$AgeInt,
      OAG = FALSE,
      method = "mono"
    )
    tibble(
      age    = names2age(g),
      births = g
    )
  }) %>%
  ungroup()

numerators

denominators <- vaesto |>
  filter(sex == "Naiset", !is.na(age), year>1988) |>
  select(reg_code, name, age, year, pop)   |> 
  group_by(reg_code,year, name, age) |>
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")  |>
  arrange(name, age, year) |>
  group_by(name, age) |>
  mutate(
    population_prev = lag(pop),                       
    mid_year_population = (population_prev + pop) / 2 
  ) |>
  ungroup() |>
  filter(!is.na(mid_year_population)) 

PASFR <- numerators |>
  left_join(denominators)   |>
  mutate(
    births = coalesce(births, 0L),                      
    asfr   = if_else(mid_year_population > 0,
                     births / mid_year_population, 0)   
  )  |>
  select(year, name,reg_code, age, asfr) |>
  group_by(name, year) |>
  mutate(
    TFR = sum(asfr, na.rm = TRUE), 
    PASFR = 100* asfr /TFR) |>
  ungroup()


ggplot(PASFR, aes(x = age, y = PASFR, group = year, colour = as.numeric(year))) +
  geom_line(alpha = 0.5, linewidth = 0.3) +
  facet_wrap(~ name) +
  scale_colour_viridis_c(option = "D", guide = "none") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

PASFR_wide <- PASFR |>
  mutate(year = as.character(year), 
         reg_code=as.numeric(reg_code)) |>  
  select(reg_code, name, age, year, PASFR) |>
  pivot_wider(
    names_from  = year,
    values_from = PASFR
  ) |>
  arrange(reg_code, age)

write.table(
  PASFR_wide,
  file = "data/alue/pasfr.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE,
  fileEncoding = "UTF-8"
)

#####################################
#8 Mx NOT WORKING!
#####################################
pxweb_query_list <- 
  list("Vuosi"=c("1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024"),
       "Alue"=c("SSS","KU020","KU005","KU009","KU010","KU016","KU018","KU019","KU035","KU043","KU046","KU047","KU049","KU050","KU051","KU052","KU060","KU061","KU062","KU065","KU069","KU071","KU072","KU074","KU075","KU076","KU077","KU078","KU079","KU081","KU082","KU086","KU111","KU090","KU091","KU097","KU098","KU102","KU103","KU105","KU106","KU108","KU109","KU139","KU140","KU142","KU143","KU145","KU146","KU153","KU148","KU149","KU151","KU152","KU165","KU167","KU169","KU170","KU171","KU172","KU176","KU177","KU178","KU179","KU181","KU182","KU186","KU202","KU204","KU205","KU208","KU211","KU213","KU214","KU216","KU217","KU218","KU224","KU226","KU230","KU231","KU232","KU233","KU235","KU236","KU239","KU240","KU320","KU241","KU322","KU244","KU245","KU249","KU250","KU256","KU257","KU260","KU261","KU263","KU265","KU271","KU272","KU273","KU275","KU276","KU280","KU284","KU285","KU286","KU287","KU288","KU290","KU291","KU295","KU297","KU300","KU301","KU304","KU305","KU312","KU316","KU317","KU318","KU398","KU399","KU400","KU407","KU402","KU403","KU405","KU408","KU410","KU416","KU417","KU418","KU420","KU421","KU422","KU423","KU425","KU426","KU444","KU430","KU433","KU434","KU435","KU436","KU438","KU440","KU441","KU475","KU478","KU480","KU481","KU483","KU484","KU489","KU491","KU494","KU495","KU498","KU499","KU500","KU503","KU504","KU505","KU508","KU507","KU529","KU531","KU535","KU536","KU538","KU541","KU543","KU545","KU560","KU561","KU562","KU563","KU564","KU309","KU576","KU577","KU578","KU445","KU580","KU581","KU599","KU583","KU854","KU584","KU592","KU593","KU595","KU598","KU601","KU604","KU607","KU608","KU609","KU611","KU638","KU614","KU615","KU616","KU619","KU620","KU623","KU624","KU625","KU626","KU630","KU631","KU635","KU636","KU678","KU710","KU680","KU681","KU683","KU684","KU686","KU687","KU689","KU691","KU694","KU697","KU698","KU700","KU702","KU704","KU707","KU729","KU732","KU734","KU736","KU790","KU738","KU739","KU740","KU742","KU743","KU746","KU747","KU748","KU791","KU749","KU751","KU753","KU755","KU758","KU759","KU761","KU762","KU765","KU766","KU768","KU771","KU777","KU778","KU781","KU783","KU831","KU832","KU833","KU834","KU837","KU844","KU845","KU846","KU848","KU849","KU850","KU851","KU853","KU857","KU858","KU859","KU886","KU887","KU889","KU890","KU892","KU893","KU895","KU785","KU905","KU908","KU092","KU915","KU918","KU921","KU922","KU924","KU925","KU927","KU931","KU934","KU935","KU936","KU941","KU946","KU976","KU977","KU980","KU981","KU989","KU992"),
       "Sukupuoli"=c("SSS","1","2"),
       "Ikä"=c("SSS","000","1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-"),
       "Tiedot"=c("vm11"))

px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/kuol/statfin_kuol_pxt_12ak.px",
            query = pxweb_query_list)

deaths_5 <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text") |>
  rename(
    year      = "Vuosi",
    Kunta      = "Alue",
    sex       = "Sukupuoli",
    age_g       = "Ikä",
    deaths    = "Kuolleet"
  ) |>
  filter(age_g!="Yhteensä", sex!="Yhteensä")

saveRDS(deaths_5,file = "data/alue/deaths_5.rds")



numerators <- readRDS("data/alue/deaths_5.rds") |> 
  left_join(municipality_2_region) |> 
  group_by(year, reg_code, name,age_g,sex) |>
  summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") |>
  ungroup()

denominators <- vaesto |>
  filter(sex!="Yhteensä", year>1988, age!="Yhteensä") |>
  mutate(
    age = as.numeric(age),  
    age_g = case_when(
      age < 1                      ~ "0",
      age >= 1   & age <= 4        ~ "1 - 4",
      age >= 5   & age <= 9        ~ "5 - 9",
      age >= 10  & age <= 14       ~ "10 - 14",
      age >= 15  & age <= 19       ~ "15 - 19",
      age >= 20  & age <= 24       ~ "20 - 24",
      age >= 25  & age <= 29       ~ "25 - 29",
      age >= 30  & age <= 34       ~ "30 - 34",
      age >= 35  & age <= 39       ~ "35 - 39",
      age >= 40  & age <= 44       ~ "40 - 44",
      age >= 45  & age <= 49       ~ "45 - 49",
      age >= 50  & age <= 54       ~ "50 - 54",
      age >= 55  & age <= 59       ~ "55 - 59",
      age >= 60  & age <= 64       ~ "60 - 64",
      age >= 65  & age <= 69       ~ "65 - 69",
      age >= 70  & age <= 74       ~ "70 - 74",
      age >= 75  & age <= 79       ~ "75 - 79",
      age >= 80  & age <= 84       ~ "80 - 84",
      age >= 85  & age <= 89       ~ "85 - 89",
      age >= 90  & age <= 94       ~ "90 - 94",
      age >= 95                    ~ "95 -",
      TRUE ~ NA_character_
    )
  )  |> group_by(year, reg_code, name,age_g,sex) |>
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")  |>
  arrange(name, age_g, year) |>
  group_by(name, age_g) |>
  mutate(
    population_prev = lag(pop),                       
    mid_year_population = (population_prev + pop) / 2
  ) |>
  ungroup() |>
  filter(!is.na(mid_year_population))  

mortality_rates <-numerators |> 
  left_join(denominators) |>
  mutate(
    deaths = coalesce(deaths, 0L),                     
    asmr   = if_else(mid_year_population > 0,
                     deaths / mid_year_population, 0),   
    asmr   = if_else(asmr > 1,
                     1, asmr)  ,   
    asmr   = if_else(asmr ==0,
                     0.00001, asmr)  
  ) 

asmr <- mortality_rates |>
  mutate(
    age = case_when(
      age_g == "95 -" ~ "95-100",
      TRUE ~ gsub(" ", "", age_g)         
    )
  ) |>
  separate(age, into = c("a0","a1"), sep = "-", convert = TRUE)|>
  mutate(    a1 = case_when(
    age_g == "0" ~ 0,
    TRUE ~ a1
  )) |>
  uncount(a1 - a0 + 1, .remove = FALSE) |>
  group_by(year, name, sex,age_g) |>
  mutate(age = a0 + row_number() - 1) |>
  ungroup() |>
  select(year, reg_code, sex,age, asmr)  


#####################################
#9 age and sex specific migration
#####################################

pxweb_query_list <- 
  list(
    "Alue"=c("SSS","KU020","KU005","KU009","KU010","KU016","KU018","KU019","KU035","KU043","KU046","KU047","KU049","KU050","KU051","KU052","KU060","KU061","KU062","KU065","KU069","KU071","KU072","KU074","KU075","KU076","KU077","KU078","KU079","KU081","KU082","KU086","KU111","KU090","KU091","KU097","KU098","KU102","KU103","KU105","KU106","KU108","KU109","KU139","KU140","KU142","KU143","KU145","KU146","KU153","KU148","KU149","KU151","KU152","KU165","KU167","KU169","KU170","KU171","KU172","KU176","KU177","KU178","KU179","KU181","KU182","KU186","KU202","KU204","KU205","KU208","KU211","KU213","KU214","KU216","KU217","KU218","KU224","KU226","KU230","KU231","KU232","KU233","KU235","KU236","KU239","KU240","KU320","KU241","KU322","KU244","KU245","KU249","KU250","KU256","KU257","KU260","KU261","KU263","KU265","KU271","KU272","KU273","KU275","KU276","KU280","KU284","KU285","KU286","KU287","KU288","KU290","KU291","KU295","KU297","KU300","KU301","KU304","KU305","KU312","KU316","KU317","KU318","KU398","KU399","KU400","KU407","KU402","KU403","KU405","KU408","KU410","KU416","KU417","KU418","KU420","KU421","KU422","KU423","KU425","KU426","KU444","KU430","KU433","KU434","KU435","KU436","KU438","KU440","KU441","KU475","KU478","KU480","KU481","KU483","KU484","KU489","KU491","KU494","KU495","KU498","KU499","KU500","KU503","KU504","KU505","KU508","KU507","KU529","KU531","KU535","KU536","KU538","KU541","KU543","KU545","KU560","KU561","KU562","KU563","KU564","KU309","KU576","KU577","KU578","KU445","KU580","KU581","KU599","KU583","KU854","KU584","KU592","KU593","KU595","KU598","KU601","KU604","KU607","KU608","KU609","KU611","KU638","KU614","KU615","KU616","KU619","KU620","KU623","KU624","KU625","KU626","KU630","KU631","KU635","KU636","KU678","KU710","KU680","KU681","KU683","KU684","KU686","KU687","KU689","KU691","KU694","KU697","KU698","KU700","KU702","KU704","KU707","KU729","KU732","KU734","KU736","KU790","KU738","KU739","KU740","KU742","KU743","KU746","KU747","KU748","KU791","KU749","KU751","KU753","KU755","KU758","KU759","KU761","KU762","KU765","KU766","KU768","KU771","KU777","KU778","KU781","KU783","KU831","KU832","KU833","KU834","KU837","KU844","KU845","KU846","KU848","KU849","KU850","KU851","KU853","KU857","KU858","KU859","KU886","KU887","KU889","KU890","KU892","KU893","KU895","KU785","KU905","KU908","KU092","KU915","KU918","KU921","KU922","KU924","KU925","KU927","KU931","KU934","KU935","KU936","KU941","KU946","KU976","KU977","KU980","KU981","KU989","KU992"),
    "Vuosi"=c("1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024"),
    "Sukupuoli"=c("SSS","1","2"),
    "Ikä"=c("SSS","0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-"),
    "Tiedot"=c("vm43_netto"))

px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/muutl/statfin_muutl_pxt_11a2.px",
            query = pxweb_query_list)

btw_municipality_mig <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

saveRDS(btw_municipality_mig,file = "data/alue/btw_municipality_mig.rds")

btw_municipality_mig <- readRDS("data/alue/btw_municipality_mig.rds")

pxweb_query_list <- 
  list(
    "Alue"=c("SSS","KU020","KU005","KU009","KU010","KU016","KU018","KU019","KU035","KU043","KU046","KU047","KU049","KU050","KU051","KU052","KU060","KU061","KU062","KU065","KU069","KU071","KU072","KU074","KU075","KU076","KU077","KU078","KU079","KU081","KU082","KU086","KU111","KU090","KU091","KU097","KU098","KU102","KU103","KU105","KU106","KU108","KU109","KU139","KU140","KU142","KU143","KU145","KU146","KU153","KU148","KU149","KU151","KU152","KU165","KU167","KU169","KU170","KU171","KU172","KU176","KU177","KU178","KU179","KU181","KU182","KU186","KU202","KU204","KU205","KU208","KU211","KU213","KU214","KU216","KU217","KU218","KU224","KU226","KU230","KU231","KU232","KU233","KU235","KU236","KU239","KU240","KU320","KU241","KU322","KU244","KU245","KU249","KU250","KU256","KU257","KU260","KU261","KU263","KU265","KU271","KU272","KU273","KU275","KU276","KU280","KU284","KU285","KU286","KU287","KU288","KU290","KU291","KU295","KU297","KU300","KU301","KU304","KU305","KU312","KU316","KU317","KU318","KU398","KU399","KU400","KU407","KU402","KU403","KU405","KU408","KU410","KU416","KU417","KU418","KU420","KU421","KU422","KU423","KU425","KU426","KU444","KU430","KU433","KU434","KU435","KU436","KU438","KU440","KU441","KU475","KU478","KU480","KU481","KU483","KU484","KU489","KU491","KU494","KU495","KU498","KU499","KU500","KU503","KU504","KU505","KU508","KU507","KU529","KU531","KU535","KU536","KU538","KU541","KU543","KU545","KU560","KU561","KU562","KU563","KU564","KU309","KU576","KU577","KU578","KU445","KU580","KU581","KU599","KU583","KU854","KU584","KU592","KU593","KU595","KU598","KU601","KU604","KU607","KU608","KU609","KU611","KU638","KU614","KU615","KU616","KU619","KU620","KU623","KU624","KU625","KU626","KU630","KU631","KU635","KU636","KU678","KU710","KU680","KU681","KU683","KU684","KU686","KU687","KU689","KU691","KU694","KU697","KU698","KU700","KU702","KU704","KU707","KU729","KU732","KU734","KU736","KU790","KU738","KU739","KU740","KU742","KU743","KU746","KU747","KU748","KU791","KU749","KU751","KU753","KU755","KU758","KU759","KU761","KU762","KU765","KU766","KU768","KU771","KU777","KU778","KU781","KU783","KU831","KU832","KU833","KU834","KU837","KU844","KU845","KU846","KU848","KU849","KU850","KU851","KU853","KU857","KU858","KU859","KU886","KU887","KU889","KU890","KU892","KU893","KU895","KU785","KU905","KU908","KU092","KU915","KU918","KU921","KU922","KU924","KU925","KU927","KU931","KU934","KU935","KU936","KU941","KU946","KU976","KU977","KU980","KU981","KU989","KU992"),
    "Vuosi"=c("1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024"),
    "Sukupuoli"=c("SSS","1","2"),
    "Ikä"=c("SSS","0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-"),
    "Tiedot"=c("vm4142"))

px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/muutl/statfin_muutl_pxt_11a7.px",
            query = pxweb_query_list)

internationa_mig <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

saveRDS(internationa_mig,file = "data/alue/internationa_mig.rds")

internationa_mig <- readRDS("data/alue/internationa_mig.rds")

mig_age <- btw_municipality_mig |>
  left_join(internationa_mig) |>
  rename(
    year = Vuosi,
    Kunta = Alue,
    sex  = Sukupuoli,
    age_g = Ikä,
    net_mig_internal = `Kuntien välinen nettomuutto`,
    net_mig_international = Nettomaahanmuutto
  ) |>
  mutate(
    net_mig_count = net_mig_internal + net_mig_international
  ) |>
  left_join(municipality_2_region) |>
  group_by(year, reg_code,name, age_g, sex) |>
  summarise(net_mig_count = sum(net_mig_count, na.rm = TRUE), .groups = "drop") 

mig_1y <- mig_age |>
  filter(sex!="Yhteensä", age_g!="Yhteensä")  |>
  mutate(
    age = case_when(
      age_g == "75 -" ~ "75-100",
      TRUE ~ gsub(" ", "", age_g)          
    )
  ) |>
  separate(age, into = c("a0","a1"), sep = "-", convert = TRUE) |>
  mutate(net_mig_count=net_mig_count/(a1 - a0 + 1))|>
  uncount(a1 - a0 + 1, .remove = FALSE) |>
  group_by(year, name, sex, age_g) |>
  mutate(age = a0 + row_number() - 1, 
         reg_code=as.numeric(reg_code)) |>
  ungroup() |>
  select(year, name, reg_code,sex, age, net_mig_count) 

migration <- mig_1y |>
  mutate(
    year = as.character(year),
    age = as.character(age)
  ) |>
  select(name, reg_code,  age,sex, year, net_mig_count) |>
  pivot_wider(
    names_from  = year,
    values_from = net_mig_count
  ) |>
  arrange(reg_code, sex, as.numeric(age))

migrationM <- migration |> 
  filter(sex=="Miehet") |> 
  select(!sex)

write.table(
  migrationM,
  file = "data/alue/migrationM.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE,
  fileEncoding = "UTF-8"
)

migrationF <- migration |> 
  filter(sex=="Naiset") |> 
  select(!sex)

write.table(
  migrationF,
  file = "data/alue/migrationF.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE,
  fileEncoding = "UTF-8"
)

