getwd()
library(bayesPop)
library(wpp2024)
library(scales)
library(tidyverse)
library(writexl)
library(bayesMig)
library(readxl)

set.seed(123)

maakunta_koodit <- read_excel(
  "meta_data/kunnat_ja_kuntapohjaiset_alueet_2025_suomeksi_ruotsiksi_englanniksi (1).xlsx", 
  range = "A2:AJ310"
) |>
  select(
    reg_code = 'Maakunnan koodi',
    name = Maakunta
  ) |>
  distinct() |>
  bind_rows(
    tibble(reg_code = "100", name = "Koko maa")
  )

maakunta_koodit_export <- maakunta_koodit |>
  mutate(
    country_code = 100,
    reg_code = as.numeric(reg_code),
    location_type = case_when(
      reg_code == 100 ~ 0,
      TRUE ~ 4
    )  ) |>
  select(country_code, reg_code, name, location_type) |>
  arrange(-reg_code)


#tehdään ennusteet vaihteittan. 
#1. syntyvyys
#2. elinajanodote
#3. muuttoliike
###########
#fertility
###########
my.tfr.file <- "data/alue/tfr.txt"
head(read.delim(my.tfr.file, check.names = FALSE))
nat.tfr.dir <- "data/TFR1unc/sim20241101"

subnat.tfr.dir <- "results/alue/tfr"
tfr.preds <- tfr.predict.subnat(246, my.tfr.file = my.tfr.file,
                                sim.dir = nat.tfr.dir, output.dir = subnat.tfr.dir,
                                annual = TRUE, start.year = 2025, end.year = 2050,
                                nr.traj = 1000)

tfr.preds <- get.regtfr.prediction(subnat.tfr.dir)
tfr.subnat <-tfr.preds[["246"]]
nat.tfr.pred <- get.tfr.prediction(nat.tfr.dir)

###########
#elinajanodote
###########
my.e0M.file <- "data/alue/e0M.txt"
my.e0F.file <- "data/alue/e0F.txt"
head(read.delim(my.e0F.file, check.names = FALSE))
nat.e0.dir <- "data/sim20251101"
subnat.e0.dir <- "results/alue/e0"

e0.preds <- e0.predict.subnat(246, my.e0.file = my.e0F.file,
                              sim.dir = nat.e0.dir, output.dir = subnat.e0.dir,
                              annual = TRUE, start.year = 2025, end.year = 2050,
                              predict.jmale = TRUE, my.e0M.file = my.e0M.file,
                              nr.traj = 1000)

e0.subnat <- get.rege0.prediction(subnat.e0.dir, 246)
e0.trajectories.plot(e0.subnat, "Uusimaa", both.sexes = TRUE)
e0.trajectories.plot(e0.subnat, 100, both.sexes = FALSE)

#############
# Migration
#############
my.mig.file <- "data/alue/mig_rates.txt"
head(read.delim(my.mig.file, check.names = FALSE))

subnat.mig.dir <- "results/alue/mig"
mc<- run.mig.mcmc(nr.chains = 3, iter = 100000,
                  output.dir = subnat.mig.dir,
                  present.year = 2024, start.year = 1990,
                  my.mig.file = my.mig.file, annual = TRUE,
                  replace.output = TRUE, verbose.iter = 50000)

mig.subnat <- mig.predict(sim.dir = subnat.mig.dir, nr.traj = 1000,
                          burnin = 1000, end.year = 2050, save.as.ascii = 1000)

mig.subnat <- get.mig.prediction(sim.dir = subnat.mig.dir)
mig.trajectories.plot(mig.subnat, "Uusimaa", nr.traj = 30)
mig.trajectories.plot(mig.subnat, "Koko maa", nr.traj = 30)


#############################
# Population projections
###############################

location.file <- "data/alue/wafips.txt"
popM0.file<- "data/alue/popM.txt"
popF0.file<- "data/alue/popF.txt"
mxM.file <- "data/alue/mxM.txt"
mxF.file<- "data/alue/mxF.txt"
mig.file <- "data/alue/mig_counts.txt"
mig.traj.file<- file.path(subnat.mig.dir, 
                          "predictions/ascii_trajectories.csv")
subnat.tfr.results<- file.path(subnat.tfr.dir, "subnat/c246")
subnat.e0.results <- file.path(subnat.e0.dir,"subnat_ar1/c246")
subnat.pop.dir <- "results/alue/pop"
pop.subnat <- pop.predict.subnat(output.dir = subnat.pop.dir,
                                 locations = location.file, default.country = 246,
                                 verbose = TRUE, annual = TRUE, wpp.year = 2024,
                                 present.year = 2024, end.year = 2050,
                                 nr.traj = 1000, replace.output = TRUE,
                                 inputs = list(
                                   popM = popM0.file, popF = popF0.file,
                                   mxM = mxM.file,mxF = mxF.file,
                                   mig = mig.file, migtraj = mig.traj.file,
                                   tfr.sim.dir = subnat.tfr.results,
                                   e0F.sim.dir = subnat.e0.results, 
                                   e0M.sim.dir = "joint_"  ),
                                 mig.age.method = "rc",
                                 mig.is.rate = c(FALSE, TRUE),
                                 keep.vital.events = TRUE,
                                 pasfr.ignore.phase2 = TRUE
                                 )

pop.aggr <- pop.aggregate.subnat(pop.subnat, regions = 100,
                                locations = location.file)

pop.subnat <- get.pop.prediction(subnat.pop.dir)
pop.aggr <- get.pop.aggregation(sim.dir = subnat.pop.dir)

#############################
# results
###############################


# Aluekohtaiset ennusteet
data <- map_dfr(setdiff(1:21, c(3, 20)), ~ {
  pop.trajectories.table(pop.subnat, expression =paste0("P", .x,"[0:17]"), pi = c(50, 80)) |> 
    as.data.frame() |> 
    rownames_to_column("year") |> 
    mutate(reg_code = .x)
}) |> 
  filter(year > 2010) |> 
  left_join(maakunta_koodit_export) |> 
  mutate(ennuste = "bayesPop")

combined <- data |> 
  bind_rows(readRDS("data/alue/tk_ennuste.rds")) |> 
  filter(year < 2041, !is.na(name))

write_xlsx(combined, "alueelliset_lapset.xlsx")

# Kuva koko väestöstä
pop_all<-ggplot(combined, aes(as.numeric(year), median, color = ennuste, fill = ennuste)) +
  geom_ribbon(aes(ymin = `0.1`, ymax = `0.9`), alpha = 0.2) +
  geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`), alpha = 0.4) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2025, linetype = "dashed") +
  facet_wrap(~name,  scale = "free_y") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "t")) +
  theme_minimal()  +
  labs(
    y = "",
    x = "",
    title = ""
  )
ggsave("figs/all_pop.svg",pop_all, width = 12, height = 10)

data2 <- data %>%
  filter(year>2025 ) %>%
  select(name, year, median, `0.1`, `0.25`, `0.75`, `0.9`) %>%
  rename(
    vuosi = year,
    keskiennuste = median,
    alaraja80 = `0.1`,
    alaraja50 = `0.25`,
    ylaraja50 = `0.75`,
    ylaraja80 = `0.9`
  )%>%
mutate(across(where(is.numeric), ~ round(.x, 0)))
data_list <- split(data2 %>% select(-name), data2$name)
write_xlsx(data_list, path = "maakuntakohtaiset_ennusteet_0_17.xlsx")



# Aluekohtaiset ennusteet
data <- map_dfr(setdiff(1:21, c(3, 20)), ~ {
  pop.trajectories.table(pop.subnat, expression =paste0("P", .x,"[0:17]","/P", .x), pi = c(50, 80)) |> 
    as.data.frame() |> 
    rownames_to_column("year") |> 
    mutate(reg_code = .x)
}) |> 
  filter(year > 2010) |> 
  left_join(maakunta_koodit_export) |> 
  mutate(ennuste = "bayesPop")
write_xlsx(data, "alueelliset_lapset_osuus.xlsx")


# Kuva koko väestöstä
pop_all<-ggplot(data, aes(as.numeric(year), median, color = ennuste, fill = ennuste)) +
  geom_ribbon(aes(ymin = `0.1`, ymax = `0.9`), alpha = 0.2) +
  geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`), alpha = 0.4) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2025, linetype = "dashed") +
  facet_wrap(~name, nrow=1) +
  theme_minimal()  +
  labs(
    y = "",
    x = "",
    title = ""
  )
ggsave("figs/osuus.svg",pop_all, width = 16, height = 4)
