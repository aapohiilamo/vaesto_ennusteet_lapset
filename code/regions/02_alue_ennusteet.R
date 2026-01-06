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
  "data/meta_data/kunnat_ja_kuntapohjaiset_alueet_2025_suomeksi_ruotsiksi_englanniksi (1).xlsx", 
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
                                nr.traj = 10000)

tfr.preds <- get.regtfr.prediction(subnat.tfr.dir)
tfr.subnat <-tfr.preds[["246"]]
nat.tfr.pred <- get.tfr.prediction(nat.tfr.dir)
tfr.trajectories.plot(nat.tfr.pred, 246, half.child = FALSE, nr.traj = 50, pi = 95, ylim = c(0.5,2.5))

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
                              nr.traj = 10000)

e0.subnat <- get.rege0.prediction(subnat.e0.dir, 246)
e0.trajectories.plot(e0.subnat, "Uusimaa", both.sexes = TRUE)
#e0.trajectories.plot(e0.subnat, 246, both.sexes = FALSE)

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

mig.subnat <- mig.predict(sim.dir = subnat.mig.dir, nr.traj = 10000,
                          burnin = 1000, end.year = 2050, save.as.ascii = 10000)

mig.subnat <- get.mig.prediction(sim.dir = subnat.mig.dir)
mig.trajectories.plot(mig.subnat, "Uusimaa", nr.traj = 30)
#mig.trajectories.plot(mig.subnat, "Finland", nr.traj = 30)


#############################
# Population projections
###############################

location.file <- "data/alue/locations.txt"
popM0.file<- "data/alue/popM.txt"
popF0.file<- "data/alue/popF.txt"
mxM.file <- "data/alue/mxM.txt"
mxF.file<- "data/alue/mxF.txt"
pasfr.file <- "data/alue/pasfr.txt"

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
                                   mxM = mxM.file,
                                   mxF = mxF.file,
                                   mig = mig.file, 
                                   pasfr = pasfr.file,
                                   migtraj = mig.traj.file,
                                   tfr.sim.dir = subnat.tfr.results,
                                   e0F.sim.dir = subnat.e0.results, 
                                   e0M.sim.dir = "joint_"  ),
                                 mig.age.method = "rc",
                                 mig.is.rate = c(FALSE, TRUE),
                                 keep.vital.events = TRUE,
                                 pasfr.ignore.phase2 = TRUE
                                 )

pop.aggr <- pop.aggregate.subnat(pop.subnat, regions = 246,
                                locations = location.file)

pop.subnat <- get.pop.prediction(subnat.pop.dir)
pop.aggr <- get.pop.aggregation(sim.dir = subnat.pop.dir)

#############################
# results
###############################

# Aluekohtaiset ennusteet
combined <- map_dfr(setdiff(1:21, c(3, 20)), ~ {
  pop.trajectories.table(pop.subnat, expression =paste0("P", .x,"[0:17]"), pi = c(50, 80)) |> 
    as.data.frame() |> 
    rownames_to_column("year") |> 
    mutate(reg_code = .x)
}) |> 
  filter(year > 2010) |> 
  left_join(maakunta_koodit_export) |> 
  mutate(ennuste = "bayesPop")
write_xlsx(combined, "data/alueelliset_lapset.xlsx")


data2 <- combined |> 
  filter(year>2024) |> 
  select(name, year, median, `0.1`, `0.25`, `0.75`, `0.9`) |> 
  rename(
    vuosi = year,
    keskiennuste = median,
    alaraja80 = `0.1`,
    alaraja50 = `0.25`,
    ylaraja50 = `0.75`,
    ylaraja80 = `0.9`
  )|> 
mutate(across(where(is.numeric), ~ round(.x, 0)))
data_list <- split(data2 |>  select(-name), data2$name)
write_xlsx(data_list, path = "results/ennusteet_excel/maakuntakohtaiset_ennusteet_0_17.xlsx")


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
write_xlsx(data, "data/alueelliset_lapset_osuus.xlsx")

