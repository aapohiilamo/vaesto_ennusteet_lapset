###
library(eurostat)
library(readr)
library(countrycode)
library(bayesPop)
library(bayesMig)
library(bayesLife)
library(tidyverse)
library(writexl)
library(pxweb)
###populations
# Load the file
load("data/TFR1unc/sim20241101/traj_country246.rda")
trajectories[1, ] <- 1.25
save(trajectories, file = "data/TFR1unc/sim20241101/predictions/traj_country246.rda")

my.mig.file <- "data/my_net_migration_rates.txt"
head(read.delim(my.mig.file, check.names = FALSE), n=20)

mig.dir <- "data/mig"
mc<- run.mig.mcmc(nr.chains = 4, 
                  iter = 100000,
                  output.dir = mig.dir,
                  my.mig.file = my.mig.file,
                  present.year = 2025,
                  start.year = 2000,
                  wpp.year = 2024,
                  annual = TRUE,
                  parallel= TRUE,
                  replace.output = TRUE)

mig.predictions <- mig.predict(sim.dir = mig.dir, 
                               nr.traj = 10000, 
                               burnin = 1000, 
                               end.year = 2050, 
                               save.as.ascii = 'all', 
                               replace.output = TRUE)

mig.trajectories.plot(mig.predictions, "Finland", pi = 80, ylim = c(-0.02, 0.02), nr.traj = 0)

########
#national forecast
########
sim.dir <- file.path(find.package("bayesPop"), "ex-data", "Pop")
nat.tfr.dir <- "data/TFR1unc/sim20241101"
nat.e0.dir <- "data/sim20251101"
mig.file <- "data/mig_counts_Finland.txt"
mig.traj.file <- file.path(mig.dir, "predictions/ascii_trajectories.csv")
popM0.file<- "data/popM_koko.txt"
popF0.file<- "data/popF_koko.txt"

nat.e0.pred <- get.e0.prediction(nat.e0.dir)
e0.trajectories.table(nat.e0.pred, 246, pi = 80,  both.sexes = TRUE)

pred<- pop.predict(countries=246,default.country = 246,wpp.year = 2024, present.year = 2024,end.year = 2050,
                   output.dir = "results/finland",annual = TRUE,nr.traj=1000,
                   inputs = list( popM = popM0.file, popF = popF0.file,
                                  e0F.sim.dir = nat.e0.dir, 
                                  e0M.sim.dir = "joint_",
                                  tfr.sim.dir = nat.tfr.dir,                   
                                  mig = mig.file, migtraj = mig.traj.file),
                   mig.age.method = "rc",
                   mig.is.rate = c(FALSE, TRUE),
                   pasfr.ignore.phase2 = FALSE,
                   replace.output = TRUE, keep.vital.events = TRUE)  
pred <- get.pop.prediction("results/finland")
pop.trajectories.table(pred, country = "Finland", pi = 80)


observed <- 
  get.pop.ex("G246", pred, as.dt = TRUE, observed = TRUE) |>   mutate(metric = "mig") |> 
  bind_rows(get.pop.ex("P246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "pop") ) |> 
  bind_rows(get.pop.ex("P246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child") ) |> 
  bind_rows(get.pop.ex("P246[0:12]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "preteen") ) |> 
  bind_rows(get.pop.ex("P246[13:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "teen") ) |> 
  bind_rows(get.pop.ex("P246[0]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "0") ) |> 
  bind_rows(get.pop.ex("P246[3]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "3") ) |> 
  bind_rows(get.pop.ex("P246[6]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "6") ) |> 
  bind_rows(get.pop.ex("P246[12]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "12") ) |> 
  bind_rows(get.pop.ex("P246[15]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "15") ) |> 
  bind_rows(get.pop.ex("P246[17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "seven olds") ) |> 
  bind_rows(get.pop.ex("P246[0:17]/P246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child_to_pop")) |> 
  bind_rows(get.pop.ex("G246[0:19]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child_mig")  ) |> 
  bind_rows(get.pop.ex("F246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "tfr") ) |> 
  bind_rows(get.pop.ex("B246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "births")) |> 
  bind_rows(get.pop.ex("Q246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "Death before 18")) |>
  bind_rows(get.pop.ex("D246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "Deaths")) |>
  mutate(trajectory=0) |>
  rename(median=indicator)

trajectories <- 
  get.pop.ex("G246", pred, as.dt = TRUE) |> mutate(metric = "mig") |>
  bind_rows(get.pop.ex("P246", pred, as.dt = TRUE) |> mutate(metric = "pop")) |>
  bind_rows( get.pop.ex("P246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "child")) |>  
  bind_rows(get.pop.ex("P246[0:12]", pred, as.dt = TRUE) |> mutate(metric = "preteen") ) |> 
  bind_rows(get.pop.ex("P246[13:17]", pred, as.dt = TRUE) |> mutate(metric = "teen") ) |> 
  bind_rows(get.pop.ex("P246[7]", pred, as.dt = TRUE) |> mutate(metric = "seven olds") ) |> 
  bind_rows(get.pop.ex("P246[0]", pred, as.dt = TRUE) |> mutate(metric = "0") ) |> 
  bind_rows(get.pop.ex("P246[3]", pred, as.dt = TRUE) |> mutate(metric = "3") ) |> 
  bind_rows(get.pop.ex("P246[6]", pred, as.dt = TRUE) |> mutate(metric = "6") ) |> 
  bind_rows(get.pop.ex("P246[12]", pred, as.dt = TRUE) |> mutate(metric = "12") ) |> 
  bind_rows(get.pop.ex("P246[15]", pred, as.dt = TRUE) |> mutate(metric = "15") ) |> 
  bind_rows(get.pop.ex("P246[0:17]/P246", pred, as.dt = TRUE) |> mutate(metric = "child_to_pop")) |>
  bind_rows(get.pop.ex("G246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "child_mig")) |>
  bind_rows(get.pop.ex("F246", pred, as.dt = TRUE) |> mutate(metric = "tfr")) |>
  bind_rows(get.pop.ex("B246", pred, as.dt = TRUE) |> mutate(metric = "births") ) |>
  bind_rows(get.pop.ex("Q246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "Death before 18")) |>
  bind_rows(get.pop.ex("D246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "Deaths")) |>
  mutate(
    indicator = case_when(
      metric %in% c("Death before 18", "child_to_pop", "tfr") ~ indicator,
      TRUE ~ round(indicator)
    )
  )
pop.trajectories.plot(pred, 246, expression="G246", pi= 80,nr.traj = 0)

intervals <- trajectories |>
  group_by(year, metric) |>
  summarize(
    lower_80 = quantile(indicator, 0.10),
    upper_80 = quantile(indicator, 0.90),
    lower_50 = quantile(indicator, 0.25),
    upper_50 = quantile(indicator, 0.75),
    median = median(indicator),
    .groups = "drop"
  ) |>
  bind_rows(observed)|>
  arrange(year,metric)

write_xlsx(intervals, "data/ennusteet.xlsx")
write_xlsx(trajectories, "data/simulaatiot.xlsx")

data_for_trajectories <- get.pop.exba("S246_M{}", pred, as.dt=TRUE) |> 
  rename(mortality_m=indicator) |> 
  left_join(get.pop.exba("S246_F{}", pred, as.dt=TRUE)) |> 
  rename(mortality_f=indicator)
saveRDS(data_for_trajectories, "data/input_perherakenne_kuolleisuus.rds")

data_for_fertility <- get.pop.exba("F246{}", pred, as.dt=TRUE) |> rename(fertility_f=indicator)
saveRDS(data_for_fertility, "data/input_perherakenne_syntyvyys.rds")



####sensitivity - no netmigration
mig.traj.file <- file.path(mig.dir, "predictions/ascii_trajectories.csv")
dt <- fread(mig.traj.file, encoding = "UTF-8")
dt[, Mig := 0]
fwrite(dt, file.path(mig.dir, "predictions/ascii_trajectories_.csv"))
sim.dir <- file.path(find.package("bayesPop"), "ex-data", "Pop")
nat.tfr.dir <- "data/TFR1unc/sim20241101"
nat.e0.dir <- "data/sim20251101"
mig.file <- "data/mig_counts_Finland.txt"
mig.traj.file <- file.path(mig.dir, "predictions/ascii_trajectories_.csv")
popM0.file<- "data/popM_koko.txt"
popF0.file<- "data/popF_koko.txt"

pred<- pop.predict(countries=246,default.country = 246,wpp.year = 2024, present.year = 2024,end.year = 2050,
                   output.dir = "results/finland",annual = TRUE,nr.traj=1000,
                   inputs = list( popM = popM0.file, popF = popF0.file,
                                  e0F.sim.dir = nat.e0.dir, 
                                  e0M.sim.dir = "joint_",
                                  tfr.sim.dir = nat.tfr.dir,                   
                                  mig = mig.file, migtraj = mig.traj.file),
                   mig.age.method = "rc",
                   mig.is.rate = c(FALSE, TRUE),
                   pasfr.ignore.phase2 = FALSE,
                   replace.output = TRUE, keep.vital.events = TRUE)  

observed <- 
  get.pop.ex("G246", pred, as.dt = TRUE, observed = TRUE) |>   mutate(metric = "mig") |> 
  bind_rows(get.pop.ex("P246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "pop") ) |> 
  bind_rows(get.pop.ex("P246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child") ) |> 
  bind_rows(get.pop.ex("P246[0:12]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "preteen") ) |> 
  bind_rows(get.pop.ex("P246[13:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "teen") ) |> 
  bind_rows(get.pop.ex("P246[17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "seven olds") ) |> 
  bind_rows(get.pop.ex("P246[0:17]/P246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child_to_pop")) |> 
  bind_rows(get.pop.ex("G246[0:19]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child_mig")  ) |> 
  bind_rows(get.pop.ex("F246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "tfr") ) |> 
  bind_rows(get.pop.ex("B246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "births")) |> 
  bind_rows(get.pop.ex("Q246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "Death before 18")) |>
  bind_rows(get.pop.ex("D246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "Deaths")) |>
  mutate(trajectory=0) |>
  rename(median=indicator)

trajectories <- 
  get.pop.ex("G246", pred, as.dt = TRUE) |> mutate(metric = "mig") |>
  bind_rows(get.pop.ex("P246", pred, as.dt = TRUE) |> mutate(metric = "pop")) |>
  bind_rows( get.pop.ex("P246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "child")) |>  
  bind_rows(get.pop.ex("P246[0:12]", pred, as.dt = TRUE) |> mutate(metric = "preteen") ) |> 
  bind_rows(get.pop.ex("P246[13:17]", pred, as.dt = TRUE) |> mutate(metric = "teen") ) |> 
  bind_rows(get.pop.ex("P246[17]", pred, as.dt = TRUE) |> mutate(metric = "seven olds") ) |> 
  bind_rows(get.pop.ex("P246[0:17]/P246", pred, as.dt = TRUE) |> mutate(metric = "child_to_pop")) |>
  bind_rows(get.pop.ex("G246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "child_mig")) |>
  bind_rows(get.pop.ex("F246", pred, as.dt = TRUE) |> mutate(metric = "tfr")) |>
  bind_rows(get.pop.ex("B246", pred, as.dt = TRUE) |> mutate(metric = "births") ) |>
  bind_rows(get.pop.ex("Q246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "Death before 18")) |>
  bind_rows(get.pop.ex("D246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "Deaths")) |>
  mutate(
    indicator = case_when(
      metric %in% c("Death before 18", "child_to_pop", "tfr") ~ indicator,
      TRUE ~ round(indicator)
    )
  )
pop.trajectories.plot(pred, 246, expression="G246", pi= 80,nr.traj = 0)

intervals <- trajectories |>
  group_by(year, metric) |>
  summarize(
    lower_80 = quantile(indicator, 0.10),
    upper_80 = quantile(indicator, 0.90),
    lower_50 = quantile(indicator, 0.25),
    upper_50 = quantile(indicator, 0.75),
    median = median(indicator),
    .groups = "drop"
  ) |>
  bind_rows(observed)|>
  arrange(year,metric)

write_xlsx(intervals, "data/ennusteet_no_migration.xlsx")
write_xlsx(trajectories, "data/simulaatiot_no_migraion.xlsx")



#############################################high immigration
####sensitivity - no netmigration
mig.traj.file <- file.path(mig.dir, "predictions/ascii_trajectories.csv")
dt <- fread(mig.traj.file, encoding = "UTF-8")
dt[, Mig := 0.0105]
fwrite(dt, file.path(mig.dir, "predictions/ascii_trajectories_.csv"))
sim.dir <- file.path(find.package("bayesPop"), "ex-data", "Pop")
nat.tfr.dir <- "data/TFR1unc/sim20241101"
nat.e0.dir <- "data/sim20251101"
mig.file <- "data/mig_counts_Finland.txt"
mig.traj.file <- file.path(mig.dir, "predictions/ascii_trajectories_.csv")
popM0.file<- "data/popM_koko.txt"
popF0.file<- "data/popF_koko.txt"

pred<- pop.predict(countries=246,default.country = 246,wpp.year = 2024, present.year = 2024,end.year = 2050,
                   output.dir = "results/finland",annual = TRUE,nr.traj=1000,
                   inputs = list( popM = popM0.file, popF = popF0.file,
                                  e0F.sim.dir = nat.e0.dir, 
                                  e0M.sim.dir = "joint_",
                                  tfr.sim.dir = nat.tfr.dir,                   
                                  mig = mig.file, migtraj = mig.traj.file),
                   mig.age.method = "rc",
                   mig.is.rate = c(FALSE, TRUE),
                   pasfr.ignore.phase2 = FALSE,
                   replace.output = TRUE, keep.vital.events = TRUE)  

observed <- 
  get.pop.ex("G246", pred, as.dt = TRUE, observed = TRUE) |>   mutate(metric = "mig") |> 
  bind_rows(get.pop.ex("P246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "pop") ) |> 
  bind_rows(get.pop.ex("P246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child") ) |> 
  bind_rows(get.pop.ex("P246[0:12]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "preteen") ) |> 
  bind_rows(get.pop.ex("P246[13:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "teen") ) |> 
  bind_rows(get.pop.ex("P246[17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "seven olds") ) |> 
  bind_rows(get.pop.ex("P246[0:17]/P246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child_to_pop")) |> 
  bind_rows(get.pop.ex("G246[0:19]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child_mig")  ) |> 
  bind_rows(get.pop.ex("F246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "tfr") ) |> 
  bind_rows(get.pop.ex("B246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "births")) |> 
  bind_rows(get.pop.ex("Q246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "Death before 18")) |>
  bind_rows(get.pop.ex("D246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "Deaths")) |>
  mutate(trajectory=0) |>
  rename(median=indicator)

trajectories <- 
  get.pop.ex("G246", pred, as.dt = TRUE) |> mutate(metric = "mig") |>
  bind_rows(get.pop.ex("P246", pred, as.dt = TRUE) |> mutate(metric = "pop")) |>
  bind_rows( get.pop.ex("P246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "child")) |>  
  bind_rows(get.pop.ex("P246[0:12]", pred, as.dt = TRUE) |> mutate(metric = "preteen") ) |> 
  bind_rows(get.pop.ex("P246[13:17]", pred, as.dt = TRUE) |> mutate(metric = "teen") ) |> 
  bind_rows(get.pop.ex("P246[17]", pred, as.dt = TRUE) |> mutate(metric = "seven olds") ) |> 
  bind_rows(get.pop.ex("P246[0:17]/P246", pred, as.dt = TRUE) |> mutate(metric = "child_to_pop")) |>
  bind_rows(get.pop.ex("G246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "child_mig")) |>
  bind_rows(get.pop.ex("F246", pred, as.dt = TRUE) |> mutate(metric = "tfr")) |>
  bind_rows(get.pop.ex("B246", pred, as.dt = TRUE) |> mutate(metric = "births") ) |>
  bind_rows(get.pop.ex("Q246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "Death before 18")) |>
  bind_rows(get.pop.ex("D246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "Deaths")) |>
  mutate(
  indicator = case_when(
    metric %in% c("Death before 18", "child_to_pop", "tfr") ~ indicator,
    TRUE ~ round(indicator)
    )
  )

pop.trajectories.plot(pred, 246, expression="G246", pi= 80,nr.traj = 0)

intervals <- trajectories |>
  group_by(year, metric) |>
  summarize(
    lower_80 = quantile(indicator, 0.10),
    upper_80 = quantile(indicator, 0.90),
    lower_50 = quantile(indicator, 0.25),
    upper_50 = quantile(indicator, 0.75),
    median = median(indicator),
    .groups = "drop"
  ) |>
  bind_rows(observed) |>
  arrange(year,metric)

write_xlsx(intervals, "data/ennusteet_high_migration.xlsx")
write_xlsx(trajectories, "data/simulaatiot_high_migraion.xlsx")


############################################low fertility

load("data/TFR1unc/sim20241101/predictions/traj_country246.rda")
# Check that 'trajectories' exists and is a matrix
stopifnot(exists("trajectories"), is.matrix(trajectories))

# Replace all indicators with 1
trajectories[,] <- 1

# Save back to the same file (overwrite)
save(trajectories, file = "data/TFR1unc/sim20241101/predictions/traj_country246.rda")
mig.dir <- "data/mig"


sim.dir <- file.path(find.package("bayesPop"), "ex-data", "Pop")
nat.tfr.dir <- "data/TFR1unc/sim20241101"
nat.e0.dir <- "data/sim20251101"
mig.file <- "data/mig_counts_Finland.txt"
mig.traj.file <- file.path(mig.dir, "predictions/ascii_trajectories.csv")
popM0.file<- "data/popM_koko.txt"
popF0.file<- "data/popF_koko.txt"

pred<- pop.predict(countries=246,default.country = 246,wpp.year = 2024, present.year = 2024,end.year = 2050,
                   output.dir = "results/finland",annual = TRUE,nr.traj=1000,
                   inputs = list( popM = popM0.file, popF = popF0.file,
                                  e0F.sim.dir = nat.e0.dir, 
                                  e0M.sim.dir = "joint_",
                                  tfr.sim.dir = nat.tfr.dir,                   
                                  mig = mig.file, migtraj = mig.traj.file),
                   mig.age.method = "rc",
                   mig.is.rate = c(FALSE, TRUE),
                   pasfr.ignore.phase2 = FALSE,
                   replace.output = TRUE, keep.vital.events = TRUE)  

observed <- 
  get.pop.ex("G246", pred, as.dt = TRUE, observed = TRUE) |>   mutate(metric = "mig") |> 
  bind_rows(get.pop.ex("P246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "pop") ) |> 
  bind_rows(get.pop.ex("P246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child") ) |> 
  bind_rows(get.pop.ex("P246[0:12]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "preteen") ) |> 
  bind_rows(get.pop.ex("P246[13:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "teen") ) |> 
  bind_rows(get.pop.ex("P246[17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "seven olds") ) |> 
  bind_rows(get.pop.ex("P246[0:17]/P246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child_to_pop")) |> 
  bind_rows(get.pop.ex("G246[0:19]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child_mig")  ) |> 
  bind_rows(get.pop.ex("F246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "tfr") ) |> 
  bind_rows(get.pop.ex("B246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "births")) |> 
  bind_rows(get.pop.ex("Q246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "Death before 18")) |>
  bind_rows(get.pop.ex("D246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "Deaths")) |>
  mutate(trajectory=0) |>
  rename(median=indicator)

trajectories <- 
  get.pop.ex("G246", pred, as.dt = TRUE) |> mutate(metric = "mig") |>
  bind_rows(get.pop.ex("P246", pred, as.dt = TRUE) |> mutate(metric = "pop")) |>
  bind_rows( get.pop.ex("P246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "child")) |>  
  bind_rows(get.pop.ex("P246[0:12]", pred, as.dt = TRUE) |> mutate(metric = "preteen") ) |> 
  bind_rows(get.pop.ex("P246[13:17]", pred, as.dt = TRUE) |> mutate(metric = "teen") ) |> 
  bind_rows(get.pop.ex("P246[17]", pred, as.dt = TRUE) |> mutate(metric = "seven olds") ) |> 
  bind_rows(get.pop.ex("P246[0:17]/P246", pred, as.dt = TRUE) |> mutate(metric = "child_to_pop")) |>
  bind_rows(get.pop.ex("G246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "child_mig")) |>
  bind_rows(get.pop.ex("F246", pred, as.dt = TRUE) |> mutate(metric = "tfr")) |>
  bind_rows(get.pop.ex("B246", pred, as.dt = TRUE) |> mutate(metric = "births") ) |>
  bind_rows(get.pop.ex("Q246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "Death before 18")) |>
  bind_rows(get.pop.ex("D246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "Deaths")) |>
  mutate(
    indicator = case_when(
      metric %in% c("Death before 18", "child_to_pop", "tfr") ~ indicator,
      TRUE ~ round(indicator)
    )
  )
pop.trajectories.plot(pred, 246, expression="G246", pi= 80,nr.traj = 0)

intervals <- trajectories |>
  group_by(year, metric) |>
  summarize(
    lower_80 = quantile(indicator, 0.10),
    upper_80 = quantile(indicator, 0.90),
    lower_50 = quantile(indicator, 0.25),
    upper_50 = quantile(indicator, 0.75),
    median = median(indicator),
    .groups = "drop"
  ) |>
  bind_rows(observed)|>
  arrange(year,metric)

write_xlsx(intervals, "data/ennusteet_low_tfr.xlsx")
write_xlsx(trajectories, "data/simulaatiot_low_tfr.xlsx")



#####################high
load("data/TFR1unc/sim20241101/predictions/traj_country246.rda")

# Check that 'trajectories' exists and is a matrix
stopifnot(exists("trajectories"), is.matrix(trajectories))

# Replace all indicators with 1
trajectories[,] <- 1.87

# Save back to the same file (overwrite)
save(trajectories, file = "data/TFR1unc/sim20241101/predictions/traj_country246.rda")


sim.dir <- file.path(find.package("bayesPop"), "ex-data", "Pop")
nat.tfr.dir <- "data/TFR1unc/sim20241101"
nat.e0.dir <- "data/sim20251101"
mig.file <- "data/mig_counts_Finland.txt"
mig.traj.file <- file.path(mig.dir, "predictions/ascii_trajectories.csv")
popM0.file<- "data/popM_koko.txt"
popF0.file<- "data/popF_koko.txt"

pred<- pop.predict(countries=246,default.country = 246,wpp.year = 2024, present.year = 2024,end.year = 2050,
                   output.dir = "results/finland",annual = TRUE,nr.traj=1000,
                   inputs = list( popM = popM0.file, popF = popF0.file,
                                  e0F.sim.dir = nat.e0.dir, 
                                  e0M.sim.dir = "joint_",
                                  tfr.sim.dir = nat.tfr.dir,                   
                                  mig = mig.file, migtraj = mig.traj.file),
                   mig.age.method = "rc",
                   mig.is.rate = c(FALSE, TRUE),
                   pasfr.ignore.phase2 = FALSE,
                   replace.output = TRUE, keep.vital.events = TRUE)  

observed <- 
  get.pop.ex("G246", pred, as.dt = TRUE, observed = TRUE) |>   mutate(metric = "mig") |> 
  bind_rows(get.pop.ex("P246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "pop") ) |> 
  bind_rows(get.pop.ex("P246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child") ) |> 
  bind_rows(get.pop.ex("P246[0:12]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "preteen") ) |> 
  bind_rows(get.pop.ex("P246[13:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "teen") ) |> 
  bind_rows(get.pop.ex("P246[17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "seven olds") ) |> 
  bind_rows(get.pop.ex("P246[0:17]/P246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child_to_pop")) |> 
  bind_rows(get.pop.ex("G246[0:19]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "child_mig")  ) |> 
  bind_rows(get.pop.ex("F246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "tfr") ) |> 
  bind_rows(get.pop.ex("B246", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "births")) |> 
  bind_rows(get.pop.ex("Q246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "Death before 18")) |>
  bind_rows(get.pop.ex("D246[0:17]", pred, as.dt = TRUE, observed = TRUE) |> mutate(metric = "Deaths")) |>
  mutate(trajectory=0) |>
  rename(median=indicator)

trajectories <- 
  get.pop.ex("G246", pred, as.dt = TRUE) |> mutate(metric = "mig") |>
  bind_rows(get.pop.ex("P246", pred, as.dt = TRUE) |> mutate(metric = "pop")) |>
  bind_rows( get.pop.ex("P246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "child")) |>  
  bind_rows(get.pop.ex("P246[0:12]", pred, as.dt = TRUE) |> mutate(metric = "preteen") ) |> 
  bind_rows(get.pop.ex("P246[13:17]", pred, as.dt = TRUE) |> mutate(metric = "teen") ) |> 
  bind_rows(get.pop.ex("P246[17]", pred, as.dt = TRUE) |> mutate(metric = "seven olds") ) |> 
  bind_rows(get.pop.ex("P246[0:17]/P246", pred, as.dt = TRUE) |> mutate(metric = "child_to_pop")) |>
  bind_rows(get.pop.ex("G246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "child_mig")) |>
  bind_rows(get.pop.ex("F246", pred, as.dt = TRUE) |> mutate(metric = "tfr")) |>
  bind_rows(get.pop.ex("B246", pred, as.dt = TRUE) |> mutate(metric = "births") ) |>
  bind_rows(get.pop.ex("Q246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "Death before 18")) |>
  bind_rows(get.pop.ex("D246[0:17]", pred, as.dt = TRUE) |> mutate(metric = "Deaths")) |>
  mutate(
    indicator = case_when(
      metric %in% c("Death before 18", "child_to_pop", "tfr") ~ indicator,
      TRUE ~ round(indicator)
    )
  )
pop.trajectories.plot(pred, 246, expression="G246", pi= 80,nr.traj = 0)

intervals <- trajectories |>
  group_by(year, metric) |>
  summarize(
    lower_80 = quantile(indicator, 0.10),
    upper_80 = quantile(indicator, 0.90),
    lower_50 = quantile(indicator, 0.25),
    upper_50 = quantile(indicator, 0.75),
    median = median(indicator),
    .groups = "drop"
  ) |>
  bind_rows(observed)|>
  arrange(year,metric)

write_xlsx(intervals, "data/ennusteet_high_tfr.xlsx")
write_xlsx(trajectories, "data/simulaatiot_high_tfr.xlsx")


#####päämalli

load("data/TFR1unc/sim20241101/traj_country246.rda")
trajectories[1, ] <- 1.25
save(trajectories, file = "data/TFR1unc/sim20241101/predictions/traj_country246.rda")

sim.dir <- file.path(find.package("bayesPop"), "ex-data", "Pop")
nat.tfr.dir <- "data/TFR1unc/sim20241101"
nat.e0.dir <- "data/sim20251101"
mig.file <- "data/mig_counts_Finland.txt"
mig.traj.file <- file.path(mig.dir, "predictions/ascii_trajectories.csv")
popM0.file<- "data/popM_koko.txt"
popF0.file<- "data/popF_koko.txt"

pred<- pop.predict(countries=246,default.country = 246,wpp.year = 2024, present.year = 2024,end.year = 2050,
                   output.dir = "results/finland",annual = TRUE,nr.traj=1000,
                   inputs = list( popM = popM0.file, popF = popF0.file,
                                  e0F.sim.dir = nat.e0.dir, 
                                  e0M.sim.dir = "joint_",
                                  tfr.sim.dir = nat.tfr.dir,                   
                                  mig = mig.file, migtraj = mig.traj.file),
                   mig.age.method = "rc",
                   mig.is.rate = c(FALSE, TRUE),
                   pasfr.ignore.phase2 = FALSE,
                   replace.output = TRUE, keep.vital.events = TRUE)  
