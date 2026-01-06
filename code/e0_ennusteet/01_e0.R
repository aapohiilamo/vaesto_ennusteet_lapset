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
  time_format = "date",  
  type = "code"          
)

df_birth_total <- df |>
  filter(
    unit == "YR",
    age %in% c("Y0", "Y_LT1"),
    sex == "F"                    
  )

df_birth_total <- df_birth_total |>
  mutate(year = year(TIME_PERIOD))

df_birth_total <- df_birth_total |>
  mutate(
    country_code = countrycode(geo, origin = "eurostat", destination = "un",
                               custom_match = c("XK" = 383)),                # Kosovo
    name         = countrycode(geo, origin = "eurostat", destination = "country.name",
                               custom_match = c("XK" = "Kosovo"))
  ) |>
  filter(!is.na(country_code))

e0_template_single_years <- df_birth_total |>
  select(name, country_code, year, values) |>
  arrange(country_code, year) |>
  filter(year==2024) |>
  distinct(name, country_code, year, .keep_all = TRUE) |>
  pivot_wider(
    names_from  = year,
    values_from = values,
    names_sort  = TRUE
  ) |>
  arrange(name, country_code) |>
  filter(country_code!=383, country_code!=674) 

data(e0F1)
e0_template_single_years<-e0F1 |> left_join(e0_template_single_years) |>
  filter(country_code<900)

write.table(
  e0_template_single_years,
  file = "data/my_e0_single_years.txt",
  sep = "\t",              
  quote = FALSE,           
  row.names = FALSE,       
  na = ""                  
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

df_birth_total <- df |>
  filter(
    unit == "YR",
    age %in% c("Y0", "Y_LT1"),
    sex == "M"                    
  )

df_birth_total <- df_birth_total |>
  mutate(year = year(TIME_PERIOD))

df_birth_total <- df_birth_total |>
  mutate(
    country_code = countrycode(geo, origin = "eurostat", destination = "un",
                               custom_match = c("XK" = 383)),                # Kosovo
    name         = countrycode(geo, origin = "eurostat", destination = "country.name",
                               custom_match = c("XK" = "Kosovo"))
  ) |>
  filter(!is.na(country_code))

e0_template_single_years <- df_birth_total |>
  select(name, country_code, year, values) |>
  arrange(country_code, year) |>
  filter(year==2024) |>
  distinct(name, country_code, year, .keep_all = TRUE) |>
  pivot_wider(
    names_from  = year,
    values_from = values,
    names_sort  = TRUE
  ) |>
  arrange(name, country_code) |>
  filter(country_code!=383, country_code!=674)

data(e0M1)
e0_template_single_years_men<-e0M1 |> left_join(e0_template_single_years) |>
  filter(country_code<900)

write.table(
  e0_template_single_years_men,
  file = "data/my_eM0_single_years.txt",
  sep = "\t",              
  quote = FALSE,           
  row.names = FALSE,       
  na = ""                  
)


######################

library(bayesLife)

e0dir <- "sim20251101"

seed <- 20251101
my_file <- "data/my_e0_single_years.txt"

t1 <- Sys.time()

# simulate MCMC using female data
m <- run.e0.mcmc(iter = 210000, thin = 50,
                 my.e0.file =my_file,
                 nr.chains = 3, output.dir = e0dir, replace.output = TRUE,
                 start.year = 1873, present.year = 2024, wpp.year = 2024, 
                 annual = TRUE, seed = seed,use.wpp.data = FALSE,
                 parallel = TRUE # if set to TRUE, run it from a command line and NOT from RStudio
)
men_file <- "data/my_eM0_single_years.txt"

pred <- e0.predict(sim.dir = e0dir, end.year = 2100, replace.output = TRUE,
                   burnin = 10000, nr.traj = 1000, my.e0.file = men_file,seed = seed, 
                   predict.jmale	=TRUE)

nat.e0.pred <- get.e0.prediction(e0dir)
e0.trajectories.plot(nat.e0.pred, 246, pi = 80, 
                     col = rep("darkblue", 5), nr.traj = 0, 
                     show.legend = FALSE, both.sexes = TRUE)

pred <- e0.predict(sim.dir = e0dir, end.year = 2100, replace.output = TRUE,
                   burnin = 10000, nr.traj = 10, 
                   seed = seed,    predict.jmale = FALSE)

nat.e0.pred <- get.e0.prediction(e0dir)
e0.trajectories.plot(nat.e0.pred, 246, pi = 50, 
                     col = rep("darkblue", 5), nr.traj = 0, 
                     show.legend = FALSE, both.sexes = TRUE)

pred <- e0.predict(sim.dir = e0dir, end.year = 2100, replace.output = TRUE,
                   burnin = 100, nr.traj = 10, my.e0.file = my_file,seed = seed, 
                   predict.jmale	=TRUE)
head(read.delim(my_file, check.names = FALSE))

both.pred <- e0.jmale.predict(pred, my.e0.file = my_file)
, countries.index = c("246"))

nat.e0.pred <- get.e0.prediction(e0dir)
e0.trajectories.plot(nat.e0.pred, 246, pi = 80, 
                     col = rep("darkblue", 5), nr.traj = 0, 
                     show.legend = FALSE, both.sexes = TRUE)

# generate MCMCs and projections for HIV/AIDS countries
# (MCMCs for small countries were already generated within the previous step)
data(include_2024, package = "bayesLife")
countries <- subset(include_2024, include_code == 3)$country_code
me0 <- run.e0.mcmc.extra(sim.dir = e0dir, countries = countries)
my_file <- "data/my_eM0_single_years.txt"

both.pred <- e0.jmale.predict(pred, my.e0.file = my_file)

t2 <- Sys.time()

cat("\nEstimation time: ", t2-t1)

# generate predictions for all countries (female and male)
pred <- e0.predict(sim.dir = e0dir, end.year = 2100, replace.output = TRUE,
                   burnin = 10, nr.traj = 10, seed = seed)
my_file <- "data/my_eM0_single_years.txt"

both.pred <- e0.jmale.predict(pred, my.e0.file = my_file)
m <- get.e0.mcmc(e0dir)

fit <- e0.jmale.estimate(m, verbose = TRUE, countries.index	= 246)

nat.e0.pred <- get.e0.prediction(e0dir)
e0.trajectories.plot(nat.e0.pred, 246, pi = 80, 
                     col = rep("darkblue", 5), nr.traj = 0, 
                     show.legend = FALSE, both.sexes = TRUE)

# align medians with to WPP 2024 middle series
e0.shift.prediction.to.wpp(e0dir, stat = "mean") # female
e0.shift.prediction.to.wpp(e0dir, stat = "mean", joint.male = TRUE) # male 

# to remove the adjustment, run 
# e0.median.reset(e0dir) # female
# e0.median.reset(e0dir, joint.male = TRUE) # female

t3 <- Sys.time()
cat("\nProjection time: ", t3-t2)
cat("\nTotal time: ", t3-t1)

# How to retrieve predictions
#################################
# Retrieve the MCMC and prediction objects using 
m <- get.e0.mcmc(e0dir)
pred <- get.e0.prediction(e0dir) # contains both sexes
predM <- get.e0.prediction(e0dir, joint.male = TRUE) # extract only male prediction object
