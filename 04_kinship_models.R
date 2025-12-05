nat.pop.dir <- "results/finland"

pop.nat <- get.pop.prediction(nat.pop.dir)

library(tidyverse)


data_for_trajectories <- get.pop.exba("S246_M{}", pop.nat, as.dt=TRUE) |> 
  rename(mortality_m=indicator) |> 
  left_join(get.pop.exba("S246_F{}", pop.nat, as.dt=TRUE)) |> 
  rename(mortality_f=indicator)

mortality_raw <- read_table("U:/Documents/Itla ennuste/data/Mx_1x1.txt", 
                            skip = 3,
                            col_names = c("year", "age", "female", "male", "total"))

mortality_past <- mortality_raw %>%
  mutate(mortality_m = 1 - as.numeric(female),
         mortality_f = 1 - as.numeric(male),
         year=as.numeric(year),
         age=as.numeric(age)) |>
  select(year, age, mortality_m,mortality_f)  %>%
  filter(!is.na(age)) |>
  mutate(across(
    c(mortality_m, mortality_f),
    ~ ifelse(is.na(.x) | .x < 0 | .x > 1, 0, .x)
  )) |>
  bind_rows(data_for_trajectories |> filter(year!=2024))

data_for_fertility <- get.pop.exba("F246{}", pred, as.dt=TRUE) |> rename(fertility_f=indicator)

fertility_past <- read_table("N:/Aapo_selection/mortality_simulation/00_data/01_raw/asfr_1776_2024.txt", skip = 0)%>%
#fertility_past <- read_table("U:/Documents/Itla ennuste/data/FINasfrRR.txt", skip = 2)%>%
  mutate(fertility_f = ASFR ,
         year=as.numeric(Year),
         age=as.numeric(Age))|>
  filter(!is.na(age)) |>
  select(year, age, fertility_f) |>
  bind_rows(data_for_fertility|> filter(year!=2024))

fertility_filled <- fertility_past %>%
  mutate(
    year = as.integer(year),
    trajectory = as.integer(trajectory),
    age = as.integer(age)
  ) %>%
  # make all combinations present, fill missing median with 0
  complete(
    nesting(year, trajectory),         # every year × trajectory that exists in data
    age = 0:100,
    fill = list(fertility_f = 0)
  ) %>%
  arrange(trajectory, year, age)

timestamp()

a<-0
data<-mortality_past |> filter(trajectory==a | is.na(trajectory)) |> filter(age<101, year>1877)
fertility_data<- fertility_filled |> filter(trajectory==a | is.na(trajectory)) |> filter(age<101, year>1877)

mortality_mmatrix <- data |> 
  select(year,age,mortality_m) |>
  arrange(age, year) |>
  pivot_wider(names_from = year, values_from = mortality_m) %>%
  select(-age) %>%
  as.matrix()

mortality_fmatrix <- data  |>
  select(year,age,mortality_f) |>
  arrange(age, year) |>
  pivot_wider(names_from = year, values_from = mortality_f) %>%
  select(-age) %>%
  as.matrix()

ff <- fertility_data |> 
  select(year,age,fertility_f) |>
  mutate(age=as.numeric(age)) |>
  arrange(age, year) |>
  pivot_wider(names_from = year, values_from = fertility_f) %>%
  select(-age) %>%
  as.matrix()

years <- ncol(ff)
ages <- nrow(ff)
fm <- rbind(matrix(0, 5, years),  
            ff[-((ages-4):ages),]) * 1.05

kin_out_time_varying <- DemoKin::kin2sex(
  pf = mortality_fmatrix,       # Female survival matrix (age x year)
  pm = mortality_mmatrix,       # Male survival matrix (age x year)
  ff = ff,       # Female fertility matrix (age x year)
  fm = fm,       # Male fertility matrix (age x year)
  sex_focal = "f",              # Focal individual is female
  time_invariant = FALSE,       # Use time-varying model
  birth_female = 1/2.04,            # Sex ratio at birth (50% female)
  output_cohort = 1890:2013, 
  output_age_focal = 10,# Focus on the 1900 birth cohort
  output_kin = c("ys","os","ya","oa","m","gm","ggm","coa", "cya")  # Select specific kin types
  
)

kin_data<- kin_out_time_varying$kin_summary %>%
  # Combine siblings, aunts/uncles, cousins, and nieces/nephews
  mutate(kin = case_when(
    kin %in% c("ys", "os") ~ "s",           # All siblings
    kin %in% c("ya", "oa") ~ "a",           # All aunts/uncles
    kin %in% c("coa", "cya") ~ "c",         # All cousins
    kin %in% c("nys", "nos") ~ "n",         # All nieces/nephews
    TRUE ~ kin)) %>%
  #filter(age_focal==10) |>
  # Select specific ages for comparison
  # Sum across sex for total kin counts
  group_by(kin, age_focal, cohort) %>%
  summarise(count = sum(count_living)) |>
  mutate(trajectory=0) 

for (a in 1:1000) {
  timestamp()
  data<-mortality_past |> filter(trajectory==a | is.na(trajectory)) |> filter(age<101, year>1877)
  fertility_data<- fertility_filled |> filter(trajectory==a | is.na(trajectory)) |> filter(age<101, year>1877)
  
  mortality_mmatrix <- data |> 
    select(year,age,mortality_m) |>
    arrange(age, year) |>
    pivot_wider(names_from = year, values_from = mortality_m) %>%
    select(-age) %>%
    as.matrix()
  
  mortality_fmatrix <- data  |>
    select(year,age,mortality_f) |>
    arrange(age, year) |>
    pivot_wider(names_from = year, values_from = mortality_f) %>%
    select(-age) %>%
    as.matrix()
  
  ff <- fertility_data |> 
    select(year,age,fertility_f) |>
    mutate(age=as.numeric(age)) |>
    arrange(age, year) |>
    pivot_wider(names_from = year, values_from = fertility_f) %>%
    select(-age) %>%
    as.matrix()
  
  years <- ncol(ff)
  ages <- nrow(ff)
  fm <- rbind(matrix(0, 5, years),  
              ff[-((ages-4):ages),]) * 1.05
  
  kin_out_time_varying <- DemoKin::kin2sex(
    pf = mortality_fmatrix,       # Female survival matrix (age x year)
    pm = mortality_mmatrix,       # Male survival matrix (age x year)
    ff = ff,       # Female fertility matrix (age x year)
    fm = fm,       # Male fertility matrix (age x year)
    sex_focal = "f",              # Focal individual is female
    time_invariant = FALSE,       # Use time-varying model
    birth_female = 1/2.04,            # Sex ratio at birth (50% female)
    output_cohort = 2013:2050,
    output_age_focal = 10,# Focus on the 1900 birth cohort
    output_kin = c("ys","os","ya","oa","m","gm","ggm","coa", "cya")  # Select specific kin types
  )
  
  kin_data<- kin_out_time_varying$kin_summary %>%
    # Combine siblings, aunts/uncles, cousins, and nieces/nephews
    mutate(kin = case_when(
      kin %in% c("ys", "os") ~ "s",           # All siblings
      kin %in% c("ya", "oa") ~ "a",           # All aunts/uncles
      kin %in% c("coa", "cya") ~ "c",         # All cousins
      kin %in% c("nys", "nos") ~ "n",         # All nieces/nephews
      TRUE ~ kin)) %>%
    #filter(age_focal==10) |>
    # Select specific ages for comparison
    # Sum across sex for total kin counts
    group_by(kin, age_focal, cohort) %>%
    summarise(count = sum(count_living)) |>
    mutate(trajectory=a) |>
    bind_rows(kin_data)
}

write_xlsx(kin_data, "data/kin_simulations.xlsx")

intervals_kin <- kin_data %>%
  group_by(kin, cohort, age_focal) %>%
  summarize(
    lower_80 = quantile(count, 0.10),
    upper_80 = quantile(count, 0.90),
    lower_50 = quantile(count, 0.25),
    upper_50 = quantile(count, 0.75),
    median = median(count),
    .groups = "drop"
  )
write_xlsx(intervals_kin, "data/ennusteet_perherakenne.xlsx")


library(readxl)
kin_data <- read_excel("data/kin_simulations.xlsx")

intervals_kin_siblings <- kin_data %>%
  filter(kin=="s") %>%
  mutate(
    n = 20,
    p = count / n,
    x = 1 - dbinom(0, n, p)
  ) %>%
  group_by(kin, cohort, age_focal) %>%
  summarize(
    lower_80 = quantile( x, 0.10),
    upper_80 = quantile( x, 0.90),
    lower_50 = quantile( x, 0.25),
    upper_50 = quantile( x, 0.75),
    median = median( x),
    .groups = "drop"
  )

write_xlsx(intervals_kin_siblings, "data/ennusteet_perherakenne_todennäköisyydet_sib.xlsx")



siblings <- intervals_kin |> 
  filter(age_focal==10,kin=="ggm", cohort>1899)


fig<-siblings %>%
  ggplot(aes(x = as.integer(cohort+10))) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "orange", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "orange", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "orange", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "orange", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "orange") +
  geom_line(aes(y = upper_50), color = "orange") +
  geom_line(aes(y = median), color = "orange", size = 1.2) +
  labs(x = "", y = "Lasta per nainen", title = "Syntyvyys") +
  theme_minimal(base_size = 14) +
  theme(  text = element_text(family = "raleway"))
ggsave("figs/siblings.svg", fig, width = 12, height = 4, units = "in")
fig



n <- 15
mean_sibs <- 1.6
p <- mean_sibs / n
p0 <- dbinom(0, n, p)
p0

kin_out_time_varying <- DemoKin::kin2sex(
  pf = mortality_fmatrix,       # Female survival matrix (age x year)
  pm = mortality_mmatrix,       # Male survival matrix (age x year)
  ff = fertility_matrix,       # Female fertility matrix (age x year)
  fm = fertilitym_matrix,       # Male fertility matrix (age x year)
  sex_focal = "f",              # Focal individual is female
  time_invariant = FALSE,       # Use time-varying model
  birth_female = .5,            # Sex ratio at birth (50% female)
  output_cohort = 1990:2050          # Focus on the 1900 birth cohort
)

data<- kin_out_time_varying$kin_summary %>%
  # Combine siblings, aunts/uncles, cousins, and nieces/nephews
  mutate(kin = case_when(
    kin %in% c("ys", "os") ~ "s",           # All siblings
    kin %in% c("ya", "oa") ~ "a",           # All aunts/uncles
    kin %in% c("coa", "cya") ~ "c",         # All cousins
    kin %in% c("nys", "nos") ~ "n",         # All nieces/nephews
    TRUE ~ kin)) %>%
  # Select specific ages for comparison
  filter(age_focal %in% c(10)) %>%
  # Sum across sex for total kin counts
  group_by(kin, age_focal, cohort) %>%
  summarise(count = sum(count_living)) |>
  filter(kin %in% c("gm", "ggm")) 

observed <- get.pop.ex(paste0("S246_M{", a, "}"), pred, as.dt=TRUE) |> rename(mortality_m=indicator)




|>
  
  head(mortality_f)



kin_out_time_varying <- DemoKin::kin2sex(
  pf = mortality_fmatrix,       # Female survival matrix (age x year)
  pm = mortality_mmatrix,       # Male survival matrix (age x year)
  ff = fertility_matrix,       # Female fertility matrix (age x year)
  fm = fertilitym_matrix,       # Male fertility matrix (age x year)
  sex_focal = "f",              # Focal individual is female
  time_invariant = FALSE,       # Use time-varying model
  birth_female = .5,            # Sex ratio at birth (50% female)
  output_cohort = 1990:2050          # Focus on the 1900 birth cohort
)

data<- kin_out_time_varying$kin_summary %>%
  # Combine siblings, aunts/uncles, cousins, and nieces/nephews
  mutate(kin = case_when(
    kin %in% c("ys", "os") ~ "s",           # All siblings
    kin %in% c("ya", "oa") ~ "a",           # All aunts/uncles
    kin %in% c("coa", "cya") ~ "c",         # All cousins
    kin %in% c("nys", "nos") ~ "n",         # All nieces/nephews
    TRUE ~ kin)) %>%
  # Select specific ages for comparison
  filter(age_focal %in% c(10)) %>%
  # Sum across sex for total kin counts
  group_by(kin, age_focal, cohort) %>%
  summarise(count = sum(count_living)) |>
  filter(kin %in% c("gm", "ggm")) 



data %>%
  ggplot() +
  geom_line(aes(cohort+10, count), size=3)  +
  theme_bw() +
  labs(x = "Age of focal", y= "Number of living female relatives") +
  facet_wrap(~kin, scales = "free_y")+ 
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "raleway"),panel.grid = element_blank(), axis.line = element_line())

