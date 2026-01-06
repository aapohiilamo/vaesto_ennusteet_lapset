library(tidyverse)
library(DemoKin)
library(openxlsx)
library(readxl)
library(writexl)

data_for_trajectories <- readRDS("data/input_perherakenne_kuolleisuus.rds")

mortality_raw <- read_table("U:/Documents/Itla ennuste/data/Mx_1x1.txt", 
                            skip = 3,
                            col_names = c("year", "age", "female", "male", "total"))

mortality_past <- mortality_raw |>
  mutate(mortality_m = 1 - as.numeric(female),
         mortality_f = 1 - as.numeric(male),
         year=as.numeric(year),
         age=as.numeric(age)) |>
  select(year, age, mortality_m,mortality_f)  |>
  filter(!is.na(age)) |>
  mutate(across(
    c(mortality_m, mortality_f),
    ~ ifelse(is.na(.x) | .x < 0 | .x > 1, 0, .x)
  )) |>
  bind_rows(data_for_trajectories |> filter(year!=2024))

data_for_fertility <- readRDS("data/input_perherakenne_syntyvyys.rds")
fertility_past <- read_table("N:/Aapo_selection/mortality_simulation/00_data/01_raw/asfr_1776_2024.txt", skip = 0)|>
  mutate(fertility_f = ASFR ,
         year=as.numeric(Year),
         age=as.numeric(Age))|>
  filter(!is.na(age)) |>
  select(year, age, fertility_f) |>
  bind_rows(data_for_fertility|> filter(year!=2024))

fertility_filled <- fertility_past |>
  mutate(
    year = as.integer(year),
    trajectory = as.integer(trajectory),
    age = as.integer(age)
  ) |>
  complete(
    nesting(year, trajectory),        
    age = 0:100,
    fill = list(fertility_f = 0)
  ) |>
  arrange(trajectory, year, age)

timestamp()

a<-0
data<-mortality_past |> filter(trajectory==a | is.na(trajectory)) |> filter(age<101, year>1877)
fertility_data<- fertility_filled |> filter(trajectory==a | is.na(trajectory)) |> filter(age<101, year>1877)

mortality_mmatrix <- data |> 
  select(year,age,mortality_m) |>
  arrange(age, year) |>
  pivot_wider(names_from = year, values_from = mortality_m) |>
  select(-age) |>
  as.matrix()

mortality_fmatrix <- data  |>
  select(year,age,mortality_f) |>
  arrange(age, year) |>
  pivot_wider(names_from = year, values_from = mortality_f) |>
  select(-age) |>
  as.matrix()

ff <- fertility_data |> 
  select(year,age,fertility_f) |>
  mutate(age=as.numeric(age)) |>
  arrange(age, year) |>
  pivot_wider(names_from = year, values_from = fertility_f) |>
  select(-age) |>
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
  output_kin = c("ys","os","m","gm","ggm"),  # Select specific kin types
  #output_kin = c("ys","os","ya","oa","m","gm","ggm","coa", "cya"),  # Select specific kin types
  output_age_focal = 10
  
)

kin_data<- kin_out_time_varying$kin_summary |>
  mutate(kin = case_when(
    kin %in% c("ys", "os") ~ "s",           
    kin %in% c("ya", "oa") ~ "a",          
    kin %in% c("coa", "cya") ~ "c",         # All cousins
    kin %in% c("nys", "nos") ~ "n",         # All nieces/nephews
    TRUE ~ kin)) |>
  #filter(age_focal==10) |>

  group_by(kin, age_focal, cohort) |>
  summarise(count = sum(count_living)) |>
  mutate(trajectory=0) 

for (a in 1:1000) {
  timestamp()
  data<-mortality_past |> filter(trajectory==a | is.na(trajectory)) |> filter(age<101, year>1877)
  fertility_data<- fertility_filled |> filter(trajectory==a | is.na(trajectory)) |> filter(age<101, year>1877)
  
  mortality_mmatrix <- data |> 
    select(year,age,mortality_m) |>
    arrange(age, year) |>
    pivot_wider(names_from = year, values_from = mortality_m) |>
    select(-age) |>
    as.matrix()
  
  mortality_fmatrix <- data  |>
    select(year,age,mortality_f) |>
    arrange(age, year) |>
    pivot_wider(names_from = year, values_from = mortality_f) |>
    select(-age) |>
    as.matrix()
  
  ff <- fertility_data |> 
    select(year,age,fertility_f) |>
    mutate(age=as.numeric(age)) |>
    arrange(age, year) |>
    pivot_wider(names_from = year, values_from = fertility_f) |>
    select(-age) |>
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
    output_cohort = 2013:2040,
    output_age_focal = 10,# Focus on the 1900 birth cohort
    output_kin = c("ys","os","m","gm","ggm")
    #output_kin = c("ys","os","ya","oa","m","gm","ggm","coa", "cya")  # Select specific kin types
  )
  
  kin_data<- kin_out_time_varying$kin_summary |>
    mutate(kin = case_when(
      kin %in% c("ys", "os") ~ "s",           
      kin %in% c("ya", "oa") ~ "a",           
      kin %in% c("coa", "cya") ~ "c",       
      kin %in% c("nys", "nos") ~ "n",   
      TRUE ~ kin)) |>
    group_by(kin, age_focal, cohort) |>
    summarise(count = sum(count_living)) |>
    mutate(trajectory=a) |>
    bind_rows(kin_data)
}
saveRDS(kin_data, "data/kin_simulations.rds")

intervals_kin <- kin_data |>
  group_by(kin, cohort, age_focal) |>
  summarize(
    lower_80 = quantile(count, 0.10),
    upper_80 = quantile(count, 0.90),
    lower_50 = quantile(count, 0.25),
    upper_50 = quantile(count, 0.75),
    median = median(count),
    .groups = "drop"
  )
write_xlsx(intervals_kin, "data/ennusteet_perherakenne.xlsx")

siblings <- intervals_kin |> 
  filter(age_focal==10,kin=="ggm", cohort>1899)

fig<-siblings |>
  ggplot(aes(x = as.integer(cohort+10))) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "orange", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "orange", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "orange", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "orange", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "orange") +
  geom_line(aes(y = upper_50), color = "orange") +
  geom_line(aes(y = median), color = "orange", size = 1.2) +
  labs(x = "", y = "Isoisovanhempia elossa", title = "Isoisovanhempia") +
  theme_minimal(base_size = 14) +
  theme(  text = element_text(family = "raleway"))
ggsave("figs/siblings.svg", fig, width = 12, height = 4, units = "in")
fig



n <- 15
mean_sibs <- 1.6
p <- mean_sibs / n
p0 <- dbinom(0, n, p)
p0

ennusteet_perherakenne <- read_excel("data/ennusteet_perherakenne.xlsx")
ennusteet_wide <- ennusteet_perherakenne |>
  filter(kin %in% c("ggm", "gm", "m", "s")) |>
  mutate(
    kin_fi = recode(
      kin,
      "ggm" = "isoisovanhempia",
      "gm"  = "isovanhempia",
      "m"   = "vanhempia",
      "s"   = "sisaruksia"
    )
  ) |>
  select(-kin) |>
  pivot_longer(
    cols = c(lower_80, upper_80, lower_50, upper_50, median),
    names_to = "stat",
    values_to = "value"
  ) |>
  mutate(
    ci = case_when(
      stat %in% c("lower_80", "upper_80") ~ "80",
      stat %in% c("lower_50", "upper_50") ~ "50",
      stat == "median"                    ~ "med"
    ),
    bound = case_when(
      stat %in% c("lower_80", "lower_50") ~ "lower",
      stat %in% c("upper_80", "upper_50") ~ "upper",
      stat == "median"                    ~ "median"
    ),
    colname = case_when(
      ci %in% c("80", "50") ~ paste0(kin_fi, ci, "_", bound),
      ci == "med"           ~ paste0(kin_fi, "_median")
    )
  ) |>
  mutate(Vuosi=cohort+10)   |>
  filter(Vuosi >= 1900)

enn_w <- ennusteet_wide |>
  mutate(
    estimate = case_when(
      stat %in% c("median", "Median", "Keskiennuste") ~ "Keskiennuste",
      grepl("^lower_", stat) ~ paste0(ci, "_lower"),
      grepl("^upper_", stat) ~ paste0(ci, "_upper"),
      TRUE ~ stat
    )
  ) |>
  select(kin_fi, Vuosi, estimate, value)

enn_tabs <- enn_w |>
  pivot_wider(
    names_from  = estimate,
    values_from = value
  ) |>
  arrange(kin_fi, Vuosi)

sheet_list <- split(enn_tabs |> select(-kin_fi), enn_tabs$kin_fi)
names(sheet_list) <- substr(gsub('[:\\\\/?*\\[\\]]', "_", names(sheet_list)), 1, 31)
write_xlsx(sheet_list, "results/ennusteet_excel/ennusteet_sukulaiset.xlsx")



# ---- Transform kin, pivot wider ----
ennusteet_wide <- ennusteet_perherakenne %>%
  filter(kin %in% c("ggm", "gm", "m", "s")) %>%
  mutate(
    kin_fi = recode(
      kin,
      "ggm" = "isoisovanhempia",
      "gm"  = "isovanhempia",
      "m"   = "vanhempia",
      "s"   = "sisaruksia"
    )
  ) %>%
  select(-kin) %>%
  pivot_longer(
    cols = c(lower_80, upper_80, lower_50, upper_50, median),
    names_to = "stat",
    values_to = "value"
  ) %>%
  mutate(
    ci = case_when(
      stat %in% c("lower_80", "upper_80") ~ "80",
      stat %in% c("lower_50", "upper_50") ~ "50",
      stat == "median"                    ~ "med"
    ),
    bound = case_when(
      stat %in% c("lower_80", "lower_50") ~ "lower",
      stat %in% c("upper_80", "upper_50") ~ "upper",
      stat == "median"                    ~ "median"
    ),
    colname = case_when(
      ci %in% c("80", "50") ~ paste0(kin_fi, ci, "_", bound),
      ci == "med"           ~ paste0(kin_fi, "_median")
    )
  ) %>%
  select(cohort, age_focal, colname, value) %>%
  pivot_wider(
    names_from  = colname,
    values_from = value
  )

ennusteet_wide_1900 <- ennusteet_wide %>%
  mutate(cohort=cohort+10)  %>%
  filter(cohort >= 1900)

write.xlsx(
  ennusteet_wide_1900,
  file = "data/ennusteet_perherakenne_wide_lkm.xlsx",
  overwrite = TRUE
)


kin_data <- readRDS("data/kin_simulations.rds")
intervals_kin_siblings <- kin_data |>
  mutate(
    vuosi=cohort+10,
    lambda = count,                
    x = case_when(kin=="s"~1 - dpois(0, lambda), 
                  kin=="m"~1 - (1 - count / 2)^2,
                  kin=="gm"~1 - (1 - count / 4)^4,
                  kin=="ggm"~1 - (1 - count / 8)^8,
                  TRUE ~ NA_real_)
  ) |>
  group_by(kin, vuosi) |>
  summarise(
    lower_80 = quantile(x, 0.10, na.rm = TRUE),
    upper_80 = quantile(x, 0.90, na.rm = TRUE),
    lower_50 = quantile(x, 0.25, na.rm = TRUE),
    upper_50 = quantile(x, 0.75, na.rm = TRUE),
    median   = median(x, na.rm = TRUE),
    .groups = "drop"
  )

write_xlsx(intervals_kin_siblings, "data/ennusteet_perherakenne_todennäköisyydet.xlsx")


ennusteet_wide <- intervals_kin_siblings %>%
  filter(kin %in% c("ggm", "gm", "m", "s")) %>%
  mutate(
    kin_fi = recode(
      kin,
      "ggm" = "isoisovanhempia",
      "gm"  = "isovanhempia",
      "m"   = "vanhempia",
      "s"   = "sisaruksia"
    )
  ) %>%
  select(-kin) %>%
  pivot_longer(
    cols = c(lower_80, upper_80, lower_50, upper_50, median),
    names_to = "stat",
    values_to = "value"
  ) %>%
  mutate(
    ci = case_when(
      stat %in% c("lower_80", "upper_80") ~ "80",
      stat %in% c("lower_50", "upper_50") ~ "50",
      stat == "median"                    ~ "med"
    ),
    bound = case_when(
      stat %in% c("lower_80", "lower_50") ~ "lower",
      stat %in% c("upper_80", "upper_50") ~ "upper",
      stat == "median"                    ~ "median"
    ),
    colname = case_when(
      ci %in% c("80", "50") ~ paste0(kin_fi, ci, "_", bound),
      ci == "med"           ~ paste0(kin_fi, "_median")
    )
  ) %>%
  select(vuosi,  colname, value) %>%
  pivot_wider(
    names_from  = colname,
    values_from = value
  )

write.xlsx(
  ennusteet_wide,
  file = "data/ennusteet_perherakenne_wide_tod.xlsx",
  overwrite = TRUE
)
