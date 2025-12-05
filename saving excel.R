library(tidyverse)
library(openxlsx)
library(readxl)

library(readxl)
library(dplyr)
library(openxlsx)

input_files  <- c("data/ennusteet.xlsx",
                  "data/ennusteet_high_tfr.xlsx",
                  "data/ennusteet_low_tfr.xlsx",
                  "data/ennusteet_high_migration.xlsx",
                  "data/ennusteet_no_migration.xlsx"
                  )

output_files <- c("Ennusteet_koko_maa.xlsx",
                  "high_tfr_by_metric.xlsx",
                  "low_tfr_by_metric.xlsx",
                  "high_mig_by_metric.xlsx",
                  "low_mig__by_metric.xlsx")

metric_order <- c(
  "child","child_to_pop","preteen","teen","seven olds",
  "births","Deaths","Death before 18",
  "child_mig","tfr"
)

metric_expl_fi <- c(
  "child"           = "Alle 18-vuotiaiden lasten lukumäärä.",
  "child_to_pop"    = "Lasten (0–17 v) osuus koko väestöstä.",
  "preteen"         = "Alle 13-vuotiaiden (0–12 v) lasten lukumäärä.",
  "teen"            = "13–17-vuotiaiden nuorten lukumäärä.",
  "seven olds"      = "17-vuotiaiden lukumäärä (ikäryhmä datassa).",
  "births"          = "Elävänä syntyneiden lasten lukumäärä.",
  "Deaths"          = "Kaikkien kuolemien lukumäärä.",
  "Death before 18" = "Todennäköisyys kuolla ennen 18 vuoden ikää.",

  "child_mig"       = "Lasten (0–17 v) nettomuutto.",
  "tfr"             = "Kokonaishedelmällisyysluku (TFR)."
)

# ----------------------------------------------------
# BASIC LOOP
# ----------------------------------------------------

for (i in seq_along(input_files)) {
  
  simulaatiot <- read_excel(input_files[i]) 
  
  intervals_2025 <- simulaatiot |>
    filter(as.integer(year) > 2024)
  
  metrics_present <- intersect(metric_order, unique(intervals_2025$metric))
  
  wb <- createWorkbook()
  
  for (m in metrics_present) {
    
    df_m <- intervals_2025 |>
      filter(metric == m)
    
    df_m_fi <- df_m |>
      rename(
        vuosi    = year,
        mittari  = metric,
        ala_80   = lower_80,
        yla_80   = upper_80,
        ala_50   = lower_50,
        yla_50   = upper_50,
        mediaani = median
      )
    
    addWorksheet(wb, sheetName = m)
    
    expl <- metric_expl_fi[[m]]
    if (is.null(expl)) expl <- paste("Mittari:", m)
    
    writeData(wb, m, x = expl, startRow = 1, startCol = 1, colNames = FALSE)
    
    writeData(wb, m, x = df_m_fi, startRow = 2, startCol = 1)
  }
  
  saveWorkbook(wb, output_files[i], overwrite = TRUE)
}


simulaatiot <- read_excel("data/simulaatiot.xlsx")


share_larger_2035 <- simulaatiot |>
  filter(year %in% c("2024", "2050"), metric=="child_to_pop") %>% 
  select(year, trajectory, indicator) %>%
  pivot_wider(
    names_from = year,
    values_from = indicator,
    names_prefix = "y"  ) %>%
  mutate(increase = y2024 > y2050) %>%
  summarise(share_increase = mean(increase, na.rm = TRUE))

share_larger_2035



library(dplyr)
library(tidyr)
library(openxlsx)

# ---- Transform kin, pivot wider ----
ennusteet_wide <- ennusteet_perherakenne %>%
  # keep kin groups
  filter(kin %in% c("ggm", "gm", "m", "s")) %>%
  # rename to Finnish
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
  # long format
  pivot_longer(
    cols = c(lower_80, upper_80, lower_50, upper_50, median),
    names_to = "stat",
    values_to = "value"
  ) %>%
  # column name components
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

# ---- keep cohorts from 1900 onwards ----
ennusteet_wide_1900 <- ennusteet_wide %>%
  mutate(cohort=cohort+10)  %>%
  filter(cohort >= 1900)

# ---- save to Excel ----
write.xlsx(
  ennusteet_wide_1900,
  file = "ennusteet_perherakenne_1900plus.xlsx",
  overwrite = TRUE
)

