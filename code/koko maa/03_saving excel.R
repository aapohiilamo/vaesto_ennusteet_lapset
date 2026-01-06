library(tidyverse)
library(openxlsx)
library(readxl)

input_files  <- c("data/ennusteet.xlsx",
                  "data/ennusteet_high_tfr.xlsx",
                  "data/ennusteet_low_tfr.xlsx",
                  "data/ennusteet_high_migration.xlsx",
                  "data/ennusteet_no_migration.xlsx"
                  )

output_files <- c("results/ennusteet_excel/Ennusteet_koko_maa.xlsx",
                  "results/ennusteet_excel/korkea_tfr_ennusteet.xlsx",
                  "results/ennusteet_excel/matala_tfr_ennusteet.xlsx",
                  "results/ennusteet_excel/korkea_nettomuutto_ennusteet.xlsx",
                  "results/ennusteet_excel/matala_nettomuutto_ennusteet.xlsx")

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
        keskiennuste = median
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
