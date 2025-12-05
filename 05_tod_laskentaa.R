library(tidyverse)
library(readxl)

simulaatiot <- read_excel("data/simulaatiot.xlsx") |>
  filter(year>1950)

sim <- simulaatiot %>%
  mutate(year = as.integer(year))

pop_df <- sim %>%
  filter(metric == "pop") %>%
  select(trajectory, year, pop = indicator)

mig_rate_df <- sim %>%
  filter(metric == "mig") %>%
  left_join(pop_df, by = c("trajectory", "year")) %>%
  mutate(mig_rate = indicator / pop) %>%
  select(trajectory, year, mig_rate)

# 1. Mean TFR & migration per trajectory
tfr_df <- sim %>%
  filter(metric == "tfr") %>%
  group_by(trajectory) %>%
  summarise(mean_tfr = mean(indicator, na.rm = TRUE))

mig_rate_mean <- mig_rate_df %>%
  group_by(trajectory) %>%
  summarise(mean_mig_rate = mean(mig_rate, na.rm = TRUE))

pop_2050 <-sim %>%
  filter(metric == "child") %>%
  filter(year == 2050) %>%
  select(trajectory, pop_2050 = indicator)

# 3. Combine
traj_summary <- tfr_df %>%
  left_join(mig_rate_mean, by = "trajectory") %>%
  left_join(pop_2050, by = "trajectory")


# 4a. Plot: mean TFR vs pop in 2050
ggplot(traj_summary, aes(x = pop_2050, y = mean_tfr)) +
  geom_point() +
  labs(
    x = "Väestö 2050",
    y = "Keskimääräinen TFR",
    title = "Trajektoriakohtainen väestö 2050 ja keskimääräinen TFR"
  ) +
  theme_minimal()

# 4b. (optional) Plot: mean migration vs pop in 2050
ggplot(traj_summary, aes(x = pop_2050, y = mean_mig_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Mean Migration Rate vs Population 2050") +
  theme_minimal()



model_tfr <- lm(pop_2050 ~ mean_tfr, data = traj_summary)
summary(model_tfr)$r.squared

model_mig <- lm(pop_2050 ~ mean_mig_rate, data = traj_summary)
summary(model_mig)$r.squared

simulaatiot <- read_excel("data/ennusteet.xlsx") |>
  filter(year>2000,metric=="child")

library(dplyr)

simulaatiot2 <- simulaatiot %>%
  mutate(
    # divide numeric columns by 100000
    across(c(lower_80, upper_80, lower_50, upper_50, median),
           ~ .x / 1000),
    
    # create Toteutunut column from median
    Toteutunut = median
  )
write_xlsx(simulaatiot, "data/kuvio1.xlsx")











# Read and label datasets (no functions)
high_tfr1 <- read_excel("data/ennusteet_high_tfr.xlsx") |>
  filter(year > 2000, metric == "child") |>
  mutate(scenario = "Syntyvyyden 2010 tasolle", panel = "Syntyvyyden merkitys")

low_tfr1 <- read_excel("data/ennusteet_low_tfr.xlsx") |>
  filter(year > 2000, metric == "child") |>
  mutate(scenario = "Syntyvyys 1 lapsi per nainen", panel = "Syntyvyyden merkitys")

high_tfr2 <- read_excel("data/ennusteet_no_migration.xlsx") |>
  filter(year > 2000, metric == "child") |>
  mutate(scenario = "Nolla nettomuuttoa", panel = "Maahanmuuton merkitys")

low_tfr2 <- read_excel("data/ennusteet_high_migration.xlsx") |>
  filter(year > 2000, metric == "child") |>
  mutate(scenario = "2023 maahanmuuton taso", panel = "Maahanmuuton merkitys")

# Combine and scale
tfr_all <- bind_rows(high_tfr1, low_tfr1, high_tfr2, low_tfr2) |>
  mutate(
    year = as.integer(year),
    across(c(lower_80, upper_80, lower_50, upper_50, median),
           ~ .x / 1000000)
  )
tfr_all$panel <- factor(
  tfr_all$panel,
  levels = c("Syntyvyyden merkitys", "Maahanmuuton merkitys")
)


tfr_all$scenario <- factor(
  tfr_all$scenario,
  levels = c("Syntyvyyden 2010 tasolle", "Syntyvyys 1 lapsi per nainen","2023 maahanmuuton taso","Nolla nettomuuttoa")
)

ggplot(tfr_all, aes(x = year)) +
  
  # 80% CI (50% opacity)
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80, fill = scenario),
              alpha = 0.5, colour = NA) +
  
  # 50% CI (100% opacity)
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50, fill = scenario),
              alpha = 1.0, colour = NA) +
  
  # Thicker median lines
  geom_line(aes(y = median, colour = scenario), linewidth = 1.5) +
  
  # Panels side-by-side
  facet_wrap(~ panel, nrow = 1) +
  
  labs(
    x = "",
    y = "Miljoonaa lasta",
    colour = "",
    fill   = ""
  ) +
  
  # Datawrapper aesthetic
  scale_color_manual(values = c("#006DDB", "#FF6A39", "#1BC47D", "#9A4DFF")) +
  scale_fill_manual(values  = c("#006DDB", "#FF6A39", "#1BC47D", "#9A4DFF")) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    strip.text = element_text(size = 13, face = "bold"),
    
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    
    axis.text = element_text(size = 11, colour = "grey20"),
    
    panel.spacing = unit(2, "lines"),
    plot.margin = margin(10, 20, 20, 20)
  )

ggsave(
  filename = "tfr_scenarios.svg",
  plot = last_plot(),
  device = "svg",
  width = 10,
  height = 5,
  units = "in"
)


alueelliset_lapset <- read_excel("alueelliset_lapset.xlsx") 


library(dplyr)
library(ggplot2)
library(geomtextpath)

# 1) Prepare data: years numeric + CI columns
alue_plot <- alueelliset_lapset |>
  filter(ennuste=="bayesPop")|>
  mutate(
    year = as.integer(year),
    lower_80 = `0.1`,
    upper_80 = `0.9`,
    lower_50 = `0.25`,
    upper_50 = `0.75`
  )
library(dplyr)
library(ggplot2)
library(geomtextpath)

set.seed(2123)  # for reproducible random panels

# 1) Make sure year is numeric (if not already)
alue_plot <- alue_plot |>
  mutate(year = as.integer(year))

region_colors <- hue_pal()(length(unique(alue_plot$name)))
names(region_colors) <- unique(alue_plot$name)
ggplot(alue_plot, aes(x = year, group = name)) +
  
  # 80% CI
  geom_ribbon(
    aes(ymin = lower_80, ymax = upper_80, fill = name),
    alpha = 0.5, colour = NA
  ) +
  
  # 50% CI
  geom_ribbon(
    aes(ymin = lower_50, ymax = upper_50, fill = name),
    alpha = 1, colour = NA
  ) +
  
  # Median line with text
  geom_textpath(
    aes(y = median, label = name, colour = name),
    linewidth = 1,
    size = 4,
    hjust = 0,
    text_smoothing = 60,
    lineend = "round",
    show.legend = FALSE
  ) +
  
  facet_wrap(~ name, scales = "free_y") +
  
  labs(
    x = "",
    y = "Lapsia"
  ) +
  
  scale_fill_manual(values = region_colors, guide = "none") +   # remove fill legend
  scale_colour_manual(values = region_colors, guide = "none") + # remove line legend
  scale_y_continuous(labels = scales::label_number()) +   # ← FIX HERE
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    
    strip.text = element_blank(),   # remove facet titles
    
    axis.text  = element_text(size = 10, colour = "grey20"),
    panel.spacing = unit(1, "lines"),
    plot.margin  = margin(10, 15, 15, 15)
  )


ggsave(
  filename = "alues.svg",
  plot = last_plot(),
  device = "svg",
  width = 12,
  height = 10,
  units = "in"
)



alueelliset_lapset <- read_excel("alueelliset_lapset_osuus.xlsx") 


# 1) Prepare data: years numeric + CI columns
alue_plot <- alueelliset_lapset |>
  filter(ennuste=="bayesPop")|>
  mutate(
    year = as.integer(year),
    lower_80 = `0.1`,
    upper_80 = `0.9`,
    lower_50 = `0.25`,
    upper_50 = `0.75`
  )

set.seed(2123)  # for reproducible random panels

# 1) Make sure year is numeric (if not already)
alue_plot <- alue_plot |>
  mutate(year = as.integer(year))

region_colors <- hue_pal()(length(unique(alue_plot$name)))
names(region_colors) <- unique(alue_plot$name)
ggplot(alue_plot, aes(x = year, group = name)) +
  
  # 80% CI
  geom_ribbon(
    aes(ymin = lower_80, ymax = upper_80, fill = name),
    alpha = 0.5, colour = NA
  ) +
  
  # 50% CI
  geom_ribbon(
    aes(ymin = lower_50, ymax = upper_50, fill = name),
    alpha = 1, colour = NA
  ) +
  
  # Median line with text
  geom_textpath(
    aes(y = median, label = name, colour = name),
    linewidth = 1,
    size = 4,
    hjust = 0,
    text_smoothing = 60,
    lineend = "round",
    show.legend = FALSE
  ) +
  
  facet_wrap(~ name) +
  
  labs(
    x = "",
    y = "Lapsia"
  ) +
  
  scale_fill_manual(values = region_colors, guide = "none") +   # remove fill legend
  scale_colour_manual(values = region_colors, guide = "none") + # remove line legend
  scale_y_continuous(labels = scales::label_number()) +   # ← FIX HERE
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    
    strip.text = element_blank(),   # remove facet titles
    
    axis.text  = element_text(size = 10, colour = "grey20"),
    panel.spacing = unit(1, "lines"),
    plot.margin  = margin(10, 15, 15, 15)
  )


ggsave(
  filename = "alues_osuus.svg",
  plot = last_plot(),
  device = "svg",
  width = 12,
  height = 10,
  units = "in"
)



