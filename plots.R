#plots national
#all children, share of population, total population (births)
# 0,3,6,12 and 15 year olds
#sensitivity to immigration and births?

#plots area
#all children, share of population, birhts
# 0,3,6,12 and 15 year olds

#plots kinship
#number of siblings alive,
#number of parents, 
#grandparents, grandgrandparents


library(ggplot2)
library(scales)
library(showtext)
library(tidyverse)
library(patchwork)
library(readxl)

intervals <- read_excel("data/ennusteet.xlsx") |>
  filter(year>1950)

# Väestö Plot
vaesto_plot <- intervals |>
  filter(metric == "child") 
|>
  ggplot(aes(x = as.integer(year))) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "steelblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "steelblue", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "steelblue", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "steelblue", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "steelblue") +
  geom_line(aes(y = upper_50), color = "steelblue") +
  geom_line(aes(y = median), color = "steelblue", size = 1.1) +
  scale_y_continuous(labels = label_number(suffix = "m", scale = 1/1000000)) +
  labs(x = "", y = "Lasten määrä", title = "") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "raleway"),panel.grid = element_blank(), axis.line = element_line())

# Työikäiset Plot
tyoikaiset_plot <- intervals |>
  filter(metric == "pop") |>
  ggplot(aes(x = as.integer(year))) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "orange", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "orange", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "orange", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "orange", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "orange") +
  geom_line(aes(y = upper_50), color = "orange") +
  geom_line(aes(y = median), color = "orange", size = 1.1) +
  scale_y_continuous(labels = label_number(suffix = "m", scale = 1/1000000)) +
  labs(x = "", y = "Koko väestö", title = "") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "raleway"),panel.grid = element_blank(), axis.line = element_line())

# Huoltosuhde Plot
huolto_plot <- intervals |>
  filter(metric == "child_to_pop") |>
  ggplot(aes(x = as.integer(year))) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "purple", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "purple", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "purple", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "purple", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "purple") +
  geom_line(aes(y = upper_50), color = "purple") +
  geom_line(aes(y = median), color = "purple", size = 1.1) +
  labs(x = "", y = "Lapsia/väestö", title = "Lasten osuus väestöstä") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "raleway"),panel.grid = element_blank(), axis.line = element_line())

# Combine (example with patchwork)
combined_population <- vaesto_plot  | huolto_plot
combined_population
ggsave("figs/kuvio1.svg", combined_population, width = 9, height = 3, units = "in")

# Elinajanodote (e0)
plot <- intervals |>
  filter(metric %in% c("0", "3", "6", "12", "15")) |>
  mutate(metric = factor(metric, levels = c("0", "3", "6", "12", "15"))) |>  # ensure correct facet order
  ggplot(aes(x = as.integer(year))) +
  facet_grid(~metric) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "steelblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "steelblue", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "steelblue", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "steelblue", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "steelblue") +
  geom_line(aes(y = upper_50), color = "steelblue") +
  geom_line(aes(y = median), color = "steelblue", size = 1.2) +
  geom_vline(xintercept = 2025, color = "black", linetype = "dashed", linewidth = 0.8) +  # vertical dashed line
  labs(x = "", y = "Vuosia", title = "Lapsia") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "raleway"),
    panel.grid = element_blank(),
    axis.line = element_line()
  )

ggsave("figs/kuvio2.svg", plot, width = 9, height = 3, units = "in")

intervals <- read_excel("data/ennusteet_high_tfr.xlsx") |>
  filter(year>1999,metric == "child") |>
  mutate(mutate="Korkea hedelmällisyys (vuoden 2010 hedelmällisyys)" )

intervals <- read_excel("data/ennusteet_low_tfr.xlsx") |>
  filter(year>1999,metric == "child") |>
  mutate(mutate="Matala hedelmällisyys (yksi lapsi naista kohden") |>
  bind_rows(intervals) |>
  mutate(sensitiivisyys="Syntyvyys")
         
  
migration <- read_excel("data/ennusteet_high_migration.xlsx") |>
  filter(year>1999,metric == "child") |>
  mutate(mutate="Korkea muuttovoitto (vuoden 2022 taso)" )

intervals <- read_excel("data/ennusteet_no_migration.xlsx") |>
  filter(year>1999,metric == "child") |>
  mutate(mutate="Ei muuttovoitto") |>
  bind_rows(migration) |>
  mutate(sensitiivisyys="Muuttovoitto") |>
  bind_rows(intervals)


plot <- intervals %>%
    ggplot(aes(x = as.integer(year))) +
  facet_grid(~ sensitiivisyys) +
  # ribbons per group
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80, fill = `mutate`, group = `mutate`),
              alpha = 0.15) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50, fill = `mutate`, group = `mutate`),
              alpha = 0.35) +
  # lines per group
  geom_line(aes(y = lower_80, color = `mutate`, group = `mutate`), linetype = "dashed") +
  geom_line(aes(y = upper_80, color = `mutate`, group = `mutate`), linetype = "dashed") +
  geom_line(aes(y = lower_50, color = `mutate`, group = `mutate`)) +
  geom_line(aes(y = upper_50, color = `mutate`, group = `mutate`)) +
  geom_line(aes(y = median,   color = `mutate`, group = `mutate`), linewidth = 1.1) +
  # reference year
  geom_vline(xintercept = 2025, color = "black", linetype = "dashed", linewidth = 0.8) +
  labs(x = "", y = "Vuosia", title = "Lapsia", color = "Group", fill = "Group") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "raleway"),
    panel.grid = element_blank(),
    axis.line = element_line()
  )

plot

ggsave("figs/kuvio3.svg", plot, width = 9, height = 3, units = "in")


###sensitiivisyydet


# Syntyvyys
syntyvyys_plot <- intervals |>
  filter(metric == "births") |>
  ggplot(aes(x = as.integer(year))) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "orange", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "orange", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "orange", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "orange", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "orange") +
  geom_line(aes(y = upper_50), color = "orange") +
  geom_line(aes(y = median), color = "orange", size = 1.2) +
  labs(x = "", y = "Lasta per nainen", title = "Syntyvyys") +
  theme_minimal(base_size = 14) +
  theme(  text = element_text(family = "raleway"),
          panel.grid = element_blank(), axis.line = element_line())

# Maahanmuutto (Muuttaneita)
muuttaneita_plot <- intervals |>
  filter(metric == "child_mig") |>
  ggplot(aes(x = as.integer(year))) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "purple", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "purple", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "purple", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "purple", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "purple") +
  geom_line(aes(y = upper_50), color = "purple") +
  geom_line(aes(y = median), color = "purple", size = 1.2) +
  scale_y_continuous(labels = label_number(suffix = "t", scale = 1/1000)) +
  labs(x = "", y = "Nettomuuttujia", title = "Maahanmuutto") +
  theme_minimal(base_size = 14) +
  theme(  text = element_text(family = "raleway"),
          panel.grid = element_blank(), axis.line = element_line())

# Combine
combined_plot <- e0_plot | syntyvyys_plot | muuttaneita_plot
combined_plot
library(ggplot2)
ggsave("figs/elementit.svg", combined_plot, width = 12, height = 4, units = "in")


set.seed(123)  
simulaatiot <- read_excel("simulaatiot.xlsx")

ids <- sample(unique(simulaatiot$trajectory), 1000)

simulaatiot <- simulaatiot |>
  filter(trajectory %in% ids)

means <- simulaatiot |>
  filter(year >= 2024, year <= 2050) |>
  group_by(trajectory) |>
  summarize(
    e0 = e0[year == 2050],
    Syntyvyys = mean(Syntyvyys),
    Muuttaneita = mean(Muuttaneita),
    pop_2050 = pop[year == 2050], 
    tyo_2050= tyoikaiset[year == 2050], 
    huolto_2050 = huolto[year == 2050]
  ) 

# 2. Make long data for plotting
means_long <- means |>
  pivot_longer(cols = c(e0, Syntyvyys, Muuttaneita),
               names_to = "variable", values_to = "value") |> ungroup()

# 3. Separate plots for each variable with style

# e0 vs population
mean_e0_plot <- means_long |>
  filter(variable == "e0") |>
  ggplot(aes(x = value, y = pop_2050 )) +
  geom_point(color = "steelblue", alpha = 0.15, size = 1) +
  # Add red dot
  geom_point(aes(x = 86.5, y = 6177141), color = "#00008B", size = 3, alpha = 0.8) +
  
  labs(x = "Elinajanodote e0 (vuotta)", y = "Väestö 2050 (m)", title = "") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "raleway"),
    panel.grid = element_blank(),
    axis.line = element_line()
  ) +
  scale_y_continuous(labels = label_number(suffix = "m", scale = 1/1000000))

# Syntyvyys vs population
mean_syntyvyys_plot <- means_long |>
  filter(variable == "Syntyvyys") |>
  ggplot(aes(x = value, y = pop_2050 )) +
  geom_point(color = "orange", alpha = 0.15, size = 1) +
  # Add red dot
  geom_point(aes(x = 1.26, y = 6177141), color = "#00008B",  size = 3, alpha = 0.8) +
  labs(x = "Syntyvyys (lasta per nainen)", y = "", title = "") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "raleway"),
    panel.grid = element_blank(),
    axis.line = element_line()
  ) +
  scale_y_continuous(labels = label_number(suffix = "m", scale = 1/1000000))

# Muuttaneita vs population
mean_muuttaneita_plot <- means_long |>
  filter(variable == "Muuttaneita") |>
  mutate(value = value ) |>
  ggplot(aes(x = value, y = pop_2050 )) +
  geom_point(color = "purple", alpha = 0.15, size = 1) +
  # Add red dot
  geom_point(aes(x = 40000, y = 6177141), color = "#00008B",  size = 3, alpha = 0.8) +
  labs(x = "Nettomaahanmuuttajia/vuosi ", y = "", title = "") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "raleway"),
    panel.grid = element_blank(), 
    axis.line = element_line(),
    axis.ticks = element_line(color = "black", size = 0.5)
  ) +
  scale_x_continuous(labels = label_number(suffix = "t", scale = 1/1000)) +
  scale_y_continuous(labels = label_number(suffix = "m", scale = 1/1000000))

# Combine the three plots
means_scatter_plot <- mean_e0_plot | mean_syntyvyys_plot | mean_muuttaneita_plot

manual_legend <- tibble(x = 1, y = 1, label = "Tilastokeskuksen ennusteen oletukset")

legend_plot <- ggplot(manual_legend, aes(x, y)) +
  geom_point(color = "#00008B", size = 3) +
  geom_text(aes(label = label), vjust = -0.5,hjust = -0.5, size = 4, family = "raleway") +
  theme_void()

# Stack plot and manual legend
means_scatter_plot_ <- means_scatter_plot / legend_plot + plot_layout(heights = c(10, 1))
means_scatter_plot_

# Save as SVG
ggsave("figs/elementit_vs_population.svg", means_scatter_plot, width = 9, height = 3, units = "in")



##probabilities
means_long |>
  filter(variable == "Muuttaneita") |>
  summarise(
    pct_over_40000 = mean(value > 40000) * 100
  )

means_long |>
  filter(variable == "e0") |>
  summarise(
    lower_10 = quantile(pop_2050, 0.10),
    upper_90 = quantile(pop_2050, 0.90)
  )

means_long |>
  filter(variable == "Muuttaneita") |>
  summarise(
    pct_over_40000 = mean(pop_2050 > 5635971) * 100
  )


means_long |>
  filter(variable == "e0") |>
  summarise(
    pct_over_40000 = mean(tyo_2050 > 3293886) * 100
  )

means_long |>
  filter(variable == "e0") |>
  summarise(
    pct_over_40000 = mean(huolto_2050 > .4034192) * 100
  )


means_long |>
  filter(variable == "e0") |>
  summarise(
    pct_over_40000 = mean(value > 85) * 100
  )

means_long |>
  filter(variable == "e0") |>
  summarise(
    pct_over_40000 = mean(value > 84) * 100
  )

means_long |>
  filter(variable == "e0") |>
  summarise(
    lower_10 = quantile(value, 0.10),
    upper_90 = quantile(value, 0.90)
  )

means_long |>
  filter(variable == "Muuttaneita") |>
  summarise(
    lower_10 = quantile(value, 0.10),
    upper_90 = quantile(value, 0.90)
  )

means_long |>
  filter(variable == "Syntyvyys") |>
  summarise(
    lower_10 = quantile(value, 0.10),
    upper_90 = quantile(value, 0.90)
  )


means_long |>
  filter(variable == "e0") |>
  summarise(threshold_20th_percentile = quantile(value, 0.20))

##taulukko  nettisivuille.
intervals_2025 <- intervals |>
  filter(as.numeric(year) >= 2025)

intervals_split <- list(
  "Koko väestö" = intervals_2025 |>
    filter(variable == "pop") |>
    mutate(across(where(is.numeric), round)),
  
  "Työikäiset 18 - 64" = intervals_2025 |>
    filter(variable == "tyoikaiset") |>
    mutate(across(where(is.numeric), round)),
  
  "Muuttaneita" = intervals_2025 |>
    filter(variable == "Muuttaneita") |>
    mutate(across(where(is.numeric), round)),
  
  "Syntyvyys" = intervals_2025 |>
    filter(variable == "Syntyvyys"),
  
  "e0" = intervals_2025 |>
    filter(variable == "e0"),
  
  "huolto" = intervals_2025 |>
    filter(variable == "huolto")
)

# Write to Excel
write_xlsx(intervals_split, "ennusteet_sorsa.xlsx")
