# ============================================================
# EMU430 — TRNC Tourism Analytics
# Script 04: Descriptive Statistics & Additional Analysis
# ============================================================

library(tidyverse)
library(scales)
library(patchwork)
library(knitr)

panel <- read_csv("data/processed/trnc_panel.csv") |> 
  mutate(period = factor(period, levels = c("Pre-COVID", "COVID", "Recovery")))

# ── 1. Descriptive statistics table ──────────────────────────

desc <- panel |>
  select(
    `Year`                = year,
    `Arrivals`            = arrivals_total,
    `Bednights`           = bednights,
    `Occupancy (%)`       = occupancy_pct,
    `Avg stay (days)`     = avg_stay_days,
    `TRNC CPI`            = cpi_trnc,
    `R&H CPI`             = cpi_rh_trnc,
    `TRNC infl. YoY%`     = cpi_trnc_yoy,
    `Turkey infl. YoY%`   = cpi_turkey_yoy,
    `EUR/TRY`             = eur_try,
    `USD/TRY`             = usd_try
  ) |>
  pivot_longer(-Year, names_to = "Variable") |>
  group_by(Variable) |>
  summarise(
    N       = sum(!is.na(value)),
    Mean    = mean(value, na.rm = TRUE),
    SD      = sd(value, na.rm = TRUE),
    Min     = min(value, na.rm = TRUE),
    Median  = median(value, na.rm = TRUE),
    Max     = max(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.double), ~ round(., 2)))

write_csv(desc, "output/tables/descriptive_stats.csv")
cat("Saved: output/tables/descriptive_stats.csv\n")
print(desc, n = 20)

# ── 2. Pre-COVID vs Recovery period comparison ────────────────

period_comp <- panel |>
  filter(period != "COVID") |>
  group_by(period) |>
  summarise(
    n_years          = n(),
    mean_arrivals    = mean(arrivals_total),
    mean_bednights   = mean(bednights),
    mean_occupancy   = mean(occupancy_pct),
    mean_avg_stay    = mean(avg_stay_days),
    mean_eur_try     = mean(eur_try),
    .groups = "drop"
  )

write_csv(period_comp, "output/tables/period_comparison.csv")
cat("Saved: output/tables/period_comparison.csv\n")
cat("\n=== Pre-COVID vs Recovery comparison ===\n")
print(period_comp)

# ── 3. Air vs Sea arrivals share over time ────────────────────

p_share <- panel |>
  select(year, arrivals_air, arrivals_sea, period) |>
  pivot_longer(c(arrivals_air, arrivals_sea),
               names_to = "mode",
               values_to = "n") |>
  mutate(mode = dplyr::recode(mode, # Burada dplyr:: ekledik
                              "arrivals_air" = "Air arrivals",
                              "arrivals_sea" = "Sea arrivals"
  )) |>
  ggplot(aes(x = year, y = n, fill = mode)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = c("Air arrivals" = "#185FA5",
                                "Sea arrivals" = "#1D9E75")) +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "TRNC Tourist Arrivals by Mode of Entry, 2015–2024",
    subtitle = "Air arrivals dominate; sea share increased post-2019",
    x = NULL, y = "Number of arrivals", fill = NULL,
    caption  = "Source: TRNC Tourism Planning Dept. Statistical Yearbook 2024"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

ggsave("output/figures/11_air_sea.png", p_share,
       width = 10, height = 5, dpi = 150)
cat("Saved: 11_air_sea.png\n")

# ── 4. Monthly seasonality (2025 data) ───────────────────────

monthly_2025 <- tribble(
  ~month_n, ~month, ~tc_2025, ~foreign_2025, ~total_2025, ~occ_2025,
  1L,  "Jan", 158234, 19170, 177404, 31.6,
  2L,  "Feb", 154925, 23280, 178205, 28.4,
  3L,  "Mar", 134557, 30480, 165037, 29.8,
  4L,  "Apr", 171885, 27659, 199544, 38.2,
  5L,  "May", 180093, 28828, 208921, 43.6,
  6L,  "Jun", 182327, 28943, 211270, 48.9,
  7L,  "Jul", 198773, 31601, 230374, 59.8,
  8L,  "Aug", 210459, 36197, 246656, 68.4,
  9L,  "Sep", 225112, 45317, 270429, 58.3,
 10L,  "Oct", 221430, 40809, 262239, 54.4,
 11L,  "Nov", 204051, 24385, 228436, 40.7,
 12L,  "Dec", 191474, 19740, 211214, 30.9
) |>
  mutate(month = factor(month, levels = month))

p_seasonal <- ggplot(monthly_2025,
                     aes(x = month, y = total_2025, fill = occ_2025)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = paste0(occ_2025, "%")), vjust = -0.4, size = 3) +
  scale_fill_gradient(low = "#9FE1CB", high = "#0F6E56",
                      name = "Occupancy (%)") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Monthly Tourist Arrivals and Occupancy Rate: TRNC 2025",
    subtitle = "Bar height = total arrivals | Colour intensity = occupancy rate",
    x = NULL, y = "Total arrivals (excl. TRNC citizens)",
    caption  = "Source: TRNC Tourism Planning Dept. Ocak-Aralik 2025 Bulletin"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "right")

ggsave("output/figures/12_seasonality_2025.png", p_seasonal,
       width = 11, height = 5.5, dpi = 150)
cat("Saved: 12_seasonality_2025.png\n")

# ── 5. TRNC CPI vs R&H sub-index ────────────────────────────

p_cpi_comp <- panel |>
  filter(!is.na(cpi_trnc)) |>
  select(year, cpi_trnc, cpi_rh_trnc) |>
  pivot_longer(-year, names_to = "series") |>
  mutate(series = dplyr::recode(series, # Buraya dplyr:: ekledik
                                "cpi_trnc"    = "General CPI",
                                "cpi_rh_trnc" = "Restaurants & Hotels CPI"
  )) |>
  ggplot(aes(x = year, y = value, color = series)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("General CPI" = "#534AB7",
                                "Restaurants & Hotels CPI" = "#D85A30")) +
  scale_x_continuous(breaks = 2017:2024) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "TRNC Consumer Price Index: General vs Restaurants & Hotels, 2017–2024",
    subtitle = "Base year 2015 = 100 | R&H sub-index consistently above general CPI",
    x = NULL, y = "CPI index (2015 = 100)", color = NULL,
    caption  = "Source: TRNC Statistical Institute (TUFE_12_ANA_GRUP_ENDEKSLER)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

ggsave("output/figures/13_cpi_comparison.png", p_cpi_comp,
       width = 10, height = 5, dpi = 150)
cat("Saved: 13_cpi_comparison.png\n")

cat("\n--- Script 04 complete. ---\n")
