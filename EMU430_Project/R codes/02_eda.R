# ============================================================
# EMU430 — TRNC Tourism Analytics
# Script 02: Exploratory Data Analysis — All Plots
# ============================================================

library(tidyverse)
library(scales)
library(patchwork)
library(corrplot)
library(ggrepel)

panel <- read_csv("data/processed/trnc_panel.csv") |> 
  mutate(period = factor(period, levels = c("Pre-COVID", "COVID", "Recovery")))

# ── Shared theme ──────────────────────────────────────────────

th <- theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "gray35", size = 11),
    plot.caption    = element_text(color = "gray50", size = 9, hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

# Color palette
C_GREEN  <- "#1D9E75"
C_CORAL  <- "#D85A30"
C_PURPLE <- "#534AB7"
C_AMBER  <- "#BA7517"
C_BLUE   <- "#185FA5"
C_GRAY   <- "#5F5E5A"

# COVID shading — reusable
covid_shade <- list(
  annotate("rect", xmin = 2019.5, xmax = 2021.5,
           ymin = -Inf, ymax = Inf,
           fill = "firebrick", alpha = 0.07),
  annotate("text", x = 2020.5, y = Inf,
           label = "COVID-19", vjust = 1.5, size = 3,
           color = "firebrick", fontface = "italic")
)

# ── Plot 1: Tourist arrivals trend ────────────────────────────

p1 <- ggplot(panel, aes(x = year, y = arrivals_total)) +
  covid_shade +
  geom_area(fill = C_GREEN, alpha = 0.12) +
  geom_line(color = C_GREEN, linewidth = 1.2) +
  geom_point(color = C_GREEN, size = 3, fill = "white", shape = 21,
             stroke = 1.5) +
  geom_text_repel(aes(label = comma(arrivals_total, scale = 1e-6,
                                     suffix = "M", accuracy = 0.01)),
                  size = 3, color = "gray30", nudge_y = 60000) +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(labels = comma, limits = c(0, 3e6)) +
  labs(
    title    = "Tourist Arrivals to Northern Cyprus, 2015–2024",
    subtitle = "Total foreign visitors entering TRNC via air and sea ports",
    x = NULL, y = "Total arrivals",
    caption  = "Source: TRNC Tourism Planning Dept. Statistical Yearbook 2024"
  ) + th

ggsave("output/figures/01_arrivals_trend.png", p1,
       width = 10, height = 5.5, dpi = 150)
cat("Saved: 01_arrivals_trend.png\n")

# ── Plot 2: YoY growth rate ───────────────────────────────────

p2 <- panel |>
  filter(!is.na(arrivals_yoy)) |>
  ggplot(aes(x = year, y = arrivals_yoy,
             fill = arrivals_yoy >= 0)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(arrivals_yoy, 1), "%"),
                vjust = ifelse(arrivals_yoy >= 0, -0.4, 1.4)),
            size = 3.2) +
  scale_fill_manual(values = c("TRUE" = C_GREEN, "FALSE" = C_CORAL),
                    guide = "none") +
  scale_x_continuous(breaks = 2016:2024) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Year-over-Year Growth in Tourist Arrivals, 2016–2024",
    subtitle = "Green = positive growth | Red = decline",
    x = NULL, y = "YoY change (%)",
    caption  = "Source: TRNC Tourism Planning Dept. Statistical Yearbook 2024"
  ) + th

ggsave("output/figures/02_arrivals_yoy.png", p2,
       width = 10, height = 5, dpi = 150)
cat("Saved: 02_arrivals_yoy.png\n")

# ── Plot 3: Arrivals vs TRNC CPI (dual-axis standardised) ─────

p3_data <- panel |>
  filter(!is.na(cpi_trnc)) |>
  mutate(
    z_arrivals = as.numeric(scale(arrivals_total)),
    z_cpi      = as.numeric(scale(cpi_trnc))
  )

p3 <- ggplot(p3_data, aes(x = year)) +
  covid_shade +
  geom_line(aes(y = z_arrivals, color = "Tourist arrivals"), linewidth = 1.2) +
  geom_point(aes(y = z_arrivals, color = "Tourist arrivals"), size = 2.5) +
  geom_line(aes(y = z_cpi, color = "TRNC CPI (general)"),
            linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = z_cpi, color = "TRNC CPI (general)"), size = 2.5) +
  scale_color_manual(values = c("Tourist arrivals" = C_GREEN,
                                "TRNC CPI (general)" = C_CORAL)) +
  scale_x_continuous(breaks = 2017:2024) +
  labs(
    title    = "Tourist Arrivals vs TRNC Inflation: Standardised Comparison, 2017–2024",
    subtitle = "Both variables expressed as z-scores (mean = 0, SD = 1) for visual comparison",
    x = NULL, y = "Z-score", color = NULL,
    caption  = "Sources: TRNC Tourism Planning Dept.; TRNC Statistical Institute"
  ) + th

ggsave("output/figures/03_arrivals_vs_cpi.png", p3,
       width = 10, height = 5, dpi = 150)
cat("Saved: 03_arrivals_vs_cpi.png\n")

# ── Plot 4: EUR/TRY over time ─────────────────────────────────

p4 <- ggplot(panel, aes(x = year, y = eur_try)) +
  covid_shade +
  geom_area(fill = C_PURPLE, alpha = 0.10) +
  geom_line(aes(linetype = fx_conf), color = C_PURPLE, linewidth = 1.1) +
  geom_point(color = C_PURPLE, size = 2.5) +
  geom_text_repel(aes(label = round(eur_try, 1)), size = 3,
                  color = "gray30", nudge_y = 0.8) +
  scale_x_continuous(breaks = 2015:2024) +
  scale_linetype_manual(values = c("EVDS (exact)" = "solid",
                                   "World Bank (approx)" = "dashed")) +
  labs(
    title    = "EUR/TRY Annual Average Exchange Rate, 2015–2024",
    subtitle = "Rising values = TRY depreciation → TRNC becomes cheaper for European visitors",
    x = NULL, y = "EUR / TRY",
    linetype = "Data source:",
    caption  = "Sources: TCMB EVDS (2020–2024); World Bank WDI (2015–2019, approximate)"
  ) + th

ggsave("output/figures/04_eur_try.png", p4,
       width = 10, height = 5, dpi = 150)
cat("Saved: 04_eur_try.png\n")

# ── Plot 5: Occupancy rate ────────────────────────────────────

p5 <- ggplot(panel, aes(x = year, y = occupancy_pct, fill = period)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(occupancy_pct, "%")),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("Pre-COVID" = C_GREEN,
                                "COVID"    = C_CORAL,
                                "Recovery" = C_AMBER)) +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(limits = c(0, 75)) +
  labs(
    title    = "Hotel Occupancy Rate in TRNC, 2015–2024",
    subtitle = "Annual average accommodation occupancy (%)",
    x = NULL, y = "Occupancy rate (%)", fill = "Period:",
    caption  = "Source: TRNC Tourism Planning Dept. Statistical Yearbook 2024"
  ) + th

ggsave("output/figures/05_occupancy.png", p5,
       width = 10, height = 5, dpi = 150)
cat("Saved: 05_occupancy.png\n")

# ── Plot 6: Average length of stay ───────────────────────────

p6 <- ggplot(panel, aes(x = year, y = avg_stay_days, color = period)) +
  covid_shade +
  geom_line(color = C_BLUE, linewidth = 1.1) +
  geom_point(aes(color = period), size = 4) +
  geom_text(aes(label = avg_stay_days), vjust = -1.2, size = 3.5) +
  scale_color_manual(values = c("Pre-COVID" = C_GREEN,
                                 "COVID"    = C_CORAL,
                                 "Recovery" = C_AMBER)) +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(limits = c(2.5, 4.5)) +
  labs(
    title    = "Average Length of Tourist Stay in TRNC, 2015–2024",
    subtitle = "Mean nights per visiting tourist",
    x = NULL, y = "Average stay (nights)", color = "Period:",
    caption  = "Source: TRNC Tourism Planning Dept. Statistical Yearbook 2024"
  ) + th

ggsave("output/figures/06_avg_stay.png", p6,
       width = 10, height = 5, dpi = 150)
cat("Saved: 06_avg_stay.png\n")

# ── Plot 7: Scatter — log(arrivals) vs log(EUR/TRY) ──────────

p7 <- ggplot(panel, aes(x = log_eur_try, y = log_arrivals,
                         color = period, label = year)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "gray60", linewidth = 0.7, linetype = "dashed",
              inherit.aes = FALSE,
              aes(x = log_eur_try, y = log_arrivals)) +
  geom_point(size = 4) +
  geom_text_repel(size = 3.5, fontface = "bold") +
  scale_color_manual(values = c("Pre-COVID" = C_GREEN,
                                 "COVID"    = C_CORAL,
                                 "Recovery" = C_AMBER)) +
  labs(
    title    = "Log Tourist Arrivals vs Log EUR/TRY, 2015–2024",
    subtitle = "Each point = one year | Dashed line = OLS fit across all years",
    x = "log(EUR/TRY annual average)",
    y = "log(Tourist arrivals)",
    color    = "Period:",
    caption  = "Sources: TRNC Tourism Planning Dept.; TCMB EVDS / World Bank"
  ) + th

ggsave("output/figures/07_scatter_arrivals_fx.png", p7,
       width = 8, height = 6, dpi = 150)
cat("Saved: 07_scatter_arrivals_fx.png\n")

# ── Plot 8: Correlation heatmap ───────────────────────────────

cor_data <- panel |>
  select(
    `Tourist arrivals`  = arrivals_total,
    `Overnight stays`   = bednights,
    `Occupancy (%)`     = occupancy_pct,
    `Avg stay (days)`   = avg_stay_days,
    `TRNC CPI`          = cpi_trnc,
    `TRNC R&H CPI`      = cpi_rh_trnc,
    `Turkey CPI YoY%`   = cpi_turkey_yoy,
    `EUR/TRY`           = eur_try,
    `USD/TRY`           = usd_try
  ) |>
  drop_na()

cor_mat <- cor(cor_data, use = "complete.obs", method = "pearson")

png("output/figures/08_correlation.png", width = 1000, height = 900, res = 130)
corrplot(
  cor_mat,
  method      = "color",
  type        = "upper",
  tl.col      = "black",
  tl.cex      = 0.82,
  addCoef.col = "black",
  number.cex  = 0.72,
  number.digits = 2,
  col         = colorRampPalette(c(C_CORAL, "white", C_GREEN))(200),
  title       = "Pairwise Pearson Correlation Matrix",
  mar         = c(0, 0, 2, 0)
)
dev.off()
cat("Saved: 08_correlation.png\n")

# ── Plot 9: Patchwork overview dashboard ──────────────────────

p_dash <- (p1 + p5) / (p3 + p4) +
  plot_annotation(
    title   = "TRNC Tourism: Key Indicators Dashboard",
    subtitle = "Data: TRNC Tourism Planning Dept. | TRNC Statistical Institute | TCMB EVDS",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

ggsave("output/figures/00_dashboard.png", p_dash,
       width = 14, height = 10, dpi = 150)
cat("Saved: 00_dashboard.png\n")

cat("\n--- Script 02 complete. All plots saved to output/figures/ ---\n")
