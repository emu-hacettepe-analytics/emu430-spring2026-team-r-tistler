# ============================================================
# EMU430 — TRNC Tourism Analytics
# Script 03: Regression Analysis
# ============================================================
# Three OLS models, all with log(arrivals) as DV.
# Interpretations follow each model.
# IMPORTANT: n=10 (or 8 with CPI). Small sample — report
# confidence intervals, not just p-values. No causal claims.
# ============================================================

library(tidyverse)
library(broom)
library(car)
library(scales)
library(patchwork)

panel <- read_csv("data/processed/trnc_panel.csv") |> 
  mutate(period = factor(period, levels = c("Pre-COVID", "COVID", "Recovery")))

# ── Helper: pretty regression table ──────────────────────────

fmt_model <- function(mod, label) {
  tidy(mod, conf.int = TRUE) |>
    mutate(
      model = label,
      sig   = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.10  ~ ".",
        TRUE             ~ ""
      ),
      across(c(estimate, std.error, conf.low, conf.high),
             ~ round(., 4))
    ) |>
    select(model, term, estimate, std.error, conf.low, conf.high,
           p.value, sig)
}

# ── Model A: Macro-only (2015–2024, n=10) ────────────────────
# log(arrivals) ~ log(EUR/TRY) + Turkey CPI + COVID
# Uses only variables available for all 10 years.

data_A <- panel |>
  select(year, log_arrivals, log_eur_try, cpi_turkey_yoy, covid) |>
  drop_na()

cat("=== MODEL A: log(arrivals) ~ log(EUR/TRY) + Turkey CPI YoY + COVID ===\n")
cat("n =", nrow(data_A), "| Years:", paste(data_A$year, collapse = ", "), "\n\n")

mod_A <- lm(log_arrivals ~ log_eur_try + cpi_turkey_yoy + covid,
            data = data_A)
print(summary(mod_A))

# ── Model B: TRNC CPI (2017–2024, n=8) ───────────────────────
# log(arrivals) ~ log(TRNC CPI) + log(EUR/TRY) + COVID
# More precise price measure but shorter window.

data_B <- panel |>
  select(year, log_arrivals, log_cpi_trnc, log_cpi_rh,
         log_eur_try, cpi_turkey_yoy, covid) |>
  drop_na()

cat("\n=== MODEL B: log(arrivals) ~ log(TRNC CPI) + log(EUR/TRY) + COVID ===\n")
cat("n =", nrow(data_B), "| Years:", paste(data_B$year, collapse = ", "), "\n\n")

mod_B <- lm(log_arrivals ~ log_cpi_trnc + log_eur_try + covid,
            data = data_B)
print(summary(mod_B))

cat("\nVIF (Model B):\n")
tryCatch(print(vif(mod_B)),
         error = function(e) cat("VIF not computable:", e$message, "\n"))

# ── Model C: R&H CPI (2017–2024, n=8) ────────────────────────
# Uses the Restaurants & Hotels CPI sub-index — most relevant
# hospitality-specific price variable.

cat("\n=== MODEL C: log(arrivals) ~ log(R&H CPI) + log(EUR/TRY) + COVID ===\n")
cat("n =", nrow(data_B), "\n\n")

mod_C <- lm(log_arrivals ~ log_cpi_rh + log_eur_try + covid,
            data = data_B)
print(summary(mod_C))

# ── Robustness: exclude COVID years ──────────────────────────

data_noCOV <- panel |>
  filter(covid == 0) |>
  select(year, log_arrivals, log_eur_try, cpi_turkey_yoy) |>
  drop_na()

cat("\n=== MODEL D: Non-COVID years only — log(arrivals) ~ log(EUR/TRY) + Turkey CPI ===\n")
cat("n =", nrow(data_noCOV), "| Years:", paste(data_noCOV$year, collapse = ", "), "\n\n")

mod_D <- lm(log_arrivals ~ log_eur_try + cpi_turkey_yoy,
            data = data_noCOV)
print(summary(mod_D))

# ── Tidy results table ────────────────────────────────────────

all_coefs <- bind_rows(
  fmt_model(mod_A, "A: Macro (n=10)"),
  fmt_model(mod_B, "B: TRNC CPI (n=8)"),
  fmt_model(mod_C, "C: R&H CPI (n=8)"),
  fmt_model(mod_D, "D: No-COVID (n=8)")
)

write_csv(all_coefs, "output/tables/regression_coefficients.csv")
cat("\nSaved: output/tables/regression_coefficients.csv\n")

# Model fit summary
fit_summary <- bind_rows(
  glance(mod_A) |> mutate(model = "A: Macro (n=10)"),
  glance(mod_B) |> mutate(model = "B: TRNC CPI (n=8)"),
  glance(mod_C) |> mutate(model = "C: R&H CPI (n=8)"),
  glance(mod_D) |> mutate(model = "D: No-COVID (n=8)")
) |>
  select(model, r.squared, adj.r.squared, sigma, AIC, BIC, nobs)

write_csv(fit_summary, "output/tables/model_fit.csv")
cat("Saved: output/tables/model_fit.csv\n")

print(fit_summary)

# ── Coefficient plot (Models A, B, C) ────────────────────────

coef_plot_data <- all_coefs |>
  filter(
    model != "D: No-COVID (n=8)",
    term != "(Intercept)"
  ) |>
  mutate(
    # Burada dplyr:: ekleyerek çakışmayı önlüyoruz
    term = dplyr::recode(term,
                         "log_eur_try"    = "log(EUR/TRY)",
                         "cpi_turkey_yoy" = "Turkey CPI YoY%",
                         "log_cpi_trnc"   = "log(TRNC CPI)",
                         "log_cpi_rh"     = "log(TRNC R&H CPI)",
                         "covid"          = "COVID dummy"
    )
  )

p_coef <- ggplot(coef_plot_data,
                 aes(x = estimate, y = term, color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = position_dodge(width = 0.55), size = 3.5) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.2,
    position = position_dodge(width = 0.55),
    linewidth = 0.8
  ) +
  scale_color_manual(values = c(
    "A: Macro (n=10)"   = "#1D9E75",
    "B: TRNC CPI (n=8)" = "#534AB7",
    "C: R&H CPI (n=8)"  = "#BA7517"
  )) +
  labs(
    title    = "OLS Regression Coefficients with 95% Confidence Intervals",
    subtitle = "Dependent variable: log(tourist arrivals) | Estimates > 0 = positive association",
    x = "Coefficient estimate", y = NULL, color = "Model:",
    caption  = "Note: Wide CIs reflect small sample size (n ≤ 10). Interpret as associations only."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray35"),
    legend.position = "bottom"
  )

ggsave("output/figures/09_coefficient_plot.png", p_coef,
       width = 10, height = 5.5, dpi = 150)
cat("Saved: 09_coefficient_plot.png\n")

# ── Residual diagnostics (Model A) ───────────────────────────

aug_A <- augment(mod_A, data = data_A)

p_res1 <- ggplot(aug_A, aes(.fitted, .resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(color = "#534AB7", size = 3) +
  geom_text_repel(aes(label = year), size = 3, color = "gray40") +
  geom_smooth(method = "loess", se = FALSE, color = "#D85A30",
              linewidth = 0.8) +
  labs(title = "Residuals vs Fitted (Model A)",
       x = "Fitted values", y = "Residuals") +
  theme_minimal(base_size = 12)

p_res2 <- ggplot(aug_A, aes(sample = .resid)) +
  stat_qq(color = "#534AB7", size = 2.5) +
  stat_qq_line(color = "#D85A30", linewidth = 0.9) +
  labs(title = "Q-Q Plot of Residuals (Model A)",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_minimal(base_size = 12)

p_diag <- p_res1 + p_res2
ggsave("output/figures/10_diagnostics.png", p_diag,
       width = 11, height = 5, dpi = 150)
cat("Saved: 10_diagnostics.png\n")

# Shapiro-Wilk test
sw <- shapiro.test(aug_A$.resid)
cat(sprintf("\nShapiro-Wilk test on Model A residuals: W = %.4f, p = %.4f\n",
            sw$statistic, sw$p.value))
if (sw$p.value < 0.05) {
  cat("⚠ Residuals may deviate from normality (p < 0.05).\n")
  cat("  With n=10 this test has very low power — inspect Q-Q plot.\n")
} else {
  cat("✓ No strong evidence against normality at α=0.05.\n")
}

cat("\n--- Script 03 complete. ---\n")
