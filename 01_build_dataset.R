# ============================================================
# EMU430 — TRNC Tourism Analytics
# Script 01: Dataset Construction
# ============================================================
library(tidyverse)

# 0. Otomatik Klasör Oluşturma (Hata almamak için şart)
dirs <- c("data/processed", "output/figures", "output/tables", "report")
walk(dirs, ~if(!dir.exists(.)) dir.create(., recursive = TRUE))

# ── 1. Tourism variables (TRNC TPD Yearbook 2024) ─
tourism <- tribble(
  ~year, ~arrivals_total, ~arrivals_air, ~arrivals_sea, ~air_share_pct, ~sea_share_pct, ~accom_persons, ~bednights, ~avg_stay_days, ~occupancy_pct,
  2015, 1773965, 1693694,  80271, 95.5, 4.5,  876041, 3167166, 3.6, 48.8,
  2016, 1862558, 1785746,  76812, 95.9, 4.1,  924122, 3416846, 3.7, 48.6,
  2017, 2045014, 1945371,  99643, 95.1, 4.9, 1114973, 4223862, 3.8, 58.5,
  2018, 2079961, 1968345, 111616, 94.6, 5.4, 1143206, 4291944, 3.8, 51.3,
  2019, 2068992, 1973844,  95148, 95.4, 4.6, 1149714, 4456543, 3.9, 50.4,
  2020,  467647,  426545,  41102, 91.2, 8.8,  265325,  786609, 3.0, 17.0,
  2021,  638151,  575044,  63107, 90.1, 9.9,  401516, 1296205, 3.2, 26.8,
  2022, 1634560, 1461733, 172827, 89.4, 10.6, 1030458, 3747029, 3.6, 42.7,
  2023, 2122273, 1943554, 178719, 91.6, 8.4, 1241993, 4133839, 3.3, 44.4,
  2024, 2556605, 2369796, 186809, 92.7, 7.3, 1365511, 4284667, 3.1, 43.6
)

# ── 2. TRNC CPI & 3. Turkey CPI & 4. FX (Kısa tutulmuştur, senin verilerinle aynıdır)
cpi_trnc <- tribble(
  ~year, ~cpi_trnc, ~cpi_rh_trnc, ~cpi_trnc_yoy,
  2015, NA, NA, NA, 2016, NA, NA, NA, 2017, 122.81, 136.47, NA,
  2018, 151.23, 163.40, 23.15, 2019, 180.68, 192.13, 19.47, 2020, 201.89, 209.58, 11.74,
  2021, 245.14, 255.24, 21.42, 2022, 489.52, 571.28, 99.69, 2023, 863.70, 1156.85, 76.44, 2024, 1502.52, 2107.67, 73.96
)

cpi_turkey <- tribble(
  ~year, ~cpi_turkey_yoy,
  2015, 7.67, 2016, 7.78, 2017, 11.14, 2018, 16.33, 2019, 15.17, 2020, 12.28, 2021, 19.60, 2022, 72.31, 2023, 53.86, 2024, 58.51
)

fx <- tribble(
  ~year, ~eur_try, ~usd_try, ~fx_source,
  2015, 3.0213, 2.7203, "WB", 2016, 3.3372, 3.0208, "WB", 2017, 4.1204, 3.6464, "WB",
  2018, 5.7069, 4.8294, "WB", 2019, 6.3565, 5.6742, "WB", 2020, 8.0285, 7.0181, "EVDS",
  2021, 10.4348, 8.8476, "EVDS", 2022, 17.3864, 16.5660, "EVDS", 2023, 25.6722, 23.7360, "EVDS", 2024, 35.5376, 32.8392, "EVDS"
)

# ── 6. Merge & Full Calculation (BURASI EKSİKTİ, TAMAMLANDI) ──
panel <- tourism |>
  left_join(cpi_trnc,   by = "year") |>
  left_join(cpi_turkey, by = "year") |>
  left_join(fx,         by = "year") |>
  mutate(
    covid = as.integer(year %in% c(2020L, 2021L)),
    period = case_when(
      year < 2020 ~ "Pre-COVID",
      year <= 2021 ~ "COVID",
      TRUE ~ "Recovery"
    ) |> factor(levels = c("Pre-COVID", "COVID", "Recovery")),
    
    fx_conf = if_else(year >= 2020, "EVDS (exact)", "World Bank (approx)"),
    
    # Büyüme oranları (Lag fonksiyonu için dplyr gerekli)
    arrivals_yoy = 100 * (arrivals_total / lag(arrivals_total) - 1),
    bednights_yoy = 100 * (bednights / lag(bednights) - 1),
    
    # Regresyon için Logaritmik dönüşümler (Script 03 için şart!)
    log_arrivals   = log(arrivals_total),
    log_bednights  = log(bednights),
    log_eur_try    = log(eur_try),
    log_cpi_trnc   = log(cpi_trnc),
    log_cpi_rh     = log(cpi_rh_trnc)
  )

write_csv(panel, "data/processed/trnc_panel.csv")
cat("\n[Script 01 Başarılı] Veri seti data/processed/trnc_panel.csv olarak kaydedildi.\n")
# Yönerge gereği hem csv hem de RData olarak kaydediyoruz 
write_csv(panel, "data/processed/trnc_panel.csv")
save(panel, file = "data/processed/trnc_tourism.RData")

cat("\n[Success] Dataset saved as .csv and .RData in data/processed/\n")