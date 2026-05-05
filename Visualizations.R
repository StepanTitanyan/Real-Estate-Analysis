# ================================================================
# When Listings Go Silent: Testing Whether Inactivity Indicates
# Property Sales — Yerevan Residential Real Estate, 2024–2025
# ================================================================
# REVISED — adds:
#   Fig 20–21: District choropleth maps (price + inactivity rate)
#   Fig 22:    Portal vs. cadastre normalized comparison
#   Fig 23–24: Price trajectory analysis (professor feedback)
#   Fig 25:    District price trends (small multiples)
#
# ── INSTALL ALL REQUIRED PACKAGES (run once) ────────────────
# install.packages(c(
#   "tidyverse",   # ggplot2, dplyr, tidyr, readr, purrr, stringr
#   "lubridate",   # date arithmetic
#   "readxl",      # read .xlsx price index
#   "scales",      # axis label formatting
#   "patchwork",   # combine multiple ggplots
#   "ggrepel",     # non-overlapping text labels
#   "broom",       # tidy OLS output
#   "sf",          # read shapefiles and spatial joins
#   "zoo"          # na.approx() and rollmedian() for Fig 25
# ))
#
# ── EXPECTED FOLDER STRUCTURE ────────────────────────────
# project/
#   data/
#     [sale listings .csv]         <- Armenian filename
#     [rental listings .csv]       <- Armenian filename
#     [price index .xlsx]          <- Armenian filename
#   shapes/
#     Yerevan-Districts.shp
#     Yerevan-Districts.dbf
#     Yerevan-Districts.prj
#     Yerevan-Districts.shx
#     Yerevan-Districts.cpg
#   figures/                       <- created automatically on run
#   yerevan_listing_inactivity.R   <- this script
#
# Set working directory to project/ root, then source this file.
# All 25 figures saved to ./figures/ at 300 dpi.
# ================================================================


# 0. PACKAGES & GLOBAL SETTINGS ──────────────────────────────────
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(scales)
  library(patchwork)
  library(ggrepel)
  library(broom)
  library(sf)
  library(zoo)
})

dir.create("figures", showWarnings = FALSE)

REF_DATE   <- as.Date("2025-12-31")
THRESHOLDS <- c(90, 120, 150, 180)

# ── Color constants ──────────────────────────────────────────────
COL_APT       <- "#2166ac"
COL_HOUSE     <- "#d6604d"
COL_SILENT    <- "#762a83"
COL_ACTIVE    <- "#4dac26"
COL_PRICE_IDX <- "#1b7837"
COL_PORTAL    <- "#e67e22"
COL_CADASTRE  <- "#2980b9"
THRESH_COLORS <- c("90"="#66c2a5","120"="#fc8d62","150"="#8da0cb","180"="#e78ac3")
PREMIUM_COLORS <- c(
  "Below market  (< -10%)"     = "#1a9641",
  "At market  (+-10%)"         = "#a6d96a",
  "Slightly above  (+10-30%)"  = "#fdae61",
  "Far above market  (> +30%)" = "#d7191c"
)

# ── 10% trimmed mean (matches Python visualizer exactly) ─────────
trimmed_mean_10 <- function(x) {
  x <- sort(x[!is.na(x) & is.finite(x)])
  n <- length(x)
  if (n == 0L) return(NA_real_)
  k <- floor(n * 0.1)
  if (n - 2L * k <= 0L) return(NA_real_)
  mean(x[(k + 1L):(n - k)])
}

# ── Publication theme ─────────────────────────────────────────────
theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", size = base_size + 1, hjust = 0),
      plot.subtitle = element_text(color = "grey40", size = base_size - 1,
                                   hjust = 0, margin = margin(b = 6)),
      plot.caption  = element_text(color = "grey55", size = base_size - 2,
                                   hjust = 0, margin = margin(t = 8)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3, color = "grey88"),
      axis.text     = element_text(color = "grey30"),
      axis.title    = element_text(color = "grey20"),
      strip.text    = element_text(face = "bold"),
      legend.position = "bottom",
      plot.margin   = margin(12, 14, 8, 12)
    )
}
theme_set(theme_pub())

save_fig <- function(p, name, w = 9, h = 5) {
  ggsave(file.path("figures", paste0(name, ".png")),
         plot = p, width = w, height = h, dpi = 300, bg = "white")
  message("  saved: figures/", name, ".png")
  invisible(p)
}

# ── Armenian to English district name dictionary ──────────────────
DIST_MAP <- c(
  "Ajapnyak"         = "Ajapnyak",    # fallback for already-English
  "Arabkir"          = "Arabkir",
  "Avan"             = "Avan",
  "Davtashen"        = "Davtashen",
  "Erebuni"          = "Erebuni",
  "Kentron"          = "Kentron",
  "Nor Nork"         = "Nor Nork",
  "Nork-Marash"      = "Nork-Marash",
  "Nubarashen"       = "Nubarashen",
  "Shengavit"        = "Shengavit",
  # Armenian names
  "Ajapnyak"             = "Ajapnyak",
  "\u0531\u057b\u0561\u0583\u0576\u0575\u0561\u056f"       = "Ajapnyak",
  "\u0531\u057e\u0561\u576"                                 = "Avan",
  "\u0531\u0580\u0561\u0562\u056f\u056b\u0580"             = "Arabkir",
  "\u0534\u0561\u057e\u0569\u0561\u577\u0565\u576"         = "Davtashen",
  "\u0535\u0580\u0587\u0578\u0582\u576\u056b"              = "Erebuni",
  "\u0554\u0561\u576\u0561\u584\u0565\u057c \u0566\u0587\u056f\u0578\u0582\u576" = "Kanaker-Zeytun",
  "\u053f\u0565\u576\u057f\u0580\u0578\u576"               = "Kentron",
  "\u0544\u0561\u056c\u0561\u0569\u056b\u0561 \u054d\u0565\u0562\u0561\u057d\u057f\u056b\u0561" = "Malatia-Sebastia",
  "\u0546\u0578\u0580 \u0546\u0578\u0580\u584"             = "Nor Nork",
  "\u0546\u0578\u0580\u584 \u0544\u0561\u0580\u0561\u577"  = "Nork-Marash",
  "\u0546\u0578\u582\u0562\u0561\u0580\u0561\u577\u0565\u576" = "Nubarashen",
  "\u0547\u0565\u576\u563\u0561\u057e\u056b\u0569"         = "Shengavit",
  "\u054e\u0561\u570\u0561\u563\u576\u056b \u0569\u0561\u572\u0561\u574\u0561\u057d" = "Vahagni"
)


# ================================================================
# 1. DATA LOADING & CLEANING
# ================================================================

message("\n1. Loading data ...")

# ── 1.1  Sale listings ───────────────────────────────────────────
raw_sale <- read_csv(
  file.path("data", "bnakaranev-tun_vajark.csv"),        # <- rename file if needed
  locale         = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

# The actual file name contains Armenian characters:
# tryCatch so the script runs even if only one path works
if (!exists("raw_sale") || nrow(raw_sale) == 0) {
  # Fallback: try the Armenian filename directly
  raw_sale <- read_csv(
    file.path("data", "\u0562\u0576\u0561\u056f\u0561\u0580\u0561\u576-\u0587-\u057f\u0578\u582\u576_\u057e\u0561\u0573\u0561\u057c\u056f.csv"),
    locale = locale(encoding = "UTF-8"), show_col_types = FALSE
  )
}

names(raw_sale) <- c("publish_raw","update_raw","region","district",
                     "price_amd_raw","price_usd_raw","area_bldg","area_land",
                     "price_sqm_raw","rooms_raw","floor_raw","type_arm","link")

sale <- raw_sale %>%
  mutate(
    publish_date  = dmy(publish_raw),
    update_date   = dmy(update_raw),
    last_active   = coalesce(update_date, publish_date),
    price_usd     = as.numeric(price_usd_raw),
    price_amd     = as.numeric(price_amd_raw),
    price_sqm     = as.numeric(price_sqm_raw),
    area_bldg     = as.numeric(area_bldg),
    rooms         = suppressWarnings(as.numeric(gsub("=","",rooms_raw))),
    floor_curr    = suppressWarnings(as.numeric(sub("/.*","",gsub("=","",floor_raw)))),
    floor_tot     = suppressWarnings(as.numeric(sub(".*/","",gsub("=","",floor_raw)))),
    type = case_when(
      type_arm == "\u0532\u576\u0561\u056f\u0561\u0580\u0561\u576"    ~ "Apartment",
      type_arm == "\u0531\u057c\u0561\u576\u0571\u576\u0561\u057f\u0578\u582\u576" ~ "House",
      TRUE ~ NA_character_
    ),
    days_inactive = as.integer(REF_DATE - last_active),
    publish_ym    = floor_date(publish_date, "month"),
    active_ym     = floor_date(last_active,  "month")
  ) %>%
  filter(
    region        == "\u0535\u0580\u0587\u0561\u576",   # Yerevan
    !is.na(price_usd), price_usd > 0,
    area_bldg     > 0,
    days_inactive >= 0
  ) %>%
  filter(price_usd < quantile(price_usd, 0.995, na.rm = TRUE)) %>%
  mutate(
    district_en = case_when(
      district == "\u0531\u057b\u0561\u0583\u0576\u0575\u0561\u056f"       ~ "Ajapnyak",
      district == "\u0531\u057e\u0561\u576"                                 ~ "Avan",
      district == "\u0531\u0580\u0561\u0562\u056f\u056b\u0580"             ~ "Arabkir",
      district == "\u0534\u0561\u057e\u0569\u0561\u577\u0565\u576"         ~ "Davtashen",
      district == "\u0535\u0580\u0587\u0578\u0582\u576\u056b"              ~ "Erebuni",
      district == "\u0554\u0561\u576\u0561\u584\u0565\u057c \u0566\u0587\u056f\u0578\u0582\u576" ~ "Kanaker-Zeytun",
      district == "\u053f\u0565\u576\u057f\u0580\u0578\u576"               ~ "Kentron",
      district == "\u0544\u0561\u056c\u0561\u0569\u056b\u0561 \u054d\u0565\u0562\u0561\u057d\u057f\u056b\u0561" ~ "Malatia-Sebastia",
      district == "\u0546\u0578\u0580 \u0546\u0578\u0580\u584"             ~ "Nor Nork",
      district == "\u0546\u0578\u0580\u584 \u0544\u0561\u0580\u0561\u577"  ~ "Nork-Marash",
      district == "\u0546\u0578\u582\u0562\u0561\u0580\u0561\u577\u0565\u576" ~ "Nubarashen",
      district == "\u0547\u0565\u576\u563\u0561\u057e\u056b\u0569"         ~ "Shengavit",
      district == "\u054e\u0561\u570\u0561\u563\u576\u056b \u0569\u0561\u572\u0561\u574\u0561\u057d" ~ "Vahagni",
      TRUE ~ district
    )
  )

# ── 1.2  Rental listings ─────────────────────────────────────────
raw_rent <- read_csv(
  file.path("data", "\u0562\u0576\u0561\u056f\u0561\u0580\u0561\u576-\u0587-\u057f\u0578\u582\u576_\u057e\u0561\u0580\u056e\u0561\u056f\u0561\u056c\u0578\u582\u0569\u0575\u0578\u0582\u576.csv"),
  locale = locale(encoding = "UTF-8"), show_col_types = FALSE
)
names(raw_rent) <- c("publish_raw","update_raw","region","district",
                     "price_amd_raw","price_usd_raw","area_bldg","area_land",
                     "price_sqm_raw","rooms_raw","floor_raw","type_arm","link")

rent <- raw_rent %>%
  mutate(
    publish_date = dmy(publish_raw),
    price_sqm    = as.numeric(price_sqm_raw),
    area_bldg    = as.numeric(area_bldg),
    type = case_when(
      type_arm == "\u0532\u576\u0561\u056f\u0561\u0580\u0561\u576"    ~ "Apartment",
      type_arm == "\u0531\u057c\u0561\u576\u0571\u576\u0561\u057f\u0578\u582\u576" ~ "House",
      TRUE ~ NA_character_
    ),
    publish_ym = floor_date(publish_date, "month")
  ) %>%
  filter(region == "\u0535\u0580\u0587\u0561\u576",
         !is.na(price_sqm), price_sqm > 0, area_bldg > 0) %>%
  mutate(
    district_en = case_when(
      district == "\u0531\u057b\u0561\u0583\u0576\u0575\u0561\u056f"       ~ "Ajapnyak",
      district == "\u0531\u057e\u0561\u576"                                 ~ "Avan",
      district == "\u0531\u0580\u0561\u0562\u056f\u056b\u0580"             ~ "Arabkir",
      district == "\u0534\u0561\u057e\u0569\u0561\u577\u0565\u576"         ~ "Davtashen",
      district == "\u0535\u0580\u0587\u0578\u0582\u576\u056b"              ~ "Erebuni",
      district == "\u0554\u0561\u576\u0561\u584\u0565\u057c \u0566\u0587\u056f\u0578\u0582\u576" ~ "Kanaker-Zeytun",
      district == "\u053f\u0565\u576\u057f\u0580\u0578\u576"               ~ "Kentron",
      district == "\u0544\u0561\u056c\u0561\u0569\u056b\u0561 \u054d\u0565\u0562\u0561\u057d\u057f\u056b\u0561" ~ "Malatia-Sebastia",
      district == "\u0546\u0578\u0580 \u0546\u0578\u0580\u584"             ~ "Nor Nork",
      district == "\u0546\u0578\u0580\u584 \u0544\u0561\u0580\u0561\u577"  ~ "Nork-Marash",
      district == "\u0546\u0578\u582\u0562\u0561\u0580\u0561\u577\u0565\u576" ~ "Nubarashen",
      district == "\u0547\u0565\u576\u563\u0561\u057e\u056b\u0569"         ~ "Shengavit",
      district == "\u054e\u0561\u570\u0561\u563\u576\u056b \u0569\u0561\u572\u0561\u574\u0561\u057d" ~ "Vahagni",
      TRUE ~ district
    )
  )

# ── 1.3  Price index ─────────────────────────────────────────────
price_raw <- read_excel(
  file.path("data", "\u0532\u576\u0561\u056f\u0561\u0580\u0561\u576\u576\u0565\u0580\u056b_\u563\u576\u0565\u0580.xlsx"),
  sheet = "Yerevan"
) %>%
  select(date = 1, price_sqm_amd = 2) %>%
  mutate(date = as.Date(date), price_sqm_amd = as.numeric(price_sqm_amd)) %>%
  filter(!is.na(date), !is.na(price_sqm_amd)) %>%
  arrange(date)

price_idx <- price_raw %>%
  mutate(ym = floor_date(date, "month"),
         mom_pct = (price_sqm_amd / lag(price_sqm_amd) - 1) * 100) %>%
  filter(!is.na(mom_pct))

# ── 1.4  Shapefile ───────────────────────────────────────────────
# Name_hy = Armenian (exact match with listing district column)
# Name_en = English  (used for map labels)
districts_sf <- read_sf(file.path("shapes", "Yerevan-Districts.shp"))
suppressWarnings({
  ctrd <- st_coordinates(st_centroid(st_geometry(districts_sf)))
})
districts_sf$centroid_x <- ctrd[, 1]
districts_sf$centroid_y <- ctrd[, 2]

cat("Sale:", nrow(sale), "| Rent:", nrow(rent),
    "| Price index:", nrow(price_idx),
    "| Districts:", nrow(districts_sf), "\n")


# ================================================================
# 2. SHARED COMPUTATIONS
# ================================================================

message("2. Shared computations ...")

# ── Price premium (static: vs. whole-dataset peer group) ─────────
sale <- sale %>%
  group_by(district_en, type, rooms) %>%
  mutate(peer_median_sqm = median(price_sqm, na.rm = TRUE),
         price_premium   = (price_sqm - peer_median_sqm) / peer_median_sqm) %>%
  ungroup() %>%
  mutate(
    premium_bin = case_when(
      is.na(price_premium)   ~ NA_character_,
      price_premium < -0.10  ~ "Below market  (< -10%)",
      price_premium <=  0.10 ~ "At market  (+-10%)",
      price_premium <=  0.30 ~ "Slightly above  (+10-30%)",
      TRUE                   ~ "Far above market  (> +30%)"
    ),
    premium_bin = factor(premium_bin, levels = c(
      "Below market  (< -10%)", "At market  (+-10%)",
      "Slightly above  (+10-30%)", "Far above market  (> +30%)"
    ))
  )

# ── Monthly inactive series ───────────────────────────────────────
inactive_monthly <- map_dfr(THRESHOLDS, function(t) {
  cutoff <- floor_date(REF_DATE - t, "month")
  sale %>%
    filter(days_inactive >= t,
           active_ym >= as.Date("2024-01-01"),
           active_ym <= cutoff) %>%
    count(active_ym, name = "n") %>%
    mutate(threshold = as.character(t))
})

comp_data <- inactive_monthly %>%
  left_join(price_idx %>% select(ym, price_sqm_amd, mom_pct),
            by = c("active_ym" = "ym")) %>%
  filter(!is.na(mom_pct))

comp_120 <- filter(comp_data, threshold == "120")

# ── Price trajectory ─────────────────────────────────────────────
# monthly district median at time of listing publication
monthly_dist_med <- sale %>%
  filter(!is.na(price_sqm), price_sqm > 0) %>%
  group_by(publish_ym, district_en) %>%
  summarise(period_median = median(price_sqm, na.rm = TRUE),
            n_period = n(), .groups = "drop")

# current district median = last 3 months of available data
current_cutoff_ym <- floor_date(
  max(sale$publish_date, na.rm = TRUE) %m-% months(2), "month"
)
current_market <- sale %>%
  filter(!is.na(price_sqm), price_sqm > 0, publish_ym >= current_cutoff_ym) %>%
  group_by(district_en) %>%
  summarise(current_median = median(price_sqm, na.rm = TRUE),
            n_current = n(), .groups = "drop")

sale_trajectory <- sale %>%
  filter(!is.na(price_sqm), price_sqm > 0, !is.na(district_en)) %>%
  left_join(monthly_dist_med %>% filter(n_period >= 5),
            by = c("publish_ym","district_en")) %>%
  left_join(current_market %>% filter(n_current >= 10),
            by = "district_en") %>%
  mutate(
    premium_at_publish = (price_sqm - period_median)  / period_median  * 100,
    premium_now        = (price_sqm - current_median) / current_median * 100,
    premium_drift      = premium_now - premium_at_publish,
    status = if_else(days_inactive >= 120,
                     "Silent  (>= 120 days)",
                     "Recently active  (< 120 days)")
  ) %>%
  filter(!is.na(premium_at_publish), !is.na(premium_now),
         is.finite(premium_at_publish), is.finite(premium_now),
         abs(premium_at_publish) <= 150,
         abs(premium_now)        <= 150)


# ================================================================
# LAYER 1: MARKET COMPOSITION  (Figs 1-5)
# ================================================================

message("Layer 1: Market composition ...")

p1 <- sale %>%
  count(district_en, type) %>%
  group_by(district_en) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(district_en = fct_reorder(district_en, total)) %>%
  ggplot(aes(x = n, y = district_en, fill = type)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = c(Apartment = COL_APT, House = COL_HOUSE)) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .05))) +
  labs(title = "Fig. 1 — Residential sale listings by district",
       subtitle = "Arabkir and Kentron together account for nearly half of all listings",
       x = "Number of listings", y = NULL, fill = "Type",
       caption = "Source: senyak.am | Yerevan only, Jan 2024 - Dec 2025")
save_fig(p1, "fig01_district_counts")

p2 <- count(sale, type) %>%
  mutate(pct = n / sum(n),
         lbl = paste0(type, "\n", comma(n), "\n(", percent(pct, .1), ")")) %>%
  ggplot(aes(y = "", x = n, fill = type)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = lbl), position = position_stack(vjust = .5),
            color = "white", fontface = "bold", size = 4.2) +
  scale_fill_manual(values = c(Apartment = COL_APT, House = COL_HOUSE)) +
  scale_x_continuous(labels = comma) +
  labs(title = "Fig. 2 — Market composition by property type",
       subtitle = "Apartments constitute the overwhelming majority of sale listings",
       x = "Count", y = NULL, caption = "Source: senyak.am") +
  theme(axis.text.y = element_blank(), legend.position = "none")
save_fig(p2, "fig02_type_composition", w = 7, h = 3)

p3 <- sale %>% filter(price_usd <= 750000) %>%
  ggplot(aes(x = price_usd, fill = type, color = type)) +
  geom_histogram(bins = 60, alpha = .62, position = "identity") +
  scale_x_continuous(labels = label_dollar(scale = 1e-3, suffix = "k"),
                     breaks = seq(0, 750000, 150000)) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values  = c(Apartment = COL_APT, House = COL_HOUSE)) +
  scale_color_manual(values = c(Apartment = COL_APT, House = COL_HOUSE)) +
  labs(title = "Fig. 3 — Asking price distribution by property type",
       subtitle = "Both distributions are right-skewed; houses span a wider and higher range",
       x = "Asking price (USD)", y = "Listing count", fill = "Type", color = "Type",
       caption = "Source: senyak.am | Listings above $750k not shown")
save_fig(p3, "fig03_price_histogram", w = 10, h = 5)

p4 <- sale %>% filter(area_bldg <= 400, price_usd <= 600000) %>%
  ggplot(aes(x = area_bldg, y = price_usd, color = type)) +
  geom_point(alpha = .10, size = .7, shape = 16) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.4) +
  scale_y_continuous(labels = label_dollar(scale = 1e-3, suffix = "k")) +
  scale_color_manual(values = c(Apartment = COL_APT, House = COL_HOUSE)) +
  labs(title = "Fig. 4 — Building area vs asking price",
       subtitle = "Prices scale predictably with area; houses exhibit substantially greater dispersion",
       x = "Building area (m2)", y = "Price (USD)", color = "Type",
       caption = "Source: senyak.am | Listings above 400 m2 or $600k excluded")
save_fig(p4, "fig04_area_price_scatter", w = 9, h = 5)

p5 <- sale %>%
  filter(!is.na(price_sqm), price_sqm > 0) %>%
  group_by(district_en, type) %>%
  summarise(med = median(price_sqm, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n >= 20) %>%
  mutate(district_en = fct_reorder(district_en, med, .fun = max)) %>%
  ggplot(aes(x = med, y = district_en, color = type)) +
  geom_point(aes(size = n), alpha = .85) +
  scale_x_continuous(labels = label_number(big.mark = ",", suffix = " AMD")) +
  scale_color_manual(values = c(Apartment = COL_APT, House = COL_HOUSE)) +
  scale_size_continuous(range = c(2, 9), guide = "none") +
  labs(title = "Fig. 5 — Median asking price per m2 by district",
       subtitle = "Kentron commands a ~2x premium over the citywide median; point size = listing count",
       x = "Median price per m2 (AMD)", y = NULL, color = "Type",
       caption = "Source: senyak.am | Districts with < 20 listings per type excluded")
save_fig(p5, "fig05_district_psqm", w = 10, h = 5)


# ================================================================
# LAYER 2: TEMPORAL ACTIVITY  (Figs 6-7)
# ================================================================

message("Layer 2: Temporal activity ...")

p6 <- sale %>%
  count(publish_ym, type) %>%
  ggplot(aes(x = publish_ym, y = n, fill = type)) +
  geom_col(width = 27) +
  scale_fill_manual(values = c(Apartment = COL_APT, House = COL_HOUSE)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_y_continuous(labels = comma) +
  labs(title = "Fig. 6 — Monthly new listing publications",
       subtitle = "Listing inflow is relatively stable across the two-year window",
       x = NULL, y = "New listings published", fill = "Type",
       caption = "Source: senyak.am")
save_fig(p6, "fig06_monthly_published", w = 10, h = 5)

p7 <- sale %>%
  count(active_ym, type) %>%
  filter(active_ym >= as.Date("2024-01-01")) %>%
  ggplot(aes(x = active_ym, y = n, fill = type)) +
  geom_col(width = 27) +
  scale_fill_manual(values = c(Apartment = COL_APT, House = COL_HOUSE)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_y_continuous(labels = comma) +
  labs(title = "Fig. 7 — Month of last observed listing activity",
       subtitle = "Activity concentrated in H1 2025; drop in recent months reflects proximity to observation date",
       x = NULL, y = "Listings last active", fill = "Type",
       caption = "Source: senyak.am | Last activity = update date if available, else publish date")
save_fig(p7, "fig07_monthly_last_active", w = 10, h = 5)


# ================================================================
# LAYER 3: INACTIVITY MEASUREMENT  (Figs 8-12)
# ================================================================

message("Layer 3: Inactivity measurement ...")

p8 <- sale %>%
  filter(days_inactive <= 730) %>%
  ggplot(aes(x = days_inactive)) +
  geom_histogram(bins = 73, fill = "#4393c3", color = "white",
                 linewidth = .15, alpha = .88) +
  geom_vline(xintercept = 120, color = "#d73027",
             linewidth = 1.1, linetype = "dashed") +
  annotate("text", x = 126, y = Inf, label = "120-day\nthreshold",
           hjust = 0, vjust = 1.5, color = "#d73027", size = 3.5) +
  scale_x_continuous(breaks = c(0,30,60,90,120,180,270,365,540,730)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Fig. 8 — Distribution of days since last listing activity",
       subtitle = paste0("Pronounced right tail; many listings silent well beyond 120 days\n",
                         "Reference date: ", format(REF_DATE, "%B %d, %Y")),
       x = "Days since last activity", y = "Listing count",
       caption = "Source: senyak.am | Listings beyond 730 days not shown")
save_fig(p8, "fig08_days_inactive_hist", w = 10, h = 5)

p9 <- sale %>%
  mutate(bucket = case_when(
    days_inactive < 30  ~ "< 1 month",
    days_inactive < 90  ~ "1-3 months",
    days_inactive < 120 ~ "3-4 months",
    days_inactive < 180 ~ "4-6 months",
    TRUE                ~ "6+ months"
  ), bucket = factor(bucket, levels = c("< 1 month","1-3 months",
                                        "3-4 months","4-6 months","6+ months"))) %>%
  count(bucket) %>% mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = bucket, y = n, fill = bucket)) +
  geom_col(width = .72, show.legend = FALSE) +
  geom_text(aes(label = paste0(comma(n), "\n", percent(pct, .1))),
            vjust = -.2, size = 3.3) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0,.18))) +
  labs(title = "Fig. 9 — Listing inactivity by duration bucket",
       subtitle = "More than a third of all listings have been silent for six months or more",
       x = "Inactivity duration", y = "Listing count",
       caption = "Source: senyak.am")
save_fig(p9, "fig09_inactivity_buckets", w = 8, h = 5)

p10 <- sale %>%
  group_by(district_en) %>%
  summarise(n = n(), inactive = sum(days_inactive >= 120),
            rate = inactive / n, .groups = "drop") %>%
  filter(n >= 30) %>%
  mutate(district_en = fct_reorder(district_en, rate)) %>%
  ggplot(aes(x = rate, y = district_en, fill = rate)) +
  geom_col(width = .68) +
  geom_text(aes(label = paste0(percent(rate, .1), "   (n = ", comma(n), ")")),
            hjust = -.05, size = 3) +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0,.22))) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", guide = "none") +
  labs(title = "Fig. 10 — District-level inactivity rate (>= 120-day threshold)",
       subtitle = "Peripheral districts tend to show higher inactivity rates than central ones",
       x = "Share of listings silent for 120+ days", y = NULL,
       caption = "Source: senyak.am | Districts with fewer than 30 listings excluded")
save_fig(p10, "fig10_district_inactivity", w = 9, h = 5)

p11 <- map_dfr(THRESHOLDS, function(t) {
  sale %>% group_by(type) %>%
    summarise(rate = mean(days_inactive >= t), .groups = "drop") %>%
    mutate(threshold = t)
}) %>%
  filter(!is.na(type)) %>%
  mutate(threshold = factor(threshold)) %>%
  ggplot(aes(x = threshold, y = rate, fill = type)) +
  geom_col(position = "dodge", width = .68) +
  geom_text(aes(label = percent(rate, .1)),
            position = position_dodge(.68), vjust = -.35, size = 3) +
  scale_fill_manual(values = c(Apartment = COL_APT, House = COL_HOUSE)) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0,.13))) +
  labs(title = "Fig. 11 — Inactivity rates by property type and threshold",
       subtitle = "Houses consistently exceed apartments in inactivity at every threshold",
       x = "Inactivity threshold (days)", y = "Inactivity rate", fill = "Type",
       caption = "Source: senyak.am")
save_fig(p11, "fig11_type_threshold", w = 8, h = 5)

p12 <- inactive_monthly %>%
  ggplot(aes(x = active_ym, y = n, color = threshold, group = threshold)) +
  geom_line(linewidth = .95) + geom_point(size = 2.2) +
  scale_color_manual(values = THRESH_COLORS, labels = paste0(THRESHOLDS," days"),
                     name = "Threshold") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_y_continuous(labels = comma) +
  labs(title = "Fig. 12 — Monthly inactive listing counts under four thresholds",
       subtitle = "Listings whose last activity was in that month and have since exceeded the threshold",
       x = NULL, y = "Inactive listing count",
       caption = "Source: senyak.am | Months within threshold distance of Dec 31, 2025 excluded")
save_fig(p12, "fig12_monthly_inactive_thresholds", w = 11, h = 5)


# ================================================================
# LAYER 4: PRICE-PREMIUM ANALYSIS  (Figs 13-15)
# ================================================================

message("Layer 4: Price-premium analysis ...")

p13 <- sale %>%
  filter(!is.na(premium_bin)) %>%
  mutate(status = if_else(days_inactive >= 120,
                          "Silent  (>= 120 days)","Recently active  (< 120 days)")) %>%
  count(status, premium_bin) %>%
  group_by(status) %>% mutate(pct = n / sum(n)) %>% ungroup() %>%
  ggplot(aes(x = pct, y = premium_bin, fill = status)) +
  geom_col(position = "dodge", width = .68) +
  scale_x_continuous(labels = percent) +
  scale_fill_manual(values = c("Silent  (>= 120 days)" = COL_SILENT,
                               "Recently active  (< 120 days)" = COL_ACTIVE)) +
  labs(title = "Fig. 13 — Price positioning: silent vs recently active listings",
       subtitle = "Overrepresentation of overpriced listings among silent ones suggests staleness",
       x = "Share of listings within group", y = "Price position vs district peers",
       fill = NULL,
       caption = "Source: senyak.am | Peer group = same district x type x room count")
save_fig(p13, "fig13_price_position", w = 10, h = 5)

p14 <- sale %>%
  filter(!is.na(premium_bin)) %>%
  group_by(premium_bin) %>%
  summarise(inact_rate = mean(days_inactive >= 120), n = n(), .groups = "drop") %>%
  ggplot(aes(x = premium_bin, y = inact_rate, fill = premium_bin)) +
  geom_col(width = .7, show.legend = FALSE) +
  geom_text(aes(label = paste0(percent(inact_rate, .1), "\n(n = ", comma(n), ")")),
            vjust = -.18, size = 3.2) +
  scale_fill_manual(values = PREMIUM_COLORS) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0,.20))) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  labs(title = "Fig. 14 — 120-day inactivity rate by price-premium category",
       subtitle = "Inactivity rises with premium, suggesting overpricing drives a significant share of silence",
       x = "Price position relative to district peers", y = "Inactivity rate (>= 120 days)",
       caption = "Source: senyak.am")
save_fig(p14, "fig14_inactivity_by_premium", w = 9, h = 5)

p15 <- sale %>%
  filter(days_inactive >= 120) %>%
  mutate(heuristic = case_when(
    is.na(price_premium)                           ~ "Unclear - no price data",
    price_premium <= .10 & !is.na(update_date)     ~ "Possibly sold / withdrawn\n(near-market, was updated)",
    price_premium <= .10 & is.na(update_date)      ~ "Possibly sold / withdrawn\n(near-market, never updated)",
    price_premium <= .30                           ~ "Ambiguous\n(moderate premium 10-30%)",
    TRUE                                           ~ "Likely stale\n(far above market > 30%)"
  ), heuristic = factor(heuristic, levels = c(
    "Possibly sold / withdrawn\n(near-market, was updated)",
    "Possibly sold / withdrawn\n(near-market, never updated)",
    "Ambiguous\n(moderate premium 10-30%)",
    "Likely stale\n(far above market > 30%)",
    "Unclear - no price data"
  ))) %>%
  count(heuristic) %>% mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = n, y = fct_rev(heuristic), fill = heuristic)) +
  geom_col(width = .72, show.legend = FALSE) +
  geom_text(aes(label = paste0(comma(n), "  (", percent(pct, .1), ")")),
            hjust = -.05, size = 3.2) +
  scale_fill_manual(values = c(
    "Possibly sold / withdrawn\n(near-market, was updated)"  = "#1a9641",
    "Possibly sold / withdrawn\n(near-market, never updated)"= "#78c679",
    "Ambiguous\n(moderate premium 10-30%)"                   = "#fdae61",
    "Likely stale\n(far above market > 30%)"                 = "#d7191c",
    "Unclear - no price data"                                = "#bdbdbd"
  )) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0,.22))) +
  labs(title = "Fig. 15 — Heuristic classification of listings silent for 120+ days",
       subtitle = "Decomposing prolonged inactivity into plausible market-exit and stale-listing categories",
       x = "Count", y = NULL,
       caption = "Source: senyak.am | Premium relative to district x type x room peer group")
save_fig(p15, "fig15_heuristic_classification", w = 10, h = 5)


# ================================================================
# LAYER 5: PRICE INDEX COMPARISON  (Figs 16-19)
# ================================================================

message("Layer 5: Price index comparison ...")

p16 <- comp_120 %>%
  mutate(z_inactive = as.numeric(scale(n)),
         z_price    = as.numeric(scale(mom_pct))) %>%
  pivot_longer(c(z_inactive, z_price), names_to = "series", values_to = "z") %>%
  mutate(series = recode(series, "z_inactive" = "120-day inactive listings",
                         "z_price" = "MoM price change (%)")) %>%
  ggplot(aes(x = active_ym, y = z, color = series, group = series)) +
  geom_hline(yintercept = 0, color = "grey72", linewidth = .4) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.4) +
  scale_color_manual(values = c("120-day inactive listings" = COL_SILENT,
                                "MoM price change (%)" = COL_PRICE_IDX)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  labs(title = "Fig. 16 — Inactive listing count vs price change: z-score comparison",
       subtitle = "Both series standardized for visual comparability",
       x = NULL, y = "Z-score", color = NULL,
       caption = "Source: senyak.am; Statistical Committee of Armenia")
save_fig(p16, "fig16_normalized_comparison", w = 11, h = 5)

cor_table <- comp_data %>%
  group_by(threshold) %>% arrange(active_ym) %>%
  summarise(
    `Concurrent`           = cor(n, mom_pct,          use = "complete.obs"),
    `Price 1 month later`  = cor(n, lead(mom_pct, 1), use = "complete.obs"),
    `Price 2 months later` = cor(n, lead(mom_pct, 2), use = "complete.obs"),
    .groups = "drop"
  ) %>%
  pivot_longer(-threshold, names_to = "timing", values_to = "r") %>%
  mutate(timing = factor(timing, levels = c("Concurrent","Price 1 month later",
                                            "Price 2 months later")))

p17 <- cor_table %>%
  ggplot(aes(x = factor(threshold), y = timing, fill = r)) +
  geom_tile(color = "white", linewidth = .8) +
  geom_text(aes(label = sprintf("%.2f", r)), fontface = "bold", size = 4.8) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9641",
                       midpoint = 0, limits = c(-1, 1), name = "Pearson r") +
  labs(title = "Fig. 17 — Pearson r: inactive listing count vs price change",
       subtitle = "By threshold and timing of price observation relative to inactivity month",
       x = "Inactivity threshold (days)", y = NULL,
       caption = "Source: senyak.am; Statistical Committee of Armenia")
save_fig(p17, "fig17_correlation_heatmap", w = 8, h = 4)

p18 <- comp_120 %>%
  ggplot(aes(x = n, y = mom_pct)) +
  geom_point(color = COL_SILENT, size = 3.2, alpha = .88) +
  geom_smooth(method = "lm", color = COL_PRICE_IDX, fill = COL_PRICE_IDX,
              alpha = .14, linewidth = 1.1) +
  geom_text_repel(aes(label = format(active_ym, "%b %y")),
                  size = 3, color = "grey45", max.overlaps = 12) +
  scale_y_continuous(labels = function(x) paste0(round(x, 2), "%")) +
  labs(title = "Fig. 18 — 120-day inactive listings vs monthly price change",
       subtitle = "Each point = one month; OLS fit with 95% confidence interval",
       x = "Count of listings last active that month and now silent 120+ days",
       y = "Month-over-month price change (%)",
       caption = "Source: senyak.am; Statistical Committee of Armenia")
save_fig(p18, "fig18_scatter_regression", w = 9, h = 6)

mod <- lm(mom_pct ~ n, data = comp_120); coefs <- tidy(mod, conf.int = TRUE)
gfit <- glance(mod)
cat("Fig. 19 OLS: beta =", round(coefs$estimate[2], 5),
    "p =", round(coefs$p.value[2], 4),
    "R2 =", round(gfit$r.squared, 3), "\n")

p19 <- coefs %>% filter(term != "(Intercept)") %>%
  mutate(term = "Inactive count (120-day)") %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, color = "grey60", linewidth = .7, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = .12, linewidth = 1, color = COL_SILENT) +
  geom_point(size = 5, color = COL_SILENT) +
  geom_text(aes(label = paste0("beta = ", round(estimate, 4),
                               "\np = ", round(p.value, 3))),
            nudge_y = .28, size = 3.5) +
  annotate("text", x = Inf, y = -Inf,
           label = paste0("R2 = ", round(gfit$r.squared, 3),
                          "   Adj. R2 = ", round(gfit$adj.r.squared, 3),
                          "   n = ", gfit$nobs),
           hjust = 1.05, vjust = -1, size = 3.5, color = "grey40") +
  labs(title = "Fig. 19 — OLS coefficient: 120-day inactive count to price change",
       subtitle = "Point estimate with 95% CI; DV = month-over-month % price change",
       x = "Estimated coefficient", y = NULL,
       caption = "Source: senyak.am; Statistical Committee of Armenia") +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
save_fig(p19, "fig19_coefficient_plot", w = 9, h = 4)


# ================================================================
# LAYER 6: SPATIAL ANALYSIS  (Figs 20-21)
# ================================================================
#
# Replicates the Python visualizer choropleth (Block 15) exactly:
# - Same 3-month window, trimmed mean, min-n = 3 rule
# - District labels in English (Name_en from shapefile)
# - Price shown as "Xk" (thousands AMD)
# - Grey for districts with insufficient data
# - Merge key: shapefile Name_hy = listing district column (exact match)
# ================================================================

message("Layer 6: Spatial analysis ...")

latest_date  <- max(sale$publish_date, na.rm = TRUE)
window_start <- latest_date %m-% months(2)

district_price_recent <- sale %>%
  filter(publish_date >= window_start, !is.na(price_sqm), price_sqm > 0) %>%
  group_by(district) %>%
  summarise(mean_m2 = trimmed_mean_10(price_sqm), n = n(), .groups = "drop") %>%
  mutate(mean_m2 = if_else(n < 3, NA_real_, mean_m2))

district_inactivity <- sale %>%
  group_by(district) %>%
  summarise(n = n(), inactive = sum(days_inactive >= 120),
            rate = inactive / n, .groups = "drop") %>%
  filter(n >= 20)

# Merge into shapefile — Name_hy matches district column exactly
map_price <- districts_sf %>%
  left_join(district_price_recent, by = c("Name_hy" = "district"))

map_inact <- districts_sf %>%
  left_join(district_inactivity, by = c("Name_hy" = "district"))

# Centroid label data frames
label_price <- map_price %>% st_drop_geometry() %>%
  filter(!is.na(mean_m2)) %>%
  mutate(label = paste0(Name_en, "\n", round(mean_m2 / 1000, 0), "k AMD"))

label_inact <- map_inact %>% st_drop_geometry() %>%
  filter(!is.na(rate)) %>%
  mutate(label = paste0(Name_en, "\n", percent(rate, .1)))

## Fig. 20 — Choropleth: mean sale price per m2 ────────────────────
p20 <- ggplot(map_price) +
  geom_sf(aes(fill = mean_m2), color = "black", linewidth = 0.5) +
  geom_text(
    data = label_price,
    aes(x = centroid_x, y = centroid_y, label = label),
    size = 2.9, fontface = "bold", color = "black",
    bg.color = "white", bg.r = 0.18
  ) +
  scale_fill_viridis_c(
    option    = "plasma",
    name      = "Mean price\nper m2 (AMD)",
    labels    = label_number(big.mark = ","),
    na.value  = "lightgrey",
    direction = 1
  ) +
  labs(
    title    = paste0("Fig. 20 — Mean asking price per m2 by Yerevan district"),
    subtitle = paste0("3-month window ending ", format(latest_date, "%B %Y"),
                      " | Trimmed mean | Grey = fewer than 3 listings"),
    caption  = "Source: senyak.am | Yerevan administrative district boundaries"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = .5,
                                   margin = margin(b = 4)),
    plot.subtitle   = element_text(color = "grey40", size = 10, hjust = .5,
                                   margin = margin(b = 10)),
    plot.caption    = element_text(color = "grey55", size = 9, hjust = .5,
                                   margin = margin(t = 10)),
    legend.position = "right",
    legend.title    = element_text(size = 9),
    plot.margin     = margin(12, 12, 12, 12)
  )
save_fig(p20, "fig20_choropleth_price", w = 11, h = 11)

## Fig. 21 — Choropleth: inactivity rate ───────────────────────────
p21 <- ggplot(map_inact) +
  geom_sf(aes(fill = rate), color = "black", linewidth = 0.5) +
  geom_text(
    data = label_inact,
    aes(x = centroid_x, y = centroid_y, label = label),
    size = 2.9, fontface = "bold", color = "white",
    bg.color = "grey20", bg.r = 0.12
  ) +
  scale_fill_gradient(
    low = "#fee0d2", high = "#67000d",
    name = "Inactivity\nrate (120d+)",
    labels = percent, na.value = "lightgrey"
  ) +
  labs(
    title   = "Fig. 21 — Listing inactivity rate (>= 120 days) by Yerevan district",
    subtitle = "Share of listings in each district that have not been updated for 4+ months",
    caption  = "Source: senyak.am | Districts with < 20 listings shown in grey"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = .5,
                                   margin = margin(b = 4)),
    plot.subtitle   = element_text(color = "grey40", size = 10, hjust = .5,
                                   margin = margin(b = 10)),
    plot.caption    = element_text(color = "grey55", size = 9, hjust = .5,
                                   margin = margin(t = 10)),
    legend.position = "right",
    legend.title    = element_text(size = 9),
    plot.margin     = margin(12, 12, 12, 12)
  )
save_fig(p21, "fig21_choropleth_inactivity", w = 11, h = 11)


# ================================================================
# LAYER 7: PORTAL VS. CADASTRE  (Fig 22)
# ================================================================
#
# Adapted from the Python visualizer Block 9.
# Three-panel layout:
#   A: raw price level comparison (both AMD/m2 on same axis)
#   B: normalized to [-1, +1] for shape comparison
#   C: binary direction (1 = month-over-month increase, 0 = decrease)
# Pearson correlations with 1- and 2-month lags shown as annotation.
# ================================================================

message("Layer 7: Portal vs. cadastre ...")

N_MONTHS <- 16L

portal_monthly <- sale %>%
  filter(!is.na(price_sqm), price_sqm > 0) %>%
  group_by(publish_ym) %>%
  summarise(portal_price = trimmed_mean_10(price_sqm), n = n(), .groups = "drop") %>%
  mutate(portal_price = if_else(n < 5, NA_real_, portal_price)) %>%
  arrange(publish_ym) %>%
  mutate(ym = publish_ym) %>% select(ym, portal_price)

combined <- inner_join(portal_monthly,
                       price_idx %>% select(ym, cad_price = price_sqm_amd),
                       by = "ym") %>%
  filter(!is.na(portal_price), !is.na(cad_price)) %>%
  arrange(ym) %>%
  slice_tail(n = N_MONTHS) %>%
  mutate(
    norm_fn   = function(x) { rng <- range(x, na.rm=T); if(diff(rng)==0) return(x*0); 2*(x-rng[1])/diff(rng)-1 },
    portal_norm = { x <- portal_price; rng <- range(x,na.rm=T); 2*(x-rng[1])/diff(rng)-1 },
    cad_norm    = { x <- cad_price;    rng <- range(x,na.rm=T); 2*(x-rng[1])/diff(rng)-1 },
    portal_dir  = as.integer(portal_price > lag(portal_price)),
    cad_dir     = as.integer(cad_price    > lag(cad_price))
  )

r_lag1 <- cor(combined$portal_price, lag(combined$cad_price),    use = "complete.obs")
r_lag2 <- cor(combined$portal_price, lag(combined$cad_price, 2), use = "complete.obs")

panel_a <- combined %>%
  select(ym, Portal = portal_price, Cadastre = cad_price) %>%
  pivot_longer(-ym, names_to = "series", values_to = "val") %>%
  ggplot(aes(x = ym, y = val, color = series, group = series)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  scale_color_manual(values = c(Portal = COL_PORTAL, Cadastre = COL_CADASTRE)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(subtitle = "Raw price levels (AMD per m2)", x = NULL,
       y = "AMD per m2", color = NULL) +
  theme(legend.position = "top")

panel_b <- combined %>%
  select(ym, Portal = portal_norm, Cadastre = cad_norm) %>%
  pivot_longer(-ym, names_to = "series", values_to = "val") %>%
  ggplot(aes(x = ym, y = val, color = series, group = series)) +
  geom_hline(yintercept = 0, color = "grey80", linewidth = .3) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  scale_color_manual(values = c(Portal = COL_PORTAL, Cadastre = COL_CADASTRE)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  annotate("text", x = min(combined$ym), y = Inf,
           label = paste0("r(portal, cadastre lag-1) = ", round(r_lag1, 3),
                          "\nr(portal, cadastre lag-2) = ", round(r_lag2, 3)),
           hjust = 0, vjust = 1.4, size = 3.3, color = "grey30", fontface = "italic") +
  labs(subtitle = "Normalized to [-1, +1] for shape comparison",
       x = NULL, y = "Normalized value", color = NULL) +
  theme(legend.position = "top")

panel_c <- combined %>%
  select(ym, Portal = portal_dir, Cadastre = cad_dir) %>%
  pivot_longer(-ym, names_to = "series", values_to = "dir") %>%
  filter(!is.na(dir)) %>%
  ggplot(aes(x = ym, y = as.integer(dir), color = series,
             group = series, linetype = series)) +
  geom_step(linewidth = .9) +
  scale_color_manual(values = c(Portal = COL_PORTAL, Cadastre = COL_CADASTRE)) +
  scale_linetype_manual(values = c(Portal = "solid", Cadastre = "dashed")) +
  scale_y_continuous(breaks = c(0, 1), labels = c("Decrease (0)", "Increase (1)")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  labs(subtitle = "Monthly direction (1 = price increase, 0 = decrease)",
       x = "Month", y = "Direction", color = NULL, linetype = NULL) +
  theme(legend.position = "top")

p22 <- (panel_a / panel_b / panel_c) +
  plot_annotation(
    title    = "Fig. 22 — Portal listing prices vs. official cadastre price index",
    subtitle = paste0("Last ", N_MONTHS,
                      " months of overlap | Portal = trimmed mean of senyak.am listings"),
    caption  = "Source: senyak.am; Statistical Committee of Armenia",
    theme    = theme_pub()
  )
save_fig(p22, "fig22_portal_vs_cadastre", w = 11, h = 12)


# ================================================================
# LAYER 8: PRICE TRAJECTORY ANALYSIS  (Figs 23-24)
# ================================================================
#
# Addresses the professor's feedback directly:
# "if a few months ago the price was considered high, but now it is
#  considered low, but still not sold or not removed."
#
# premium_at_publish : listing price % above district median on the
#                      month the listing was first published
# premium_now        : same listing price % above the CURRENT
#                      district median (last 3 months)
# premium_drift      : premium_now - premium_at_publish
#
# Negative drift = the market rose toward the asking price.
# The listing became relatively cheaper over time.
# These silent listings are the most consistent with a sale
# (the seller's price was eventually met by the market) or with
# a seller who is now motivated to accept near-market bids.
#
# Positive drift = the listing grew more overpriced as the market
# moved away from it.  Clear stale-listing signal.
# ================================================================

message("Layer 8: Price trajectory analysis ...")

## Fig. 23 — Trajectory scatter ────────────────────────────────────
# x = premium at publication, y = current premium, y=x = no drift.
# Points below the diagonal have become relatively cheaper.

p23 <- sale_trajectory %>%
  slice_sample(prop = .35) %>%    # downsample for readability
  ggplot(aes(x = premium_at_publish, y = premium_now, color = status)) +
  # quadrant backgrounds
  annotate("rect", xmin = -100, xmax = 0, ymin = -100, ymax = 0,
           fill = "#e8f5e9", alpha = .25) +
  annotate("rect", xmin = 0, xmax = 100, ymin = -100, ymax = 0,
           fill = "#fff9c4", alpha = .4) +
  annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100,
           fill = "#ffebee", alpha = .25) +
  annotate("rect", xmin = -100, xmax = 0, ymin = 0, ymax = 100,
           fill = "#f3e5f5", alpha = .25) +
  # reference lines
  geom_abline(slope = 1, intercept = 0,
              color = "grey50", linewidth = .8, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey70", linewidth = .3) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = .3) +
  # data
  geom_point(alpha = .20, size = .8, shape = 16) +
  # quadrant labels
  annotate("text", x = -60, y =  80,
           label = "Was below market\nnow above market", color = "grey45",
           size = 3, lineheight = .85) +
  annotate("text", x = -60, y = -80,
           label = "Was & still\nbelow market", color = "#1a9641",
           size = 3, lineheight = .85) +
  annotate("text", x =  60, y =  80,
           label = "Was & still\nabove market (stale)", color = "#d7191c",
           size = 3, fontface = "bold", lineheight = .85) +
  annotate("text", x =  60, y = -80,
           label = "Was above market\nnow BELOW market\n[key zone]",
           color = "#e65100", size = 3.2, fontface = "bold", lineheight = .85) +
  scale_color_manual(values = c(
    "Silent  (>= 120 days)"        = COL_SILENT,
    "Recently active  (< 120 days)" = COL_ACTIVE
  ), guide = guide_legend(override.aes = list(alpha = 1, size = 2.5))) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(-100, 100)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-100, 100)) +
  labs(
    title    = "Fig. 23 — Price trajectory: premium at publication vs. current market position",
    subtitle = paste0(
      "Each point = one listing | Dashed diagonal = no market drift\n",
      "Below diagonal: listing became relatively cheaper as the market moved upward"
    ),
    x       = "Price premium at publication (% above district median that month)",
    y       = "Current price premium (% above district median today)",
    color   = NULL,
    caption = paste0(
      "Source: senyak.am | Current median = last 3 months per district\n",
      "Points capped at +/-100% | 35% random sample shown for readability"
    )
  )
save_fig(p23, "fig23_price_trajectory_scatter", w = 10, h = 9)


## Fig. 24 — Premium drift distribution for silent listings ─────────
silent_drift <- sale_trajectory %>%
  filter(days_inactive >= 120, abs(premium_drift) <= 120)

pct_neg   <- mean(silent_drift$premium_drift < 0, na.rm = TRUE)
pct_pos   <- mean(silent_drift$premium_drift > 0, na.rm = TRUE)
med_drift <- median(silent_drift$premium_drift, na.rm = TRUE)

p24 <- silent_drift %>%
  mutate(
    drift_sign = if_else(
      premium_drift < 0,
      "Market rose toward listing price\n(negative drift - sold/motivated-seller signal)",
      "Listing grew more overpriced\n(positive drift - stale-listing signal)"
    )
  ) %>%
  ggplot(aes(x = premium_drift, fill = drift_sign)) +
  geom_histogram(bins = 60, alpha = .88, color = "white", linewidth = .1) +
  geom_vline(xintercept = 0,          color = "black",   linewidth = 1.0) +
  geom_vline(xintercept = med_drift,  color = "#7b1fa2", linewidth = 0.8,
             linetype = "dotted") +
  annotate("text", x = med_drift + 1.5, y = Inf,
           label = paste0("Median drift\n", round(med_drift, 1), " pp"),
           color = "#7b1fa2", hjust = 0, vjust = 1.4, size = 3.2) +
  annotate("text", x = -55, y = Inf,
           label = paste0(percent(pct_neg, .1), "\nof silent listings"),
           color = "#1a9641", hjust = .5, vjust = 1.6, size = 4, fontface = "bold") +
  annotate("text", x =  55, y = Inf,
           label = paste0(percent(pct_pos, .1), "\nof silent listings"),
           color = "#d7191c", hjust = .5, vjust = 1.6, size = 4, fontface = "bold") +
  scale_fill_manual(values = c(
    "Market rose toward listing price\n(negative drift - sold/motivated-seller signal)"  = "#1a9641",
    "Listing grew more overpriced\n(positive drift - stale-listing signal)" = "#d7191c"
  )) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .18))) +
  labs(
    title    = "Fig. 24 — Market position drift for listings silent >= 120 days",
    subtitle = paste0(
      "Negative drift = the market rose toward the listing price (consistent with eventual sale)\n",
      "Positive drift = the listing became more overpriced as the market diverged (stale signal)"
    ),
    x       = "Premium drift (current premium minus premium at publication), percentage points",
    y       = "Count",
    fill    = NULL,
    caption = paste0(
      "Source: senyak.am | Values capped at +/-120 pp for display\n",
      "This analysis directly addresses the professor's question: ",
      "listings that were expensive but whose market has since risen above them"
    )
  ) +
  theme(legend.position = "top",
        legend.text = element_text(size = 8.5))
save_fig(p24, "fig24_premium_drift", w = 11, h = 6)


# ================================================================
# LAYER 9: DISTRICT PRICE TRENDS  (Fig 25)
# ================================================================
#
# Adapted from the Python visualizer Block 14:
# Small-multiples — one panel per district — showing monthly
# trimmed mean (dashed, transparent) and 3-month rolling median
# (solid, bold).  Panels marked "Insufficient data" when fewer
# than 40% of months have >= 3 listings (same rule as Python).
# ================================================================

message("Layer 9: District price trends ...")

dist_monthly <- sale %>%
  filter(!is.na(price_sqm), price_sqm > 0) %>%
  group_by(publish_ym, district_en) %>%
  summarise(tm = trimmed_mean_10(price_sqm), n_obs = n(), .groups = "drop") %>%
  mutate(tm = if_else(n_obs < 3, NA_real_, tm))

all_ym       <- sort(unique(dist_monthly$publish_ym))
all_districts <- unique(dist_monthly$district_en)

dist_full <- expand_grid(publish_ym = all_ym, district_en = all_districts) %>%
  left_join(dist_monthly, by = c("publish_ym","district_en")) %>%
  arrange(district_en, publish_ym) %>%
  group_by(district_en) %>%
  mutate(
    n_good    = sum(!is.na(tm)),
    total_m   = n(),
    bad_data  = n_good / total_m < 0.40,
    # linear interpolation over NAs then 3-month rolling median
    tm_interp = na.approx(tm, na.rm = FALSE),
    tm_smooth = rollmedian(tm_interp, k = 3, fill = NA, align = "right")
  ) %>%
  ungroup()

# Order districts by median price, highest first
dist_order <- dist_full %>%
  group_by(district_en) %>%
  summarise(med = median(tm_interp, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(med)) %>% pull(district_en)

dist_full <- mutate(dist_full,
                    district_en = factor(district_en, levels = dist_order))

# "Insufficient data" label data (one row per bad district)
bad_label_df <- dist_full %>%
  filter(bad_data) %>%
  group_by(district_en) %>%
  summarise(
    label_x = median(all_ym),
    label_y = Inf,
    .groups = "drop"
  )

n_cols <- 3L
n_rows <- ceiling(length(dist_order) / n_cols)

p25 <- ggplot(dist_full, aes(x = publish_ym)) +
  geom_line(aes(y = tm_interp), color = COL_APT,
            linewidth = .75, alpha = .40, linetype = "dashed") +
  geom_line(aes(y = tm_smooth), color = COL_APT,
            linewidth = 1.5, na.rm = TRUE) +
  geom_text(
    data    = bad_label_df,
    mapping = aes(x = label_x, y = label_y, label = "Insufficient data"),
    inherit.aes = FALSE,
    color = "#d73027", vjust = 1.6, size = 3.0, fontface = "italic"
  ) +
  facet_wrap(~ district_en, ncol = n_cols, scales = "free_y") +
  scale_x_date(date_labels = "%b\n%y", date_breaks = "4 months") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +
  labs(
    title    = "Fig. 25 — Monthly sale price per m2 trends by Yerevan district",
    subtitle = paste0(
      "Dashed = monthly trimmed mean  |  Solid = 3-month rolling median\n",
      "Panels ordered by overall median price (highest first) | y-axis: AMD thousands"
    ),
    x = "Month", y = "Price per m2 (AMD x 1,000)",
    caption = paste0(
      "Source: senyak.am | Months with < 3 listings interpolated\n",
      "Districts with < 40% of months having sufficient data marked as insufficient"
    )
  ) +
  theme(
    strip.text       = element_text(face = "bold", size = 10),
    axis.text.x      = element_text(size = 7.5),
    panel.grid.major = element_line(linewidth = .25, color = "grey88")
  )
save_fig(p25, "fig25_district_price_trends",
         w = as.integer(5 * n_cols),
         h = as.integer(4 * n_rows))


# ================================================================
# COMPLETE
# ================================================================
cat("\n", strrep("=", 55), "\n", sep = "")
cat(" All 25 figures saved to ./figures/\n")
cat(strrep("=", 55), "\n\n", sep = "")
cat("Install list:\n")
cat("  install.packages(c('tidyverse','lubridate','readxl',\n")
cat("    'scales','patchwork','ggrepel','broom','sf','zoo'))\n\n")
cat("FILE NAMES EXPECTED IN WORKING DIRECTORY:\n")
cat("  bnakaranev-tun_vajark.csv      (sale listings - Armenian filename)\n")
cat("  bnakaranev-tun_vardzakalut.csv (rental listings - Armenian filename)\n")
cat("  Bnakarannerignerexls           (price index - Armenian xlsx)\n")
cat("  shapes/Yerevan-Districts.shp (+ .dbf .prj .shx .cpg)\n\n")
cat("NOTE: The script uses Unicode escapes for Armenian filenames.\n")
cat("If read_csv fails, set your working directory to the folder\n")
cat("with data/ and shapes/ subfolders, then re-run.\n")