# Cadastre vs Portal Validation Analysis
# Project: When Listings Go Silent
# Purpose:
#   1. Validate scraped marketplace apartment-sale prices against official cadastre prices.
#   2. Test whether cadastre prices published in later months correspond to portal prices from earlier months.
#   3. Compare both price levels and month-to-month changes across multiple lags.
#   4. Save cleaned/intermediate data, correlation tables, and plots.

# -------------------------------
# 0. Packages
# -------------------------------
required_pkgs <- c(
  "tidyverse", "readxl", "lubridate", "zoo", "scales"
)

missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Please install the missing packages before running this script: ",
    paste(missing_pkgs, collapse = ", "),
    call. = FALSE
  )
}

library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(scales)
library(janitor)

# -------------------------------
# 1. Paths and settings
# -------------------------------
# Use forward slashes in Windows paths to avoid problems with backslash escapes.
project_dir <- "C:/Users/rsari/R_projects/dataviz_project/Project"

raw_dir       <- file.path(project_dir, "data", "raw")
processed_dir <- file.path(project_dir, "data", "processed", "cadastre_verification_analysis")
figures_dir   <- file.path(project_dir, "figures", "cadastre_verification_analysis")

if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

portal_path   <- file.path(raw_dir, "apartment_house_sale.csv")
cadastre_path <- file.path(raw_dir, "apartment_price.xlsx")

# Main analysis controls
min_listings_per_month <- 5
analysis_months <- 16
max_lag_months <- 4
rolling_window <- 3
trim_proportion <- 0.10

# -------------------------------
# 2. Helper functions
# -------------------------------
parse_number_strict <- function(x) {
  # Keeps only digits, decimal point, sign, and scientific notation symbols.
  # This is close to the cleaning logic used in the Python version.
  x %>%
    as.character() %>%
    str_replace_all("[^0-9Ee+\\-.]", "") %>%
    na_if("") %>%
    as.numeric()
}

trimmed_mean_safe <- function(x, trim = 0.10) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(NA_real_)

  x <- sort(x)
  k <- floor(n * trim)

  if ((n - 2 * k) <= 0) return(NA_real_)
  mean(x[(k + 1):(n - k)])
}

roll_mean_right <- function(x, width = 3) {
  zoo::rollapplyr(
    data = x,
    width = width,
    FUN = function(v) mean(v, na.rm = TRUE),
    partial = TRUE,
    fill = NA_real_
  )
}

normalize_minus1_plus1 <- function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)

  if (!is.finite(min_x) || !is.finite(max_x) || max_x == min_x) {
    return(rep(NA_real_, length(x)))
  }

  ((x - min_x) / (max_x - min_x)) * 2 - 1
}

safe_cor <- function(x, y, method = "pearson") {
  temp <- tibble(x = x, y = y) %>% drop_na()

  if (nrow(temp) < 3) return(NA_real_)
  if (sd(temp$x) == 0 || sd(temp$y) == 0) return(NA_real_)

  suppressWarnings(cor(temp$x, temp$y, method = method))
}

safe_cor_p_value <- function(x, y, method = "pearson") {
  temp <- tibble(x = x, y = y) %>% drop_na()

  if (nrow(temp) < 3) return(NA_real_)
  if (sd(temp$x) == 0 || sd(temp$y) == 0) return(NA_real_)

  suppressWarnings(cor.test(temp$x, temp$y, method = method)$p.value)
}

# -------------------------------
# 3. Load and clean portal data
# -------------------------------
portal_raw <- readr::read_csv(
  portal_path,
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

portal_clean <- portal_raw %>%
  mutate(
    `Հրապարակվել է` = lubridate::dmy(`Հրապարակվել է`),
    `Թարմացվել է`   = lubridate::dmy(`Թարմացվել է`),
    `Գին (֏)` = parse_number_strict(`Գին (֏)`),
    `Գին ($)` = parse_number_strict(`Գին ($)`),
    `Տան մակերես (մ²)` = parse_number_strict(`Տան մակերես (մ²)`),
    `Հողատարածքի մակերես (մ²)` = parse_number_strict(`Հողատարածքի մակերես (մ²)`),
    `մ² գին (֏)` = parse_number_strict(`մ² գին (֏)`),
    across(c(`Մարզ`, `Համայնք`, `Սենյակներ`, `Հարկ`, `Տեսակ`, `Հղում`), as.character),
    across(c(`Մարզ`, `Համայնք`, `Տեսակ`), str_squish)
  )

# Keep only Yerevan apartment sale listings, because the cadastre benchmark sheet is for Yerevan apartment prices.
portal_yerevan_apartments <- portal_clean %>%
  filter(
    `Մարզ` == "Երևան",
    str_to_lower(`Տեսակ`) == str_to_lower("Բնակարան"),
    !is.na(`Հրապարակվել է`),
    !is.na(`մ² գին (֏)`)
  )

# Use a 24-month window based on the latest portal publication date in the dataset.
# This is more reproducible than using Sys.Date(), because the script gives the same result later.
max_portal_date <- max(portal_yerevan_apartments$`Հրապարակվել է`, na.rm = TRUE)
cutoff_date <- max_portal_date %m-% months(24)

# IQR outlier removal by community.
# This removes very unusual m² prices within each Yerevan district/community.
portal_yerevan_apartments_clean <- portal_yerevan_apartments %>%
  filter(`Հրապարակվել է` >= cutoff_date) %>%
  group_by(`Համայնք`) %>%
  mutate(
    q1 = quantile(`մ² գին (֏)`, 0.25, na.rm = TRUE),
    q3 = quantile(`մ² գին (֏)`, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower_bound = q1 - 1.5 * iqr,
    upper_bound = q3 + 1.5 * iqr,
    is_iqr_outlier = `մ² գին (֏)` < lower_bound | `մ² գին (֏)` > upper_bound
  ) %>%
  ungroup() %>%
  filter(!is_iqr_outlier) %>%
  select(-q1, -q3, -iqr, -lower_bound, -upper_bound, -is_iqr_outlier)

# Monthly portal series: raw trimmed mean and 3-month rolling smoothed mean.
portal_monthly <- portal_yerevan_apartments_clean %>%
  mutate(month = floor_date(`Հրապարակվել է`, unit = "month")) %>%
  group_by(month) %>%
  summarise(
    n_listings = n(),
    portal_raw_level = trimmed_mean_safe(`մ² գին (֏)`, trim = trim_proportion),
    portal_mean_level = mean(`մ² գին (֏)`, na.rm = TRUE),
    portal_median_level = median(`մ² գին (֏)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(month) %>%
  mutate(
    portal_raw_level = if_else(n_listings < min_listings_per_month, NA_real_, portal_raw_level),
    portal_smooth_level = roll_mean_right(portal_raw_level, width = rolling_window),

    portal_raw_abs_change = portal_raw_level - lag(portal_raw_level),
    portal_raw_pct_change = (portal_raw_level / lag(portal_raw_level)) - 1,

    portal_smooth_abs_change = portal_smooth_level - lag(portal_smooth_level),
    portal_smooth_pct_change = (portal_smooth_level / lag(portal_smooth_level)) - 1
  )

# -------------------------------
# 4. Load and clean cadastre data
# -------------------------------
cadastre_raw <- readxl::read_excel(cadastre_path, sheet = "Yerevan")

# The first column is the month/date. The first numeric column after that is the cadastre price level.
cadastre_first_numeric_col <- cadastre_raw %>%
  select(-1) %>%
  select(where(is.numeric)) %>%
  names() %>%
  first()

if (is.na(cadastre_first_numeric_col)) {
  stop("No numeric cadastre price column was found in the Yerevan sheet.", call. = FALSE)
}

cadastre_monthly <- cadastre_raw %>%
  rename(month = 1) %>%
  transmute(
    month = floor_date(as.Date(month), unit = "month"),
    cadastre_raw_level = as.numeric(.data[[cadastre_first_numeric_col]])
  ) %>%
  filter(!is.na(month), !is.na(cadastre_raw_level)) %>%
  arrange(month) %>%
  mutate(
    cadastre_smooth_level = roll_mean_right(cadastre_raw_level, width = rolling_window),

    cadastre_raw_abs_change = cadastre_raw_level - lag(cadastre_raw_level),
    cadastre_raw_pct_change = (cadastre_raw_level / lag(cadastre_raw_level)) - 1,

    cadastre_smooth_abs_change = cadastre_smooth_level - lag(cadastre_smooth_level),
    cadastre_smooth_pct_change = (cadastre_smooth_level / lag(cadastre_smooth_level)) - 1
  )

# Save cleaned monthly datasets.
readr::write_csv(portal_yerevan_apartments_clean, file.path(processed_dir, "portal_yerevan_apartment_sale_clean.csv"))
readr::write_csv(portal_monthly, file.path(processed_dir, "portal_monthly_yerevan_apartment_sale_prices.csv"))
readr::write_csv(cadastre_monthly, file.path(processed_dir, "cadastre_monthly_yerevan_apartment_prices.csv"))

# -------------------------------
# 5. Build lagged comparison datasets
# -------------------------------
# Lag interpretation:
#   lag_months = 0 means portal month t is compared with cadastre month t.
#   lag_months = 1 means portal month t is compared with cadastre month t + 1.
#   lag_months = 2 means portal month t is compared with cadastre month t + 2.
# This matches the project idea that cadastre prices published later may correspond to earlier portal prices.

make_lagged_data <- function(lag_months) {
  portal_monthly %>%
    mutate(
      lag_months = lag_months,
      cadastre_publication_month = month %m+% months(lag_months)
    ) %>%
    left_join(
      cadastre_monthly,
      by = c("cadastre_publication_month" = "month")
    ) %>%
    rename(portal_month = month) %>%
    arrange(portal_month)
}

lagged_all <- map_dfr(0:max_lag_months, make_lagged_data)

# Keep only the latest analysis window for each lag.
# We keep this separate from the full aligned data, so the full result is also available if needed.
lagged_recent <- lagged_all %>%
  group_by(lag_months) %>%
  filter(!is.na(portal_raw_level), !is.na(cadastre_raw_level)) %>%
  arrange(portal_month) %>%
  slice_tail(n = analysis_months) %>%
  ungroup()

readr::write_csv(lagged_all, file.path(processed_dir, "cadastre_portal_lagged_aligned_full.csv"))
readr::write_csv(lagged_recent, file.path(processed_dir, "cadastre_portal_lagged_aligned_recent.csv"))

# -------------------------------
# 6. Correlation analysis
# -------------------------------
series_definitions <- tribble(
  ~series_type,              ~portal_col,                  ~cadastre_col,                  ~price_basis,
  "raw_level",              "portal_raw_level",           "cadastre_raw_level",           "Price level",
  "raw_abs_change",         "portal_raw_abs_change",      "cadastre_raw_abs_change",      "Absolute monthly change",
  "raw_pct_change",         "portal_raw_pct_change",      "cadastre_raw_pct_change",      "Percentage monthly change",
  "smoothed_level",         "portal_smooth_level",        "cadastre_smooth_level",        "Price level",
  "smoothed_abs_change",    "portal_smooth_abs_change",   "cadastre_smooth_abs_change",   "Absolute monthly change",
  "smoothed_pct_change",    "portal_smooth_pct_change",   "cadastre_smooth_pct_change",   "Percentage monthly change"
)

correlation_results <- crossing(
  lag_months = 0:max_lag_months,
  series_definitions
) %>%
  rowwise() %>%
  mutate(
    data_for_corr = list(
      lagged_recent %>%
        filter(lag_months == .env$lag_months) %>%
        select(
          portal_month,
          cadastre_publication_month,
          portal_value = all_of(portal_col),
          cadastre_value = all_of(cadastre_col)
        ) %>%
        drop_na(portal_value, cadastre_value)
    ),
    n_observations = nrow(data_for_corr),
    pearson_corr = safe_cor(data_for_corr$portal_value, data_for_corr$cadastre_value, method = "pearson"),
    pearson_p_value = safe_cor_p_value(data_for_corr$portal_value, data_for_corr$cadastre_value, method = "pearson"),
    spearman_corr = safe_cor(data_for_corr$portal_value, data_for_corr$cadastre_value, method = "spearman"),
    spearman_p_value = safe_cor_p_value(data_for_corr$portal_value, data_for_corr$cadastre_value, method = "spearman")
  ) %>%
  ungroup() %>%
  select(
    series_type,
    price_basis,
    lag_months,
    n_observations,
    pearson_corr,
    pearson_p_value,
    spearman_corr,
    spearman_p_value
  ) %>%
  arrange(series_type, lag_months)

best_lags <- correlation_results %>%
  group_by(series_type) %>%
  filter(!is.na(pearson_corr)) %>%
  slice_max(order_by = abs(pearson_corr), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(series_type)

# Direction agreement: do portal and cadastre move in the same direction?
direction_agreement <- lagged_recent %>%
  group_by(lag_months) %>%
  summarise(
    n_raw_direction_months = sum(!is.na(portal_raw_abs_change) & !is.na(cadastre_raw_abs_change)),
    raw_same_direction_share = mean(
      sign(portal_raw_abs_change) == sign(cadastre_raw_abs_change),
      na.rm = TRUE
    ),
    n_smoothed_direction_months = sum(!is.na(portal_smooth_abs_change) & !is.na(cadastre_smooth_abs_change)),
    smoothed_same_direction_share = mean(
      sign(portal_smooth_abs_change) == sign(cadastre_smooth_abs_change),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

readr::write_csv(correlation_results, file.path(processed_dir, "cadastre_portal_lag_correlation_results.csv"))
readr::write_csv(best_lags, file.path(processed_dir, "cadastre_portal_best_lags_by_series.csv"))
readr::write_csv(direction_agreement, file.path(processed_dir, "cadastre_portal_direction_agreement.csv"))

# -------------------------------
# 7. Plots
# -------------------------------
# 7.1 Pearson correlations by lag
cor_long_pearson <- correlation_results %>%
  mutate(
    smoothing = if_else(str_detect(series_type, "smoothed"), "Smoothed 3-month series", "Raw monthly series"),
    metric = case_when(
      str_detect(series_type, "level") ~ "Price levels",
      str_detect(series_type, "abs_change") ~ "Absolute monthly changes",
      str_detect(series_type, "pct_change") ~ "Percentage monthly changes",
      TRUE ~ series_type
    )
  )

p_corr_pearson <- ggplot(cor_long_pearson, aes(x = lag_months, y = pearson_corr, group = smoothing, color = smoothing)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ metric, ncol = 1) +
  scale_x_continuous(breaks = 0:max_lag_months) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    title = "Portal vs Cadastre Pearson Correlations by Publication Lag",
    subtitle = "Lag k means portal prices in month t are compared with cadastre prices published in month t + k",
    x = "Cadastre publication lag in months",
    y = "Pearson correlation",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(figures_dir, "cadastre_portal_pearson_correlations_by_lag.png"),
  plot = p_corr_pearson,
  width = 10,
  height = 8,
  dpi = 300
)

# 7.2 Spearman correlations by lag
p_corr_spearman <- ggplot(cor_long_pearson, aes(x = lag_months, y = spearman_corr, group = smoothing, color = smoothing)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ metric, ncol = 1) +
  scale_x_continuous(breaks = 0:max_lag_months) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    title = "Portal vs Cadastre Spearman Correlations by Publication Lag",
    subtitle = "Spearman checks whether the relationship is monotonic and is less sensitive to extreme values",
    x = "Cadastre publication lag in months",
    y = "Spearman correlation",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(figures_dir, "cadastre_portal_spearman_correlations_by_lag.png"),
  plot = p_corr_spearman,
  width = 10,
  height = 8,
  dpi = 300
)


# ============================================================
# Additional validation 1:
# Portal vs Cadastre percentage gap over time
# ============================================================

selected_lag <- 2

gap_data <- lagged_recent %>%
  filter(lag_months == selected_lag) %>%
  transmute(
    portal_month,
    cadastre_publication_month,
    
    portal_price_raw = portal_raw_level,
    portal_price_smoothed = portal_smooth_level,
    
    cadastre_price_raw = cadastre_raw_level,
    cadastre_price_smoothed = cadastre_smooth_level,
    
    gap_raw_amd = portal_raw_level - cadastre_raw_level,
    gap_raw_pct = (portal_raw_level - cadastre_raw_level) / cadastre_raw_level * 100,
    
    gap_smoothed_amd = portal_smooth_level - cadastre_smooth_level,
    gap_smoothed_pct = (portal_smooth_level - cadastre_smooth_level) / cadastre_smooth_level * 100
  ) %>%
  filter(
    !is.na(portal_price_raw),
    !is.na(cadastre_price_raw),
    !is.na(gap_raw_pct)
  )

write_csv(
  gap_data,
  file.path(processed_dir, "portal_cadastre_gap_lag2.csv")
)

gap_summary <- gap_data %>%
  summarise(
    selected_lag = selected_lag,
    n_months = n(),
    
    mean_gap_raw_pct = mean(gap_raw_pct, na.rm = TRUE),
    median_gap_raw_pct = median(gap_raw_pct, na.rm = TRUE),
    sd_gap_raw_pct = sd(gap_raw_pct, na.rm = TRUE),
    
    mean_gap_smoothed_pct = mean(gap_smoothed_pct, na.rm = TRUE),
    median_gap_smoothed_pct = median(gap_smoothed_pct, na.rm = TRUE),
    sd_gap_smoothed_pct = sd(gap_smoothed_pct, na.rm = TRUE)
  )

write_csv(
  gap_summary,
  file.path(processed_dir, "portal_cadastre_gap_summary_lag2.csv")
)

print(gap_summary)

gap_plot <- ggplot(gap_data, aes(x = portal_month)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6, alpha = 0.7) +
  geom_line(aes(y = gap_raw_pct, color = "Raw monthly gap"), linewidth = 0.8, alpha = 0.65) +
  geom_point(aes(y = gap_raw_pct, color = "Raw monthly gap"), size = 1.8, alpha = 0.75) +
  geom_line(aes(y = gap_smoothed_pct, color = "Smoothed 3-month gap"), linewidth = 1.1) +
  geom_point(aes(y = gap_smoothed_pct, color = "Smoothed 3-month gap"), size = 2) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  labs(
    title = "Portal–Cadastre Price Gap Over Time",
    subtitle = paste0(
      "Portal month t compared with cadastre publication month t + ",
      selected_lag,
      ". Positive values mean portal prices are above cadastre prices."
    ),
    x = "Portal listing month",
    y = "Gap relative to cadastre price (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(figures_dir, "portal_cadastre_percentage_gap_lag2.png"),
  plot = gap_plot,
  width = 11,
  height = 6,
  dpi = 300
)

gap_plot

# ============================================================
# Additional validation 2:
# Quarterly trend comparison
# ============================================================

quarterly_data <- gap_data %>%
  mutate(
    portal_quarter = floor_date(portal_month, unit = "quarter")
  ) %>%
  group_by(portal_quarter) %>%
  summarise(
    portal_quarterly_price = mean(portal_price_raw, na.rm = TRUE),
    cadastre_quarterly_price = mean(cadastre_price_raw, na.rm = TRUE),
    portal_quarterly_smoothed = mean(portal_price_smoothed, na.rm = TRUE),
    cadastre_quarterly_smoothed = mean(cadastre_price_smoothed, na.rm = TRUE),
    n_months = n(),
    .groups = "drop"
  ) %>%
  filter(n_months >= 2) %>%
  mutate(
    portal_quarterly_pct_change = (portal_quarterly_price / lag(portal_quarterly_price) - 1) * 100,
    cadastre_quarterly_pct_change = (cadastre_quarterly_price / lag(cadastre_quarterly_price) - 1) * 100,
    
    portal_quarterly_norm = normalize_minus1_plus1(portal_quarterly_price),
    cadastre_quarterly_norm = normalize_minus1_plus1(cadastre_quarterly_price)
  )

write_csv(
  quarterly_data,
  file.path(processed_dir, "portal_cadastre_quarterly_validation_lag2.csv")
)

quarterly_corr <- quarterly_data %>%
  summarise(
    selected_lag = selected_lag,
    n_quarters = sum(!is.na(portal_quarterly_price) & !is.na(cadastre_quarterly_price)),
    pearson_level_corr = cor(portal_quarterly_price, cadastre_quarterly_price, use = "complete.obs", method = "pearson"),
    spearman_level_corr = cor(portal_quarterly_price, cadastre_quarterly_price, use = "complete.obs", method = "spearman"),
    pearson_pct_change_corr = cor(portal_quarterly_pct_change, cadastre_quarterly_pct_change, use = "complete.obs", method = "pearson"),
    spearman_pct_change_corr = cor(portal_quarterly_pct_change, cadastre_quarterly_pct_change, use = "complete.obs", method = "spearman")
  )

write_csv(
  quarterly_corr,
  file.path(processed_dir, "portal_cadastre_quarterly_correlation_lag2.csv")
)

print(quarterly_corr)

# Normalized quarterly trend plot
quarterly_long <- quarterly_data %>%
  select(
    portal_quarter,
    portal_quarterly_norm,
    cadastre_quarterly_norm
  ) %>%
  pivot_longer(
    cols = c(portal_quarterly_norm, cadastre_quarterly_norm),
    names_to = "source",
    values_to = "normalized_price"
  ) %>%
  mutate(
    source = case_when(
      source == "portal_quarterly_norm" ~ "Portal quarterly price",
      source == "cadastre_quarterly_norm" ~ "Cadastre quarterly price",
      TRUE ~ source
    )
  )

quarterly_trend_plot <- ggplot(
  quarterly_long,
  aes(x = portal_quarter, y = normalized_price, color = source)
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.3) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-Q%q") +
  labs(
    title = "Quarterly Portal and Cadastre Trend Comparison",
    subtitle = paste0(
      "Using lag ",
      selected_lag,
      ": portal quarter compared with cadastre prices published later"
    ),
    x = "Portal quarter",
    y = "Normalized price level",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(figures_dir, "portal_cadastre_quarterly_trend_lag2.png"),
  plot = quarterly_trend_plot,
  width = 11,
  height = 6,
  dpi = 300
)

quarterly_trend_plot

# -------------------------------
# 8. Console summary
# -------------------------------
message("\nAnalysis complete.")
message("Processed files saved to: ", processed_dir)
message("Figures saved to: ", figures_dir)
message("\nBest lags by series type based on absolute Pearson correlation:")
print(best_lags)
message("\nDirection agreement by lag:")
print(direction_agreement)

