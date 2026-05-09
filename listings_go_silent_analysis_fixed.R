# Listings Go Silent Analysis
# Project: When Listings Go Silent
# Purpose:
#   1. Use scraped apartment/house sale listings to study listing inactivity.
#   2. Focus first on Yerevan apartment sale listings.
#   3. Define silence as days since the last visible listing activity.
#   4. Test whether silence is related to local price premium, community, cohort, and update behavior.
#
# Important interpretation note:
#   This dataset does not directly show whether a listing was sold.
#   Therefore, the analysis studies listing silence/inactivity, not confirmed sales.

# -------------------------------
# 0. Packages
# -------------------------------
required_pkgs <- c(
  "tidyverse", "lubridate", "scales"
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
library(lubridate)
library(scales)

# -------------------------------
# 1. Paths and settings
# -------------------------------
# Edit this path if your project folder is elsewhere.
# Use forward slashes on Windows.
project_dir <- "C:/Users/rsari/R_projects/dataviz_project/Project"

raw_dir        <- file.path(project_dir, "data", "raw")
processed_dir  <- file.path(project_dir, "data", "processed", "silence_analysis")
figures_dir    <- file.path(project_dir, "figures", "silence_analysis")

if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

sale_path <- file.path(raw_dir, "apartment_house_sale.csv")

# Main analysis controls
analysis_property_type <- "Բնակարան"      # first phase: apartments only
analysis_region <- "Երևան"                # first phase: Yerevan only
recent_month_window <- 24                  # keep recent listings relative to max date in the data

long_silence_threshold <- 90               # descriptive/common long-silence threshold
very_long_silence_threshold <- 180         # stricter analytical outcome used in comparison/regression

min_benchmark_n_month <- 10                # community-month benchmark threshold
min_benchmark_n_quarter <- 20              # community-quarter fallback threshold
min_benchmark_n_community <- 30            # community-level fallback threshold

min_eligible_community <- 30               # minimum eligible listings shown in community plot
min_monthly_cohort_n <- 30                 # minimum eligible listings shown in cohort plot

# -------------------------------
# 2. Helper functions
# -------------------------------
parse_number_strict <- function(x) {
  # Keeps only digits, decimal point, sign, and scientific notation symbols.
  x %>%
    as.character() %>%
    str_replace_all("[^0-9Ee+\\-.]", "") %>%
    na_if("") %>%
    as.numeric()
}

parse_date_safe <- function(x) {
  # Dates in the supplied file are day/month/year.
  lubridate::dmy(x, quiet = TRUE)
}

# English labels for Yerevan communities for cleaner plots.
community_map_en <- c(
  "Աջափնյակ" = "Ajapnyak",
  "Ավան" = "Avan",
  "Արաբկիր" = "Arabkir",
  "Դավթաշեն" = "Davtashen",
  "Էրեբունի" = "Erebuni",
  "Քանաքեռ Զեյթուն" = "Kanaker-Zeytun",
  "Կենտրոն" = "Kentron",
  "Մալաթիա Սեբաստիա" = "Malatia-Sebastia",
  "Նոր Նորք" = "Nor Nork",
  "Նորք Մարաշ" = "Nork-Marash",
  "Նուբարաշեն" = "Nubarashen",
  "Շենգավիթ" = "Shengavit",
  "Վահագնի թաղամաս" = "Vahagni District"
)

# -------------------------------
# 3. Load and clean sale listings
# -------------------------------
sale_raw <- readr::read_csv(
  sale_path,
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

sale_clean <- sale_raw %>%
  mutate(
    published_date = parse_date_safe(`Հրապարակվել է`),
    updated_date_raw = parse_date_safe(`Թարմացվել է`),

    price_amd = parse_number_strict(`Գին (֏)`),
    price_usd = parse_number_strict(`Գին ($)`),
    area_sqm = parse_number_strict(`Տան մակերես (մ²)`),
    land_area_sqm = parse_number_strict(`Հողատարածքի մակերես (մ²)`),
    sqm_price_amd = parse_number_strict(`մ² գին (֏)`),

    region = str_squish(as.character(`Մարզ`)),
    community = str_squish(as.character(`Համայնք`)),
    property_type = str_squish(as.character(`Տեսակ`)),
    rooms_raw = str_squish(as.character(`Սենյակներ`)),
    floor_raw = str_squish(as.character(`Հարկ`)),
    listing_url = as.character(`Հղում`),

    # Extract the first number from rooms. Values like "8 և ավել" become 8.
    rooms_numeric = readr::parse_number(str_extract(rooms_raw, "\\d+")),

    # Clean floor strings like ="5/9" into current floor and total floors.
    floor_clean = floor_raw %>%
      str_replace_all('="', "") %>%
      str_replace_all('"', ""),
    current_floor = readr::parse_number(str_extract(floor_clean, "^\\d+")),
    total_floors = readr::parse_number(str_extract(floor_clean, "(?<=/)\\d+"))
  ) %>%
  mutate(
    # If update date is missing or earlier than publication date, do not treat it as valid activity.
    updated_date = case_when(
      is.na(updated_date_raw) ~ as.Date(NA),
      updated_date_raw < published_date ~ as.Date(NA),
      TRUE ~ updated_date_raw
    )
  )

# -------------------------------
# 4. First analysis subset: Yerevan apartments
# -------------------------------
listings_subset_pre <- sale_clean %>%
  filter(
    region == analysis_region,
    property_type == analysis_property_type,
    !is.na(published_date),
    !is.na(sqm_price_amd),
    !is.na(area_sqm),
    area_sqm > 0,
    sqm_price_amd > 0
  )

# Use the latest observed publication/update date inside the analysis subset.
# This makes the analysis reproducible and avoids relying on Sys.Date().
observation_date <- max(
  c(listings_subset_pre$published_date, listings_subset_pre$updated_date),
  na.rm = TRUE
)

listings_base <- listings_subset_pre %>%
  mutate(
    published_month = floor_date(published_date, unit = "month"),
    published_quarter = floor_date(published_date, unit = "quarter"),

    last_activity_date = coalesce(updated_date, published_date),
    was_updated = !is.na(updated_date) & updated_date > published_date,
    update_delay_days = as.numeric(updated_date - published_date),

    silence_days = as.numeric(observation_date - last_activity_date),
    listing_age_days = as.numeric(observation_date - published_date),

    # 90+ days: descriptive/common long silence
    eligible_for_90 = listing_age_days >= long_silence_threshold,
    long_silent_90 = silence_days >= long_silence_threshold,

    # 180+ days: stricter analytical outcome
    eligible_for_180 = listing_age_days >= very_long_silence_threshold,
    very_long_silent_180 = silence_days >= very_long_silence_threshold,

    community_en = recode(community, !!!community_map_en, .default = community)
  ) %>%
  filter(
    silence_days >= 0,
    listing_age_days >= 0
  )

# Keep recent data to reduce distortion from very old listings.
max_published_date <- max(listings_base$published_date, na.rm = TRUE)
recent_cutoff_date <- max_published_date %m-% months(recent_month_window)

listings_recent <- listings_base %>%
  filter(published_date >= recent_cutoff_date)

message("Listings after subset filtering: ", nrow(listings_base))
message("Listings in recent window before outlier removal: ", nrow(listings_recent))

# -------------------------------
# 5. Outlier removal for price and area
# -------------------------------
# Remove extreme sqm prices and extreme apartment areas within each community.
# This keeps local benchmarks from being driven by unusual luxury listings or bad scraped values.
listings_clean <- listings_recent %>%
  group_by(community) %>%
  mutate(
    q1_price = quantile(sqm_price_amd, 0.25, na.rm = TRUE),
    q3_price = quantile(sqm_price_amd, 0.75, na.rm = TRUE),
    iqr_price = q3_price - q1_price,
    price_lower = q1_price - 1.5 * iqr_price,
    price_upper = q3_price + 1.5 * iqr_price,
    price_outlier = sqm_price_amd < price_lower | sqm_price_amd > price_upper,

    q1_area = quantile(area_sqm, 0.25, na.rm = TRUE),
    q3_area = quantile(area_sqm, 0.75, na.rm = TRUE),
    iqr_area = q3_area - q1_area,
    area_lower = q1_area - 1.5 * iqr_area,
    area_upper = q3_area + 1.5 * iqr_area,
    area_outlier = area_sqm < area_lower | area_sqm > area_upper
  ) %>%
  ungroup() %>%
  filter(!price_outlier, !area_outlier) %>%
  select(
    -q1_price, -q3_price, -iqr_price, -price_lower, -price_upper, -price_outlier,
    -q1_area, -q3_area, -iqr_area, -area_lower, -area_upper, -area_outlier
  )

message("Listings after outlier removal: ", nrow(listings_clean))

# -------------------------------
# 6. Build local portal benchmarks for price premium
# -------------------------------
# Main benchmark: community-month median m² price.
# Fallback 1: community-quarter median m² price.
# Fallback 2: community-level median m² price over recent period.
# Fallback 3: Yerevan-month median m² price.
#
# This avoids unstable benchmarks when a community-month has too few listings.

community_month_benchmark <- listings_clean %>%
  group_by(community, community_en, published_month) %>%
  summarise(
    benchmark_cm_n = n(),
    benchmark_cm_median = median(sqm_price_amd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    benchmark_cm_median = if_else(
      benchmark_cm_n >= min_benchmark_n_month,
      benchmark_cm_median,
      NA_real_
    )
  )

community_quarter_benchmark <- listings_clean %>%
  group_by(community, community_en, published_quarter) %>%
  summarise(
    benchmark_cq_n = n(),
    benchmark_cq_median = median(sqm_price_amd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    benchmark_cq_median = if_else(
      benchmark_cq_n >= min_benchmark_n_quarter,
      benchmark_cq_median,
      NA_real_
    )
  )

community_overall_benchmark <- listings_clean %>%
  group_by(community, community_en) %>%
  summarise(
    benchmark_community_n = n(),
    benchmark_community_median = median(sqm_price_amd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    benchmark_community_median = if_else(
      benchmark_community_n >= min_benchmark_n_community,
      benchmark_community_median,
      NA_real_
    )
  )

yerevan_month_benchmark <- listings_clean %>%
  group_by(published_month) %>%
  summarise(
    benchmark_ym_n = n(),
    benchmark_ym_median = median(sqm_price_amd, na.rm = TRUE),
    .groups = "drop"
  )

analysis_data <- listings_clean %>%
  left_join(
    community_month_benchmark,
    by = c("community", "community_en", "published_month")
  ) %>%
  left_join(
    community_quarter_benchmark,
    by = c("community", "community_en", "published_quarter")
  ) %>%
  left_join(
    community_overall_benchmark,
    by = c("community", "community_en")
  ) %>%
  left_join(
    yerevan_month_benchmark,
    by = "published_month"
  ) %>%
  mutate(
    benchmark_sqm_price = coalesce(
      benchmark_cm_median,
      benchmark_cq_median,
      benchmark_community_median,
      benchmark_ym_median
    ),
    benchmark_source = case_when(
      !is.na(benchmark_cm_median) ~ "Community-month median",
      is.na(benchmark_cm_median) & !is.na(benchmark_cq_median) ~ "Community-quarter median",
      is.na(benchmark_cm_median) & is.na(benchmark_cq_median) & !is.na(benchmark_community_median) ~ "Community overall median",
      TRUE ~ "Yerevan-month median"
    ),

    price_premium_pct = (sqm_price_amd - benchmark_sqm_price) / benchmark_sqm_price * 100,
    price_premium_band = case_when(
      is.na(price_premium_pct) ~ NA_character_,
      price_premium_pct < -10 ~ "Below market (< -10%)",
      price_premium_pct >= -10 & price_premium_pct <= 10 ~ "Near market (-10% to +10%)",
      price_premium_pct > 10 & price_premium_pct <= 30 ~ "Moderately above (+10% to +30%)",
      price_premium_pct > 30 ~ "Highly above (> +30%)"
    ),
    price_premium_band = factor(
      price_premium_band,
      levels = c(
        "Below market (< -10%)",
        "Near market (-10% to +10%)",
        "Moderately above (+10% to +30%)",
        "Highly above (> +30%)"
      )
    ),

    silence_band = case_when(
      silence_days <= 30 ~ "0-30 days",
      silence_days <= 60 ~ "31-60 days",
      silence_days <= 90 ~ "61-90 days",
      silence_days <= 180 ~ "91-180 days",
      TRUE ~ "180+ days"
    ),
    silence_band = factor(
      silence_band,
      levels = c("0-30 days", "31-60 days", "61-90 days", "91-180 days", "180+ days")
    ),

    update_status = if_else(was_updated, "Updated at least once", "No recorded update")
  ) %>%
  filter(!is.na(benchmark_sqm_price), !is.na(price_premium_pct))

# Save main processed data and benchmark tables.
readr::write_csv(analysis_data, file.path(processed_dir, "sale_listings_silence_analysis_yerevan_apartments.csv"))
readr::write_csv(community_month_benchmark, file.path(processed_dir, "community_month_price_benchmarks.csv"))
readr::write_csv(community_quarter_benchmark, file.path(processed_dir, "community_quarter_price_benchmarks.csv"))
readr::write_csv(community_overall_benchmark, file.path(processed_dir, "community_overall_price_benchmarks.csv"))
readr::write_csv(yerevan_month_benchmark, file.path(processed_dir, "yerevan_month_price_benchmarks.csv"))

# -------------------------------
# 7. Summary tables used by plots
# -------------------------------

# Descriptive: full distribution of silence durations.
silence_band_summary <- analysis_data %>%
  count(silence_band, name = "n_listings") %>%
  mutate(share = n_listings / sum(n_listings))

# Price-band summary:
#   90+ days  = common/standard long silence, useful descriptively.
#   180+ days = stricter "very long silence", used for main comparison/regression.
price_band_summary <- analysis_data %>%
  group_by(price_premium_band) %>%
  summarise(
    n_listings = n(),

    median_silence_days = median(silence_days, na.rm = TRUE),
    mean_silence_days = mean(silence_days, na.rm = TRUE),

    # 90-day descriptive outcome
    eligible_90_n = sum(eligible_for_90, na.rm = TRUE),
    long_silent_90_share_all = mean(long_silent_90, na.rm = TRUE),
    long_silent_90_share_eligible = mean(
      long_silent_90[eligible_for_90],
      na.rm = TRUE
    ),

    # 180-day stricter analytical outcome
    eligible_180_n = sum(eligible_for_180, na.rm = TRUE),
    very_long_silent_180_share_all = mean(very_long_silent_180, na.rm = TRUE),
    very_long_silent_180_share_eligible = mean(
      very_long_silent_180[eligible_for_180],
      na.rm = TRUE
    ),

    median_price_premium_pct = median(price_premium_pct, na.rm = TRUE),

    .groups = "drop"
  )

community_summary <- analysis_data %>%
  group_by(community, community_en) %>%
  summarise(
    n_listings = n(),
    median_sqm_price = median(sqm_price_amd, na.rm = TRUE),
    median_silence_days = median(silence_days, na.rm = TRUE),

    eligible_90_n = sum(eligible_for_90, na.rm = TRUE),
    long_silent_90_share_eligible = mean(
      long_silent_90[eligible_for_90],
      na.rm = TRUE
    ),

    eligible_180_n = sum(eligible_for_180, na.rm = TRUE),
    very_long_silent_180_share_eligible = mean(
      very_long_silent_180[eligible_for_180],
      na.rm = TRUE
    ),

    median_price_premium_pct = median(price_premium_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(eligible_180_n >= min_eligible_community) %>%
  arrange(desc(very_long_silent_180_share_eligible))

cohort_summary <- analysis_data %>%
  filter(eligible_for_180) %>%
  group_by(published_month) %>%
  summarise(
    n_listings = n(),
    very_long_silent_180_share = mean(very_long_silent_180, na.rm = TRUE),
    median_silence_days = median(silence_days, na.rm = TRUE),
    median_price_premium_pct = median(price_premium_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_listings >= min_monthly_cohort_n)

update_summary <- analysis_data %>%
  group_by(update_status) %>%
  summarise(
    n_listings = n(),

    eligible_90_n = sum(eligible_for_90, na.rm = TRUE),
    median_silence_days_eligible_90 =
      median(silence_days[eligible_for_90], na.rm = TRUE),
    long_silent_90_share_eligible =
      mean(long_silent_90[eligible_for_90], na.rm = TRUE),

    eligible_180_n = sum(eligible_for_180, na.rm = TRUE),
    median_silence_days_eligible_180 =
      median(silence_days[eligible_for_180], na.rm = TRUE),
    very_long_silent_180_share_eligible =
      mean(very_long_silent_180[eligible_for_180], na.rm = TRUE),

    median_update_delay_days = median(update_delay_days, na.rm = TRUE),
    .groups = "drop"
  )

benchmark_source_summary <- analysis_data %>%
  count(benchmark_source, name = "n_listings") %>%
  mutate(share = n_listings / sum(n_listings))

readr::write_csv(silence_band_summary, file.path(processed_dir, "silence_band_summary.csv"))
readr::write_csv(price_band_summary, file.path(processed_dir, "price_premium_band_summary.csv"))
readr::write_csv(community_summary, file.path(processed_dir, "community_silence_summary.csv"))
readr::write_csv(cohort_summary, file.path(processed_dir, "published_month_cohort_summary.csv"))
readr::write_csv(update_summary, file.path(processed_dir, "update_behavior_summary.csv"))
readr::write_csv(benchmark_source_summary, file.path(processed_dir, "benchmark_source_summary.csv"))

# -------------------------------
# 8. Plots
# -------------------------------
plot_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "bottom"
  )

# 8.1 Silence distribution: histogram
p_silence_hist <- ggplot(analysis_data, aes(x = silence_days)) +
  geom_histogram(binwidth = 15, boundary = 0, alpha = 0.85, fill = "#5E748A", color = "white") +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribution of Listing Silence",
    subtitle = paste0(
      "Yerevan apartment sale listings. Silence = days since last visible listing activity. Observation date: ",
      observation_date
    ),
    x = "Silence days",
    y = "Number of listings"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  filename = file.path(figures_dir, "01_silence_days_distribution_histogram.png"),
  plot = p_silence_hist,
  width = 10,
  height = 6,
  dpi = 300
)

# 8.2 Silence distribution: silence bands
p_silence_bands <- ggplot(silence_band_summary, aes(x = silence_band, y = share)) +
  geom_col(alpha = 0.9, fill = "#5E748A", color = "white") +
  geom_text(aes(label = percent(share, accuracy = 0.1)), vjust = -0.3, size = 3.8) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Share of Listings by Silence Band",
    subtitle = "Shows how much short-term and long-term inactivity exists in the listing data",
    x = "Silence band",
    y = "Share of listings"
  ) +
  plot_theme

ggsave(
  filename = file.path(figures_dir, "02_silence_band_distribution.png"),
  plot = p_silence_bands,
  width = 10,
  height = 6,
  dpi = 300
)

# 8.3 Price premium analysis: silence by price premium band
price_band_colors <- c(
  "Below market (< -10%)" = "#7F7F7F",        # muted blue
  "Near market (-10% to +10%)" = "#5E748A",  # neutral gray
  "Moderately above (+10% to +30%)" = "#F2A541", # muted orange
  "Highly above (> +30%)" = "#D95F02"        # darker orange/red
)

p_price_premium_box <- analysis_data %>%
  filter(!is.na(price_premium_band)) %>%
  ggplot(aes(x = price_premium_band, y = silence_days)) +
  geom_boxplot(outlier.alpha = 0.15, fill = price_band_colors) +
  stat_summary(fun = median, geom = "point", size = 2.5) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Listing Silence by Price Premium Band",
    subtitle = "Price premium is measured relative to a local portal benchmark, usually community-month median m² price",
    x = "Price premium band",
    y = "Silence days"
  ) +
  plot_theme

ggsave(
  filename = file.path(figures_dir, "03_silence_by_price_premium_band_boxplot.png"),
  plot = p_price_premium_box,
  width = 11,
  height = 6,
  dpi = 300
)

# 8.4 Share of 180+ day silence by price premium band
p_very_long_silent_price <- price_band_summary %>%
  filter(!is.na(price_premium_band), eligible_180_n > 0) %>%
  ggplot(aes(x = price_premium_band, y = very_long_silent_180_share_eligible)) +
  geom_col(alpha = 0.85, fill = price_band_colors) +
  geom_text(
    aes(label = paste0(
      percent(very_long_silent_180_share_eligible, accuracy = 0.1),
      "\n",
      "n=", eligible_180_n
    )),
    vjust = -0.2,
    size = 3.4
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Share of 180+ Day Silence by Price Premium Band",
    subtitle = paste0(
      "Very long silence = ", very_long_silence_threshold,
      "+ days since last activity. Only listings old enough to reach this threshold are included."
    ),
    x = "Price premium band",
    y = "Share with 180+ day silence"
  ) +
  plot_theme

ggsave(
  filename = file.path(figures_dir, "04_very_long_silent_share_by_price_premium_band.png"),
  plot = p_very_long_silent_price,
  width = 11,
  height = 6,
  dpi = 300
)

# 8.5 Community comparison: 180+ day silence share by community
p_community <- community_summary %>%
  mutate(community_en = fct_reorder(community_en, very_long_silent_180_share_eligible)) %>%
  ggplot(aes(x = community_en, y = very_long_silent_180_share_eligible)) +
  geom_col(alpha = 0.85, fill = "#5E748A", color = "white") +
  geom_text(
    aes(label = paste0(
      percent(very_long_silent_180_share_eligible, accuracy = 0.1),
      "  n=", eligible_180_n
    )),
    hjust = -0.05,
    size = 3.3
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, min(1, max(community_summary$very_long_silent_180_share_eligible, na.rm = TRUE) * 1.12))
  ) +
  labs(
    title = "180+ Day Silence Share by Yerevan Community",
    subtitle = paste0(
      "Only communities with at least ", min_eligible_community,
      " eligible listings are shown."
    ),
    x = "Community",
    y = "Share with 180+ day silence"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.margin = margin(5.5, 35, 5.5, 5.5)
  )

ggsave(
  filename = file.path(figures_dir, "05_very_long_silent_share_by_community.png"),
  plot = p_community,
  width = 10,
  height = 7,
  dpi = 300
)

# 8.6 Published-month cohort analysis for 180+ day silence
p_cohort <- ggplot(cohort_summary, aes(x = published_month, y = very_long_silent_180_share)) +
  geom_line(linewidth = 1, color = "#5E748A") +
  geom_point(aes(size = n_listings), alpha = 1, color = "#5E748A") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  labs(
    title = "180+ Day Silence Share by Publication Month Cohort",
    subtitle = paste0(
      "Only listings old enough to reach ", very_long_silence_threshold,
      " days of silence are included; months with at least ",
      min_monthly_cohort_n,
      " listings are shown."
    ),
    x = "Publication month",
    y = "Share with 180+ day silence"
  ) +
  plot_theme

ggsave(
  filename = file.path(figures_dir, "06_very_long_silent_share_by_publication_month.png"),
  plot = p_cohort,
  width = 11,
  height = 6,
  dpi = 300
)

# 8.7 Update behavior analysis
update_status_colors <- c(
  "No recorded update" = "#7F7F7F",       # muted brown
  "Updated at least once" = "#F2A541"     # muted teal
)

p_update <- analysis_data %>%
  filter(eligible_for_180) %>%
  ggplot(aes(x = update_status, y = silence_days, fill = update_status)) +
  geom_boxplot(outlier.alpha = 0.15, alpha = 0.75) +
  stat_summary(fun = median, geom = "point", size = 2.5) +
  scale_fill_manual(values = update_status_colors) +
  guides(fill = "none") +
  labs(
    title = "Listing Silence by Update Status",
    subtitle = paste0(
      "Restricted to listings old enough to reach ",
      very_long_silence_threshold,
      " days of silence"
    ),
    x = "Update status",
    y = "Silence days"
  ) +
  plot_theme

ggsave(
  filename = file.path(figures_dir, "07_silence_by_update_status_boxplot.png"),
  plot = p_update,
  width = 9,
  height = 6,
  dpi = 300
)

# 8.8 Update delay distribution for listings that were updated.
p_update_delay <- analysis_data %>%
  filter(was_updated, !is.na(update_delay_days), update_delay_days >= 0) %>%
  ggplot(aes(x = update_delay_days)) +
  geom_histogram(binwidth = 15, boundary = 0, alpha = 0.85, fill = "#5E748A", color = "white") +
  labs(
    title = "Distribution of Time Until Recorded Update",
    subtitle = "Update delay = updated date minus published date, for listings with a recorded update",
    x = "Days between publication and recorded update",
    y = "Number of listings"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  filename = file.path(figures_dir, "08_update_delay_distribution.png"),
  plot = p_update_delay,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 9. Logistic regression:
# Does price premium predict very long silence?
# ============================================================

# Outcome:
#   very_long_silent_180 = 1 if silence_days >= 180, 0 otherwise
#
# Important:
#   For a fair 180-day outcome, we only use listings old enough to possibly
#   become 180-day silent:
#   listing_age_days >= 180
#
# Note:
#   The main controlled model intentionally does NOT include was_updated.
#   Update status is mechanically related to silence because silence is defined
#   using the last update date. A sensitivity model with update status is saved separately.

model_data <- analysis_data %>%
  mutate(
    log_area_sqm = log(area_sqm),
    very_long_silent_180_num = as.integer(very_long_silent_180),
    very_long_silent_180 = factor(very_long_silent_180_num, levels = c(0, 1)),
    community = factor(community),
    published_quarter = factor(as.character(published_quarter)),
    was_updated = factor(
      if_else(is.na(was_updated), FALSE, was_updated),
      levels = c(FALSE, TRUE),
      labels = c("Not updated", "Updated")
    )
  ) %>%
  filter(
    eligible_for_180,
    !is.na(very_long_silent_180_num),
    !is.na(price_premium_pct),
    !is.na(area_sqm),
    !is.na(log_area_sqm),
    !is.na(rooms_numeric),
    !is.na(community),
    !is.na(published_quarter)
  )

message("Rows used in 180-day logistic regression: ", nrow(model_data))
message(
  "180+ day silence share in model data: ",
  round(mean(model_data$very_long_silent_180_num == 1) * 100, 2),
  "%"
)

# -------------------------------
# 9.1 Simple logistic regression
# -------------------------------
logit_simple <- glm(
  very_long_silent_180 ~ price_premium_pct,
  data = model_data,
  family = binomial(link = "logit")
)

summary(logit_simple)

# -------------------------------
# 9.2 Controlled logistic regression
# -------------------------------
logit_controlled <- glm(
  very_long_silent_180 ~
    price_premium_pct +
    log_area_sqm +
    rooms_numeric +
    community +
    published_quarter,
  data = model_data,
  family = binomial(link = "logit")
)

summary(logit_controlled)

# -------------------------------
# 9.3 Sensitivity model with update status
# -------------------------------
logit_with_update <- glm(
  very_long_silent_180 ~
    price_premium_pct +
    log_area_sqm +
    rooms_numeric +
    was_updated +
    community +
    published_quarter,
  data = model_data,
  family = binomial(link = "logit")
)

summary(logit_with_update)

# -------------------------------
# 9.4 Convert coefficients to odds ratios
# -------------------------------
logit_to_odds_table <- function(model) {
  coef_table <- summary(model)$coefficients

  tibble(
    term = rownames(coef_table),
    estimate_log_odds = coef_table[, "Estimate"],
    std_error = coef_table[, "Std. Error"],
    z_value = coef_table[, "z value"],
    p_value = coef_table[, "Pr(>|z|)"],
    odds_ratio = exp(estimate_log_odds),
    conf_low = exp(estimate_log_odds - 1.96 * std_error),
    conf_high = exp(estimate_log_odds + 1.96 * std_error)
  ) %>%
    arrange(p_value)
}

logit_simple_results <- logit_to_odds_table(logit_simple)
logit_controlled_results <- logit_to_odds_table(logit_controlled)
logit_with_update_results <- logit_to_odds_table(logit_with_update)

write_csv(
  logit_simple_results,
  file.path(processed_dir, "logistic_regression_180_simple_results.csv")
)

write_csv(
  logit_controlled_results,
  file.path(processed_dir, "logistic_regression_180_controlled_results.csv")
)

write_csv(
  logit_with_update_results,
  file.path(processed_dir, "logistic_regression_180_with_update_status_results.csv")
)

print(logit_simple_results)
print(logit_controlled_results)
print(logit_with_update_results)

# -------------------------------
# 9.5 Focused interpretation table for price premium
# -------------------------------
price_premium_effect <- logit_controlled_results %>%
  filter(term == "price_premium_pct") %>%
  mutate(
    odds_ratio_for_10pp_increase = odds_ratio ^ 10,
    conf_low_for_10pp_increase = conf_low ^ 10,
    conf_high_for_10pp_increase = conf_high ^ 10
  )

write_csv(
  price_premium_effect,
  file.path(processed_dir, "logistic_regression_180_price_premium_effect.csv")
)

print(price_premium_effect)

message(
  "\nInterpretation helper: odds_ratio_for_10pp_increase shows how the odds of 180+ day silence change ",
  "when price premium increases by 10 percentage points, holding area, rooms, community, and publication quarter constant."
)

# -------------------------------
# 9.6 Predicted probability curve for price premium
# -------------------------------
# To visualize the controlled model, we vary price premium while holding other
# variables at typical/reference values.

premium_grid <- tibble(
  price_premium_pct = seq(
    quantile(model_data$price_premium_pct, 0.05, na.rm = TRUE),
    quantile(model_data$price_premium_pct, 0.95, na.rm = TRUE),
    length.out = 100
  ),
  log_area_sqm = median(model_data$log_area_sqm, na.rm = TRUE),
  rooms_numeric = median(model_data$rooms_numeric, na.rm = TRUE),
  community = factor(
    names(sort(table(model_data$community), decreasing = TRUE))[1],
    levels = levels(model_data$community)
  ),
  published_quarter = factor(
    names(sort(table(model_data$published_quarter), decreasing = TRUE))[1],
    levels = levels(model_data$published_quarter)
  )
)

premium_predictions <- predict(
  logit_controlled,
  newdata = premium_grid,
  type = "link",
  se.fit = TRUE
)

premium_plot_data <- premium_grid %>%
  mutate(
    fit_link = premium_predictions$fit,
    se_link = premium_predictions$se.fit,

    pred_prob = plogis(fit_link),
    pred_low = plogis(fit_link - 1.96 * se_link),
    pred_high = plogis(fit_link + 1.96 * se_link)
  )

write_csv(
  premium_plot_data,
  file.path(processed_dir, "logistic_regression_180_price_premium_predictions.csv")
)

p_logit_premium <- ggplot(
  premium_plot_data,
  aes(x = price_premium_pct, y = pred_prob)
) +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), alpha = 0.20, fill = "#5E748A") +
  geom_line(linewidth = 1.1, color = "#5E748A") +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_x_continuous(labels = function(x) paste0(round(x), "%")) +
  labs(
    title = "Predicted Probability of 180+ Day Silence by Price Premium",
    subtitle = "Controlled logistic regression; area, rooms, community, and publication quarter held constant",
    x = "Price premium relative to local portal benchmark",
    y = "Predicted probability of 180+ day silence"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(figures_dir, "09_logistic_predicted_probability_180_by_price_premium.png"),
  plot = p_logit_premium,
  width = 9,
  height = 6,
  dpi = 300
)

p_logit_premium

# -------------------------------
# 9.7 Logistic regression by price premium band
# -------------------------------
band_model_data <- analysis_data %>%
  filter(
    eligible_for_180,
    !is.na(very_long_silent_180),
    !is.na(price_premium_band)
  ) %>%
  mutate(
    very_long_silent_180 = factor(as.integer(very_long_silent_180), levels = c(0, 1)),
    price_premium_band = factor(
      price_premium_band,
      levels = levels(analysis_data$price_premium_band)
    )
  )

logit_band <- glm(
  very_long_silent_180 ~ price_premium_band,
  data = band_model_data,
  family = binomial(link = "logit")
)

logit_band_results <- logit_to_odds_table(logit_band)

write_csv(
  logit_band_results,
  file.path(processed_dir, "logistic_regression_180_price_band_results.csv")
)

print(logit_band_results)

# -------------------------------
# 10. Console summary
# -------------------------------
message("\nListings-go-silent analysis complete.")
message("Observation date used: ", observation_date)
message("Raw input file: ", sale_path)
message("Processed outputs saved to: ", processed_dir)
message("Figures saved to: ", figures_dir)

message("\nMain subset:")
message("Region: ", analysis_region)
message("Property type: ", analysis_property_type)
message("Recent window: last ", recent_month_window, " months relative to latest published date")
message("Listings after cleaning and benchmark creation: ", nrow(analysis_data))

message("\nBenchmark source distribution:")
print(benchmark_source_summary)

message("\nSilence band summary:")
print(silence_band_summary)

message("\nPrice premium band summary:")
print(price_band_summary)

message("\nCommunity summary:")
print(community_summary)

message("\nUpdate behavior summary:")
print(update_summary)
