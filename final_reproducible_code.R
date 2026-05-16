required_pkgs <- c(
  "tidyverse", "lubridate", "scales", "readxl", "zoo"
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
library(readxl)
library(zoo)

# ===============================================================
# PART A: LISTINGS GO SILENT ANALYSIS
# ===============================================================

project_dir <- "C:/Users/rsari/R_projects/dataviz_project/Project"

raw_dir        <- file.path(project_dir, "data", "raw")
processed_dir  <- file.path(project_dir, "data", "processed", "silence_analysis")
figures_dir    <- file.path(project_dir, "figures", "silence_analysis")

# Mostly for console summary
silence_processed_dir_summary <- processed_dir
silence_figures_dir_summary <- figures_dir

if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

sale_path <- file.path(raw_dir, "apartment_house_sale.csv")

analysis_property_type <- "Բնակարան"
analysis_region <- "Երևան"
recent_month_window <- 24

long_silence_threshold <- 90
very_long_silence_threshold <- 180

min_benchmark_n_month <- 10
min_benchmark_n_quarter <- 20
min_benchmark_n_community <- 30

min_eligible_community <- 30
min_monthly_cohort_n <- 30


# 2. Helper functions

parse_number_strict <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("[^0-9Ee+\\-.]", "") %>%
    na_if("") %>%
    as.numeric()
}

parse_date_safe <- function(x) {
  # Dates in the original file are day/month/year.
  lubridate::dmy(x, quiet = TRUE)
}

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


# 3. Load and clean sale listings

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


# 4. First analysis subset: Yerevan apartments

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

    eligible_for_90 = listing_age_days >= long_silence_threshold,
    long_silent_90 = silence_days >= long_silence_threshold,

    eligible_for_180 = listing_age_days >= very_long_silence_threshold,
    very_long_silent_180 = silence_days >= very_long_silence_threshold,

    community_en = recode(community, !!!community_map_en, .default = community)
  ) %>%
  filter(
    silence_days >= 0,
    listing_age_days >= 0
  )

max_published_date <- max(listings_base$published_date, na.rm = TRUE)
recent_cutoff_date <- max_published_date %m-% months(recent_month_window)

listings_recent <- listings_base %>%
  filter(published_date >= recent_cutoff_date)

message("Listings after subset filtering: ", nrow(listings_base))
message("Listings in recent window before outlier removal: ", nrow(listings_recent))


# 5. Outlier removal for price and area

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


# 6. Build local portal benchmarks for price premium

# IDEA:
# Main benchmark: community-month median m^2 price.
# Fallback 1: community-quarter median m^2 price.
# Fallback 2: community-level median m^2 price over recent period.
# Fallback 3: Yerevan-month median m^2 price.


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

readr::write_csv(analysis_data, file.path(processed_dir, "sale_listings_silence_analysis_yerevan_apartments.csv"))
readr::write_csv(community_month_benchmark, file.path(processed_dir, "community_month_price_benchmarks.csv"))
readr::write_csv(community_quarter_benchmark, file.path(processed_dir, "community_quarter_price_benchmarks.csv"))
readr::write_csv(community_overall_benchmark, file.path(processed_dir, "community_overall_price_benchmarks.csv"))
readr::write_csv(yerevan_month_benchmark, file.path(processed_dir, "yerevan_month_price_benchmarks.csv"))


# 7. Summary tables used by plots

silence_band_summary <- analysis_data %>%
  count(silence_band, name = "n_listings") %>%
  mutate(share = n_listings / sum(n_listings))

price_band_summary <- analysis_data %>%
  group_by(price_premium_band) %>%
  summarise(
    n_listings = n(),

    median_silence_days = median(silence_days, na.rm = TRUE),
    mean_silence_days = mean(silence_days, na.rm = TRUE),

    eligible_90_n = sum(eligible_for_90, na.rm = TRUE),
    long_silent_90_share_all = mean(long_silent_90, na.rm = TRUE),
    long_silent_90_share_eligible = mean(
      long_silent_90[eligible_for_90],
      na.rm = TRUE
    ),

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


# 8. Plots

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
  "Below market (< -10%)" = "#7F7F7F",
  "Near market (-10% to +10%)" = "#5E748A",
  "Moderately above (+10% to +30%)" = "#F2A541",
  "Highly above (> +30%)" = "#D95F02"
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
  "No recorded update" = "#7F7F7F",
  "Updated at least once" = "#F2A541"
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


# 9. Logistic regression:
# Does price premium predict very long silence?

# Outcome:
#   very_long_silent_180 = 1 if silence_days >= 180, 0 otherwise
#
# Important:
#   For a fair 180-day outcome, we only use listings old enough to possibly
#   become 180-day silent: listing_age_days >= 180
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


# 9.1 Simple logistic regression

logit_simple <- glm(
  very_long_silent_180 ~ price_premium_pct,
  data = model_data,
  family = binomial(link = "logit")
)

summary(logit_simple)


# 9.2 Controlled logistic regression

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


# 9.3 Sensitivity model with update status

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


# 9.4 Convert coefficients to odds ratios

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


# 9.5 Focused interpretation table for price premium

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


# 9.6 Predicted probability curve for price premium

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


# 9.7 Logistic regression by price premium band

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



# 10. MAP VISUALIZATIONS


shapefile_path <- file.path(project_dir, "shapes", "Yerevan-Districts.shp")

if (!file.exists(shapefile_path)) {
  stop(
    "Shapefile not found at: ", shapefile_path, "\n",
    "Expected location: <project_dir>/shapes/Yerevan-Districts.shp",
    call. = FALSE
  )
}

districts_sf <- sf::st_read(shapefile_path, quiet = TRUE)

name_col <- "Name_hy"

if (!name_col %in% names(districts_sf)) {
  stop("Column 'Name_hy' was not found in the shapefile.", call. = FALSE)
}

message("Using shapefile name column: ", name_col)


# 10.2 Prepare map join data

normalise <- function(x) {
  stringr::str_squish(stringr::str_to_lower(as.character(x)))
}

districts_named <- districts_sf %>%
  dplyr::mutate(
    shp_name_raw = as.character(.data[[name_col]]),
    shp_name_norm = normalise(shp_name_raw)
  )

map_stats <- community_summary %>%
  dplyr::mutate(
    community_norm = normalise(community)
  ) %>%
  dplyr::select(
    community,
    community_en,
    community_norm,
    median_sqm_price,
    very_long_silent_180_share_eligible,
    eligible_180_n
  )

districts_plot <- districts_named %>%
  dplyr::left_join(
    map_stats,
    by = c("shp_name_norm" = "community_norm")
  )

unmatched_shp <- districts_plot %>%
  dplyr::filter(is.na(community_en)) %>%
  sf::st_drop_geometry() %>%
  dplyr::pull(shp_name_raw)

if (length(unmatched_shp) > 0) {
  message(
    "\nUnmatched shapefile district(s): ",
    paste(unmatched_shp, collapse = ", "),
    "\nThese will appear grey on the map."
  )
}

message("\nMatched map rows:")

districts_plot %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    shp_name_raw,
    community_en,
    median_sqm_price,
    very_long_silent_180_share_eligible,
    eligible_180_n
  ) %>%
  print()


# 10.3 Price map label data


price_label_data <- districts_plot %>%
  dplyr::filter(
    !is.na(median_sqm_price),
    !is.na(community_en)
  ) %>%
  sf::st_point_on_surface()

price_label_xy <- sf::st_coordinates(price_label_data)

price_label_data <- price_label_data %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    x = price_label_xy[, 1],
    y = price_label_xy[, 2],
    label = paste0(
      community_en,
      "\n",
      round(median_sqm_price / 1000),
      "k AMD"
    ),
    x = dplyr::case_when(
      community_en == "Arabkir" ~ x - 0.006,
      community_en == "Kanaker-Zeytun" ~ x + 0.012,
      community_en == "Davtashen" ~ x - 0.006,
      community_en == "Avan" ~ x + 0.006,
      community_en == "Nork-Marash" ~ x + 0.010,
      community_en == "Kentron" ~ x + 0.004,
      TRUE ~ x
    ),
    y = dplyr::case_when(
      community_en == "Arabkir" ~ y + 0.006,
      community_en == "Kanaker-Zeytun" ~ y + 0.009,
      community_en == "Davtashen" ~ y + 0.008,
      community_en == "Avan" ~ y + 0.006,
      community_en == "Nork-Marash" ~ y - 0.004,
      community_en == "Kentron" ~ y - 0.002,
      TRUE ~ y
    )
  )



# 10.4 Choropleth map:
# Median asking price per square meter


p_map_price <- ggplot() +
  geom_sf(
    data = districts_plot,
    aes(fill = median_sqm_price),
    color = "white",
    linewidth = 0.65
  ) +
  geom_label(
    data = price_label_data,
    aes(x = x, y = y, label = label),
    size = 3.25,
    color = "gray15",
    fontface = "bold",
    lineheight = 0.9,
    label.size = 0.15,
    label.padding = grid::unit(0.13, "lines"),
    fill = scales::alpha("white", 0.78)
  ) +
  scale_fill_gradient(
    low = "#d9ecff",
    high = "#08306b",
    na.value = "grey82",
    labels = function(x) paste0(round(x / 1000), "k"),
    name = "Median AMD/m²"
  ) +
  coord_sf(datum = NA, expand = FALSE) +
  labs(
    title = "Median Asking Price per m² by Yerevan District",
    subtitle = "Community median m² price · 24-month analysis window · Grey = fewer than 30 listings",
    caption = "Source: scraped portal listings | Yerevan administrative district boundaries"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    
    plot.title = element_text(
      face = "bold",
      size = 20,
      color = "black",
      hjust = 0.5,
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "gray35",
      hjust = 0.5,
      margin = margin(b = 14)
    ),
    plot.caption = element_text(
      size = 10,
      color = "gray45",
      hjust = 0.5,
      margin = margin(t = 12)
    ),
    
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold", color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    
    plot.margin = margin(20, 30, 20, 30)
  )

ggsave(
  filename = file.path(figures_dir, "10_choropleth_median_price_by_district.png"),
  plot = p_map_price,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)



# 10.5 Inactivity map label data

silent_label_data <- districts_plot %>%
  dplyr::filter(
    !is.na(very_long_silent_180_share_eligible),
    !is.na(community_en)
  ) %>%
  sf::st_point_on_surface()

silent_label_xy <- sf::st_coordinates(silent_label_data)

silent_label_data <- silent_label_data %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    x = silent_label_xy[, 1],
    y = silent_label_xy[, 2],
    label = paste0(
      community_en,
      "\n",
      scales::percent(
        very_long_silent_180_share_eligible,
        accuracy = 0.1
      )
    ),
    x = dplyr::case_when(
      community_en == "Arabkir" ~ x - 0.006,
      community_en == "Kanaker-Zeytun" ~ x + 0.012,
      community_en == "Davtashen" ~ x - 0.006,
      community_en == "Avan" ~ x + 0.006,
      community_en == "Nork-Marash" ~ x + 0.010,
      community_en == "Kentron" ~ x + 0.004,
      TRUE ~ x
    ),
    y = dplyr::case_when(
      community_en == "Arabkir" ~ y + 0.006,
      community_en == "Kanaker-Zeytun" ~ y + 0.009,
      community_en == "Davtashen" ~ y + 0.008,
      community_en == "Avan" ~ y + 0.006,
      community_en == "Nork-Marash" ~ y - 0.004,
      community_en == "Kentron" ~ y - 0.002,
      TRUE ~ y
    )
  )



# 10.6 Choropleth map:
# Share of very long silent listings

p_map_silent <- ggplot() +
  geom_sf(
    data = districts_plot,
    aes(fill = very_long_silent_180_share_eligible),
    color = "white",
    linewidth = 0.65
  ) +
  geom_label(
    data = silent_label_data,
    aes(x = x, y = y, label = label),
    size = 3.25,
    color = "gray15",
    fontface = "bold",
    lineheight = 0.9,
    label.size = 0.15,
    label.padding = grid::unit(0.13, "lines"),
    fill = scales::alpha("white", 0.78)
  ) +
  scale_fill_gradient(
    low = "#fff5eb",
    high = "#7f2704",
    na.value = "grey82",
    labels = scales::percent_format(accuracy = 1),
    name = "Very long silent share"
  ) +
  coord_sf(datum = NA, expand = FALSE) +
  labs(
    title = "Share of Very Long Silent Listings by Yerevan District",
    subtitle = "Share of listings silent for 180+ days among eligible listings · Grey = insufficient data",
    caption = "Source: scraped portal listings | Yerevan administrative district boundaries"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    
    plot.title = element_text(
      face = "bold",
      size = 20,
      color = "black",
      hjust = 0.5,
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "gray35",
      hjust = 0.5,
      margin = margin(b = 14)
    ),
    plot.caption = element_text(
      size = 10,
      color = "gray45",
      hjust = 0.5,
      margin = margin(t = 12)
    ),
    
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold", color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    
    plot.margin = margin(20, 30, 20, 30)
  )

ggsave(
  filename = file.path(figures_dir, "11_choropleth_very_long_silent_share_by_district.png"),
  plot = p_map_silent,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)


# ===============================================================
# PART B: CADASTRE VS PORTAL VALIDATION ANALYSIS
# ===============================================================
# Purpose is to:
#   1. Validate scraped marketplace apartment-sale prices against official cadastre prices.
#   2. Test whether cadastre prices published in later months correspond to portal prices from earlier months.
#   3. Compare both price levels and month-to-month changes across multiple lags.

raw_dir       <- file.path(project_dir, "data", "raw")
processed_dir <- file.path(project_dir, "data", "processed", "cadastre_verification_analysis")
figures_dir   <- file.path(project_dir, "figures", "cadastre_verification_analysis")

# Moslty for console summary
cadastre_processed_dir_summary <- processed_dir
cadastre_figures_dir_summary <- figures_dir

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


# 2. Helper functions

parse_number_strict <- function(x) {
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


# 3. Load and clean portal data

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

portal_yerevan_apartments <- portal_clean %>%
  filter(
    `Մարզ` == "Երևան",
    str_to_lower(`Տեսակ`) == str_to_lower("Բնակարան"),
    !is.na(`Հրապարակվել է`),
    !is.na(`մ² գին (֏)`)
  )

# Use a 24-month window based on the latest portal publication date in the dataset.
max_portal_date <- max(portal_yerevan_apartments$`Հրապարակվել է`, na.rm = TRUE)
cutoff_date <- max_portal_date %m-% months(24)

# IQR outlier removal by community.
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


# 4. Load and clean cadastre data

cadastre_raw <- readxl::read_excel(cadastre_path, sheet = "Yerevan")

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

readr::write_csv(portal_yerevan_apartments_clean, file.path(processed_dir, "portal_yerevan_apartment_sale_clean.csv"))
readr::write_csv(portal_monthly, file.path(processed_dir, "portal_monthly_yerevan_apartment_sale_prices.csv"))
readr::write_csv(cadastre_monthly, file.path(processed_dir, "cadastre_monthly_yerevan_apartment_prices.csv"))

# 5. Build lagged comparison datasets

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


# 6. Correlation analysis

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


# 7. Plots

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


# Additional validation 1:
# Portal vs Cadastre percentage gap over time

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

# Additional validation 2:
# Quarterly trend comparison

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



message("\n===============================================================")
message("REPRODUCIBLE ANALYSIS COMPLETE")
message("===============================================================")

message("\nPART A: LISTINGS-GO-SILENT ANALYSIS")
message("Observation date used: ", observation_date)
message("Raw input file: ", sale_path)
message("Processed outputs saved to: ", silence_processed_dir_summary)
message("Figures saved to: ", silence_figures_dir_summary)

message("\nPART B: CADASTRE VS PORTAL VALIDATION")
message("Portal input file: ", portal_path)
message("Cadastre input file: ", cadastre_path)
message("Selected cadastre lag: ", selected_lag, " months")
message("Processed outputs saved to: ", cadastre_processed_dir_summary)
message("Figures saved to: ", cadastre_figures_dir_summary)

message("\nAll plots and processed tables were regenerated successfully.")
