# When Listings Go Silent

This project analyzes whether long inactivity in Armenian real-estate portal listings can be explained by apartment overpricing. The main focus is Yerevan apartment sale listings.

The project produces all figures used in the paper from one reproducible R script.

## Main Research Question

Do apartments listed above their local market benchmark tend to remain silent for a longer period of time?

In this project, a listing is considered **silent** when it has not shown visible activity for a long time. Activity is measured using the latest available listing date:

```r
last_activity_date = updated_date if available, otherwise published_date
```

Then:

```r
silence_days = observation_date - last_activity_date
```

The main long-inactivity threshold used in the analysis is:

```r
very_long_silent_180 = silence_days >= 180
```

## Main Finding

The results do **not** support the original expectation that higher-priced apartments are more likely to go silent for a long period.

Instead, long silence appears to be common across all price-position groups. Listings below market, near market, moderately above market, and highly above market all show high rates of 180+ day silence.

The controlled logistic regression also shows only a weak relationship between price premium and the probability of 180+ day silence. Therefore, in this dataset, overpricing does not appear to be the main explanation for long listing inactivity.

## Project Structure

The expected project structure is:

```text
Project/
├── final_reproducible_code.R
├── README.md
├── data/
│   ├── raw/
│   │   ├── apartment_house_sale.csv
│   │   └── apartment_price.xlsx
│   └── processed/
│       ├── silence_analysis/
│       └── cadastre_verification_analysis/
├── figures/
│   ├── silence_analysis/
│   └── cadastre_verification_analysis/
└── shapes/
    ├── Yerevan-Districts.shp
    ├── Yerevan-Districts.dbf
    ├── Yerevan-Districts.shx
    └── other related shapefile files
```

The folders under `data/processed/` and `figures/` are created automatically by the R script if they do not already exist.

## Required Input Files

The script expects the following raw data files:

```text
data/raw/apartment_house_sale.csv
data/raw/apartment_price.xlsx
```

For the map visualizations, the script also expects the Yerevan district shapefile:

```text
shapes/Yerevan-Districts.shp
```

The shapefile must include its related files, such as `.dbf`, `.shx`, and any other sidecar files required by the shapefile.

## Required R Packages

The script uses the following R packages:

```r
tidyverse
lubridate
scales
readxl
zoo
sf
```

Install missing packages with:

```r
install.packages(c(
  "tidyverse",
  "lubridate",
  "scales",
  "readxl",
  "zoo",
  "sf"
))
```

## How to Reproduce the Analysis

Open RStudio or R and set the project folder correctly in the script if needed.

In the submitted version, the script uses:

```r
project_dir <- "C:/Users/rsari/R_projects/dataviz_project/Project"
```

If the project is located somewhere else, update this path before running the script.

Then run the full script:

```r
source("final_reproducible_code.R")
```

Running this one file regenerates all processed tables and all figures used in the paper.

## Outputs

### Silence Analysis Outputs

Processed files are saved to:

```text
data/processed/silence_analysis/
```

Figures are saved to:

```text
figures/silence_analysis/
```

The main figures include:

```text
01_silence_days_distribution_histogram.png
02_silence_band_distribution.png
03_silence_by_price_premium_band_boxplot.png
04_very_long_silent_share_by_price_premium_band.png
05_very_long_silent_share_by_community.png
06_very_long_silent_share_by_publication_month.png
07_silence_by_update_status_boxplot.png
08_update_delay_distribution.png
09_logistic_predicted_probability_180_by_price_premium.png
10_choropleth_median_price_by_district.png
11_choropleth_very_long_silent_share_by_district.png
```

### Cadastre Validation Outputs

Processed files are saved to:

```text
data/processed/cadastre_verification_analysis/
```

Figures are saved to:

```text
figures/cadastre_verification_analysis/
```

The main validation figures include:

```text
cadastre_portal_pearson_correlations_by_lag.png
cadastre_portal_spearman_correlations_by_lag.png
portal_cadastre_percentage_gap_lag2.png
portal_cadastre_quarterly_trend_lag2.png
```

## Method Summary

### Part A: Listings-Go-Silent Analysis

The script first cleans Yerevan apartment sale listings and restricts the analysis to a recent 24-month window. It removes price and area outliers by community using the IQR rule.

It then builds local price benchmarks using a fallback system:

1. Community-month median m² price
2. Community-quarter median m² price
3. Community-level median m² price
4. Yerevan-month median m² price

Each listing receives a price premium:

```r
price_premium_pct =
  (listing_sqm_price - benchmark_sqm_price) / benchmark_sqm_price * 100
```

Listings are grouped into price premium bands:

```text
Below market (< -10%)
Near market (-10% to +10%)
Moderately above (+10% to +30%)
Highly above (> +30%)
```

The main outcome is whether a listing is silent for 180+ days, among listings old enough to reach that threshold.

The script also fits logistic regression models to test whether price premium predicts very long silence.

### Part B: Cadastre vs Portal Validation

The second part compares scraped portal asking prices with official cadastre apartment prices for Yerevan.

The purpose is not to prove that portal prices and cadastre prices are identical. Instead, it checks whether portal prices move in a broadly similar direction to official prices, possibly with a publication lag.

The script tests lags from 0 to 4 months and compares:

```text
Price levels
Absolute monthly changes
Percentage monthly changes
Raw monthly series
Smoothed 3-month series
```

It also creates a portal-cadastre price gap plot and a quarterly trend comparison.

## Important Interpretation Notes

Listing silence does **not** prove that a property was sold. A silent listing may mean several things:

```text
The apartment may have been sold.
The listing may have expired.
The owner or agency may have stopped updating it.
The listing may remain online even after the market situation changed.
The platform may not reliably remove inactive listings.
```

Therefore, the project interprets silence as a market-inactivity signal, not as confirmed sale status.

The results should also not be interpreted as proof that price has no effect at all. The better conclusion is that, in this dataset and with this silence definition, higher price premium is not a strong explanation for 180+ day listing silence.
