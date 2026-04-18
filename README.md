# CONUS Real Estate EDA

![R](https://img.shields.io/badge/R-4.x-276DC3?logo=r&logoColor=white)
![License](https://img.shields.io/badge/license-MIT-green)

> Exploratory data analysis of U.S. rental listings across the Continental United States, built with R.

---

## Table of Contents

- [Overview](#overview)
- [Dataset](#dataset)
- [Methodology](#methodology)
- [Rental Type Distribution](#rental-type-distribution)
- [Price Analysis](#price-analysis)
- [Property Size Analysis](#property-size-analysis)
- [Geospatial Analysis](#geospatial-analysis)
- [Amenity Analysis](#amenity-analysis)
- [Tools & Techniques](#tools--techniques)

---

## Overview

This project explores rental listing data from across the Continental United States (CONUS). The goal was to uncover regional pricing patterns, understand how property type affects rent and size, and visualize the distribution of amenities offered in rental listings. Analysis was performed entirely in R as part of a Statistical Computing course.

---

## Dataset

**Source:** [USA Housing Listings — Kaggle](https://www.kaggle.com/datasets/austinreese/usa-housing-listings)

Craigslist rental listings scraped from across the U.S., containing ~700,000 raw records with variables including:

| Variable | Description |
|---|---|
| `price` | Monthly rent (USD) |
| `sqfeet` | Property area in square feet |
| `beds` / `baths` | Number of bedrooms and bathrooms |
| `type` | Rental category (Apartment, House, Condo, etc.) |
| `state` | U.S. state abbreviation |
| `lat` / `long` | Geolocation coordinates |
| `cats_allowed`, `dogs_allowed` | Pet policies |
| `smoking_allowed`, `wheelchair_access` | Accessibility and lifestyle features |
| `comes_furnished`, `electric_vehicle_charge` | Additional amenities |
| `laundry_options`, `parking_options` | In-unit or shared facilities |

---

## Methodology

1. **Ingestion** — Loaded raw CSV; dropped URL and ID columns not relevant to analysis.
2. **Cleaning** — Removed records missing key variables; standardized `type` to title case and `state` to uppercase.
3. **Outlier Removal** — Applied IQR-based filtering grouped by rental type for `price`, `sqfeet`, `beds`, and `baths`, preventing outliers in one category from distorting another.
4. **Geographic Filter** — Constrained coordinates to CONUS bounding box (lat 24–50°N, lon 66–125°W).
5. **Visualization** — All plots use a consistent black-background theme with viridis / light-blue palettes.

---

## Rental Type Distribution

The dataset is heavily dominated by **Apartment** listings (~300,000), reflecting Craigslist's urban user base. Excluding apartments reveals a more balanced spread, with **House** and **Townhouse** as the next most common types.

| All Rental Types | Excluding Apartments |
|---|---|
| <img src="plots/image2.png" width="460"> | <img src="plots/image1.png" width="460"> |

---

## Price Analysis

### Price by Rental Type

Assisted Living and Flat listings command notably higher median rents, while Manufactured homes sit at the lower end of the price spectrum.

<img src="plots/image8.png" width="900">

### Price Distribution

After IQR-based outlier removal, the price distribution is right-skewed — the majority of listings fall in the $500–$2,000/month range.

<img src="plots/image4.png" width="900">

### Price vs. Square Footage

There is a moderate positive correlation between price and area. Listings colored by state reveal that coastal states (CA, NY, MA) cluster at the higher end.

| By State | With Trend Line |
|---|---|
| <img src="plots/image9.png" width="460"> | <img src="plots/image24.png" width="460"> |

---

## Property Size Analysis

### Size by Rental Type

Houses and Assisted Living units have the largest median square footage. Cottage/Cabin and In-Law units are the most compact.

<img src="plots/image7.png" width="900">

### Area Distribution

Rental property sizes follow a right-skewed distribution, with most listings between 500–1,500 sq ft.

<img src="plots/image3.png" width="900">

### Beds and Baths Distribution

Most rentals are 1–2 bed / 1–2 bath units, consistent with the apartment-heavy composition of the dataset.

| Beds | Baths |
|---|---|
| <img src="plots/image5.png" width="460"> | <img src="plots/image6.png" width="460"> |

---

## Geospatial Analysis

### Price Distribution across CONUS

Rental prices vary significantly by region. Coastal metro areas (California, Northeast corridor) exhibit the highest average rents, while the interior South and Midwest are considerably more affordable.

<img src="plots/image21.png" width="900">

### Average Price per Region

Aggregating by geographic cluster makes the coastal vs. inland pricing disparity clear.

<img src="plots/image20.png" width="900">

### Average Property Size per Region

Interior regions (Midwest, South) tend to offer larger rentals at lower prices — the classic square-footage-per-dollar advantage.

| Sq Ft per Region | Price per Region (detail) |
|---|---|
| <img src="plots/image22.png" width="460"> | <img src="plots/image23.png" width="460"> |

---

## Amenity Analysis

### Pet Policies

Pet-friendly listings are common — nearly three-quarters of listings allow cats, and nearly as many allow dogs.

| Cats Allowed | Dogs Allowed |
|---|---|
| <img src="plots/image10.png" width="460"> | <img src="plots/image11.png" width="460"> |

*74.2% of listings allow cats · 72.1% allow dogs*

### Accessibility & Lifestyle

Smoking is almost universally prohibited. EV charging stations and wheelchair accessibility remain rare, reflecting the age and composition of the housing stock.

| Smoking Allowed | Wheelchair Accessible |
|---|---|
| <img src="plots/image12.png" width="460"> | <img src="plots/image18.png" width="460"> |

| EV Charging | Comes Furnished |
|---|---|
| <img src="plots/image17.png" width="460"> | <img src="plots/image19.png" width="460"> |

*3.9% smoking allowed · 7.8% wheelchair accessible · 1% EV charging · 25.3% furnished*

### Laundry Options

In-unit washer/dryer is the most common laundry arrangement (32.7%), followed by on-site laundry and W/D hookups.

| Laundry Options | Laundry Options (detail) |
|---|---|
| <img src="plots/image13.png" width="460"> | <img src="plots/image14.png" width="460"> |

### Parking Options

Off-street and carport parking make up the majority of arrangements, together accounting for over 70% of listings.

| Parking Options | Parking Options (detail) |
|---|---|
| <img src="plots/image15.png" width="460"> | <img src="plots/image16.png" width="460"> |

---

## Code Highlights

### IQR-Based Outlier Removal (grouped by rental type)

```r
remove_outliers_multi <- function(df, columns, group_col) {
  df %>%
    group_by(across(all_of(group_col))) %>%
    filter({
      conditions <- lapply(columns, function(col) {
        Q1 <- quantile(get(col), 0.25, na.rm = TRUE)
        Q3 <- quantile(get(col), 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        get(col) >= Q1 - 1.5 * IQR & get(col) <= Q3 + 1.5 * IQR
      })
      Reduce(`&`, conditions)
    }) %>%
    ungroup()
}

df_housing_cleaned <- remove_outliers_multi(
  df      = df_housing_vars_of_interest,
  columns = c("price", "sqfeet", "beds", "baths"),
  group_col = "type"
)
```

### Black-Themed ggplot2 Boxplot

```r
ggplot(df_housing_cleaned, aes(x = type, y = price, fill = type)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = 0.8, color = "white") +
  scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.8) +
  labs(
    title = "Boxplot of Price grouped by Type of Rental",
    x = "Type of Rental", y = "Price ($)"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(color = "white", size = 14, face = "bold"),
    axis.title      = element_text(color = "white", size = 12),
    axis.text       = element_text(color = "white", size = 10),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.text     = element_text(color = "white"),
    legend.title    = element_text(color = "white", face = "bold"),
    plot.background = element_rect(fill = "black", color = NA),
    panel.background= element_rect(fill = "black", color = NA),
    panel.grid.major= element_line(color = "gray30", size = 0.5)
  )
```

### Geospatial Overlay with Stadia Maps

```r
register_stadiamaps(key = Sys.getenv("STADIA_API_KEY"))

bbox     <- c(left = -155, bottom = 20, right = -50, top = 66)
base_map <- get_stadiamap(bbox = bbox, zoom = 6, maptype = "stamen_toner")

ggmap(base_map) +
  geom_point(
    data  = df_housing_cleaned,
    aes(x = long, y = lat, color = sqfeet),
    alpha = 0.3, size = 2
  ) +
  scale_color_viridis_c() +
  labs(
    title = "Property Size Distribution across CONUS",
    x = "Longitude", y = "Latitude", color = "Area (Sq Feet)"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(color = "white", size = 14, face = "bold"),
    plot.background = element_rect(fill = "black", color = NA),
    panel.background= element_rect(fill = "black", color = NA)
  )
```

---

## Tools & Techniques

**Language:** R

**Key Libraries:**

| Library | Purpose |
|---|---|
| `tidyverse` | Data wrangling and transformation |
| `ggplot2` | Static visualizations |
| `ggmap` | Geospatial map tiles (Stadia Maps) |
| `ggcorrplot` | Correlation matrix plots |
| `viridis` | Perceptually uniform color scales |
| `skimr` | Summary statistics |
| `janitor` | Column name cleaning |
| `scales` | Axis formatting |

**Techniques:**
- IQR-based outlier removal grouped by rental category
- Geospatial visualization overlaid on Stadia Maps tiles
- Correlation analysis between price, area, beds, and baths
- Summary statistics with regional aggregation

**Dataset:** [USA Housing Listings on Kaggle](https://www.kaggle.com/datasets/austinreese/usa-housing-listings)  
**Installed packages:** [installed_packages.csv](installed_packages.csv)
