# CONUS Real Estate EDA

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
| ![Distribution of listings by type](plots_resized_720p/image2-fs8.png) | ![Distribution of listings by type (excl. Apartment)](plots_resized_720p/image1-fs8.png) |

---

## Price Analysis

### Price by Rental Type

Assisted Living and Flat listings command notably higher median rents, while Manufactured homes sit at the lower end of the price spectrum.

![Boxplot of Price grouped by Type of Rental](plots_resized_720p/image8-fs8.png)

### Price Distribution

After IQR-based outlier removal, the price distribution is right-skewed — the majority of listings fall in the $500–$2,000/month range.

![Distribution of Prices](plots_resized_720p/image4-fs8.png)

### Price vs. Square Footage

There is a moderate positive correlation between price and area. Listings colored by state reveal that coastal states (CA, NY, MA) cluster at the higher end.

| By State | With Trend Line |
|---|---|
| ![Price vs Area by State](plots_resized_720p/image9-fs8.png) | ![Scatterplot of price vs sqfeet](plots_resized_720p/image24-fs8.png) |

---

## Property Size Analysis

### Size by Rental Type

Houses and Assisted Living units have the largest median square footage. Cottage/Cabin and In-Law units are the most compact.

![Boxplot of Property Size grouped by Type of Rental](plots_resized_720p/image7-fs8.png)

### Area Distribution

Rental property sizes follow a right-skewed distribution, with most listings between 500–1,500 sq ft.

![Distribution of Area](plots_resized_720p/image3-fs8.png)

### Beds and Baths Distribution

Most rentals are 1–2 bed / 1–2 bath units, consistent with the apartment-heavy composition of the dataset.

| Beds | Baths |
|---|---|
| ![Distribution of Beds](plots_resized_720p/image5-fs8.png) | ![Distribution of Baths](plots_resized_720p/image6-fs8.png) |

---

## Geospatial Analysis

### Price Distribution across CONUS

Rental prices vary significantly by region. Coastal metro areas (California, Northeast corridor) exhibit the highest average rents, while the interior South and Midwest are considerably more affordable.

![Price Distribution across CONUS](plots_resized_720p/image21-fs8.png)

### Average Price per Region

Aggregating by geographic cluster makes the coastal vs. inland pricing disparity clear.

![Average Price per Region](plots_resized_720p/image20-fs8.png)

### Average Property Size per Region

Interior regions (Midwest, South) tend to offer larger rentals at lower prices — the classic square-footage-per-dollar advantage.

| Sq Ft per Region | Price per Region (detail) |
|---|---|
| ![Average sqfeet per region](plots_resized_720p/image22-fs8.png) | ![Price per region detail](plots_resized_720p/image23-fs8.png) |

---

## Amenity Analysis

### Pet Policies

Pet-friendly listings are common — nearly three-quarters of listings allow cats, and nearly as many allow dogs.

| Cats Allowed | Dogs Allowed |
|---|---|
| ![Cats Allowed](plots_resized_720p/image10-fs8.png) | ![Dogs Allowed](plots_resized_720p/image11-fs8.png) |

*74.2% of listings allow cats · 72.1% allow dogs*

### Accessibility & Lifestyle

Smoking is almost universally prohibited. EV charging stations and wheelchair accessibility remain rare, reflecting the age and composition of the housing stock.

| Smoking Allowed | Wheelchair Accessible |
|---|---|
| ![Smoking Allowed](plots_resized_720p/image12-fs8.png) | ![Wheelchair Access](plots_resized_720p/image18-fs8.png) |

| EV Charging | Comes Furnished |
|---|---|
| ![EV Charging](plots_resized_720p/image17-fs8.png) | ![Comes Furnished](plots_resized_720p/image19-fs8.png) |

*3.9% smoking allowed · 7.8% wheelchair accessible · 1% EV charging · 25.3% furnished*

### Laundry Options

In-unit washer/dryer is the most common laundry arrangement (32.7%), followed by on-site laundry and W/D hookups.

| Laundry Options | Laundry Options (detail) |
|---|---|
| ![Laundry Options](plots_resized_720p/image13-fs8.png) | ![Laundry Options detail](plots_resized_720p/image14-fs8.png) |

### Parking Options

Off-street and carport parking make up the majority of arrangements, together accounting for over 70% of listings.

| Parking Options | Parking Options (detail) |
|---|---|
| ![Parking Options](plots_resized_720p/image15-fs8.png) | ![Parking Options detail](plots_resized_720p/image16-fs8.png) |

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
