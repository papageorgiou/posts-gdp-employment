# GDP & Employment Infographics

A series of five dark editorial infographics comparing GDP, employment rates, and population across five major economies: **USA, Germany, Japan, Brazil, and Nigeria**.

## Charts

| # | File | Description |
|---|------|-------------|
| 1 | `01_scatter_gdp_employment.R` | Scatter — GDP per capita vs. employment rate (2015–2024 trend) |
| 2 | `02_line_gdp_growth.R` | Line — GDP per capita trajectories over time |
| 3 | `03_bar_employment_2024.R` | Bar — Employment rates snapshot, 2024 |
| 4 | `04_slope_employment_change.R` | Slope — Employment change 2015 → 2024 |
| 5 | `05_bubble_gdp_emp_pop.R` | Bubble — GDP × Employment × Population with country flags |

## Design

- **Background:** `#13131F` (midnight dark)
- **Accent:** `#C9B1FF` (soft purple)
- **Canvas:** 1200 × 2400 px

Each infographic combines a `ggplot2` chart with a full editorial layout built using `magick`: headline, stat panels, key insight, and source attribution.

## Dependencies

```r
install.packages(c("ggplot2", "tidyverse", "scales", "magick"))

# Chart 5 also requires ggflags (auto-installed on first run):
install.packages("ggflags", repos = c(
  "https://jimjam-slam.r-universe.dev", "https://cloud.r-project.org"
))
```

## Usage

Run any script individually from the project root:

```r
source("01_scatter_gdp_employment.R")
source("05_bubble_gdp_emp_pop.R")  # outputs 05_bubble_gdp_emp_pop.png
```

## Data

All data is synthetic and intended for illustration purposes only.

## License

MIT
