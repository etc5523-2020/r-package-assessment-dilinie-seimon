
# AusCovid19

<!-- badges: start -->
[![R build status](https://github.com/etc5523-2020/r-package-assessment-dilinie-seimon/workflows/R-CMD-check/badge.svg)](https://github.com/etc5523-2020/r-package-assessment-dilinie-seimon/actions)
<!-- badges: end -->

The aim of the AusCovid19 package is to visualise the spread of COVID-19 in Australia and compare the spread across states.
The application visualizes the number of confirmed COVID-19 cases, recoveries and deaths reported due to COVID-19 in Australia at date, as well as the trend of spread across the states over time. Further, the dashboard also provides the daily case numbers reported in each state.

## Installation

The development version of the AusCovid19 package can be installed from [GitHub](https://github.com/etc5523-2020/r-package-assessment-dilinie-seimon) with:

``` r
# install.packages("remotes")
remotes::install_github("etc5523-2020/r-package-assessment-dilinie-seimon")
```

## Usage

The `download data()` function allows retreival of state level COVID-19 cases (confirmed, recoveries and deaths) in Australia, and can be either 'cumulative' or 'daily' case counts.

```r
#downloading cumulative COVID-19 data
cum_df <- download_data("cumulative")

#downloading daily COVID-19 data
daily_df <- download_data("daily")
```

The `calculate_daily_counts()` function takes the cumulative counts dataframe as an input and returns the respective daily counts dataframe.

```r
calculate_daily_counts(cum_df)
```

The `get_case_count()` function returns the case count of a given type of cases (either confirmed, recoveries or deaths at a given date)

```r
#get cumulative confirmed case count on 2020-06-05
get_case_count(cum_df, "2020-06-05", "Confirmed")

#get daily death count on 2020-06-05
get_case_count(daily_df, "2020-06-05", "Deaths")
```

The `generate_value_box()` function takes a numeric vector, text label, icon and color as input arguments and returns an html value-box widget of the specified color with the sum of the vector as the value, text label as the subtitle and the icon.

```r
generate_value_box(get_case_count(daily_df, "2020-06-05", "Confirmed"), text="Confirmed Cases", icon="head-side-mask", color="aqua")
```

The `launch_app()` function launches a Shiny application visualizing the spread of COVID-19 across the states of Australia.
