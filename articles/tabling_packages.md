# Tabling Packages

## Introduction

These vignettes will walk through examples for some of the most common
tabling packages including gtsummary, kable/kableExtra, table1. All
examples here work with the toy datasets such as `mtcars`.

## gtsummary

gtsummary is commonly used for its flexibility and compatability with
different types of output. The syntax is pretty straightforward and can
make a table of all variables with one line of code and minimal
adjusting.

``` r

gtsummary::tbl_summary(mtcars) 
```

[TABLE]

You can stratify by multiple variables using `tbl_strata`

``` r

trial |>
  gtsummary::tbl_strata(
    strata = trt,
    .tbl_fun =
      ~ .x |>
        gtsummary::tbl_summary(by = stage) 
  ) 
```

[TABLE]

You can split tables by a variable, add missingness, change variable
labels, adjust which statistics are presented and more… It is a very
flexible package.

``` r

gtsummary::tbl_summary(trial, 
            by = trt,
            missing = "ifany",
            missing_text = "Missing",
            label = grade ~ "Tumor Grade",
            statistic = list(age~"{median} ({p25},{p75})",
                             marker ~ "{mean} ({sd})",
                             gtsummary::all_categorical() ~ "{n}/{N}")) |>
    gtsummary::add_p(pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 2)) |>
    gtsummary::add_overall() |>
    gtsummary::add_q(method = "fdr") |>
    gtsummary::modify_header(label = "**Variables**") |>
    gtsummary::bold_labels() |>
    gtsummary::italicize_levels() |>
    gtsummary::modify_caption("**Table 1**") 
```

[TABLE]

**Table 1** {.table .gt_table quarto-disable-processing="false"
quarto-bootstrap="false"}

gtsummary has themes that can be adjusted as shown below. This code sets
the default gtsummary package theme to be “jama” theme” and will be used
for all gtsummary objects unless otherwise specified.

``` r

# changing aesthetics using themes
gtsummary::theme_gtsummary_journal("jama", set_theme = T)
#> Setting theme "JAMA"
gtsummary::tbl_summary(mtcars) 
```

[TABLE]

Themes can apply to more than visual aspects and can set how certain
variables are displayed

``` r

# changing how continuous variables are presented using themes
gtsummary::theme_gtsummary_mean_sd(set_theme = TRUE)
gtsummary::tbl_summary(mtcars) 
```

[TABLE]

Note this theme shows median, mean and IQR

``` r

gtsummary::theme_gtsummary_eda(set_theme = TRUE) #note this theme shows median, mean and IQR
#> Setting theme "Exploratory Data Analysis"
gtsummary::tbl_summary(mtcars) 
```

[TABLE]

Explore `?theme_gtsummary` for more ways to set themes for GT Summary
Tables.

Other functions that may be of interest:

tbl_uvregression() - For running a series of univariate analyses

``` r

gtsummary::tbl_uvregression(
  trial,
  method = glm,
  y = response,
  method.args = list(family = binomial),
  exponentiate = TRUE,
  include = c("age", "grade", "stage")
)
```

[TABLE]

tbl_regression() - For summarizing a single regression model. Also
supports survival models, and some Bayesian models from the rstanarm and
brms packages

``` r

stats::glm(response ~ trt, data= trial) |> 
  gtsummary::tbl_regression(exponentiate = TRUE)
```

[TABLE]

tbl_stack() or tbl_merge - to combine table results

``` r

# stacking two tbl_regression objects
t1 <-
  stats::glm(response ~ trt, trial, family = binomial) |>
  gtsummary::tbl_regression(
    exponentiate = TRUE,
    label = list(trt ~ "Treatment (unadjusted)")
  )

t2 <-
  stats::glm(response ~ trt + grade + stage + marker, trial, family = binomial) |>
  gtsummary::tbl_regression(
    include = "trt",
    exponentiate = TRUE,
    label = list(trt ~ "Treatment (adjusted)")
  )

gtsummary::tbl_stack(list(t1, t2))
```

[TABLE]

``` r

t3 <-
  stats::glm(response ~ trt + grade + age, trial, family = binomial) |>
  gtsummary::tbl_regression(exponentiate = TRUE)
t4 <-
  survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + age, trial) |>
  gtsummary::tbl_regression(exponentiate = TRUE)

gtsummary::tbl_merge(
  tbls = list(t3, t4),
  tab_spanner = c("**Tumor Response**", "**Time to Death**")
)
```

[TABLE]

## Flextable

Flextable plays well with gtsummary with the function `as_flex_table`.
This may be of interest as flextable advertises itself as one of the few
tabling packages that plays well with HTML, PDF and Word outputs. Most
packages handle html well but sometimes struggle with losing capability
with word or pdf outputting. Note the `autofit` function should fix any
width problems that may occur with tables running off the page.

``` r

ft <- flextable::flextable(airquality[ sample.int(10),])
ft <- flextable::add_header_row(ft,
  colwidths = c(4, 2),
  values = c("Air quality", "Time")
)
ft <- flextable::theme_vanilla(ft)
ft <- flextable::add_footer_lines(ft, "Daily air quality measurements in New York, May to September 1973.")
ft <- flextable::color(ft, part = "footer", color = "#666666")
ft <- flextable::set_caption(ft, caption = "New York Air Quality Measurements")
ft
```

| Air quality |  |  |  | Time |  |
|----|----|----|----|----|----|
| Ozone | Solar.R | Wind | Temp | Month | Day |
| 41 | 190 | 7.4 | 67 | 5 | 1 |
| 28 |  | 14.9 | 66 | 5 | 6 |
| 19 | 99 | 13.8 | 59 | 5 | 8 |
| 8 | 19 | 20.1 | 61 | 5 | 9 |
| 36 | 118 | 8.0 | 72 | 5 | 2 |
| 23 | 299 | 8.6 | 65 | 5 | 7 |
|  | 194 | 8.6 | 69 | 5 | 10 |
|  |  | 14.3 | 56 | 5 | 5 |
| 18 | 313 | 11.5 | 62 | 5 | 4 |
| 12 | 149 | 12.6 | 74 | 5 | 3 |
| Daily air quality measurements in New York, May to September 1973. |  |  |  |  |  |

New York Air Quality Measurements {.table .cl-92b74370
quarto-disable-processing="true"}

Flextable also offers a variety of themes / settings that can be
adjusted.

``` r

flextable::flextable(airquality[ sample.int(10),]) |> flextable::theme_box()
```

| Ozone | Solar.R | Wind | Temp | Month | Day |
|-------|---------|------|------|-------|-----|
|       | 194     | 8.6  | 69   | 5     | 10  |
| 41    | 190     | 7.4  | 67   | 5     | 1   |
| 18    | 313     | 11.5 | 62   | 5     | 4   |
| 8     | 19      | 20.1 | 61   | 5     | 9   |
| 36    | 118     | 8.0  | 72   | 5     | 2   |
|       |         | 14.3 | 56   | 5     | 5   |
| 23    | 299     | 8.6  | 65   | 5     | 7   |
| 19    | 99      | 13.8 | 59   | 5     | 8   |
| 12    | 149     | 12.6 | 74   | 5     | 3   |
| 28    |         | 14.9 | 66   | 5     | 6   |

Or changing default settings like below

``` r

flextable::set_flextable_defaults(
  font.size = 10, theme_fun = flextable::theme_vanilla,
  padding = 6,
  background.color = "#EFEFEF")
flextable::flextable(airquality[ sample.int(10),]) |> flextable::autofit()
```

| Ozone | Solar.R | Wind | Temp | Month | Day |
|-------|---------|------|------|-------|-----|
| 19    | 99      | 13.8 | 59   | 5     | 8   |
| 12    | 149     | 12.6 | 74   | 5     | 3   |
| 18    | 313     | 11.5 | 62   | 5     | 4   |
| 41    | 190     | 7.4  | 67   | 5     | 1   |
| 8     | 19      | 20.1 | 61   | 5     | 9   |
| 23    | 299     | 8.6  | 65   | 5     | 7   |
|       | 194     | 8.6  | 69   | 5     | 10  |
| 28    |         | 14.9 | 66   | 5     | 6   |
|       |         | 14.3 | 56   | 5     | 5   |
| 36    | 118     | 8.0  | 72   | 5     | 2   |

## Kable/KableExtra

Kable can be viewed as the base package that can be built on or styled
with KableExtra. Note if you load KableExtra, kable will be loaded in
the background if it is not already. KableExtra also allows working with
piping (%\>% or \|\> ) for added simplicity when editing a table.

The first step, the kable call is pretty simple but is fairly limited in
themes etc.

``` r

kableExtra::kable(mtcars, align = "lccrr")
```

|                     | mpg  | cyl | disp  |  hp | drat | wt    | qsec  | vs  |  am | gear | carb |
|:--------------------|:-----|:---:|:-----:|----:|-----:|:------|:-----:|:---:|----:|-----:|:-----|
| Mazda RX4           | 21.0 |  6  | 160.0 | 110 | 3.90 | 2.620 | 16.46 |  0  |   1 |    4 | 4    |
| Mazda RX4 Wag       | 21.0 |  6  | 160.0 | 110 | 3.90 | 2.875 | 17.02 |  0  |   1 |    4 | 4    |
| Datsun 710          | 22.8 |  4  | 108.0 |  93 | 3.85 | 2.320 | 18.61 |  1  |   1 |    4 | 1    |
| Hornet 4 Drive      | 21.4 |  6  | 258.0 | 110 | 3.08 | 3.215 | 19.44 |  1  |   0 |    3 | 1    |
| Hornet Sportabout   | 18.7 |  8  | 360.0 | 175 | 3.15 | 3.440 | 17.02 |  0  |   0 |    3 | 2    |
| Valiant             | 18.1 |  6  | 225.0 | 105 | 2.76 | 3.460 | 20.22 |  1  |   0 |    3 | 1    |
| Duster 360          | 14.3 |  8  | 360.0 | 245 | 3.21 | 3.570 | 15.84 |  0  |   0 |    3 | 4    |
| Merc 240D           | 24.4 |  4  | 146.7 |  62 | 3.69 | 3.190 | 20.00 |  1  |   0 |    4 | 2    |
| Merc 230            | 22.8 |  4  | 140.8 |  95 | 3.92 | 3.150 | 22.90 |  1  |   0 |    4 | 2    |
| Merc 280            | 19.2 |  6  | 167.6 | 123 | 3.92 | 3.440 | 18.30 |  1  |   0 |    4 | 4    |
| Merc 280C           | 17.8 |  6  | 167.6 | 123 | 3.92 | 3.440 | 18.90 |  1  |   0 |    4 | 4    |
| Merc 450SE          | 16.4 |  8  | 275.8 | 180 | 3.07 | 4.070 | 17.40 |  0  |   0 |    3 | 3    |
| Merc 450SL          | 17.3 |  8  | 275.8 | 180 | 3.07 | 3.730 | 17.60 |  0  |   0 |    3 | 3    |
| Merc 450SLC         | 15.2 |  8  | 275.8 | 180 | 3.07 | 3.780 | 18.00 |  0  |   0 |    3 | 3    |
| Cadillac Fleetwood  | 10.4 |  8  | 472.0 | 205 | 2.93 | 5.250 | 17.98 |  0  |   0 |    3 | 4    |
| Lincoln Continental | 10.4 |  8  | 460.0 | 215 | 3.00 | 5.424 | 17.82 |  0  |   0 |    3 | 4    |
| Chrysler Imperial   | 14.7 |  8  | 440.0 | 230 | 3.23 | 5.345 | 17.42 |  0  |   0 |    3 | 4    |
| Fiat 128            | 32.4 |  4  | 78.7  |  66 | 4.08 | 2.200 | 19.47 |  1  |   1 |    4 | 1    |
| Honda Civic         | 30.4 |  4  | 75.7  |  52 | 4.93 | 1.615 | 18.52 |  1  |   1 |    4 | 2    |
| Toyota Corolla      | 33.9 |  4  | 71.1  |  65 | 4.22 | 1.835 | 19.90 |  1  |   1 |    4 | 1    |
| Toyota Corona       | 21.5 |  4  | 120.1 |  97 | 3.70 | 2.465 | 20.01 |  1  |   0 |    3 | 1    |
| Dodge Challenger    | 15.5 |  8  | 318.0 | 150 | 2.76 | 3.520 | 16.87 |  0  |   0 |    3 | 2    |
| AMC Javelin         | 15.2 |  8  | 304.0 | 150 | 3.15 | 3.435 | 17.30 |  0  |   0 |    3 | 2    |
| Camaro Z28          | 13.3 |  8  | 350.0 | 245 | 3.73 | 3.840 | 15.41 |  0  |   0 |    3 | 4    |
| Pontiac Firebird    | 19.2 |  8  | 400.0 | 175 | 3.08 | 3.845 | 17.05 |  0  |   0 |    3 | 2    |
| Fiat X1-9           | 27.3 |  4  | 79.0  |  66 | 4.08 | 1.935 | 18.90 |  1  |   1 |    4 | 1    |
| Porsche 914-2       | 26.0 |  4  | 120.3 |  91 | 4.43 | 2.140 | 16.70 |  0  |   1 |    5 | 2    |
| Lotus Europa        | 30.4 |  4  | 95.1  | 113 | 3.77 | 1.513 | 16.90 |  1  |   1 |    5 | 2    |
| Ford Pantera L      | 15.8 |  8  | 351.0 | 264 | 4.22 | 3.170 | 14.50 |  0  |   1 |    5 | 4    |
| Ferrari Dino        | 19.7 |  6  | 145.0 | 175 | 3.62 | 2.770 | 15.50 |  0  |   1 |    5 | 6    |
| Maserati Bora       | 15.0 |  8  | 301.0 | 335 | 3.54 | 3.570 | 14.60 |  0  |   1 |    5 | 8    |
| Volvo 142E          | 21.4 |  4  | 121.0 | 109 | 4.11 | 2.780 | 18.60 |  1  |   1 |    4 | 2    |

Adding a simple kable_styling() call makes it much better visually

``` r

kableExtra::kable(mtcars, align = "lccrr") |> kableExtra::kable_styling()
```

|                     | mpg  | cyl | disp  |  hp | drat | wt    | qsec  | vs  |  am | gear | carb |
|:--------------------|:-----|:---:|:-----:|----:|-----:|:------|:-----:|:---:|----:|-----:|:-----|
| Mazda RX4           | 21.0 |  6  | 160.0 | 110 | 3.90 | 2.620 | 16.46 |  0  |   1 |    4 | 4    |
| Mazda RX4 Wag       | 21.0 |  6  | 160.0 | 110 | 3.90 | 2.875 | 17.02 |  0  |   1 |    4 | 4    |
| Datsun 710          | 22.8 |  4  | 108.0 |  93 | 3.85 | 2.320 | 18.61 |  1  |   1 |    4 | 1    |
| Hornet 4 Drive      | 21.4 |  6  | 258.0 | 110 | 3.08 | 3.215 | 19.44 |  1  |   0 |    3 | 1    |
| Hornet Sportabout   | 18.7 |  8  | 360.0 | 175 | 3.15 | 3.440 | 17.02 |  0  |   0 |    3 | 2    |
| Valiant             | 18.1 |  6  | 225.0 | 105 | 2.76 | 3.460 | 20.22 |  1  |   0 |    3 | 1    |
| Duster 360          | 14.3 |  8  | 360.0 | 245 | 3.21 | 3.570 | 15.84 |  0  |   0 |    3 | 4    |
| Merc 240D           | 24.4 |  4  | 146.7 |  62 | 3.69 | 3.190 | 20.00 |  1  |   0 |    4 | 2    |
| Merc 230            | 22.8 |  4  | 140.8 |  95 | 3.92 | 3.150 | 22.90 |  1  |   0 |    4 | 2    |
| Merc 280            | 19.2 |  6  | 167.6 | 123 | 3.92 | 3.440 | 18.30 |  1  |   0 |    4 | 4    |
| Merc 280C           | 17.8 |  6  | 167.6 | 123 | 3.92 | 3.440 | 18.90 |  1  |   0 |    4 | 4    |
| Merc 450SE          | 16.4 |  8  | 275.8 | 180 | 3.07 | 4.070 | 17.40 |  0  |   0 |    3 | 3    |
| Merc 450SL          | 17.3 |  8  | 275.8 | 180 | 3.07 | 3.730 | 17.60 |  0  |   0 |    3 | 3    |
| Merc 450SLC         | 15.2 |  8  | 275.8 | 180 | 3.07 | 3.780 | 18.00 |  0  |   0 |    3 | 3    |
| Cadillac Fleetwood  | 10.4 |  8  | 472.0 | 205 | 2.93 | 5.250 | 17.98 |  0  |   0 |    3 | 4    |
| Lincoln Continental | 10.4 |  8  | 460.0 | 215 | 3.00 | 5.424 | 17.82 |  0  |   0 |    3 | 4    |
| Chrysler Imperial   | 14.7 |  8  | 440.0 | 230 | 3.23 | 5.345 | 17.42 |  0  |   0 |    3 | 4    |
| Fiat 128            | 32.4 |  4  | 78.7  |  66 | 4.08 | 2.200 | 19.47 |  1  |   1 |    4 | 1    |
| Honda Civic         | 30.4 |  4  | 75.7  |  52 | 4.93 | 1.615 | 18.52 |  1  |   1 |    4 | 2    |
| Toyota Corolla      | 33.9 |  4  | 71.1  |  65 | 4.22 | 1.835 | 19.90 |  1  |   1 |    4 | 1    |
| Toyota Corona       | 21.5 |  4  | 120.1 |  97 | 3.70 | 2.465 | 20.01 |  1  |   0 |    3 | 1    |
| Dodge Challenger    | 15.5 |  8  | 318.0 | 150 | 2.76 | 3.520 | 16.87 |  0  |   0 |    3 | 2    |
| AMC Javelin         | 15.2 |  8  | 304.0 | 150 | 3.15 | 3.435 | 17.30 |  0  |   0 |    3 | 2    |
| Camaro Z28          | 13.3 |  8  | 350.0 | 245 | 3.73 | 3.840 | 15.41 |  0  |   0 |    3 | 4    |
| Pontiac Firebird    | 19.2 |  8  | 400.0 | 175 | 3.08 | 3.845 | 17.05 |  0  |   0 |    3 | 2    |
| Fiat X1-9           | 27.3 |  4  | 79.0  |  66 | 4.08 | 1.935 | 18.90 |  1  |   1 |    4 | 1    |
| Porsche 914-2       | 26.0 |  4  | 120.3 |  91 | 4.43 | 2.140 | 16.70 |  0  |   1 |    5 | 2    |
| Lotus Europa        | 30.4 |  4  | 95.1  | 113 | 3.77 | 1.513 | 16.90 |  1  |   1 |    5 | 2    |
| Ford Pantera L      | 15.8 |  8  | 351.0 | 264 | 4.22 | 3.170 | 14.50 |  0  |   1 |    5 | 4    |
| Ferrari Dino        | 19.7 |  6  | 145.0 | 175 | 3.62 | 2.770 | 15.50 |  0  |   1 |    5 | 6    |
| Maserati Bora       | 15.0 |  8  | 301.0 | 335 | 3.54 | 3.570 | 14.60 |  0  |   1 |    5 | 8    |
| Volvo 142E          | 21.4 |  4  | 121.0 | 109 | 4.11 | 2.780 | 18.60 |  1  |   1 |    4 | 2    |

Below are a handful of other options found within the “Kable universe”.

``` r

mtcars |>
  kableExtra::kbl(caption = "Recreating booktabs style table") |>
  kableExtra::kable_classic(full_width = F, html_font = "Cambria")
```

|                     |  mpg | cyl |  disp |  hp | drat |    wt |  qsec |  vs |  am | gear | carb |
|:--------------------|-----:|----:|------:|----:|-----:|------:|------:|----:|----:|-----:|-----:|
| Mazda RX4           | 21.0 |   6 | 160.0 | 110 | 3.90 | 2.620 | 16.46 |   0 |   1 |    4 |    4 |
| Mazda RX4 Wag       | 21.0 |   6 | 160.0 | 110 | 3.90 | 2.875 | 17.02 |   0 |   1 |    4 |    4 |
| Datsun 710          | 22.8 |   4 | 108.0 |  93 | 3.85 | 2.320 | 18.61 |   1 |   1 |    4 |    1 |
| Hornet 4 Drive      | 21.4 |   6 | 258.0 | 110 | 3.08 | 3.215 | 19.44 |   1 |   0 |    3 |    1 |
| Hornet Sportabout   | 18.7 |   8 | 360.0 | 175 | 3.15 | 3.440 | 17.02 |   0 |   0 |    3 |    2 |
| Valiant             | 18.1 |   6 | 225.0 | 105 | 2.76 | 3.460 | 20.22 |   1 |   0 |    3 |    1 |
| Duster 360          | 14.3 |   8 | 360.0 | 245 | 3.21 | 3.570 | 15.84 |   0 |   0 |    3 |    4 |
| Merc 240D           | 24.4 |   4 | 146.7 |  62 | 3.69 | 3.190 | 20.00 |   1 |   0 |    4 |    2 |
| Merc 230            | 22.8 |   4 | 140.8 |  95 | 3.92 | 3.150 | 22.90 |   1 |   0 |    4 |    2 |
| Merc 280            | 19.2 |   6 | 167.6 | 123 | 3.92 | 3.440 | 18.30 |   1 |   0 |    4 |    4 |
| Merc 280C           | 17.8 |   6 | 167.6 | 123 | 3.92 | 3.440 | 18.90 |   1 |   0 |    4 |    4 |
| Merc 450SE          | 16.4 |   8 | 275.8 | 180 | 3.07 | 4.070 | 17.40 |   0 |   0 |    3 |    3 |
| Merc 450SL          | 17.3 |   8 | 275.8 | 180 | 3.07 | 3.730 | 17.60 |   0 |   0 |    3 |    3 |
| Merc 450SLC         | 15.2 |   8 | 275.8 | 180 | 3.07 | 3.780 | 18.00 |   0 |   0 |    3 |    3 |
| Cadillac Fleetwood  | 10.4 |   8 | 472.0 | 205 | 2.93 | 5.250 | 17.98 |   0 |   0 |    3 |    4 |
| Lincoln Continental | 10.4 |   8 | 460.0 | 215 | 3.00 | 5.424 | 17.82 |   0 |   0 |    3 |    4 |
| Chrysler Imperial   | 14.7 |   8 | 440.0 | 230 | 3.23 | 5.345 | 17.42 |   0 |   0 |    3 |    4 |
| Fiat 128            | 32.4 |   4 |  78.7 |  66 | 4.08 | 2.200 | 19.47 |   1 |   1 |    4 |    1 |
| Honda Civic         | 30.4 |   4 |  75.7 |  52 | 4.93 | 1.615 | 18.52 |   1 |   1 |    4 |    2 |
| Toyota Corolla      | 33.9 |   4 |  71.1 |  65 | 4.22 | 1.835 | 19.90 |   1 |   1 |    4 |    1 |
| Toyota Corona       | 21.5 |   4 | 120.1 |  97 | 3.70 | 2.465 | 20.01 |   1 |   0 |    3 |    1 |
| Dodge Challenger    | 15.5 |   8 | 318.0 | 150 | 2.76 | 3.520 | 16.87 |   0 |   0 |    3 |    2 |
| AMC Javelin         | 15.2 |   8 | 304.0 | 150 | 3.15 | 3.435 | 17.30 |   0 |   0 |    3 |    2 |
| Camaro Z28          | 13.3 |   8 | 350.0 | 245 | 3.73 | 3.840 | 15.41 |   0 |   0 |    3 |    4 |
| Pontiac Firebird    | 19.2 |   8 | 400.0 | 175 | 3.08 | 3.845 | 17.05 |   0 |   0 |    3 |    2 |
| Fiat X1-9           | 27.3 |   4 |  79.0 |  66 | 4.08 | 1.935 | 18.90 |   1 |   1 |    4 |    1 |
| Porsche 914-2       | 26.0 |   4 | 120.3 |  91 | 4.43 | 2.140 | 16.70 |   0 |   1 |    5 |    2 |
| Lotus Europa        | 30.4 |   4 |  95.1 | 113 | 3.77 | 1.513 | 16.90 |   1 |   1 |    5 |    2 |
| Ford Pantera L      | 15.8 |   8 | 351.0 | 264 | 4.22 | 3.170 | 14.50 |   0 |   1 |    5 |    4 |
| Ferrari Dino        | 19.7 |   6 | 145.0 | 175 | 3.62 | 2.770 | 15.50 |   0 |   1 |    5 |    6 |
| Maserati Bora       | 15.0 |   8 | 301.0 | 335 | 3.54 | 3.570 | 14.60 |   0 |   1 |    5 |    8 |
| Volvo 142E          | 21.4 |   4 | 121.0 | 109 | 4.11 | 2.780 | 18.60 |   1 |   1 |    4 |    2 |

Recreating booktabs style table {.table .lightable-classic
style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;"}

``` r

mtcars |>
  kableExtra::kbl() |>
  kableExtra::kable_material(c("striped", "hover"))
```

|                     |  mpg | cyl |  disp |  hp | drat |    wt |  qsec |  vs |  am | gear | carb |
|:--------------------|-----:|----:|------:|----:|-----:|------:|------:|----:|----:|-----:|-----:|
| Mazda RX4           | 21.0 |   6 | 160.0 | 110 | 3.90 | 2.620 | 16.46 |   0 |   1 |    4 |    4 |
| Mazda RX4 Wag       | 21.0 |   6 | 160.0 | 110 | 3.90 | 2.875 | 17.02 |   0 |   1 |    4 |    4 |
| Datsun 710          | 22.8 |   4 | 108.0 |  93 | 3.85 | 2.320 | 18.61 |   1 |   1 |    4 |    1 |
| Hornet 4 Drive      | 21.4 |   6 | 258.0 | 110 | 3.08 | 3.215 | 19.44 |   1 |   0 |    3 |    1 |
| Hornet Sportabout   | 18.7 |   8 | 360.0 | 175 | 3.15 | 3.440 | 17.02 |   0 |   0 |    3 |    2 |
| Valiant             | 18.1 |   6 | 225.0 | 105 | 2.76 | 3.460 | 20.22 |   1 |   0 |    3 |    1 |
| Duster 360          | 14.3 |   8 | 360.0 | 245 | 3.21 | 3.570 | 15.84 |   0 |   0 |    3 |    4 |
| Merc 240D           | 24.4 |   4 | 146.7 |  62 | 3.69 | 3.190 | 20.00 |   1 |   0 |    4 |    2 |
| Merc 230            | 22.8 |   4 | 140.8 |  95 | 3.92 | 3.150 | 22.90 |   1 |   0 |    4 |    2 |
| Merc 280            | 19.2 |   6 | 167.6 | 123 | 3.92 | 3.440 | 18.30 |   1 |   0 |    4 |    4 |
| Merc 280C           | 17.8 |   6 | 167.6 | 123 | 3.92 | 3.440 | 18.90 |   1 |   0 |    4 |    4 |
| Merc 450SE          | 16.4 |   8 | 275.8 | 180 | 3.07 | 4.070 | 17.40 |   0 |   0 |    3 |    3 |
| Merc 450SL          | 17.3 |   8 | 275.8 | 180 | 3.07 | 3.730 | 17.60 |   0 |   0 |    3 |    3 |
| Merc 450SLC         | 15.2 |   8 | 275.8 | 180 | 3.07 | 3.780 | 18.00 |   0 |   0 |    3 |    3 |
| Cadillac Fleetwood  | 10.4 |   8 | 472.0 | 205 | 2.93 | 5.250 | 17.98 |   0 |   0 |    3 |    4 |
| Lincoln Continental | 10.4 |   8 | 460.0 | 215 | 3.00 | 5.424 | 17.82 |   0 |   0 |    3 |    4 |
| Chrysler Imperial   | 14.7 |   8 | 440.0 | 230 | 3.23 | 5.345 | 17.42 |   0 |   0 |    3 |    4 |
| Fiat 128            | 32.4 |   4 |  78.7 |  66 | 4.08 | 2.200 | 19.47 |   1 |   1 |    4 |    1 |
| Honda Civic         | 30.4 |   4 |  75.7 |  52 | 4.93 | 1.615 | 18.52 |   1 |   1 |    4 |    2 |
| Toyota Corolla      | 33.9 |   4 |  71.1 |  65 | 4.22 | 1.835 | 19.90 |   1 |   1 |    4 |    1 |
| Toyota Corona       | 21.5 |   4 | 120.1 |  97 | 3.70 | 2.465 | 20.01 |   1 |   0 |    3 |    1 |
| Dodge Challenger    | 15.5 |   8 | 318.0 | 150 | 2.76 | 3.520 | 16.87 |   0 |   0 |    3 |    2 |
| AMC Javelin         | 15.2 |   8 | 304.0 | 150 | 3.15 | 3.435 | 17.30 |   0 |   0 |    3 |    2 |
| Camaro Z28          | 13.3 |   8 | 350.0 | 245 | 3.73 | 3.840 | 15.41 |   0 |   0 |    3 |    4 |
| Pontiac Firebird    | 19.2 |   8 | 400.0 | 175 | 3.08 | 3.845 | 17.05 |   0 |   0 |    3 |    2 |
| Fiat X1-9           | 27.3 |   4 |  79.0 |  66 | 4.08 | 1.935 | 18.90 |   1 |   1 |    4 |    1 |
| Porsche 914-2       | 26.0 |   4 | 120.3 |  91 | 4.43 | 2.140 | 16.70 |   0 |   1 |    5 |    2 |
| Lotus Europa        | 30.4 |   4 |  95.1 | 113 | 3.77 | 1.513 | 16.90 |   1 |   1 |    5 |    2 |
| Ford Pantera L      | 15.8 |   8 | 351.0 | 264 | 4.22 | 3.170 | 14.50 |   0 |   1 |    5 |    4 |
| Ferrari Dino        | 19.7 |   6 | 145.0 | 175 | 3.62 | 2.770 | 15.50 |   0 |   1 |    5 |    6 |
| Maserati Bora       | 15.0 |   8 | 301.0 | 335 | 3.54 | 3.570 | 14.60 |   0 |   1 |    5 |    8 |
| Volvo 142E          | 21.4 |   4 | 121.0 | 109 | 4.11 | 2.780 | 18.60 |   1 |   1 |    4 |    2 |

``` r

mtcars |>
  kableExtra::kbl() |>
  kableExtra::kable_paper(bootstrap_options = "striped", full_width = F)
```

|                     |  mpg | cyl |  disp |  hp | drat |    wt |  qsec |  vs |  am | gear | carb |
|:--------------------|-----:|----:|------:|----:|-----:|------:|------:|----:|----:|-----:|-----:|
| Mazda RX4           | 21.0 |   6 | 160.0 | 110 | 3.90 | 2.620 | 16.46 |   0 |   1 |    4 |    4 |
| Mazda RX4 Wag       | 21.0 |   6 | 160.0 | 110 | 3.90 | 2.875 | 17.02 |   0 |   1 |    4 |    4 |
| Datsun 710          | 22.8 |   4 | 108.0 |  93 | 3.85 | 2.320 | 18.61 |   1 |   1 |    4 |    1 |
| Hornet 4 Drive      | 21.4 |   6 | 258.0 | 110 | 3.08 | 3.215 | 19.44 |   1 |   0 |    3 |    1 |
| Hornet Sportabout   | 18.7 |   8 | 360.0 | 175 | 3.15 | 3.440 | 17.02 |   0 |   0 |    3 |    2 |
| Valiant             | 18.1 |   6 | 225.0 | 105 | 2.76 | 3.460 | 20.22 |   1 |   0 |    3 |    1 |
| Duster 360          | 14.3 |   8 | 360.0 | 245 | 3.21 | 3.570 | 15.84 |   0 |   0 |    3 |    4 |
| Merc 240D           | 24.4 |   4 | 146.7 |  62 | 3.69 | 3.190 | 20.00 |   1 |   0 |    4 |    2 |
| Merc 230            | 22.8 |   4 | 140.8 |  95 | 3.92 | 3.150 | 22.90 |   1 |   0 |    4 |    2 |
| Merc 280            | 19.2 |   6 | 167.6 | 123 | 3.92 | 3.440 | 18.30 |   1 |   0 |    4 |    4 |
| Merc 280C           | 17.8 |   6 | 167.6 | 123 | 3.92 | 3.440 | 18.90 |   1 |   0 |    4 |    4 |
| Merc 450SE          | 16.4 |   8 | 275.8 | 180 | 3.07 | 4.070 | 17.40 |   0 |   0 |    3 |    3 |
| Merc 450SL          | 17.3 |   8 | 275.8 | 180 | 3.07 | 3.730 | 17.60 |   0 |   0 |    3 |    3 |
| Merc 450SLC         | 15.2 |   8 | 275.8 | 180 | 3.07 | 3.780 | 18.00 |   0 |   0 |    3 |    3 |
| Cadillac Fleetwood  | 10.4 |   8 | 472.0 | 205 | 2.93 | 5.250 | 17.98 |   0 |   0 |    3 |    4 |
| Lincoln Continental | 10.4 |   8 | 460.0 | 215 | 3.00 | 5.424 | 17.82 |   0 |   0 |    3 |    4 |
| Chrysler Imperial   | 14.7 |   8 | 440.0 | 230 | 3.23 | 5.345 | 17.42 |   0 |   0 |    3 |    4 |
| Fiat 128            | 32.4 |   4 |  78.7 |  66 | 4.08 | 2.200 | 19.47 |   1 |   1 |    4 |    1 |
| Honda Civic         | 30.4 |   4 |  75.7 |  52 | 4.93 | 1.615 | 18.52 |   1 |   1 |    4 |    2 |
| Toyota Corolla      | 33.9 |   4 |  71.1 |  65 | 4.22 | 1.835 | 19.90 |   1 |   1 |    4 |    1 |
| Toyota Corona       | 21.5 |   4 | 120.1 |  97 | 3.70 | 2.465 | 20.01 |   1 |   0 |    3 |    1 |
| Dodge Challenger    | 15.5 |   8 | 318.0 | 150 | 2.76 | 3.520 | 16.87 |   0 |   0 |    3 |    2 |
| AMC Javelin         | 15.2 |   8 | 304.0 | 150 | 3.15 | 3.435 | 17.30 |   0 |   0 |    3 |    2 |
| Camaro Z28          | 13.3 |   8 | 350.0 | 245 | 3.73 | 3.840 | 15.41 |   0 |   0 |    3 |    4 |
| Pontiac Firebird    | 19.2 |   8 | 400.0 | 175 | 3.08 | 3.845 | 17.05 |   0 |   0 |    3 |    2 |
| Fiat X1-9           | 27.3 |   4 |  79.0 |  66 | 4.08 | 1.935 | 18.90 |   1 |   1 |    4 |    1 |
| Porsche 914-2       | 26.0 |   4 | 120.3 |  91 | 4.43 | 2.140 | 16.70 |   0 |   1 |    5 |    2 |
| Lotus Europa        | 30.4 |   4 |  95.1 | 113 | 3.77 | 1.513 | 16.90 |   1 |   1 |    5 |    2 |
| Ford Pantera L      | 15.8 |   8 | 351.0 | 264 | 4.22 | 3.170 | 14.50 |   0 |   1 |    5 |    4 |
| Ferrari Dino        | 19.7 |   6 | 145.0 | 175 | 3.62 | 2.770 | 15.50 |   0 |   1 |    5 |    6 |
| Maserati Bora       | 15.0 |   8 | 301.0 | 335 | 3.54 | 3.570 | 14.60 |   0 |   1 |    5 |    8 |
| Volvo 142E          | 21.4 |   4 | 121.0 | 109 | 4.11 | 2.780 | 18.60 |   1 |   1 |    4 |    2 |

Please visit the following website for more examples on the kable
package:
<https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html>

## Table1

Table1 uses quick and easy syntax but is not very friendly with
testing/pvals. Note the label function is helpful here (changed the
names / levels of wt to weight and am to automatic)

``` r

Hmisc::label(mtcars$wt) <- "weight"

mtcars$am <- 
  factor(mtcars$am, 
         levels=c(0,1),
         labels=c("Automatic", # Reference
                  "Manual"))

table1::table1(~ mpg + cyl + wt | am * vs, data=mtcars)
#> Warning in table1.formula(~mpg + cyl + wt | am * vs, data = mtcars): Terms to
#> the right of '|' in formula 'x' define table columns and are expected to be
#> factors with meaningful labels.
```

[TABLE]

## tableone

Here is the simplest approach to creating a summary table with the
tableone package.

``` r

tableone::CreateTableOne(data = mtcars)
#>                   
#>                    Overall        
#>   n                    32         
#>   mpg (mean (SD))   20.09 (6.03)  
#>   cyl (mean (SD))    6.19 (1.79)  
#>   disp (mean (SD)) 230.72 (123.94)
#>   hp (mean (SD))   146.69 (68.56) 
#>   drat (mean (SD))   3.60 (0.53)  
#>   wt (mean (SD))     3.22 (0.98)  
#>   qsec (mean (SD))  17.85 (1.79)  
#>   vs (mean (SD))     0.44 (0.50)  
#>   am = Manual (%)      13 (40.6)  
#>   gear (mean (SD))   3.69 (0.74)  
#>   carb (mean (SD))   2.81 (1.62)
```

You can specify which variables in included and which are factor
variables easily in the table call.

``` r

tableone::CreateTableOne(data = mtcars,
               vars = c("mpg", "cyl", "disp", "hp"),
               factorVars = c("cyl"))
#>                   
#>                    Overall        
#>   n                    32         
#>   mpg (mean (SD))   20.09 (6.03)  
#>   cyl (%)                         
#>      4                 11 (34.4)  
#>      6                  7 (21.9)  
#>      8                 14 (43.8)  
#>   disp (mean (SD)) 230.72 (123.94)
#>   hp (mean (SD))   146.69 (68.56)
```
