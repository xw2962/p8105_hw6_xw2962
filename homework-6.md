homework 6
================
Xiaoyu Wu
2023-11-18

``` r
library(dplyr)
library(broom)
library(purrr)
library(tidyr)
library(ggplot2)
library(boot)
library(forcats)
library(modelr)
```

## Problem One

``` r
homicide_data_tidy= read.csv("./data/homicide-data.csv") |> 
# Load the data from a CSV file
  mutate(city_state = paste(city, state, sep = ", ")) |> 
# Create city_state variable
  mutate(is_solved = ifelse(disposition == "Closed by arrest", 1, 0)) |> 
# Create a binary variable indicating whether the homicide is solved
  filter(!city_state %in% c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL"),
# Omit cities Dallas, TX; Phoenix, AZ; and Kansas City. Also omit Tulsa, AL. 
  victim_race %in% c("White", "Black")) |>
# Limit your analysis those for whom victim_race is white or black
  mutate(victim_age = as.numeric(victim_age))
# Be sure that victim_age is numeric
```

``` r
baltimore_data = homicide_data_tidy|>
  filter(city_state == "Baltimore, MD") |> 
# Filter out the city of Baltimore, MD
  mutate(victim_sex = relevel(as.factor(victim_sex), ref = "Female")) |> 
# Ensure victim_sex is a factor with Female as reference
  mutate(victim_race = factor(victim_race)) 
# Ensure victim_race is a factor 

logistic_model = glm(is_solved ~ victim_age + victim_sex + victim_race, 
                      data = baltimore_data, 
                      family = "binomial")
# Fit logistic regression

odds_ratio_results = broom::tidy(logistic_model, conf.int = FALSE) |> 
  filter(term == "victim_sexMale") |> 
# Comparing male victims to female victims keeping all other variables fixed
  mutate(
    OR = exp(estimate),
    LowerCI = exp(estimate - 1.96 * std.error),
    UpperCI = exp(estimate + 1.96 * std.error)
  )
# Calculate Odds Ratio and its Confidence Interval

odds_ratio_results
```

    ## # A tibble: 1 × 8
    ##   term           estimate std.error statistic  p.value    OR LowerCI UpperCI
    ##   <chr>             <dbl>     <dbl>     <dbl>    <dbl> <dbl>   <dbl>   <dbl>
    ## 1 victim_sexMale   -0.854     0.138     -6.18 6.26e-10 0.426   0.325   0.558

``` r
fit_glm_city = function(city_data) {
  fit_logistic = glm(is_solved ~ victim_age + victim_race + victim_sex, data = city_data, family = binomial())
  tidy_result = fit_logistic|>
    broom::tidy()|>
    filter(term == "victim_sexMale")|>
    mutate(
      OR = exp(estimate),
      LowerCI = exp(estimate - 1.96 * std.error),
      UpperCI = exp(estimate + 1.96 * std.error)
    )|>
    select(term, OR, LowerCI, UpperCI)
}
# Create a function to fit logistic regression for a given city's data

result_list = homicide_data_tidy  |> 
  mutate(victim_race = factor(victim_race),
         victim_sex = factor(victim_sex)) |>
  group_by(city_state) |>
  nest() |>
  filter(city_state != "Tulsa, AL") |>
  mutate(glm_result = map(data, fit_glm_city)) |>
  unnest(glm_result)
# Applying the function to each city in the dataset
```

``` r
ggplot(result_list, aes(y = OR, x = reorder(city_state, OR))) +
  geom_point() +  
# Points for OR
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +  
# Error bars for CIs
  labs(x = "City", y = "Adjusted Odds Ratio (OR)",
       title = "Estimated ORs and CIs for Solving Homicides in Each City") +
  theme_minimal() +
  coord_flip()   
```

![](homework-6_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# Flips the axes to make the cities display horizontally
```

#### Discussion:

Here, we can see that Albuquerque, NM has the highest OR, that is
greatest likelihood of homicide cases being solved. And New York, NY has
the least OR, that is least likelihood of homicide cases being solved.
Also, we can see that higher ORs are accompanied with larger confidence
intervals(longer length of the error bar).

## Problem Two

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2022-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## using cached file: /Users/wuxiaoyu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2023-09-27 13:57:30.794807 (8.524)

    ## file min/max dates: 1869-01-01 / 2023-09-30

``` r
# Download data
```

``` r
# Function to perform linear regression and extract the required quantities
bootstrap_function = function(data, indices) {
  # Sample the data
  boot_data = data[indices,]
  
  # Fit the linear regression model
  model = lm(tmax ~ tmin + prcp, data = boot_data)
  
  # Extract R-squared
  r_squared = glance(model)$r.squared
  
  # Extract coefficients and compute log(beta1 * beta2)
  coefs = tidy(model)
  log_product = log(coefs$estimate[2] * coefs$estimate[3])
  
  return(c(r_squared, log_product))
}

# Perform the bootstrap
set.seed(123)  # For reproducibility
bootstrap_results = boot(data = weather_df, statistic = bootstrap_function, R = 5000)

# Extract results
r_squared_dist = bootstrap_results$t[,1]
log_product_dist = bootstrap_results$t[,2]

# Plotting the distributions as density plots
ggplot() +
  geom_density(aes(x = r_squared_dist), alpha = 0.7) +
  labs(title = "Density Plot of R-squared Estimates", x = "R-squared", y = "Density") +
  theme_minimal()
```

![](homework-6_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot() +
  geom_density(aes(x = log_product_dist),  alpha = 0.7) +
  labs(title = "Density Plot of Log(Beta1 * Beta2) Estimates", x = "Log(Beta1 * Beta2)", y = "Density") +
  theme_minimal()
```

![](homework-6_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
# Calculating 95% Confidence Intervals
ci_r_squared = quantile(r_squared_dist, c(0.025, 0.975), na.rm = TRUE)
ci_log_product = quantile(log_product_dist, c(0.025, 0.975), na.rm = TRUE)

list(CI_R_squared = ci_r_squared, CI_Log_Product = ci_log_product)
```

    ## $CI_R_squared
    ##      2.5%     97.5% 
    ## 0.8886348 0.9403299 
    ## 
    ## $CI_Log_Product
    ##      2.5%     97.5% 
    ## -8.957548 -4.551465

#### Discussion of the plots

A left-skewed distribution for the R-squared estimates suggests that
most bootstrap samples yield higher R-squared values, with fewer samples
resulting in lower R-squared values. This could mean that the model
generally fits well, but there are instances (or specific samples) where
the fit significantly worsens. The tail on the lower end could be due to
outliers or certain sample combinations where the predictors do not
explain the variance in the response variable as effectively. The
skewness indicates that while the model is generally reliable, there are
conditions under which its predictive power may decrease. The 2.5% and
97.5% quantile of coefficient product’s log is shown above.

A more pronounced left skew in the distribution of log(beta1\*beta2)
estimates indicates that the product of these coefficients tends to be
higher in most bootstrap samples, but with a greater tendency towards
lower values in some samples than observed in the R-squared
distribution. This could suggest that while the relationship captured by
these coefficients is generally consistent, there are more frequent
instances where this relationship diminishes or is less pronounced. The
greater skewness might reflect higher sensitivity of these coefficients
to specific sample variations, possibly due to interactions between
variables or the influence of outliers. The 2.5% and 97.5% quantile of
coefficient product’s log is shown above.

The distribution of R-squared estimates is much more narrower than the
distribution of log(beta1\*beta2) estimates, indicating that the
estimates for R-squared values are more precise.

## Problem three

``` r
birthweight_df = read.csv("./data/birthweight.csv")|>
  mutate(babysex = ifelse(babysex == 1, 1, 0))|>
  mutate(
    babysex = factor(babysex),
    frace = factor(frace),
    malform = factor(malform),
    mrace = factor(mrace),
    frace = fct_infreq(frace),
    mrace = fct_infreq(mrace))
# Reading and transforming the data
```

``` r
sum(is.na(birthweight_df))
```

    ## [1] 0

``` r
# check for missing values 
```

#### Discussions on Predictor Choosing

First, for avoiding the data leakage, we want to exclude variables like
babysex, bhead (baby’s head circumference), and blength (baby’s length),
which are known only after birth. Then, for addressing potential
multicollinearity issues, we would like to drop ppbmi (pre-pregnancy
BMI) and wtgain (weight gain during pregnancy) as these might be
calculated from other variables like pre-pregnancy weight (ppwt) and
mother’s height (mheight). Finally, we made the decision to exclude
malform (malformation) due to uncertainty about whether it is detected
before or after birth.

Then, we want to first fit a model and make final decisions on which
variables to keep:

``` r
test_lm = lm(bwt ~  smoken + ppwt + frace + parity + momage + mrace + mheight + pnumlbw + pnumsga + fincome +  menarche + gaweeks + delwt, data = birthweight_df)
summary(test_lm)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ smoken + ppwt + frace + parity + momage + 
    ##     mrace + mheight + pnumlbw + pnumsga + fincome + menarche + 
    ##     gaweeks + delwt, data = birthweight_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1725.89  -256.94     9.14   275.23  1499.69 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -504.0971   184.5320  -2.732  0.00633 ** 
    ## smoken       -11.7913     0.8970 -13.146  < 2e-16 ***
    ## ppwt          -5.8661     0.6669  -8.796  < 2e-16 ***
    ## frace2      -114.6325    71.3845  -1.606  0.10838    
    ## frace4       -83.0339    69.1452  -1.201  0.22987    
    ## frace3       -44.3238   107.2509  -0.413  0.67943    
    ## frace8        -6.6925   114.6373  -0.058  0.95345    
    ## parity       100.7420    62.6169   1.609  0.10772    
    ## momage         0.7397     1.8899   0.391  0.69552    
    ## mrace2      -168.4335    71.2715  -2.363  0.01816 *  
    ## mrace4       -26.0415    69.8162  -0.373  0.70917    
    ## mrace3       -24.2838   111.2655  -0.218  0.82724    
    ## mheight       16.5941     2.7760   5.978 2.45e-09 ***
    ## pnumlbw            NA         NA      NA       NA    
    ## pnumsga            NA         NA      NA       NA    
    ## fincome        0.2628     0.2775   0.947  0.34382    
    ## menarche      -4.0686     4.4785  -0.908  0.36367    
    ## gaweeks       52.6034     2.1065  24.972  < 2e-16 ***
    ## delwt          9.7528     0.6003  16.248  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 421.8 on 4325 degrees of freedom
    ## Multiple R-squared:  0.3243, Adjusted R-squared:  0.3218 
    ## F-statistic: 129.7 on 16 and 4325 DF,  p-value: < 2.2e-16

We did see collinearity in our dataset since we see the NA coefficients
in the result. Thus, we will exclude pnumsga and pnumlbw in our
analysis. Finally by looking at the coefficients we got, we found
delwt,gaweeks, smoken, ppwt and mheight are the most significant
variables.

#### Our model will contain delwt,gaweeks, smoken, ppwt and mheight as predictors for bwt.

``` r
lm_model = lm(bwt ~ delwt + gaweeks + smoken + ppwt + mheight, data = birthweight_df)
# Fit the model

prediction_result = birthweight_df|>
  add_predictions(lm_model, var = "fitted_values")|>
  add_residuals(lm_model, var = "residuals")
# Add predictions and residuals to the dataframe

ggplot(prediction_result, aes(x = fitted(lm_model), y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals", title = "Plot of Residuals vs Fitted Values") +
  theme_minimal()
```

![](homework-6_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Plotting residuals against fitted values
```

We can see from the plot that the residuals scatter around 0 evenly.

#### Compare your model to two others:

``` r
cv_df = 
  crossv_mc(birthweight_df, 100)
  
cv_df =
  cv_df |> 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble))

cv_df = 
  cv_df |> 
  mutate(
    proposed_mod  = map(train, \(df) lm(bwt ~ delwt + gaweeks + smoken + ppwt + mheight, data = df)),
    length_gestation_mod  = map(train, \(df) lm(bwt ~  blength + gaweeks, data = df)),
    interact_mod  = map(train, \(df) lm(bwt ~  bhead + blength + babysex + bhead * blength + blength* babysex + bhead* babysex + bhead * blength * babysex, data = df))) |> 
  mutate(
    rmse_proposed = map2_dbl(proposed_mod, test, \(mod, df) rmse(model = mod, data = df)),
    rmse_length_gestation = map2_dbl(length_gestation_mod, test, \(mod, df) rmse(model = mod, data = df)),
    rmse_interact = map2_dbl(interact_mod, test, \(mod, df) rmse(model = mod, data = df)))

cv_df |> 
  select(starts_with("rmse")) |> 
  pivot_longer(
    everything(),
    names_to = "model", 
    values_to = "rmse",
    names_prefix = "rmse_") |> 
  mutate(model = fct_inorder(model)) |> 
  ggplot(aes(x = model, y = rmse)) + geom_violin()
```

![](homework-6_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

#### Discussion:

In the plot, the model with interaction have the lowest RMSE, which
tells it is the model with best predictivity. Our proposed model has
highest rmse and least predictivity.

This may be due to the variables ‘wtgain’ (weight gain during
pregnancy), ‘momage’ (mother’s age), and ‘smoken’ (smoking status) might
not be as strongly or directly correlated with the baby’s birth weight
(‘bwt’) as the variables in the other models. For example, ‘blength’
(baby’s length) and ‘gaweeks’ (gestational weeks) used in the
length_gestation_mod are more directly related to the baby’s development
and hence, birth weight.

The interact_mod, which performed the best, included interaction terms
among ‘bhead’, ‘blength’, and ‘babysex’. This suggests that the
relationship between these predictors and birth weight is not just
linear but also involves complex interactions. Our proposed model did
not account for such interactions, which might be crucial in predicting
birth weight more accurately.
