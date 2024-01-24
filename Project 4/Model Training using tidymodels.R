# Create a data split object
home_split <- initial_split(home_sales, 
                            prop = 0.7, 
                            strata = selling_price)

# Create the training data
home_training <- home_split %>%
  training()

# Create the test data
home_test <- home_split %>% 
  testing()

# Check number of rows in each dataset
nrow(home_training)
nrow(home_test)

nrow(home_training)
[1] 1042
nrow(home_test)
[1] 450

# Distribution of selling_price in training data
home_training %>% 
  summarize(min_sell_price = min(selling_price),
            max_sell_price = max(selling_price),
            mean_sell_price = mean(selling_price),
            sd_sell_price = sd(selling_price))
# A tibble: 1 × 4
min_sell_price max_sell_price mean_sell_price sd_sell_price
<dbl>          <dbl>           <dbl>         <dbl>
  1         350000         650000         479156.        81201.

# Distribution of selling_price in test data
home_test %>% 
  summarize(min_sell_price = min(selling_price),
            max_sell_price = max(selling_price),
            mean_sell_price = mean(selling_price),
            sd_sell_price = sd(selling_price))
# A tibble: 1 × 4
min_sell_price max_sell_price mean_sell_price sd_sell_price
<dbl>          <dbl>           <dbl>         <dbl>
  1         350000         650000         478920.        80552.

# Initialize a linear regression object, linear_model
linear_model <- linear_reg() %>% 
  # Set the model engine
  set_engine('lm') %>% 
  # Set the model mode
  set_mode('regression')


# Initialize a linear regression object, linear_model
linear_model <- linear_reg() %>% 
  # Set the model engine
  set_engine('lm') %>% 
  # Set the model mode
  set_mode('regression')
# Fit the model using the training data
lm_fit <- linear_model %>% 
  fit(selling_price ~ home_age + sqft_living,
      data = home_training)
# Print lm_fit to view model information
lm_fit
parsnip model object

Fit time:  1ms 

Call:
  stats::lm(formula = selling_price ~ home_age + sqft_living, data = data)

Coefficients:
  (Intercept)     home_age  sqft_living  
291102.4      -1577.4        104.1  

>
  tidy(lm_fit)
# A tibble: 3 × 5
term        estimate std.error statistic   p.value
<chr>          <dbl>     <dbl>     <dbl>     <dbl>
  1 (Intercept)  291102.   7412.       39.3  1.45e-207
2 home_age      -1577.    171.       -9.25 1.27e- 19
3 sqft_living     104.      2.69     38.7  2.04e-203

# Predict selling_price
home_predictions <- lm_fit %>% predict(
  new_data = home_test)
# View predicted selling prices
home_predictions
# A tibble: 450 × 1
.pred
<dbl>
  1 539701.
2 633473.
3 429715.
4 409971.
5 489075.
6 532008.
7 402084.
8 484440.
9 449362.
10 535602.
# … with 440 more rows


# Combine test data with predictions
home_test_results <- home_test %>% 
  select(selling_price, home_age, sqft_living) %>% 
  bind_cols(home_predictions)
# View results
home_test_results
# A tibble: 450 × 4
selling_price home_age sqft_living   .pred
<dbl>    <dbl>       <dbl>   <dbl>
  1        487000       10        2540 539701.
2        635000        4        3350 633473.
3        495000       21        1650 429715.
4        355000       19        1430 409971.
5        464950       19        2190 489075.
6        535000        3        2360 532008.
7        356000       24        1430 402084.
8        525000       16        2100 484440.
9        552321       29        1960 449362.
10        485000        6        2440 535602.
# … with 440 more rows


# Calculating the RMSE metric
home_test_results %>% 
  rmse(truth = selling_price, estimate = .pred)
# A tibble: 1 × 3
.metric .estimator .estimate
<chr>   <chr>          <dbl>
  1 rmse    standard      47680.
# Calculating the R squared metric
home_test_results %>% 
  rsq(truth = selling_price, estimate = .pred)
# A tibble: 1 × 3
.metric .estimator .estimate
<chr>   <chr>          <dbl>
  1 rsq     standard       0.651

# Creating an R squared plot of model performance with selling price vs the predictions
ggplot(home_test_results, aes(x = selling_price, y = .pred)) +
  geom_point(alpha = 0.5) + 
  geom_abline(color = 'blue', linetype = 2) +
  coord_obs_pred()+
  labs(x = 'Actual Home Selling Price', y = 'Predicted Selling Price')

#Upon evaluation of the plot and predictions it seems our r squared line fits the model well
# however, it seems that our model overpredicts home selling prices around 400,000
# and underpredicts houses with selling prices of  around 600000. We may need to add more variables 
# to the model to improve predictions.

# Define a linear regression model
linear_model <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')
# Train linear_model with last_fit()
linear_fit <- linear_model %>% 
  last_fit(selling_price ~ ., split = home_split)
# Collect predictions and view results
predictions_df <- linear_fit %>% collect_predictions()
predictions_df 
# A tibble: 375 × 5
id                 .pred  .row selling_price .config             
<chr>              <dbl> <int>         <dbl> <chr>               
  1 train/test split 528823.     1        487000 Preprocessor1_Model1
2 train/test split 437591.     6        495000 Preprocessor1_Model1
3 train/test split 401497.     7        355000 Preprocessor1_Model1
4 train/test split 477671.     8        464950 Preprocessor1_Model1
5 train/test split 480365.    10        535000 Preprocessor1_Model1
6 train/test split 443872.    11        356000 Preprocessor1_Model1
7 train/test split 502003.    13        525000 Preprocessor1_Model1
8 train/test split 390503.    18        381000 Preprocessor1_Model1
9 train/test split 479451.    21        450000 Preprocessor1_Model1
10 train/test split 628777.    22        624000 Preprocessor1_Model1
# … with 365 more rows

# Make an R squared plot using predictions_df
ggplot(predictions_df, aes(x = selling_price, y = .pred)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(color = 'blue', linetype = 2) +
  coord_obs_pred() +
  labs(x = 'Actual Home Selling Price', y = 'Predicted Selling Price')
# This model that includes all of the other variables from the data set improves
# our predictions but we still have some issues that are similar to the last. 