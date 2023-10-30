# CAR SALES EDA ----
# *** -----

library(tidyverse)
library(janitor)
library(tidymodels)
library(gt)
library(vip)
library(tidyquant)
library(plotly)

# Load and preprocess data
car_sales <- read_csv("./00_data/car_sales_data.csv") %>%
    clean_names()


# Feature engineering
car_sales <- car_sales %>%
    mutate(car_age = year(date) - car_year)

car_sales %>% glimpse()

# # NEW: ADD EDA ----
# library(correlationfunnel)
# car_sales %>%
#     
#     select(-c(date, customer_name, salesperson, commission_earned, commission_rate)) %>%
#     # one-hot encoding
#     binarize() %>%
#     glimpse() %>%
#     
#     # correlate against desired target
#     correlate(`sale_price__-Inf_20019`) %>%
#     plot_correlation_funnel()
# 
# 

# DATA SPLIT ----

car_split <- car_sales %>%
    
    select(c(car_make, car_model, car_age, sale_price)) %>%
    
    # Splitting Training set 80% and Test set is 20%
    initial_split(
        prop = 0.80,
        strata = sale_price
    )

car_split

# * Training data ----
car_train <- training(car_split)

# * Test data ----
car_test <- testing(car_split)


# PREPROCESSING ----

# * Define the recipe ----

data_prep <- car_train %>% 
    recipe(
        sale_price ~ .,
        data = training(car_split)
        ) %>%
    update_role(car_make, car_model) %>%
    
    # Encode categorical features
    step_dummy(
        all_nominal(),
        one_hot = TRUE
    ) %>%
    
    # Normalize all features
    step_normalize(all_predictors())

car_baked <- bake(prep(data_prep), car_train)
# car_baked <- bake(car_recipe %>%
    
    # Execute the steps in the training set
    # prep() %>%
    
    # Return variables from the processed training set
    # juice() %>%
    
    # Get the glimpse of your data
    # glimpse(),
    # car_train)

# MODELING ----

# * Model 1: XGBoost model specification ----

xgboost_model <- boost_tree(
    trees = 100,
    tree_depth = 6,
    loss_reduction = 0.01, # loss function reduction required to split further
    learn_rate = 0.1 # Rate of algo adaptation to iterations
) %>%
    
    # Regression model
    set_mode("regression") %>%
    
    # Algorithm used
    set_engine("xgboost")

# * Combine recipe and model in a workflow

xgb_car_price_wf <- workflow() %>%
    
    # Add preprocessing recipe
    add_recipe(car_recipe) %>%
    
    # Add model spec
    add_model(xgboost_model) %>%
    
    # Fit the model
    fit(data = car_train)

# * Make predictions

predictions <- xgb_car_price_wf %>%
    predict(car_test)

# EVALUATE ----

# * Evaluate the model ----

# * Bind the predicted values with the original test set
combined_data <- bind_cols(car_test, predictions)

# * Compute metrics ----

metrics <- combined_data %>%
    metrics(truth = sale_price, estimate = .pred)
colnames(metrics) <- c("Metric", "Estimator", "Estimate")

# * Display Metrics ----

# MAE
metrics[,] %>%
    gt() %>%
    gtExtras::gt_hulk_col_numeric(Estimate) %>%
    fmt_number(Estimate)

# FEATURE IMPORTANCE ----

xgboost_trained <- xgb_car_price_wf %>%
    extract_fit_parsnip() %>%
    vip(5)

# Save the model
model_file_path <- "./02_car_sales_forecasting/xgboost_model.rds"
saveRDS(xgboost_trained, file = model_file_path)