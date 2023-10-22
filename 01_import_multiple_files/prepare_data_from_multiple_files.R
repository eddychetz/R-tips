# LAB 01: IMPORT MULTIPLE FILES

# * Goal: Import multiple files, join, clean, generate features, and save to different formats

# LOAD REQUIRED LIBRARIES ----

# Core
library(tidyverse)

# For handling file system 
library(fs)

# Cleaning names of columns
library(janitor)

# Create a list of file paths
file_paths <- fs::dir_ls("./00_data/houses_price_data/raw_data/")

# Function to wrangle data
wrangle_files <- function(file_paths){

  # Initialize an empty data frame
  appended_data <- data.frame()

  for (file in list(file_paths)){

    # Read each file
    data <- read_csv(file)

    # Append each data to 'appended_data'
    appended_data <- rbind(appended_data, data)
  }

  # 1. Subset the data in the CSV file and return only apartments in Mexico City ("Capital Federal") that cost less than $100,000.
  mask_apt <- appended_data$property_type == "house"
  mask_price <- appended_data$price_aprox_usd < 400000
  mask_distr <- str_detect(appended_data$place_with_parent_names, "Capital Federal")

  # combine the masks
  combined_mask <- mask_apt & mask_price & mask_distr

  # 2. filter rows using combined masks
  appended_data <- appended_data[combined_mask, ]


  appended_data <- appended_data %>%

    # 3. Split the "lat-lon" column into two columns using a comma as the separator
    # Separate the split columns into two separate columns
    mutate(
      # Create separate "lat" and "lon" columns.
      lat = as.numeric(str_split(`lat-lon`, ",", simplify = TRUE)[, 1]),
      lon = as.numeric(str_split(`lat-lon`, ",", simplify = TRUE)[, 2]),

      # 4. Get place name
      neighborhood = str_split(
          appended_data$place_with_parent_names, 
          pattern = "\\|", 
          simplify = TRUE)[, 4]
      ) %>%

    # 5. Recast all numeric features from `chr` type
    mutate_at(
      vars(price, currency, price_aprox_usd, surface_total_in_m2,
      surface_covered_in_m2,price_aprox_local_currency, price_usd_per_m2,
      price_per_m2, floor, rooms, expenses),
      as.numeric
      ) %>%
      
    # Drop `place_with_parent_names` and `lat-lon` columns
    select(-c(place_with_parent_names, `lat-lon`)) %>%

    # Drop columns that are more than 50% null values.
    select(-c(floor, expenses)) %>%

    #  Drop columns containing low- or high-cardinality categorical values.
    select(-c(property_type, operation, currency, properati_url)) %>%

    #  Drop any columns that would constitute leakage for the target "price_aprox_usd".
    select(-c(price, price_aprox_local_currency, price_per_m2, price_usd_per_m2)) %>%

    #  Drop any columns that would create issues of multi-collinearity.
    select(-c(surface_total_in_m2, rooms))

  # 6. Calculate the 10th and 90th percentiles of surface_covered_in_m2
  low <- quantile(appended_data$surface_covered_in_m2, 0.1, na.rm = TRUE)
  high <- quantile(appended_data$surface_covered_in_m2, 0.9, na.rm = TRUE)

  # 7. Remove outliers by trimming the bottom and top 10% of properties in terms of "surface_covered_in_m2"
  appended_data <- appended_data %>%
    filter(
        between(
            surface_covered_in_m2,
            left = low,
            right = high)
        )

  # 8. Return the appended data
  return(appended_data)
}

# STEP 2: Using the wrangle function ----

# Wrangle data using the `wrangle_files` function
data_tbl <- wrangle_files(file_paths) %>%
    clean_names()

# STEP 3: Saving wrangled data ----

# 1. Save wrangled data as CSV file
write_csv(data_tbl, "./00_data/houses_price_data/cleaned_data/cleaned_data_tbl.csv")

# 2. Save wrangled data in R's custom binary format
write_rds(data_tbl, "./00_data/houses_price_data/cleaned_data/cleaned_data_tbl.rds")


# 2. Save wrangled data excel format
write_excel_csv(data_tbl, "./00_data/houses_price_data/cleaned_data/cleaned_data_tbl.xlsx")


