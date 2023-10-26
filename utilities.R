
# Aggregate Geo-spatial data ----

aggregate_geospatial <- function(data){
    data %>%
        group_by(state) %>%
        summarize(total_sales = sum(total_price)) %>%
        ungroup() %>%
        mutate(sales_text = scales::dollar(total_sales))
}

# Aggregate feature importance data ----

aggregate_feat_imp <- function(model, n_features){

    feat_imp <- xgb.importance(model = model$fit) %>%

        select(c(1,4)) %>%

        arrange(desc(Frequency)) %>%

        slice_head(n = n_features)

    feat_imp
}

# Plot Geo-Spatial data ----

plot_geospatial <- function(data){

    # Create a color scale based on price
    color_scale <- colorNumeric(palette = "RdYlBu", domain = data$price_aprox_usd)

    # Create a Leaflet map
    m <- data %>%
        leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
            lat = ~lat,
            lng = ~lon,
            radius = ~sqrt(price_aprox_usd) / 200,  # Adjust the scaling factor as needed
            color = ~color_scale(price_aprox_usd),
            fillOpacity = 0.7,
            stroke = TRUE,
            label = ~glue::glue("Price: ${price_aprox_usd}")
        ) %>%
        addLegend(
            position = "topright",
            pal = color_scale,
            values = ~price_aprox_usd,
            title = "Price",
            opacity = 1
        )

    # Convert the Leaflet map to a Plot_ly map
    map <- as_widget(m)

    # Display the interactive map
    map

}

# Plot feature importance data ---

plot_feature_importance <- function(data){

    data %>% plot_ly(
            x = ~ Frequency,
            y = ~ reorder(Feature, Frequency),
            type = "bar",
            orientation = "h",
            height = "30px") %>%

        layout(
            yaxis = list(title = " "),
            xaxis = list(title = "Importance"),
            title = "Feature Importances",
            hoverlabel = list(font = list(size = 20)),
            showlegend = F
        )
}

# Predict house price ----

predict_price <- function(model, new_data){

        # perform predictions
        prediction <- model %>% predict(new_data)
}
