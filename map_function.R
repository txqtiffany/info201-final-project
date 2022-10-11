# Define all the maps
plots <- function(data, year, region) {
  data_year <- data %>%
    filter(SRVY_YEAR == year) %>%
    group_by(STATE) %>%
    summarize("Total Chinese Learners" = sum(ALL_LEVEL_TOTAL, na.rm = T)) %>%
    as.data.frame() %>%
    rename(state = STATE) # in order to accommodate the plotting

  full_map <- function(year, region) {
    data_map <- map_with_data(data_year, values = "Total Chinese Learners", )
    map <- ggplot(data = data_map, aes(
      text =
        paste(
          "Total Learners:", data_map[, "Total Chinese Learners"],
          "<br>State:", full
        )
    )) +
      geom_polygon(
        mapping = aes(
          x, y, group = group,
          fill = data_map[, "Total Chinese Learners"]
        ),
        color = "red", size = 0.4
      ) +
      coord_equal() +
      scale_fill_continuous(
        low = "white", high = "red", name =
          paste0("Chinese Learners (", year, ")"),
        label = scales::comma, na.value = "grey"
      ) + theme(
        legend.position = "right",
        axis.text = element_blank()
      ) + labs(
        title = region
      ) +
      xlab("Latitude") +
      ylab("Longtitude")
    map
  }


  regional_map <- function(year, region, include) {
    data_map <- map_with_data(data_year,
      values = "Total Chinese Learners",
      include = include
    )
    map <- ggplot(data = data_map) +
      geom_polygon(
        mapping = aes(
          x = data_map$long, data_map$lat, group = data_map$group,
          fill = data_map[, "Total Chinese Learners"],
          text = paste(
            "Total Learners:", data_map[, "Total Chinese Learners"],
            "<br>State:", data_map$full
          )
        ),
        color = "red", size = 0.4
      ) +
      coord_equal() +
      scale_fill_continuous(
        low = "white", high = "red", name =
          paste0("Chinese Learners (", year, ")"),
        label = scales::comma, na.value = "grey"
      ) + theme(
        legend.position = "right",
        axis.text = element_blank()
      ) + labs(
        title = region
      ) +
      xlab("Latitude") +
      ylab("Longtitude")
    map
  }

  # Map for the full United States with # of Chinese learners in that year
  all <- full_map(2016, "United States")

  # Map for the Western States without California
  west_wo_ca <-
    regional_map(year,
      "Western States without California for better visualization",
      include = c(
        "WA", "ID", "OR", "WY", "MT", "NV", "UT",
        "AZ", "CO", "NM"
      )
    )

  # Map for the Midwestern States
  midwest <- regional_map(year, "Midwestern States", include = c(
    "ND", "SD", "NE", "KS", "MN", "IA", "MO",
    "WI", "IL", "IN", "OH", "MI"
  ))

  # Map for the Northeastern States
  northeast <- regional_map(year, "Northeastern States",
    include = c(
      "ME", "VT", "NH", "MA", "RI", "CT", "NY",
      "PA", "NJ"
    )
  )

  # Map for the Southern States
  south <- regional_map(year, "Southern States",
    include = c(
      "TX", "OK", "AR", "KY", "WV", "VA", "DC", "NC",
      "MD", "DE", "VA", "SC", "GA", "FL", "AL", "MS",
      "LA", "MS", "TN"
    )
  )

  # Map for the Pacific States
  pacific <- regional_map(year, "Pacific States",
    include = c("AK", "HI")
  )

  plots <- list(all, west_wo_ca, midwest, northeast, south, pacific)
  plots[[as.numeric(region)]]
}
