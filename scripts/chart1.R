# Read in the csv file and create the dataframe "mla"
dataframe <- read.csv("./data/MLA_Language_Enrollment_Database.csv",
  stringsAsFactors = FALSE
)

# load the packages
library(dplyr)
library(usmap)
library(ggplot2)
library(ggrepel)

# load map information
usa <- map_data("usa")
states <- map_data("state")

chart1 <- function(dataframe) {
  # filter out the dataframe to Chinese learners in 2016
  mla_2016_ch <- dataframe %>%
    filter(SRVY_YEAR == 2016) %>%
    filter(LANGUAGE == "CHINESE" | LANGUAGE == "MANDARIN" |
      LANGUAGE == "CANTONESE" | LANGUAGE == "TAIWANESE" |
      LANGUAGE == "CHINESE, CLASSICAL" |
      LANGUAGE == "CHINESE, PRE-MODERN") %>% # including all
    # Chinese dialets
    group_by(STATE) %>%
    summarize(sum(ALL_LEVEL_TOTAL, na.rm = T)) %>%
    as.data.frame() %>%
    mutate(state = STATE) # in order to accommdate the plotting

  # Map for the full United States with # of Chinese learners in 2016
  chart1_all <- plot_usmap(
    data = mla_2016_ch, values =
      "sum(ALL_LEVEL_TOTAL, na.rm = T)", lines = "red"
  ) +
    scale_fill_continuous(
      low = "white", high = "red", name = "Chinese Learners (2016)",
      label = scales::comma, na.value = "grey"
    ) + theme(legend.position = "right") + labs(
      title = "United States",
      subtitle = "Number of Chinese Learners across higher institution
       in each states in 2016"
    )

  # Map for the Western States without California
  chart1_west_wo_ca <- plot_usmap(
    include = c(
      "WA", "ID", "OR", "WY", "MT", "NV", "UT",
      "AZ", "CO", "NM"
    ), data = mla_2016_ch,
    values = "sum(ALL_LEVEL_TOTAL, na.rm = T)", lines = "red",
    labels = TRUE, label_color = "black"
  ) +
    scale_fill_continuous(
      low = "white", high = "red", name = "Chinese Learners (2016)",
      label = scales::comma, na.value = "grey"
    ) + theme(legend.position = "right") +
    labs(title = "Western States", subtitle = "without California for better
         visual layout")

  # Map for the Midwestern States
  chart1_midwest <- plot_usmap(
    include = c(
      "ND", "SD", "NE", "KS", "MN", "IA", "MO",
      "WI", "IL", "IN", "OH", "MI"
    ), data = mla_2016_ch,
    values = "sum(ALL_LEVEL_TOTAL, na.rm = T)", lines = "red",
    labels = TRUE, label_color = "black"
  ) +
    scale_fill_continuous(
      low = "white", high = "red", name = "Chinese Learners (2016)",
      label = scales::comma, na.value = "grey"
    ) + theme(legend.position = "right") + labs(title = "Midwestern States")

  # Map for the Northeastern States
  chart1_northeast <- plot_usmap(
    include = c(
      "ME", "VT", "NH", "MA", "RI", "CT", "NY",
      "PA", "NJ"
    ), data = mla_2016_ch,
    values = "sum(ALL_LEVEL_TOTAL, na.rm = T)", lines = "red",
    labels = TRUE, label_color = "black"
  ) +
    scale_fill_continuous(
      low = "white", high = "red", name = "Chinese Learners (2016)",
      label = scales::comma, na.value = "grey"
    ) + theme(legend.position = "right") + labs(title = "Northeastern States")

  # Map for the Southern States
  chart1_south <- plot_usmap(
    include = c(
      "TX", "OK", "AR", "KY", "WV", "VA", "DC", "NC",
      "MD", "DE", "VA", "SC", "GA", "FL", "AL", "MS",
      "LA", "MS", "TN"
    ), data = mla_2016_ch,
    values = "sum(ALL_LEVEL_TOTAL, na.rm = T)", lines = "red",
    labels = TRUE, label_color = "black"
  ) +
    scale_fill_continuous(
      low = "white", high = "red", name = "Chinese Learners (2016)",
      label = scales::comma, na.value = "grey"
    ) + theme(legend.position = "right") + labs(title = "Southern States")

  # Map for the Pacific States
  chart1_pacific <- plot_usmap(
    include = c("AK", "HI"), data = mla_2016_ch,
    values = "sum(ALL_LEVEL_TOTAL, na.rm = T)", lines = "red",
    labels = TRUE, label_color = "black"
  ) +
    scale_fill_continuous(
      low = "white", high = "red", name = "Chinese Learners (2016)",
      label = scales::comma, na.value = "grey"
    ) + theme(legend.position = "right") + labs(title = "Pacific States")

  list(
    chart1_all, chart1_west_wo_ca, chart1_midwest, chart1_northeast,
    chart1_northeast, chart1_south, chart1_pacific
  )
}
