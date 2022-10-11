# Chart 1: Map

# Description

# Load libraries
library("shiny")
library("dplyr")
library("ggplot2")
library("plotly")
library("gridExtra")
library("usmap")

source("map_function.R")

# Load the prepared data
data <- read.csv("./data/MLA_Language_Enrollment_Database.csv",
  stringsAsFactors = FALSE
)

# Filter down the data to including Chinese languages only & omit the years
# that doesn't have geographic information
data <- data %>%
  filter(LANGUAGE == "CHINESE" | LANGUAGE == "MANDARIN" |
    LANGUAGE == "CANTONESE" | LANGUAGE == "TAIWANESE" |
    LANGUAGE == "CHINESE, CLASSICAL" |
    LANGUAGE == "CHINESE, PRE-MODERN") %>%
  filter(STATE_ID != "NA")

# All the years that data were being collected
year_values <- sort(unique(data$SRVY_YEAR))

# Define UI for application that renders the map and table
my_ui1 <- fluidPage(
  # Application title
  titlePanel("Map of Chinese Learners Across the States"),

  p(paste0(
    "In this page, you could see the geographical location of where ",
    "all the Chinese learners are located at as well as compare the ",
    "popularity of Chinese learning in different states over time. ",
    "By selecting the year and region, you could see a closer breakdown ",
    "of the distribution of Chinese learning in that corresponding region ",
    "of that specific year."
  )),

  sidebarPanel(
    p(paste0(
      "In this page, you could see the geographical location of where ",
      "all the Chinese learners are located at as well as compare the ",
      "popularity of Chinese learning in different states over time.
             "
    )),

    year_input <- selectInput(
      inputId = "year",
      label = "Select Year",
      choices = year_values,
      selected = "2016"
    ),

    region_input <- radioButtons(
      inputId = "region",
      label = "Select Region",
      choices = list(
        "All" = 1,
        "West" = 2,
        "Midwest" = 3,
        "Northeast" = 4,
        "South" = 5,
        "Pacific" = 6
      )
    )
  ),
  mainPanel(
    plotlyOutput("map")
  )
)

# Define server that renders a map
my_server1 <- function(input, output, data) {
  # Define a map to render in the UI
  output$map <- renderPlotly({
    plot <- plots(data, input$year, input$region)
    ggplotly(plot, tooltip = "text")
  })
}

# Start running the application
shinyApp(ui = my_ui1, server = my_server1)
