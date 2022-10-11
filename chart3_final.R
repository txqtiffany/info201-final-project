# Chart 3: Line Chart

# Description

# Load libraries
library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
source("chart3_function.R")
# Load the prepared data
old_data <- read.csv("./data/MLA_Language_Enrollment_Database.csv",
  stringsAsFactors = FALSE
)
# Trim dataset down to needed columns
needed_cols <- c("SRVY_YEAR", "LANGUAGE", "ALL_LEVEL_TOTAL")
data <- data.frame(old_data[, needed_cols])
data <- na.omit(data, needed_cols)
data <- subset(data, data$LANGUAGE != "")

# Consolidate all Chinese dialects in dataset
is_Chinese <- data$LANGUAGE == "MANDARIN" |
  data$LANGUAGE == "CANTONESE" |
  data$LANGUAGE == "TAIWANESE" |
  data$LANGUAGE == "CHINESE, CLASSICAL" |
  data$LANGUAGE == "CHINESE, PRE-MODERN"
data[is_Chinese == T, ]$LANGUAGE <- "CHINESE"

# Adjust Dataset to suitable format
data <- data %>%
  group_by(LANGUAGE, SRVY_YEAR, add = T) %>%
  arrange(SRVY_YEAR, .by_group = T) %>%
  summarize(year_total = sum(ALL_LEVEL_TOTAL))

# Make list of 10 most popluar languages overall
language_list <- data %>%
  group_by(LANGUAGE) %>%
  summarize(total = sum(year_total)) %>%
  arrange(-total) %>%
  head(10) %>%
  pull(LANGUAGE)

# Make language selectable
lang_input <- selectInput(
  inputId = "language",
  label = "Language Selected",
  choice = language_list,
  selected = language_list[7]
)

# Define a variable `scatter_sidebar_content` that is a `sidebarPanel()` for
# - a checkbox widget for searching for a state in your scatter plot
scatter_sidebar_content <- sidebarPanel(
  checkboxGroupInput("langlist", "Languages to show:",
    choices = c(language_list),
    selected = language_list[9]
  )
)

# Create a variable `scatter_panel` store the scatterplot

scatter_panel <- tabPanel(
  "Scatter",
  titlePanel("List of Languages"),
  sidebarLayout(
    scatter_sidebar_content,
    mainPanel(
      plotOutput("plot2")
    )
  )
)

# UI for chart 3 with the title, description and panel
my_ui3 <- fluidPage(
  titlePanel("Popularity of Languages"),
  p("This chart shows the level of popularity of the top 10 languages
    using the years that is recorded in this study and the types of
    languages recorded that is taught in colleges and universities
    using the mla dataframe. With this chart, you can see how each
    language can individually be
    seen in comparison to each other, by selecting certain checkboxes
    with different languages, each checkbox representing one of the
    top 10 languages overall."),
  scatter_panel
)
# Server that renders the plot
my_server3 <- function(input, output) {
  output$plot2 <- renderPlot({
    # return the plot
    selected_languages <- input$langlist

    p <- ggplot() +
      geom_line(data = mla_pop_filtered(old_data, selected_languages), aes(
        x = SRVY_YEAR,
        y = year_total,
        color = LANGUAGE
      ), size = 1) +
      xlab("YEAR") + ylab("# of STUDENTS")
    p
  })
}


# Start running the application
shinyApp(ui = my_ui3, server = my_server3)
