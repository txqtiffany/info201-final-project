# Chart 2: Bar chart

# Description

# Load libraries
library(shiny)
library(dplyr)
library(leaflet)
library(plotly)

# Load the prepared data
data <- read.csv("./data/MLA_Language_Enrollment_Database.csv",
  stringsAsFactors = FALSE
)

source("chart2_final_function.R")
# Define server that renders a map and a table
# Define UI for application that renders the map and table
my_ui2 <- fluidPage(
  # Application title
  titlePanel("Top 8 Languages in Each Year"),
  p(paste0(
    "This chart shows the top 8 popular languages out of more than",
    " 100 types of languages among the language",
    " learners in U.S. Colleges from 1958 to 2016. We can compare ",
    "where Chinese ranked (if included) with other languages. ",
    "Chinese is not consistently included on the ranking until 1990",
    ". This suggests Chinese has been embraced by more U.S. College",
    " students recently. It holds a moderately strong position."
  )),
  # Sidebar with a selectInput for the variable for analysis
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_year",
        label = "Select Year",
        choices = sort(unique(data$SRVY_YEAR)),
        selected = "2016"
      ),
      p(paste0(
        "Note: The top 8 languages include Chinese in 1960, 1961, ",
        "1971, and 1990 - 2016."
      ))
    ),
    # Display the map and table in the main panel
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

my_server2 <- function(input, output, data) {
  output$plot <- renderPlotly({
    x_label <- list(
      title = "Number of Learners"
    )
    y_label <- list(
      title = "Language"
    )
    plot_ly(interactive_bar_chart(data, input$selected_year),
      type = "bar",
      x = ~Number_of_Learners,
      y = ~LANGUAGE,
      color = "red"
    ) %>%
      layout(
        title = paste0(
          "Top 8 Languages Among Language Learners in U.S.",
          " College in ", input$selected_year
        ),
        xaxis = x_label,
        yaxis = y_label
      )
  })
}

# Start running the application
shinyApp(ui = my_ui2, server = my_server2)
