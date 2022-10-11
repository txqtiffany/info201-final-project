source("chart2_final.R")
source("chart2_final_function.R")
source("chart1_final.R")
source("chart3_final.R")
source("Intro_final.R")
source("Conclusion_final.R")

library(shinythemes)
library(shiny)
data <- read.csv("./data/MLA_Language_Enrollment_Database.csv",
  stringsAsFactors = FALSE
)
page1_panel <- tabPanel("Map", value = "one", my_ui1)
page2_panel <- tabPanel("Top Langs", value = "two", my_ui2)
page3_panel <- tabPanel("Trend", value = "three", my_ui3)
intro_panel <- tabPanel("Intro", value = "intro", my_ui_intro)
conclude_panel <- tabPanel("Conclusion", value = "conclude", my_ui_con)

my_ui <- navbarPage(
  theme = shinytheme("united"),
  "Chinese Learning in the U.S.",
  intro_panel,
  page1_panel,
  page2_panel,
  page3_panel,
  conclude_panel
)

my_server <- function(input, output) {
  output$intro <- my_server_intro(input, output)
  output$one <- my_server1(input, output, data)
  output$two <- my_server2(input, output, data)
  output$three <- my_server3(input, output)
  output$conclude <- my_server_con(input, output)
}

shinyApp(ui = my_ui, server = my_server)
