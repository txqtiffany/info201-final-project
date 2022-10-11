# Conclusion Page

# Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(maps)

# sources
source("./scripts/chart1.R")
source("./scripts/chart2.R")
source("./scripts/chart3.R")
source("./scripts/summary.R")
source("./scripts/summary-table.R")
source("./scripts/conclusion_values.R")

# read in data
data <- read.csv("./data/MLA_Language_Enrollment_Database.csv",
  stringsAsFactors = FALSE
)

# Notes
# Con 1 --> chart 3
# Con 2 --> chart 3
# Con 3 --> chart 2
# Con 4 --> chart 1
# Con 5 --> sum table

# obtain values for calculations
con_values <- con_values(data)
sum_vals <- get_summary_info(data)
summary_table <- summary_info(data) %>%
  head(5)

# Conclusion 1
con1_title <- paste0(
  "1) Chinese skyrocketed in popularity in the ",
  "early to mid 2000's"
)
con1_comment <- paste0(
  "In 2002, there were ", con_values$chinese_learners2002,
  " students taking Chinese. By 2009, this figure had risen to ",
  con_values$chinese_learners2009, ", representing an increase of ",
  con_values$per_inc_02_to_09_ch,
  "%. This rise makes sense, as it occured at the same time China
  truly emerged as the second world superpower. ",
  "Additionally, it was during this time that Americans began to take
  foriegn language learning more seriously,",
  " with enrollment in foriegn language rising an incredible ",
  con_values$per_increase_2002_to_2009_all,
  "% over the 7 year period. These events help put this drastic ",
  "increase in Chinese enrollment in perspective."
)

# Conclusion 2
con2_title <- paste0(
  "2) There has been a recent dip in Chinese ",
  "language learners"
)
con2_comment <- paste0(
  "In 2013, there were ", con_values$chinese_learners2013,
  " students taking Chinese. By 2016, that number had dropped to ",
  con_values$chinese_learners2016, ", representing a ",
  con_values$per_2013_to_2016_ch,
  "% decrease over a span of 3 years. This decrease is in spite of an overall ",
  con_values$per_2013_to_2016_all,
  "% increase in foriegn language enrollment over the same period. ",
  "While there is no obvious reason for this downward trend, ",
  "it is clear that Chinese is no longer as popular as it was in the 2000's."
)

# Conclusion 3

con3_title <- paste0(
  "3) The popularity of Chinese still pales in comparison ",
  "to other languages such as Spanish"
)
con3_comment <- paste0(
  "Out of ", sum_vals$all_learners_total_2016,
  " students that took a foriegn language ",
  "in 2016, only ", sum_vals$chinese_all_percentage_2016,
  "% of them were taking Chinese. In contrast, ",
  round(
    100 * con_values$spanish_learners2016 / sum_vals$all_learners_total_2016,
    digits = 2
  ),
  "% of them took Spanish. This suggests that Chinese has yet to become a ",
  "core foriegn language in US schools."
)

# Conclusion 4
con4_title <- paste0(
  "4) California has the highest overall percentage ",
  "of Chinese learners"
)
con4_comment <- paste0(
  sum_vals$cali_chinese_percent_2016, "% of ",
  sum_vals$chinese_learners_total_2016,
  " students learning Chinese in 2016 were from California. ",
  "This suggests that Chinese culture is likely more prevalent ",
  "in California than other states, and that its Chinese community is larger."
)

# Conclusion 5
con5_title <- paste0(
  "5) Vermont has the highest relative ",
  "percentage of Chinese learners"
)
con5_comment <- paste0(
  "Vermont's ratio of Chinese language learners to total language ",
  "learners is the highest in the nation. This might seem to imply ",
  "Vermont has a relatively large Chinese community for its size, ",
  "but a little more digging reveals that this high percentage is ",
  "actually due to the fact that Middlebury College, which has one ",
  "of the best chinese language imersion programs in the country, ",
  "is located in Vermont. At Middlebury alone, ",
  con_values$middlebury_total, " students took Chinese in 2016. That is ",
  round(
    100 * con_values$middlebury_total / summary_table[
      summary_table$STATE == "VT",
      "TOTAL_CHINESE_LEARNERS"
    ],
    digits = 2
  ),
  "% of the total Chinese learners in Vermont."
)

# About the Team
about_team <- tabPanel(
  "team",
  p("Contact Information:"),
  p("Tiffany Tian: txqtiff6@uw.edu"),
  p("Tsz Wai Tsui: ttsui3@uw.edu"),
  p("Renee Lee: renlee@uw.edu"),
  p("Emily O'Neill: eado@uw.edu")
)

# UI
conclusion_panel <- mainPanel(
  h1(con1_title),
  plotOutput("chart3_chinese"),
  p(con1_comment),
  h1(con2_title),
  plotOutput("chart3_chinese_after2000"),
  p(con2_comment),
  h1(con3_title),
  plotOutput("chart2"),
  p(con3_comment),
  h1(con4_title),
  plotOutput("chart1_all"),
  p(con4_comment),
  h1(con5_title),
  tableOutput("table"),
  p(con5_comment),
  tags$br(),
  tags$br(),
  about_team
)
my_ui_con <- fluidPage(
  conclusion_panel
)

# SERVER
my_server_con <- function(input, output) {
  # read in charts, in order displayed
  # chart 3
  chart3 <- chart3(data)
  chart3_chinese <- chart3[[1]]
  data_after2000 <- data %>%
    filter(SRVY_YEAR >= 2000)
  chart3_chinese_after2000 <- chart3(data_after2000)[[1]]

  # chart 2
  chart2 <- bar_chart(data)

  # chart 1
  chart1_all <- chart1(data)[[1]]

  # set up plots
  # conclusion 1
  output$chart3_chinese <- renderPlot(chart3_chinese)

  # conclusion 2
  output$chart3_chinese_after2000 <- renderPlot(chart3_chinese_after2000)

  # conclusion 3
  output$chart2 <- renderPlot(chart2)

  # conclusion 4
  output$chart1_all <- renderPlot(chart1_all)

  # conclusion 5
  colnames(summary_table) <- c(
    "State", "Total Chinese Learners",
    "Total Learners", "Percent Chinese Learners"
  )
  output$table <- renderTable(summary_table)
}

# Start running the application
shinyApp(ui = my_ui_con, server = my_server_con)
