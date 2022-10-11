# Introduction Page

# Description

# Load libraries
library(shiny)

text1 <- paste(
  "Given China's increasingly important role on the",
  "global stage, more and more Americans are considering learning Chinese",
  "as a second language. As a group, we were interested in",
  "examining how this trend has grown over the past few decades,",
  "and what conclusions can be drawn from its growth."
)

text2 <- paste(
  "We made our conclusions based on data gathered by a series",
  "of 25 surveys conducted by the MLA (Modern Language Association)",
  "between 1958 and 2016."
)


text3 <- paste(
  "These surveys collected detailed enrollment information",
  "on foriegn languages from colleges across the US. The data",
  "is extensive, containing 208,020 observations and 24 features.",
  "The collection of the data was primarily funded by the United",
  "States Department of Education, with additional funding from",
  "several other institutions."
)

text4 <- "After reviewing the data, we had three main questions:"

text5 <- "1) Where in the US are students studying Chinese?"

text6 <- paste(
  "2) How popular of a language is Chinese compared to other",
  "foriegn languages currently?"
)

text7 <- paste(
  "3) How does the growth of enrollment in Chinese",
  "compare to other foreign languages?"
)

text8 <- paste(
  "In the following sections, we provide charts to",
  "answer the above questions, as well as summarize",
  "the important points of the data in a table."
)


# UI
intro_panel <- mainPanel(
  includeCSS("style.css"),
  titlePanel("Introduction"),
  p(text1),
  tags$br(),
  p(text2),
  tags$br(),
  p(text3),
  tags$br(),
  p(text4),
  tags$br(),
  p(text5),
  tags$br(),
  p(text6),
  tags$br(),
  p(text7),
  tags$br(),
  p(text8),
  a("Data source link",
    href = "https://apps.mla.org/flsurvey_search",
    target = "_blank"
  ),
  imageOutput("intro_img")
)

my_ui_intro <- fluidPage(intro_panel)


# SERVER

my_server_intro <- function(input, output) {
  # Define a map to render in the UI
  output$intro_img <- renderImage(
    list(
      src = "intro_picture.jpg",
      contentType = "image/jpg",
      alt = "Intro picture"
    ),
    deleteFile = F
  )
}

# Start running the application
shinyApp(ui = my_ui_intro, server = my_server_intro)
