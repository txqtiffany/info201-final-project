# bar chart
library(dplyr)
library(ggplot2)
library(scales)

bar_chart <- function(df) {
  utils::globalVariables("language")
  # different types of chinese
  chinese <- c(
    "CHINESE", "CANTONESE", "CHINESE, CLASSICAL", "TAIWANESE",
    "CHINESE, PRE-MODERN"
  )

  # rename some columns of the dataframe
  renamed_df <- df %>%
    rename(
      language = LANGUAGE, All_Level_Learners = ALL_LEVEL_TOTAL,
      Year = SRVY_YEAR
    )

  # filter the data in 2016 and summarise it
  mla_2016 <- renamed_df %>%
    filter(Year == "2016") %>%
    group_by(language) %>%
    summarise(Number_of_Learners = sum(All_Level_Learners, na.rm = T))

  # merge all types of chinese to one row
  total_chinese <- mla_2016 %>%
    filter(language %in% chinese)
  mla_2016$Number_of_Learners[mla_2016$language == "CHINESE"] <-
    sum(total_chinese$Number_of_Learners, na.rm = T)
  mla_2016 <- mla_2016 %>%
    filter(
      language != "CANTONESE", language != "CHINESE, CLASSICAL",
      language != "TAIWANESE", language != "CHINESE, PRE-MODERN"
    ) %>%
    arrange(-Number_of_Learners) %>%
    top_n(8, Number_of_Learners)

  # choose the top 8 popular languages and put them in order
  top_8_languages <- pull(mla_2016, language)
  mla_2016_top <- mla_2016 %>%
    filter(language == top_8_languages)
  mla_2016_top$language <- factor(mla_2016_top$language,
    levels = mla_2016_top$language[
      order(mla_2016_top$Number_of_Learners)
    ]
  )

  # plot the bar chart
  bar_chart <- ggplot(mla_2016_top) +
    geom_bar(aes(x = language, y = Number_of_Learners),
      stat = "identity",
      fill = "#FF3333"
    ) +
    scale_x_discrete(labels = wrap_format(10)) +
    ggtitle(paste0(
      "Top 8 popular languages among language learners",
      " in US College in 2016"
    )) +
    xlab("Languages") +
    ylab("Number of Learners") +
    scale_y_continuous(labels = function(x) paste0(x / 1000, "k"))
  bar_chart + coord_flip()
}
