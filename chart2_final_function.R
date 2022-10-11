# function used interactive chart 2

interactive_bar_chart <- function(df, year) {
  utils::globalVariables("LANGUAGE")
  # different types of chinese
  chinese <- c(
    "CHINESE", "CANTONESE", "CHINESE, CLASSICAL", "TAIWANESE",
    "CHINESE, PRE-MODERN", "MANDARIN"
  )

  # filter the data in given year and summarise it
  mla_year <- df %>%
    filter(SRVY_YEAR == year) %>%
    group_by(LANGUAGE) %>%
    summarise(Number_of_Learners = sum(ALL_LEVEL_TOTAL, na.rm = T))

  # merge all types of chinese to one row
  total_chinese <- mla_year %>%
    filter(LANGUAGE %in% chinese)
  mla_year$Number_of_Learners[mla_year$LANGUAGE == "CHINESE"] <-
    sum(total_chinese$Number_of_Learners, na.rm = T)
  mla_year <- mla_year %>%
    filter(
      LANGUAGE != "CANTONESE", LANGUAGE != "CHINESE, CLASSICAL",
      LANGUAGE != "TAIWANESE", LANGUAGE != "CHINESE, PRE-MODERN",
      LANGUAGE != "MANDARIN"
    ) %>%
    arrange(-Number_of_Learners) %>%
    top_n(8, Number_of_Learners)

  # choose the top n popular languages and put them in order
  top_8_languages <- pull(mla_year, LANGUAGE)
  mla_year_top <- mla_year %>%
    filter(LANGUAGE == top_8_languages)
  mla_year_top$LANGUAGE <- factor(mla_year_top$LANGUAGE,
    levels = mla_year_top$LANGUAGE[
      order(mla_year_top$Number_of_Learners)
    ]
  )
  mla_year_top
}
