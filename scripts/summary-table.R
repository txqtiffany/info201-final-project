# summary table
library(tidyverse)

summary_info <- function(df) {
  # different types of Chinese
  chinese <- c(
    "CHINESE", "CANTONESE", "CHINESE, CLASSICAL", "TAIWANESE",
    "CHINESE, PRE-MODERN"
  )

  # filter the Chinese learners in 2016
  mla_chi_2016 <- df %>%
    filter(SRVY_YEAR == 2016, LANGUAGE %in% chinese) %>%
    select(SRVY_YEAR, STATE, LANGUAGE, ALL_LEVEL_TOTAL) %>%
    group_by(STATE) %>%
    summarise(TOTAL_CHINESE_LEARNERS = sum(ALL_LEVEL_TOTAL, na.rm = TRUE))

  # filter all language learners in 2016
  mla_total_2016 <- df %>%
    filter(SRVY_YEAR == 2016) %>%
    select(SRVY_YEAR, STATE, LANGUAGE, ALL_LEVEL_TOTAL) %>%
    group_by(STATE) %>%
    summarise(TOTAL_LEARNERS = sum(ALL_LEVEL_TOTAL, na.rm = TRUE))

  # create another data frame with data grouped by state
  summary_table <- left_join(mla_chi_2016, mla_total_2016, by = "STATE")
  summary_table$PCT_CHI_LEARNERS <-
    round(summary_table$TOTAL_CHINESE_LEARNERS /
      (summary_table$TOTAL_LEARNERS +
        summary_table$TOTAL_CHINESE_LEARNERS) *
      100, 1)

  # arrange in descending order of percentage of chinese learners
  summary_table <- summary_table %>%
    arrange(-PCT_CHI_LEARNERS)
  
  summary_table
}
