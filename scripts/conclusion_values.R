
library(dplyr)

df <- read.csv("./data/MLA_Language_Enrollment_Database.csv",
  stringsAsFactors = FALSE
)

source("./scripts/summary.R")

# computes additional values needed for conclusion
con_values <- function(df) {
  values <- list()

  values$spanish_learners2016 <- df %>%
    filter(SRVY_YEAR == 2016) %>%
    filter(LANGUAGE == "SPANISH") %>%
    select("ALL_LEVEL_TOTAL") %>%
    sum(na.rm = T)

  values$middlebury_total <- df %>%
    filter(UNIV == "MIDDLEBURY C") %>%
    filter(LANGUAGE == "CHINESE" | LANGUAGE == "MANDARIN" |
      LANGUAGE == "CANTONESE" |
      LANGUAGE == "TAIWANESE" |
      LANGUAGE == "CHINESE, CLASSICAL" |
      LANGUAGE == "CHINESE, PRE-MODERN") %>%
    filter(SRVY_YEAR == 2016) %>%
    summarise(total = sum(ALL_LEVEL_TOTAL))

  values$chinese_learners2013 <- df %>%
    filter(SRVY_YEAR == 2013) %>%
    filter(LANGUAGE == "CHINESE" | LANGUAGE == "MANDARIN" |
      LANGUAGE == "CANTONESE" |
      LANGUAGE == "TAIWANESE" |
      LANGUAGE == "CHINESE, CLASSICAL" |
      LANGUAGE == "CHINESE, PRE-MODERN") %>%
    select("ALL_LEVEL_TOTAL") %>%
    sum(na.rm = T)

  values$chinese_learners2016 <- df %>%
    filter(SRVY_YEAR == 2016) %>%
    filter(LANGUAGE == "CHINESE" | LANGUAGE == "MANDARIN" |
      LANGUAGE == "CANTONESE" |
      LANGUAGE == "TAIWANESE" |
      LANGUAGE == "CHINESE, CLASSICAL" |
      LANGUAGE == "CHINESE, PRE-MODERN") %>%
    select("ALL_LEVEL_TOTAL") %>%
    sum(na.rm = T)


  values$per_2013_to_2016_ch <-
    round(
      (values$chinese_learners2013 - values$chinese_learners2016) /
        values$chinese_learners2013 * 100,
      digits = 2
    )


  values$chinese_learners2002 <- df %>%
    filter(SRVY_YEAR == 2002) %>%
    filter(LANGUAGE == "CHINESE" | LANGUAGE == "MANDARIN" |
      LANGUAGE == "CANTONESE" |
      LANGUAGE == "TAIWANESE" |
      LANGUAGE == "CHINESE, CLASSICAL" |
      LANGUAGE == "CHINESE, PRE-MODERN") %>%
    select("ALL_LEVEL_TOTAL") %>%
    sum(na.rm = T)

  values$chinese_learners2009 <- df %>%
    filter(SRVY_YEAR == 2009) %>%
    filter(LANGUAGE == "CHINESE" | LANGUAGE == "MANDARIN" |
      LANGUAGE == "CANTONESE" |
      LANGUAGE == "TAIWANESE" |
      LANGUAGE == "CHINESE, CLASSICAL" |
      LANGUAGE == "CHINESE, PRE-MODERN") %>%
    select("ALL_LEVEL_TOTAL") %>%
    sum(na.rm = T)

  values$per_inc_02_to_09_ch <-
    round(
      (values$chinese_learners2009 -
        values$chinese_learners2002) /
        values$chinese_learners2002 * 100,
      digits = 2
    )

  values$total_language_learners2002 <- df %>%
    filter(SRVY_YEAR == 2002) %>%
    select(ALL_LEVEL_TOTAL) %>%
    sum(na.rm = T)

  values$total_language_learners2009 <- df %>%
    filter(SRVY_YEAR == 2009) %>%
    select(ALL_LEVEL_TOTAL) %>%
    sum(na.rm = T)

  values$total_language_learners2013 <- df %>%
    filter(SRVY_YEAR == 2013) %>%
    select(ALL_LEVEL_TOTAL) %>%
    sum(na.rm = T)

  values$total_language_learners2016 <- df %>%
    filter(SRVY_YEAR == 2016) %>%
    select(ALL_LEVEL_TOTAL) %>%
    sum(na.rm = T)

  values$per_increase_2002_to_2009_all <-
    round(
      (values$total_language_learners2009 -
        values$total_language_learners2002) /
        values$total_language_learners2002 * 100,
      digits = 2
    )

  values$per_2013_to_2016_all <-
    round(
      (values$total_language_learners2016 -
        values$total_language_learners2013) /
        values$total_language_learners2013 * 100,
      digits = 2
    )

  values
}

v <- con_values(df)
v
