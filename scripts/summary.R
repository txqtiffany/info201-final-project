# Summary Paragraph
# The questions that we have asked for our project is to find the percentages of the experience of no-native speakers learning
# Chinese, a percentage ofthe methods that students use to learn Chinese in a classroom setting, and how the trend of
# Chinese has been like for the last few years.

mla <- read.csv("./data/MLA_Language_Enrollment_Database.csv",
  stringsAsFactors = FALSE
)


get_summary_info <- function(dataset) {
  ret <- list()
  ret$length <- length(dataset)
  # Find out all people learning Chinese

  chinese_learners <- mla %>%
    filter(LANGUAGE == "MANDARIN" |
      LANGUAGE == "CHINESE" |
      LANGUAGE == "CANTONESE" |
      LANGUAGE == "TAIWANESE" |
      LANGUAGE == "CHINESE, CLASSICAL" |
      LANGUAGE == "CHINESE, PRE-MODERN")

  # Filter out to find students learning chinese in 2016

  chinese_learners_2016 <- mla %>%
    filter(SRVY_YEAR == "2016") %>%
    filter(LANGUAGE == "MANDARIN" |
      LANGUAGE == "CHINESE" |
      LANGUAGE == "CANTONESE" |
      LANGUAGE == "TAIWANESE" |
      LANGUAGE == "CHINESE, CLASSICAL" |
      LANGUAGE == "CHINESE, PRE-MODERN")

  # Filter out to find students learning a language in 2016

  all_learners_2016 <- mla %>%
    filter(SRVY_YEAR == "2016")

  # Find total of all learning languages

  all_learners_total <- sum(mla$ALL_LEVEL_TOTAL, na.rm = TRUE)
  ret$all_learners_total <- as.integer(all_learners_total)

  # Find total of all learning languages in 2016

  all_learners_total_2016 <-
    sum(all_learners_2016$ALL_LEVEL_TOTAL, na.rm = TRUE)
  ret$all_learners_total_2016 <- as.integer(all_learners_total_2016)

  # Find total all who are learning chinese
  chinese_learners_total <-
    sum(chinese_learners$ALL_LEVEL_TOTAL, na.rm = TRUE)
  ret$chinese_learners_total <- as.integer(chinese_learners_total)

  # Find total all who are learning chinese in 2016

  chinese_learners_total_2016 <-
    sum(chinese_learners_2016$ALL_LEVEL_TOTAL, na.rm = TRUE)
  ret$chinese_learners_total_2016 <
    as.integer(chinese_learners_total_2016)

  # Find the percent of students who are learning chinese out of total (in 2016)

  ret$chinese_all_percentage_2016 <-
    (round(chinese_learners_total_2016 / all_learners_total_2016, 4)) * 100

  ret$chinese_all_percentage <-
    (round(chinese_learners_total / all_learners_total, 4)) * 100

  # Find the percentage of Chinese language learners that are from
  # California in 2016

  chinese_learners_cali_2016 <- chinese_learners_2016 %>%
    filter(STATE_ID == "5")

  chinese_learners_cali <- chinese_learners %>%
    filter(STATE_ID == "5")

  chinese_learners_cali_total_2016 <-
    sum(chinese_learners_cali_2016$ALL_LEVEL_TOTAL, na.rm = TRUE)
  chinese_learners_cali_total <-
    sum(chinese_learners_cali$ALL_LEVEL_TOTAL, na.rm = TRUE)

  ret$cali_chinese_percent_2016 <-
    (round(chinese_learners_cali_total_2016 /
      chinese_learners_total_2016, 4)) * 100

  ret$cali_chinese_percent <-
    (round(chinese_learners_cali_total / chinese_learners_total, 4)) * 100

  # What school has the most number of students learning Chinese in 2016?

  ret$school_chinese_2016 <- chinese_learners_2016 %>%
    group_by(UNIV) %>%
    summarize(count = sum(ALL_LEVEL_TOTAL, na.rm = TRUE)) %>%
    filter(count == max(count, na.rm = TRUE)) %>%
    select(UNIV) %>%
    pull()

  # What school has the most number of students learning Chinese in general?

  ret$school_chinese <- chinese_learners %>%
    group_by(UNIV) %>%
    summarize(count = sum(ALL_LEVEL_TOTAL, na.rm = TRUE)) %>%
    filter(count == max(count, na.rm = TRUE)) %>%
    select(UNIV) %>%
    pull()

  # How many schools is Chinese taught in?
  school_chinese_unique_2016 <- nrow(distinct(chinese_learners_2016, UNIV))
  ret$school_chinese_unique_2016 <- as.integer(school_chinese_unique_2016)
  school_chinese_unique <- nrow(distinct(chinese_learners, UNIV))
  ret$school_chinese_unique <- as.integer(school_chinese_unique)
  return(ret)
}

get_summary_info(mla)
