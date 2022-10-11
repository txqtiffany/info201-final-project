# chart 3 Emily O'Neill

library(dplyr)
library(ggplot2)

mla_raw <- read.csv("./data/MLA_Language_Enrollment_Database.csv",
  stringsAsFactors = FALSE
)

chart3 <- function(mla_raw) {

  # Trim dataset down to needed columns
  needed_cols <- c("SRVY_YEAR", "LANGUAGE", "ALL_LEVEL_TOTAL")
  mla_trim <- data.frame(mla_raw[, needed_cols])
  mla_trim <- na.omit(mla_trim, needed_cols)
  mla_trim <- subset(mla_trim, mla_trim$LANGUAGE != "")

  # Consolidate all Chinese dialects in dataset
  is_Chinese <- mla_trim$LANGUAGE == "MANDARIN" |
    mla_trim$LANGUAGE == "CANTONESE" |
    mla_trim$LANGUAGE == "TAIWANESE" |
    mla_trim$LANGUAGE == "CHINESE, CLASSICAL" |
    mla_trim$LANGUAGE == "CHINESE, PRE-MODERN"
  mla_trim[is_Chinese == T, ]$LANGUAGE <- "CHINESE"

  # Adjust Dataset to suitable format
  mla_trim <- mla_trim %>%
    filter(SRVY_YEAR > 1975) %>%
    group_by(LANGUAGE, SRVY_YEAR, add = T) %>%
    arrange(SRVY_YEAR, .by_group = T) %>%
    summarize(year_total = sum(ALL_LEVEL_TOTAL))

  # Make list of 10 most popluar languages overall
  language_list <- mla_trim %>%
    group_by(LANGUAGE) %>%
    summarize(total = sum(year_total)) %>%
    arrange(-total) %>%
    head(10) %>%
    pull(LANGUAGE)

  # Choose titles for 2 graphs
  title_pop <- paste0(
    "Change in Popularity of 10 Most Studied",
    " Languages at US Colleges Over the\n Past",
    " Few Decades"
  )
  title_chinese <- paste0(
    "Change in Popularity of Chinese at US",
    " Colleges Over the Past Few Decades"
  )

  # Make graph "pop_graph" that includes only 10 most popular
  mla_pop <- mla_trim %>%
    filter(LANGUAGE %in% language_list)
  pop_graph <- ggplot() + ggtitle(title_pop) +
    geom_line(data = mla_pop, aes(
      x = SRVY_YEAR,
      y = year_total,
      color = LANGUAGE
    ), size = 1) +
    xlab("YEAR") + ylab("# of STUDENTS")

  # Make graph "chinese_graph" that includes only Chinese
  mla_chinese <- mla_trim %>%
    filter(LANGUAGE == "CHINESE")
  chinese_graph <- ggplot() + ggtitle(title_chinese) +
    geom_line(data = mla_chinese, aes(
      x = SRVY_YEAR,
      y = year_total,
      color = LANGUAGE
    ), size = 1) +
    xlab("YEAR") + ylab("# of STUDENTS")

  list(chinese_graph, pop_graph)
}
