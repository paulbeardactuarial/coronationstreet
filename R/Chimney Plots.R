# et.trunc.processing()
#
# Function to process truncated experience table to make suitable for plots
#

et.trunc.processing <- function(exposure.table,
                                round.base,
                                min.age,
                                max.age,
                                min.year,
                                max.year) {
  # create new variable for rounded age and year
  et.trunc <- exposure.table %>%
    mutate(round.age = floor(age / round.base) * round.base) %>%
    mutate(round.year = floor(year.exposure / round.base) * round.base)

  # filter out ages and years we don't want
  et.trunc <- et.trunc %>% filter(
    year.exposure >= min.year,
    year.exposure <= max.year,
    age >= min.age,
    age <= max.age
  )

  # create output df
  output <- et.trunc

  # create new variable for "state" - is useful for plots
  output$state[et.trunc$is.death] <- "Deaths"
  output$state[!et.trunc$is.death] <- "Survivals"
  output$state <- factor(output$state, levels = c("Survivals", "Deaths"))

  # create new variable for rounded age and rounded year
  output$round.age.f <- factor(str_c(et.trunc$round.age, et.trunc$round.age + (round.base - 1), sep = "-"),
    levels = str_c(seq(min.age, max.age, by = round.base),
      seq(min.age, max.age, by = round.base) + (round.base - 1),
      sep = "-"
    )
  )
  output$round.year.f <- factor(
    str_c(et.trunc$round.year,
      str_sub(et.trunc$round.year + (round.base - 1), start = 3, end = 4),
      sep = "-"
    ),
    levels = str_c(seq(min.year, max.year, by = round.base),
      str_sub(seq(min.year, max.year, by = round.base) + (round.base - 1), start = 3, end = 4),
      sep = "-"
    )
  )

  return(output)
}


# plot.chimneys()
#
# Function to process exposure.table and create chimney plots
#


plot.chimneys <- function(exposure.table,
                          plot.pattern = "brick.pattern.png",
                          save.results,
                          round.base = 10,
                          min.age = 0,
                          max.age = 90,
                          min.year = 1960,
                          max.year = 2023) {

  # clean the data for plotting
  plot.data <- et.trunc.processing(exposure.table,
    round.base = round.base,
    min.age = min.age,
    max.age = max.age,
    min.year = min.year,
    max.year = max.year
  )

  # list to hold results in
  chimney <- list()

  # plot rounded year vs exposure

  reps <- length(plot.data$round.year.f %>% unique())

  brick.pattern.img <- list.files(path = "./Images", pattern = plot.pattern, full.names = TRUE) %>% rep(., reps)

  chimney[[1]] <-
    ggplot(data = plot.data %>% group_by(round.year.f)) +
    geom_bar_pattern(
      aes(
        x = round.year.f,
        pattern_filename = round.year.f
      ),
      pattern = "image",
      pattern_type = "tile",
      pattern_scale = 0.8,
      pattern_spacing = 0.025
    ) +
    scale_pattern_filename_discrete(choices = brick.pattern.img) +
    scale_x_discrete(name = "Years") +
    scale_y_continuous(name = "Total Exposure") +
    theme_classic() +
    theme(legend.position = "none") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )




  # plot rounded age vs exposure

  reps <- length(plot.data$round.age.f %>% unique())

  brick.pattern.img <- list.files(path = "./Images", pattern = plot.pattern, full.names = TRUE) %>% rep(., reps)

  chimney[[2]] <- ggplot(data = plot.data %>% group_by(round.age.f)) +
    geom_bar_pattern(
      aes(
        x = round.age.f,
        pattern_filename = round.age.f
      ),
      pattern = "image",
      pattern_type = "tile",
      pattern_scale = 1,
      pattern_spacing = 0.025
    ) +
    scale_pattern_filename_discrete(choices = brick.pattern.img) +
    scale_x_discrete(name = "Ages") +
    scale_y_continuous(name = "Total Exposure") +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


  # name plot items
  names(chimney) <- c("Year", "Age")


  # save the plots
  if (save.results) {
    ggsave("./Plots/Exposure by Year (chimney).jpg", chimney$Year)
    ggsave("./Plots/Exposure by Age (chimney).jpg", chimney$Age)
  }

  return(chimney)
}
