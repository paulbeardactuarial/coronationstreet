#-----------------Scatter plots to show survivals/deaths--------------------------

experience.scatter <- function(exposure.table,
                               save.results = F) {

  scatter.data <- exposure.table

  # create new variable for "state" - is useful for plots
  scatter.data$state[exposure.table$is.death] <- "Deaths"
  scatter.data$state[!exposure.table$is.death] <- "Survivals"
  scatter.data$state <- factor(scatter.data$state, levels = c("Survivals", "Deaths"))

  scatter.plot <- ggplot(
    scatter.data %>%
      group_by(Character),
    aes(x = year.exposure, y = age, colour = state, alpha = state)
  ) +
    geom_point(shape = 19) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Age") +
    scale_alpha_manual(values = c("Survivals" = 0.3, "Deaths" = 0.85)) +
    scale_colour_manual(values = list("Survivals" = "bisque3", "Deaths" = "firebrick1")) +
    facet_wrap(~state) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "firebrick1"),
      strip.text = element_text(color = "white", size = 11, face = "bold")
    )

  if (save.results) {
    ggsave("./Plots/Experience Overview.jpg")
  }

  return(scatter.plot)
}
