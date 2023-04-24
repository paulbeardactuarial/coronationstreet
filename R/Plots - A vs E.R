#-----------------Plot Actual vs Expected ------------------------------


compare.AvsE <- function(AvsE,
                         round.base = 10,
                         mort.cutoff = 0.1) {


  AvsE$round.age <- floor(AvsE$age / round.base) * round.base

  mort.rates.by.age <- AvsE %>%
    group_by(round.age) %>%
    summarise(A = sum(A), E = sum(E), exposure = sum(exposure)) %>%
    mutate(actual.mort.rate = A / exposure) %>%
    mutate(expect.mort.rate = E / exposure) %>%
    mutate(expect.mort.rate.95U = pmin(1, expect.mort.rate + qnorm(0.975) * sqrt(expect.mort.rate * (1 - expect.mort.rate) / exposure))) %>%
    mutate(expect.mort.rate.95L = pmax(0, expect.mort.rate + qnorm(0.025) * sqrt(expect.mort.rate * (1 - expect.mort.rate) / exposure))) %>%
    mutate(H0.Accept = (actual.mort.rate <= expect.mort.rate.95U & actual.mort.rate >= expect.mort.rate.95L))

  uniq.ages <- unique(mort.rates.by.age$round.age)
  age.labs <- c(paste(head(uniq.ages,length(uniq.ages)-1),c(tail(uniq.ages,-1)-1),sep="-"),
                paste(max(uniq.ages),"+",sep=""))

  pp <-
    ggplot(data = mort.rates.by.age) +
    geom_line(aes(x = round.age, y = expect.mort.rate, colour = "black")) +
    geom_area(aes(x = round.age, y = expect.mort.rate.95U), fill = "grey", alpha = 0.5) +
    geom_area(aes(x = round.age, y = expect.mort.rate.95L), fill = "white") +
    geom_col(aes(x = round.age, y = actual.mort.rate, fill = H0.Accept), alpha = 0.5) +
    scale_colour_manual(values = "black", labels = "Expected Mortality Rates") +
    scale_fill_manual(
      name = "Mortality",
      labels = c(
        "TRUE" = "Actual Mortality Rates (falls within 95% C.I. range)",
        "FALSE" = "Actual Mortality Rates (falls outside 95% C.I. range)",
        "expect.mort.rate.95U" = "CI"
      ),
      values = c("TRUE" = "bisque3", "FALSE" = "firebrick1")
    ) +
    scale_y_continuous(name = "Mortality Rate (qx)",
                       breaks = seq(0, mort.cutoff, by = 0.02),
                       labels = scales::percent_format()) +
    scale_x_continuous(name = "Age",
                       breaks = unique(mort.rates.by.age$round.age),
                       labels = age.labs) +
    coord_cartesian(ylim = c(0, mort.cutoff)) +
    theme_classic() +
    theme(
      legend.position = c(0.3, 0.75),
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  pp

  return(pp)
}
