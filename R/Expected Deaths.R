# round.down.select()
#
# function to find for each value in vector x the closest value of y that is less than or the same
#
round.down.select <- function(x, y) {
  df <- data.frame(key = seq_along(x), x)
  combos <- crossing(df, y)
  z <- combos %>%
    filter(x > y) %>%
    group_by(key) %>%
    filter(y == max(y)) %>%
    ungroup()
  rounded.x <- left_join(df, z, by = c("key", "x")) %>%
    .$y %>%
    replace_na(min(y))
  return(rounded.x)
}



# expected.mort()
#
# function to find for each value in vector x the closest value of y that is less than or the same
#

expected.mort <- function(exposure.table,
                          expected.mort.table,
                          base.weight = 1, # 5.3 for 2020; 7.5 for 2019; 1 for 1918
                          min.age = 0,
                          max.age = 79) {
  # filter out and group exposure table
  et <- exposure.table %>%
    filter(age >= min.age & age <= max.age) %>%
    group_by(age) %>%
    summarise(exposure = n(), deaths = sum(is.death))

  # convert age to an age available in expected.mort.table
  et$age <- round.down.select(
    x = et$age,
    y = expected.mort.table$age
  )

  # join expected.mort.table to et
  expected.mortality <- left_join(et,
    expected.mort.table,
    by = "age"
  )

  # re-weight qx
  expected.mortality$qx <- expected.mortality$qx * base.weight

  A.vs.E <- expected.mortality %>%
    mutate(E = exposure * qx) %>%
    rename(A = deaths) %>%
    select(age, A, E, exposure)

  A.E.ratio <- sum(A.vs.E$A) / sum(A.vs.E$E)

  AE <- list(A.vs.E, A.E.ratio)
  names(AE) <- c("AvsE", "total.ratio")
  return(AE)
}
