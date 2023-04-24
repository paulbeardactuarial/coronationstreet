

old.mortality.table <- function(
    year = c(1911:1920),
    sex = c("male", "female")) {

  #numerical value of sex within data
  sex.value <- c(ifelse("male" %in% sex, 1, NA),ifelse("female" %in% sex, 2, NA))

  #ONS data files for deaths and population
  deathxl <- file.path("./Data/icd2.xls")
  popxl <- file.path("./Data/popln_tcm77-215653.xls")

  death.tabs <- c("description", "icd2_1", "icd2_2")
  pop.tabs <- "POPLNS"

  death.data <- map(.x = death.tabs, .f = read_xls, path = deathxl)
  pop.data <- map(.x = pop.tabs, .f = read_xls, path = popxl)

  death.data[[1]] <- rename(death.data[[1]], ICD_ = icdcode)

  all.deaths <- rbind(death.data[[2]], death.data[[3]]) %>% left_join(death.data[[1]])

  deaths.grp <- all.deaths %>%
    group_by(yr, sex, age) %>%
    summarise(deaths = sum(ndths)) %>%
    ungroup()
  deaths.grp$yr <- as.numeric(deaths.grp$yr)

  names(pop.data[[1]]) <- pop.data[[1]] %>%
    names() %>%
    tolower()

  mort.data.hist <- deaths.grp %>%
    left_join(pop.data[[1]]) %>%
    group_by(age, yr, sex) %>%
    summarise(deaths = sum(deaths), pop = sum(pop)) %>%
    mutate(mort.rate = deaths / pop)

  young.combo <- left_join(mort.data.hist %>% filter(age == "<1") %>% select(-mort.rate),
    mort.data.hist %>% filter(age == "01-04") %>% select(-mort.rate),
    by = c("yr", "sex")
  ) %>%
    mutate(
      pop = pop.x + pop.y,
      deaths = deaths.x + deaths.y,
      mort.rate = deaths / pop
    ) %>%
    select(yr, sex, deaths, pop, mort.rate)
  young.combo <- data.frame(age = rep("00-04", nrow(young.combo)), young.combo)

  mort.data.hist.2 <- bind_rows(young.combo, mort.data.hist) %>%
    filter(!age %in% c("<1", "01-04"))

  output <- mort.data.hist.2 %>%
    filter(yr %in% year, sex %in% sex.value) %>%
    group_by(age) %>%
    summarise(mort.rate = mean(mort.rate))

  output$round.age <- output$age %>%
    str_sub(0, 2) %>%
    str_replace("<1", "0") %>%
    as.numeric()

  return(output)
}
