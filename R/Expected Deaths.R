

# create new variable for rounded age and year
round.base <- 10

et.trunc <- exposure.table %>%
  mutate(round.age = floor(age / round.base) * round.base) %>%
  mutate(round.year = floor(year.exposure / round.base) * round.base)


#------------------- Expected Mortality ----------------------

base.weight <- 7.5 # 5.3 for 2020; 7.5 for 2019; 1 for 1918
mort.table.year <- 2019 # either 1918 or 2020

expected.mort <- function (base.weight,
                             mort.table.year) {

# Expected mortality rates (recent ONS data)
if (mort.table.year %in% c(2019,2020)) {
  ONS.mort.path <- paste("./Data/","ONS Mortality Rates ",mort.table.year,".csv",sep="")
  ONS.mort <- read.csv(ONS.mort.path)
  expected.mortality <- ONS.mort %>%
    group_by(age) %>%
    summarise(qx = mean(qx))
}

# Expected mortality rates (1918 ONS data)
  #WARNING - NEED to get code that pulls in 1918 data... this ain't it
if (mort.table.year == 1918) {
  expected.mortality <- mort.data.1918.male %>%
    group_by(round.age) %>%
    summarise(mort.rate = mean(mort.rate)) %>%
    rename(qx = mort.rate)
}

# re-weight qx
expected.mortality$qx <- expected.mortality$qx * base.weight

#convert 1918 into 10-year blocks instead of 5-year
if (mort.table.year == 1918) {
  expected.mortality <- expected.mortality %>%
    transmute(round.age = floor(round.age / 10) * 10) %>%
    #select(-round.age) %>%
    #rename(round.age = round.age.2) %>%
    group_by(round.age) %>%
    summarise(qx = mean(qx))
}

return(expected.mortality)

}

expected.mortality <- expected.mort (base.weight = 7.5,
                             mort.table.year = 2019)

# attach the qx rates
if (mort.table.year %in% c(2019, 2020)) {
  et.rounded <- exposure.table %>% left_join(expected.mortality, by = "age")
}
if (mort.table.year == 1918) {
  et.rounded <- et.trunc %>% left_join(expected.mortality, by = "round.age")
}













# get AvsE table and plot (using compare.AvsE() function)
x <- compare.AvsE(et.rounded, age.cutoff = 80, mort.cutoff = 0.1)
da.plot <- x[[2]] +
  labs(caption = "** Expected Mortality Rates based on 7.5x 2017-2019 UK (Mixed) Experience")
da.plot
ggsave("AvsE - 7.5x UK 2019.jpg")


mort.rates.cs <- x[[1]] %>%
  .$actual.mort.rate %>%
  expand.grid(c(1:10), .) %>%
  .$Var2
data.frame(c(0:79), mort.rates.cs) %>%
  mutate(survival.cs = 1 - mort.rates.cs) %>%
  mutate(lag.cs = lag(survival.cs, -2))
mutate()


x[[1]] %>%
  filter(round.age < 75) %>%
  .$actual.deaths %>%
  sum()
x[[1]] %>%
  filter(round.age < 75) %>%
  .$expected.deaths %>%
  sum(na.rm = T)
