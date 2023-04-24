library(ggpattern)
library(tidyverse)
library(openxlsx2)
library(readxl)

source("./R/Fandom Data Scrape.R")
source("./R/Processing for Experience Tables.R")
source("./R/Plots - Chimney.R")
source("./R/Plots - Full Exposure.R")
source("./R/UK Historical Mortality.R")
source("./R/Expected Deaths.R")
source("./R/Plots - A vs E.R")

# scrape the coronation data from fandom wiki site (option to save .csv)
character.data <- scrape.corrie.data(
  save.results = T
)

# read in character.data.csv instead...
character.data<-read.csv("./Data/character.data.csv")

# convert that scraped data into an experience table
exposure.table <- create.experience.table(
  character.data,
  curr.year = 2023
)

# get chimney plots showing exposure by year or by age (option to save .jpg)
chimney.plots <- plot.chimneys(
  exposure.table,
  plot.pattern = "brick.pattern.png",
  round.base = 10,
  min.age = 0,
  max.age = 89,
  min.year = 1960,
  max.year = 2023,
  save.results = F
)

# get scatter plot showing complete exposure overview (option to save .jpg)
scatter.plot <- experience.scatter(
  exposure.table,
  save.results = F
)

# get mortality table for males in 1918
mort.table.male.1918 <- old.mortality.table(
  year = 1918, # can select any combo of years between 1911 and 1920
  sex = "male" # can select any combo of male/female
)

# get mortality table for mixed sex in 2019
mort.table.mixed.2019 <- new.mortality.table(
  year = 2019, # can select year 2019 or 2020
  sex = c("male","female") # can select any combo of male/female
)

# get Actual vs Expected...

#...using 1918 male mortality and setting the weighting to 100%
AE.1918 <- expected.mort(exposure.table = exposure.table,
                           expected.mort.table = mort.table.male.1918,
                           base.weight = 1,  # 5.3 for 2020; 7.5 for 2019; 1 for 1918
                           min.age = 0,
                           max.age = 75)

#...using 2019 mixed mortality and setting the weighting to 750%
AE.2019 <- expected.mort(exposure.table = exposure.table,
                         expected.mort.table = mort.table.mixed.2019,
                         base.weight = 7.5,  # 5.3 for 2020; 7.5 for 2019; 1 for 1918
                         min.age = 0,
                         max.age = 75)

# put AvsE tables into a list
AvsE <- list(AE.1918$AvsE,AE.2019$AvsE)

# apply plot function to get A vs E plots of both 1918 and 2019
plot.AvsE <- map(AvsE,
                 .f=compare.AvsE,
                 round.base = 10,
                 mort.cutoff = 0.1)


