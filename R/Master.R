library(ggpattern)
library(tidyverse)
library(openxlsx2)
library(readxl)

source("./R/Fandom Data Scrape.R")
source("./R/Processing for Experience Tables.R")
source("./R/Chimney Plots.R")
source("./R/Full Exposure Plot.R")
source("./R/UK Historical Mortality.R")

# scrape the coronation data from fandom wiki site (option to save .csv)
character.data <- scrape.corrie.data(
  save.results = F
)

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
mort.male.1918 <- old.mortality.table(
  year = 1918, # can select any combo of years between 1911 and 1920
  sex = "male" # can select any combo of male/female
)
