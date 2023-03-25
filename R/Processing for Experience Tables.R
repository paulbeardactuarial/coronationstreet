
character.data <- read.csv("character.data.csv")

all.char.names <- character.data$Character %>% unique()

#---------------- Birth Years -----------------------------

chars.born <- character.data %>% filter(Field == "Born")

# birth date is messy and needs cleaning.
# decided to only have birth year as some characters only have year...
# ...and didn't want to exclude these

year.of.birth <- chars.born$Value %>%
  str_replace_all("(See \"Background information\" below)", "") %>%   #remove specific text sometimes in field
  str_replace_all(" ", "") %>%                                        #remove spaces
  str_replace_all("[a-z,A-Z,(,)]", "") %>%                            #remove letters and commas
  str_sub(start = -4) %>%
  parse_double()

# data frame of characters with a valid year of birth

valid.births <- data.frame(chars.born, year.of.birth) %>%
  filter(year.of.birth > 999) %>%                                     # removes a few entries that had a date without year of birth
  select(Character, year.of.birth)

#------------------- Exposure Years ----------------------

char.durations <- character.data %>% filter(Field == "Duration")

# function to produce a vector from the string formats used within "Duration" entries
curr.year <- 2023

# function that turns a string (as per Duration entry of fandom.wiki) into a vector of years of appearance
transcribe.duration <- function(x) {

  #apply cleaning to x to get y
  y <- str_replace_all(x, " to present", str_c(":",curr.year,sep="")) #some entries say "to present" so convert to what that means in term of appearance years
  y <- str_replace_all(y, "-", ":")                                   #change to ":" so can evaluate the code to create vector

  #some entries have extra text to distinguish Coronation Street from other shows (i.e. After Hours)
  #this bit of code removes these specials
  cut.off <- str_locate(y, "\\(Coronation Street\\)") %>% .[[1]]
  if (!is.na(cut.off)) {y <- str_sub(y, end = cut.off - 1)}

  #remove any extra text left so left with string that defines a vector of numbers
  y <- str_replace_all(y, "[a-zA-Z\\(\\)]", "")

  #evaluate string to get vector of numbers
  output <- eval(parse(text = str_c("c(", y, ")")))

  return(output)
}

# map the function that converts a duration string to vector of numeric years
years.exposure.list <- map(char.durations$Value, transcribe.duration)
names(years.exposure.list) <- char.durations$Character

# reformat into data.frame of exposure
exposure.table <- enframe(years.exposure.list) %>% unnest(value)
names(exposure.table) <- c("Character", "year.exposure")


#------------------- Calc Age ----------------------

# add year of birth and get age for each exposure
exposure.table.2 <- exposure.table %>%
  inner_join(valid.births) %>%
  mutate(age = year.exposure - year.of.birth)


#------------------- Death Years ----------------------

# character deaths dates are listed on fandom.wiki
# importantly, some of these dates are due to a character being mentioned as dead years after leaving the show
# as we are only interested in deaths while living on the street, need to match death year to exposure year.
#
#we also find it is not uncommon for characters to die within 1 year of their last appearance
#these are typically when an actor left abruptly (sometimes died) and couldn't film their last episode
#...we have decided to count these as the character still living on the street at time of character death

# create data.frame of characters and death years
char.deaths <- character.data %>% filter(Field == "Died")

#clean data to get death year
year.death <- char.deaths$Value %>%
  str_sub(start = -4) %>%
  parse_double()
char.deaths.full <- data.frame(char.deaths, year.death) %>% select(Character, year.death)

#create is.death field and add to exposure table
exposure.table.3 <- data.frame(exposure.table.2,
                               is.death = vector(length = nrow(exposure.table.2), mode="logical"))

# initial joining of deaths table to exposure table
last.appearance <- exposure.table %>%
  group_by(Character) %>%
  filter(year.exposure == max(year.exposure)) %>%
  ungroup()

#if the year of death is same as last appearance then died on street
#we also count as died on street is year of death was only 1 higher than last appearance on street...
#,,,this is represents cases where actor left abruptly and was written out, but character still essentially living on Coronation St.
dead.on.street <- left_join(last.appearance, char.deaths.full) %>%
  filter(year.death == year.exposure | year.death == year.exposure + 1) %>%
  select(-year.death)

# create vector that tells if each row of exposure.table is present in dead.on.street vector
is.death <- apply(exposure.table.2 %>% select(Character, year.exposure), 1, function(row) {
  any(apply(dead.on.street, 1, function(row2) all(row == row2)))
})


# add is.death variable to exposure table data.frame
exposure.table.4 <- data.frame(exposure.table.3, is.death)


