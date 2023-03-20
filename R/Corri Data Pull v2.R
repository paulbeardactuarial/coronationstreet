library(tidyverse)
library(rvest)
library(ggpattern)
#library(magick)

#section 1 - pull list of character names

char.list.1<-"https://coronationstreet.fandom.com/wiki/Category:List_of_main_character_appearances?from="
all.char.names.list<-list()

for (lett in LETTERS) {
char.list<-str_c(char.list.1,lett,sep="")
character.names<- rvest::read_html(char.list) %>% 
  html_elements(".category-page__member-link") %>%
  html_text() %>% 
  str_remove_all(" - List of appearances")
all.char.names.list[[lett]]<-character.names
}
all.char.names<-all.char.names.list %>% unlist() %>% as.character()
all.char.names <- all.char.names %>% unique() #remove duplicates

#pull in data for every character
all.char.data<-list()

for (i in seq_along(all.char.names)) {

char.text<-all.char.names[[i]] %>% str_replace_all(" ","_")
char.appear<-str_c("https://coronationstreet.fandom.com/wiki/",char.text,"_-_List_of_appearances")
char.bio<-str_c("https://coronationstreet.fandom.com/wiki/",char.text)

#pull the key stats for the character
char.stat<-rvest::read_html(char.bio) %>% 
  html_elements(".pi-secondary-font") %>% 
  html_text() %>%
  .[-1]
char.value<-rvest::read_html(char.bio) %>% 
  html_elements(".pi-font") %>% 
  html_text() 
char.data<-data.frame(char.stat,char.value)
all.char.data[[i]]<-char.data
}
names(all.char.data) <- all.char.names
char.long<-all.char.data %>% bind_rows(.id="char.name")

#checkpoint
write.csv(char.long,"character.data.csv")

#---------------- Birth Years -----------------------------

chars.born<-char.long %>% filter(char.stat=="Born") 

#characters that don't have a born date
setdiff(all.char.names,chars.born$char.name)
#characters that do have a born date
intersect(all.char.names,chars.born$char.name)

#clean the date of birth data to get 4-digit year
year.of.birth<-chars.born$char.value %>% 
  str_replace_all("(See \"Background information\" below)","") %>%
  str_replace_all(" ","") %>%
  str_replace_all("[a-z,A-Z,(,)]","") %>% 
  str_sub(start=-4) %>%
  parse_double() 

#df of characters with a valid year of birth
valid.births<-data.frame(chars.born,year.of.birth) %>% 
  filter(year.of.birth>999) %>% #removes entries that has only date and not year of birth
  select(char.name,year.of.birth)

#------------------- Exposure Years ----------------------

char.durations<-char.long %>% filter(char.stat=="Duration")

#function to produce a vector from the string formats used within "Duration" entries
transcribe.duration<-function(x) {
  y<-str_replace_all(x," to present",":2022")
  y<-str_replace_all(y,"-",":")
  #two lines to get rid of specials
  cut.off<-str_locate(y,"\\(Coronation Street\\)") %>% .[[1]]
  if(!is.na(cut.off)) {y<-str_sub(y,end=cut.off-1)}
  y<-str_replace_all(y,"[a-zA-Z\\(\\)]","")
  eval(parse(text=str_c("c(",y,")")))
}

#map the function to get vector of exposure for every character
years.exposure.list<-map(char.durations$char.value,transcribe.duration)
names(years.exposure.list) <- char.durations$char.name
exposure.table<-enframe(years.exposure.list) %>% unnest(value)
names(exposure.table) <- c("char.name","year.exposure")

#add year of birth and get age for each exposure
exposure.table.2 <- exposure.table %>% 
  inner_join(valid.births) %>% 
  mutate(age=year.exposure-year.of.birth)


#------------------- Death Years ----------------------

#create data.frame of characters and death years
char.deaths<-char.long %>% filter(char.stat=="Died")
year.death<-char.deaths$char.value %>% str_sub(start=-4) %>% parse_double()
char.deaths.full<-data.frame(char.deaths,year.death) %>% select(char.name,year.death)

#join deaths table to exposure table
exposure.table.3<-left_join(exposure.table.2,char.deaths.full)

#add is.death variable when year of death matches year of exposure
is.death<-(exposure.table.3$year.death==exposure.table.3$year.exposure) %>% 
  str_replace_na(replacement = F) %>% 
  as.logical() 
exposure.table.4<-data.frame(exposure.table.3,is.death)

#some of characters die within 1 year of leaving show... most likely due to actor dying in real life
#...these characters were usually still living at Coronation Street and die on holiday (or similar)
#...therefore think it is appropriate to include, so we reduce year of death by 1 (reasonable approximation)
die.after.last.appear<-exposure.table.4 %>% group_by(char.name) %>% 
  summarise(is.dead=max(is.death),year.death=mean(year.death)) %>%
  filter(!is.na(year.death),is.dead==0) %>% .$char.name
adjusters<-(exposure.table.4$char.name %in% die.after.last.appear)
exposure.table.4$year.death[adjusters] <- exposure.table.4$year.death[adjusters]-1
is.death.adj<-(exposure.table.4$year.death==exposure.table.4$year.exposure) %>% 
  str_replace_na(replacement = F) %>% 
  as.logical() 
exposure.table.4$is.death <- is.death.adj

#---------------- Extra Variables in Exposure Table ---------------------------------

#size of grouping category for age and years
round.base<-10

#create new variable for "state" - is useful for plots
exposure.table.4$state[exposure.table.4$is.death] <- "Deaths"
exposure.table.4$state[!exposure.table.4$is.death] <- "Survivals"
exposure.table.4$state <- factor(exposure.table.4$state,levels=c("Survivals","Deaths"))

#create new variable for rounded age and year
exposure.table.4<- exposure.table.4 %>% 
  mutate(round.age=floor(age/round.base)*round.base) %>%
  mutate(round.year=floor(year.exposure/round.base)*round.base)

#create new variable for rounded age and rounded year
exposure.table.4$round.age.f<-factor(str_c(exposure.table.4$round.age,exposure.table.4$round.age+(round.base-1),sep="-"),
                                     levels=str_c(seq(0,90,by=round.base),
                                                  seq(0,90,by=round.base)+(round.base-1),
                                                  sep="-"))
exposure.table.4$round.year.f<-factor(str_c(exposure.table.4$round.year,
                                            str_sub(exposure.table.4$round.year+(round.base-1),start=3,end=4),
                                            sep="-"),
                                      levels=str_c(seq(1960,2020,by=round.base),
                                                   str_sub(seq(1960,2020,by=round.base)+(round.base-1),start=3,end=4),
                                                   sep="-"))

#------------------- Expected Deaths ----------------------
base.weight<-7.5 #5.3 for 2020; 7.5 for 2019; 1 for 1918
mort.table<-2019 #either 1918 or 2020

#2020 Expected mortality rates
if(mort.table==2020) {
ONS.mort.path<-"ONS Mortality Rates 2020.csv"
ONS.mort<-read.csv(ONS.mort.path)
base.mort<-ONS.mort %>% group_by(age) %>% summarise(qx=mean(qx))
}

#2019 Expected mortality rates
if(mort.table==2019) {
  ONS.mort.path<-"ONS Mortality Rates 2019.csv"
  ONS.mort<-read.csv(ONS.mort.path)
  base.mort<-ONS.mort %>% group_by(age) %>% summarise(qx=mean(qx))
}

#1918 Expected mortality rates
if(mort.table==1918) {
base.mort <- mort.data.1918.male %>% 
  group_by(round.age) %>% 
  summarise(mort.rate=mean(mort.rate)) %>% 
  rename(qx=mort.rate)
}

#re-weight qx
base.mort$qx<-base.mort$qx*base.weight

#TEMP LINE - NEEDED TO CONVERT 1918 into 10-year blocks instead of 5-year
base.mort<-base.mort %>% 
  mutate(round.age.2=floor(round.age/10)*10) %>% 
  select(-round.age) %>% 
  rename(round.age=round.age.2) %>% 
  group_by(round.age) %>%
  summarise(qx=mean(qx))

#attach the qx rates
if(mort.table %in% c("2019","2020")) {et.rounded<-exposure.table.4 %>% left_join(base.mort, by="age")}
if(mort.table==1918) {et.rounded<-exposure.table.4 %>% left_join(base.mort, by="round.age")}

#get AvsE table and plot (using compare.AvsE() function)
x<-compare.AvsE(et.rounded, age.cutoff=80, mort.cutoff=0.1) 
da.plot<-x[[2]] +
  labs(caption="** Expected Mortality Rates based on 7.5x 2017-2019 UK (Mixed) Experience")
da.plot
ggsave("AvsE - 7.5x UK 2019.jpg")


mort.rates.cs<-x[[1]] %>% .$actual.mort.rate %>% expand.grid(c(1:10),.) %>% .$Var2
data.frame(c(0:79),mort.rates.cs) %>% 
  mutate(survival.cs=1-mort.rates.cs) %>%
  mutate(lag.cs=lag(survival.cs,-2))
  mutate()


x[[1]] %>% filter(round.age<75) %>% .$actual.deaths %>% sum()
x[[1]] %>% filter(round.age<75) %>% .$expected.deaths %>% sum(na.rm=T)



