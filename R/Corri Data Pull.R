library(tidyverse)
library(rvest)
library(ggpattern)
library(magick)

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
all.char.names <- all.char.names %>% unique() #remove duplicates... issue due to no Q characters

#pull in data for every character
all.char.data<-list()

for (i in seq_along(all.char.names)) {

char.text<-all.char.names[[i]] %>% str_replace_all(" ","_")
char.appear<-str_c("https://coronationstreet.fandom.com/wiki/",char.text,"_-_List_of_appearances")
char.bio<-str_c("https://coronationstreet.fandom.com/wiki/",char.text)
# #pulls only table on the page (appearances)
# dapage <- rvest::read_html(char.appear) %>% 
#   html_elements("table") %>% 
#   html_table() 

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

#what are the demographics looking like?
ggplot(exposure.table.2) +
  geom_histogram(aes(x=age),bins=10)


#------------------- Death Years ----------------------

char.deaths<-char.long %>% filter(char.stat=="Died")
year.death<-char.deaths$char.value %>% str_sub(start=-4) %>% parse_double()
char.deaths.full<-data.frame(char.deaths,year.death) %>% select(char.name,year.death)

exposure.table.3<-left_join(exposure.table.2,char.deaths.full)

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


#------------------- Expected Deaths ----------------------

#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables
ONS.mort.path<-"ONS Mortality Rates 2020.csv"
ONS.mort<-read.csv(ONS.mort.path)
base.mort<-ONS.mort %>% group_by(age) %>% summarise(qx=mean(qx))

base.weight<-10
base.mort$qx<-base.mort$qx*base.weight

#join expected deaths onto exposure table
exposure.table.5<-exposure.table.4 %>% left_join(base.mort)

#round the ages and exposure years so get decent sized pots
et.rounded<-exposure.table.5 %>%
  mutate(round.age=floor(age/5)*5) %>%
  mutate(round.year=floor(year.exposure/5)*5) 

#LINE BELOW IS TEMP MEASURE TO INSERT 1918 DATA for EXPECTED
mort.data.1918.2 <- mort.data.1918 %>% 
  #filter(age!="<1"&age!="85+") %>%
  group_by(round.age) %>% 
  summarise(mort.rate=mean(mort.rate))
et.rounded<-et.rounded %>% select(-qx) %>% left_join(mort.data.1918.2, by="round.age") %>% rename(qx=mort.rate)
et.rounded$dummy <- rep(1,nrow(et.rounded))

#create new variable for "state" - is useful for plots
et.rounded$state[et.rounded$is.death] <- "Deaths"
et.rounded$state[!et.rounded$is.death] <- "Survivals"
et.rounded$state <- factor(et.rounded$state,levels=c("Survivals","Deaths"))

#create new variable for rounded age and rounded year
et.rounded$round.age.f<-factor(str_c(et.rounded$round.age,et.rounded$round.age+4,sep="-"),
                               levels=str_c(seq(0,90,by=5),seq(0,90,by=5)+4,sep="-"))
et.rounded$round.year.f<-factor(str_c(et.rounded$round.year,str_sub(et.rounded$round.year+4,start=3,end=4),sep="-"),
                               levels=str_c(seq(1960,2020,by=5),str_sub(seq(1960,2020,by=5)+4,start=3,end=4),sep="-"))









