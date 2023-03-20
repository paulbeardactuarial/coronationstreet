source(".\Corri Data Pull.R")



#-----------------Scatter plots to show survivals/deaths--------------------------
ggplot(et.rounded %>% 
         group_by(char.name),#%>% 
       #mutate(is.death.2=any(is.death)),
       aes(x=year.exposure,y=age,colour=state,alpha=state)) +
  geom_point(shape=19) +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Age",breaks=seq(0,100,by=20)) +
  scale_alpha_manual(values=c("Survivals"=0.12,"Deaths"=0.75)) +
  scale_colour_manual(values=list("Survivals"="red","Deaths"="black")) +
  facet_wrap(~state) +
  theme_bw() +
  theme(legend.position="none",
        strip.background = element_rect(fill="red"),
        strip.text = element_text(color="white",size=11,face="bold"))
ggsave("Experience Overview.jpg") 


#-----------------Chimney plots to show exposure------------------------------

#rounded year vs exposure
reps<-length(et.rounded$round.year.f %>% unique())
brick.pattern.img<-list.files(pattern = 'brick.pattern.png',full.names=TRUE) %>% rep(.,reps)
ggplot(data=et.rounded %>% group_by(round.year.f)) +
  geom_bar_pattern(
    aes(x=round.year.f,
        pattern_filename = round.year.f
    ), 
    pattern         = 'image',
    pattern_type    = 'tile',
    pattern_scale   = 0.25,
    pattern_spacing = 0.025
  ) +
  scale_pattern_filename_discrete(choices = brick.pattern.img) +
  scale_x_discrete(name="Years") +
  scale_y_continuous(name="Total Exposure",breaks=seq(0,500,by=50)) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(legend.position = "none",
        #text=element_text(size=14,family="serif"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        #axis.title.y = element_text(family="serif",size=14),
        #axis.title.x = element_text(family="serif",size=14))

# #year vs exposure
# reps<-2
# brick.pattern.img<-list.files(pattern = 'brick.pattern.png',full.names=TRUE) %>% rep(.,reps)
# ggplot(data=et.rounded %>% group_by(year.exposure,dummy) %>% summarise(count=n())) +
#   geom_area_pattern(
#     aes(x=year.exposure,
#         y=count,
#         pattern_filename = as.factor(dummy)
#     ), 
#     pattern         = 'image',
#     pattern_type    = 'tile',
#     pattern_scale   = 1.5,
#     pattern_colour = 'none'
#   ) +
#   scale_pattern_filename_discrete(choices = brick.pattern.img) +
#   theme_classic() +
#   theme(legend.position = "none")

#rounded age vs exposure
reps<-length(et.rounded$round.age.f %>% unique())
brick.pattern.img<-list.files(pattern = 'brick.pattern.png',full.names=TRUE) %>% rep(.,reps)
ggplot(data=et.rounded %>% group_by(round.age.f)) +
  geom_bar_pattern(
    aes(x=round.age.f,
        pattern_filename = round.age.f
    ), 
    pattern         = 'image',
    pattern_type    = 'tile',
    pattern_scale   = 0.25,
    pattern_spacing = 0.025
  ) +
  scale_pattern_filename_discrete(choices = brick.pattern.img) +
  scale_x_discrete(name="AGES") +
  scale_y_continuous(name="TOTAL EXPOSURE",breaks=seq(0,350,by=50)) +
  theme_classic() +
  theme(legend.position = "none",
        text=element_text(size=14,family="serif"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# #age vs exposure
# reps<-2
# brick.pattern.img<-list.files(pattern = 'brick.pattern.png',full.names=TRUE) %>% rep(.,reps)
# ggplot(data=et.rounded %>% group_by(age,dummy) %>% summarise(count=n())) +
#   geom_area_pattern(
#     aes(x=age,
#         y=count,
#         pattern_filename = as.factor(dummy)
#     ), 
#     pattern         = 'image',
#     pattern_type    = 'tile',
#     pattern_scale   = 1.5,
#     pattern_colour = 'none'
#   ) +
#   scale_pattern_filename_discrete(choices = brick.pattern.img) +
#   theme_classic() +
#   theme(legend.position = "none")
















mort.rates.by.age %>% summarise(actual.deaths=sum(actual.deaths),expected.deaths=sum(expected.deaths))

mort.rates.by.age<-et.rounded %>% 
  group_by(round.age) %>% 
  summarise(exposure=n(),actual.deaths=sum(is.death),expected.deaths=sum(qx)) %>% 
  mutate(actual.mort.rate=actual.deaths/exposure) %>%
  mutate(expect.mort.rate=expected.deaths/exposure) %>%
  mutate(expect.mort.rate.95U=pmin(1,expect.mort.rate+qnorm(0.975)*sqrt(expect.mort.rate*(1-expect.mort.rate)/exposure))) %>%
  mutate(expect.mort.rate.95L=pmax(0,expect.mort.rate+qnorm(0.025)*sqrt(expect.mort.rate*(1-expect.mort.rate)/exposure))) %>%
  mutate(H0.Accept=(actual.mort.rate<=expect.mort.rate.95U & actual.mort.rate>=expect.mort.rate.95L))

mort.rates.by.year<-et.rounded %>%
  filter(age<90) %>%
  group_by(round.year) %>% 
  summarise(exposure=n(),actual.deaths=sum(is.death),expected.deaths=sum(qx)) %>% 
  mutate(actual.mort.rate=actual.deaths/exposure) %>%
  mutate(expect.mort.rate=expected.deaths/exposure) %>%
  mutate(expect.mort.rate.95U=pmin(1,expect.mort.rate+qnorm(0.975)*sqrt(expect.mort.rate*(1-expect.mort.rate)/exposure))) %>%
  mutate(expect.mort.rate.95L=pmax(0,expect.mort.rate+qnorm(0.025)*sqrt(expect.mort.rate*(1-expect.mort.rate)/exposure))) %>%
  mutate(H0.Accept=(actual.mort.rate<=expect.mort.rate.95U & actual.mort.rate>=expect.mort.rate.95L))

pp<-ggplot(data=mort.rates.by.age %>% filter(round.age<=80)) +
  geom_line(aes(x=round.age,y=expect.mort.rate)) +
  geom_area(aes(x=round.age,y=expect.mort.rate.95U),fill="grey",alpha=0.5) +
  geom_area(aes(x=round.age,y=expect.mort.rate.95L),fill="white") +
  geom_col(aes(x=round.age,y=actual.mort.rate,fill=H0.Accept),alpha=0.5) +
  scale_y_continuous(name="Mortality Rate (qx)",breaks=seq(0,0.2,by=0.02),labels=scales::percent_format()) +
  coord_cartesian(ylim=c(0,0.14)) +
  #scale_fill_discrete(list("TRUE"="green","FALSE"="red")) +
  theme_classic()


data<-exposure.table.5 %>% filter(age>=18) %>% group_by(year.exposure) %>% summarise(age=mean(age))
ggplot(data) + geom_line(aes(x=year.exposure,y=age))
















