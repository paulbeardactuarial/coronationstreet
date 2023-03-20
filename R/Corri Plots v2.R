source(".\Corri Data Pull v2.R")



#-----------------Scatter plots to show survivals/deaths--------------------------
ggplot(et.rounded %>% 
         group_by(char.name),#%>% 
       #mutate(is.death.2=any(is.death)),
       aes(x=year.exposure,y=age,colour=state,alpha=state)) +
  geom_point(shape=19) +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Age",breaks=seq(0,100,by=20)) +
  scale_alpha_manual(values=c("Survivals"=0.3,"Deaths"=0.85)) +
  scale_colour_manual(values=list("Survivals"="bisque3","Deaths"="firebrick1")) +
  facet_wrap(~state) +
  theme_bw() +
  theme(legend.position="none",
        strip.background = element_rect(fill="firebrick1"),
        strip.text = element_text(color="white",size=11,face="bold"))
ggsave("Experience Overview.jpg") 


#-----------------Chimney plots to show exposure------------------------------

#rounded year vs exposure
reps<-length(et.rounded$round.year.f %>% unique())
brick.pattern.img<-list.files(pattern = 'brick.pattern.2.png',full.names=TRUE) %>% rep(.,reps)
ggplot(data=et.rounded %>% group_by(round.year.f)) +
  geom_bar_pattern(
    aes(x=round.year.f,
        pattern_filename = round.year.f
    ), 
    pattern         = 'image',
    pattern_type    = 'tile',
    pattern_scale   = 0.8,
    pattern_spacing = 0.025
  ) +
  scale_pattern_filename_discrete(choices = brick.pattern.img) +
  scale_x_discrete(name="Years") +
  scale_y_continuous(name="Total Exposure",breaks=seq(0,1000,by=100)) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("Exposure by Year (chimney).jpg")

#rounded age vs exposure
reps<-length(et.rounded$round.age.f %>% unique())
brick.pattern.img<-list.files(pattern = 'brick.pattern.2.png',full.names=TRUE) %>% rep(.,reps)
ggplot(data=et.rounded %>% group_by(round.age.f)) +
  geom_bar_pattern(
    aes(x=round.age.f,
        pattern_filename = round.age.f
    ), 
    pattern         = 'image',
    pattern_type    = 'tile',
    pattern_scale   = 1,
    pattern_spacing = 0.025
  ) +
  scale_pattern_filename_discrete(choices = brick.pattern.img) +
  scale_x_discrete(name="Ages") +
  scale_y_continuous(name="Total Exposure",breaks=seq(0,1000,by=100)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("Exposure by Age (chimney).jpg")



#-----------------Testing mortality curves------------------------------


compare.AvsE<-function(et.rounded,
                       age.cutoff=75,
                       mort.cutoff=0.1) {

mort.rates.by.age<-et.rounded %>% 
  filter(round.age<age.cutoff) %>%
  group_by(round.age.f,round.age) %>% 
  summarise(exposure=n(),actual.deaths=sum(is.death),expected.deaths=sum(qx)) %>% 
  mutate(actual.mort.rate=actual.deaths/exposure) %>%
  mutate(expect.mort.rate=expected.deaths/exposure) %>%
  mutate(expect.mort.rate.95U=pmin(1,expect.mort.rate+qnorm(0.975)*sqrt(expect.mort.rate*(1-expect.mort.rate)/exposure))) %>%
  mutate(expect.mort.rate.95L=pmax(0,expect.mort.rate+qnorm(0.025)*sqrt(expect.mort.rate*(1-expect.mort.rate)/exposure))) %>%
  mutate(H0.Accept=(actual.mort.rate<=expect.mort.rate.95U & actual.mort.rate>=expect.mort.rate.95L))

age.labs<-levels(mort.rates.by.age$round.age.f)[c(1:nrow(mort.rates.by.age))]
pp<-
  ggplot(data=mort.rates.by.age) +
  geom_line(aes(x=round.age,y=expect.mort.rate,colour="black")) +
  geom_area(aes(x=round.age,y=expect.mort.rate.95U),fill="grey",alpha=0.5) +
  geom_area(aes(x=round.age,y=expect.mort.rate.95L),fill="white") +
  geom_col(aes(x=round.age,y=actual.mort.rate,fill=H0.Accept),alpha=0.5) +
  scale_colour_manual(values="black",labels="Expected** Mortality Rates") +
  scale_fill_manual(name="Mortality",
                    labels=c("TRUE"="Actual Mortality Rates (falls within 95% C.I. range)",
                             "FALSE"="Actual Mortality Rates (falls outside 95% C.I. range)",
                             "expect.mort.rate.95U"="CI"),
                    values=c("TRUE"="bisque3","FALSE"="firebrick1")) +
  scale_y_continuous(name="Mortality Rate (qx)",breaks=seq(0,mort.cutoff,by=0.02),labels=scales::percent_format()) +
  scale_x_continuous(name="Age",breaks=unique(mort.rates.by.age$round.age),labels=age.labs) +
  coord_cartesian(ylim=c(0,mort.cutoff)) +
  theme_classic() +
  theme(legend.position = c(0.3,0.75),
        legend.title=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
pp

list(mort.rates.by.age,pp)
}















