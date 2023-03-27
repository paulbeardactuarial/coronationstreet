
#---------------- Truncated Exposure Plots ---------------------------------

# size of grouping category for age and years
round.base <- 10
min.age <- 0
max.age <- 90
min.year <- 1960
max.year <- 2020

# create new variable for "state" - is useful for plots
et.trunc$state[et.trunc$is.death] <- "Deaths"
et.trunc$state[!et.trunc$is.death] <- "Survivals"
et.trunc$state <- factor(et.trunc$state, levels = c("Survivals", "Deaths"))

# create new variable for rounded age and rounded year
et.trunc$round.age.f <- factor(str_c(et.trunc$round.age, et.trunc$round.age + (round.base - 1), sep = "-"),
                                       levels = str_c(seq(min.age, max.age, by = round.base),
                                                      seq(min.age, max.age, by = round.base) + (round.base - 1),
                                                      sep = "-"
                                       )
)
et.trunc$round.year.f <- factor(
  str_c(et.trunc$round.year,
        str_sub(et.trunc$round.year + (round.base - 1), start = 3, end = 4),
        sep = "-"
  ),
  levels = str_c(seq(min.year, max.year, by = round.base),
                 str_sub(seq(min.year, max.year, by = round.base) + (round.base - 1), start = 3, end = 4),
                 sep = "-"
  )
)


#plot rounded year vs exposure

reps<-length(et.trunc$round.year.f %>% unique())

brick.pattern.img<-list.files(path="./Images",pattern = 'brick.pattern.2.png',full.names=TRUE) %>% rep(.,reps)

ggplot(data=et.trunc %>% group_by(round.year.f)) +
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



#plot rounded age vs exposure

reps<-length(et.trunc$round.age.f %>% unique())

brick.pattern.img<-list.files(path="./Images",pattern = 'brick.pattern.2.png',full.names=TRUE) %>% rep(.,reps)

ggplot(data=et.trunc %>% group_by(round.age.f)) +
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

