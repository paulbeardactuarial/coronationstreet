#-----------------Scatter plots to show survivals/deaths--------------------------
ggplot(et.trunc %>%
         group_by(Character),#%>%
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
