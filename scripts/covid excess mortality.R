# excess mortality

d1 <- read_csv("excess mortality data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2014-2018.csv")

d2 <- read_csv("excess mortality data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2019-2020.csv")

sum(names(d1)!=names(d2))

which(names(d1) != names(d2))

names(d1)
names(d2)

d14 <- d1 %>%
  select(1:5) %>%
  set_names(nm=c("state","year","week","date2","deaths"))

d19 <- d2 %>%
  select(1:5) %>%
  set_names(nm=c("state","year","week","date2","deaths"))

frq(d14$state)
frq(d19$state)

dat <- d14 %>%
  rbind(d19) %>%
  mutate(date = as.POSIXct(strptime(date2, format="%m/%d/%Y")),
         month=month(date)) %>%
  set_labels(month, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) %>%
  as.data.frame

str(dat)
head(dat)

frq(dat$month)

mnth_labs <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

write_rds(dat, "excess mortality data/prepared/all cause deaths 2014 - 25 Apr 2020.rds")
write_csv(dat, "excess mortality data/prepared/all cause deaths 2014 - 25 Apr 2020.csv")

dat <- read_rds("excell mortality data/prepared/all cause deaths 2014 - 25 Apr 2020.rds")

ggplot(dat, aes(x=date, y=deaths, color=state)) + 
  stat_smooth()


byYear <- dat %>%
  group_by(year, month) %>%
  summarize(deaths=sum(deaths)) 

ggplot(byYear, aes(x=month, y=deaths, group=year, color=as.factor(year))) +
  geom_line() +
  #stat_smooth(se=F) + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks=1:12, 
                     labels=get_labels(dat$month))

get_labels(dat$month)


byWk <- dat %>%
  filter(state=="New York" | state=="New York City") %>%
  group_by(year, month, week, date) %>%
  summarize(deaths=sum(deaths)) %>%
  as.data.frame

str(byWk)

head(byWk)

ggplot(byWk, aes(x=week, y=deaths, group=as.factor(year))) +
  geom_line() + 
  scale_x_datetime()














  # get COVID deaths (source https://covidtracking.com/api)

dths <- read_csv("excess mortality data/daily.csv") 

frq(dths$state)

ny <- dths %>%
  filter(state=="NY") %>%
  select(date3=date, covid_cum=death) %>%
  mutate(date2=as.POSIXct(strptime(date3, format="%Y%m%d")),
         week=week(date2),
         date=ifelse(week==11, "2020-03-14",
                     ifelse(week==12, "2020-03-21",
                            ifelse(week==13, "2020-03-28",
                                   ifelse(week==14, "2020-04-04",
                                          ifelse(week==15, "2020-04-11",
                                                 ifelse(week==16, "2020-04-18",
                                                        ifelse(week==17, "2020-04-25", "2020-05-02"))))))),
         year=2020) %>%
  arrange(date2) %>%
  mutate(covid_cum = tidyr::replace_na(covid_cum, 0),
         covid=covid_cum - lag(covid_cum)) 


nyWk <- ny %>%
  group_by(year, week, date) %>%
  summarize(covid=sum(covid)) %>%
  mutate(date=as.POSIXct(strptime(date,format="%Y-%m-%d"))) %>%
  na.omit(covid) %>%
  as.data.frame

str(nyWk)

out <- byWk %>%
  left_join(nyWk) %>%
  .[1:329,]

str(out)
head(out)

write_csv(out, "excess mortality data/prepared/New York state, deaths by week, with covid, 2014 - 25 Apr 2020.csv")
write_rds(out, "excess mortality data/prepared/New York state, deaths by week, with covid, 2014 - 25 Apr 2020.rds")


read_rds("excess mortality data/prepared/New York state, deaths by week, with covid, 2014 - 25 Apr 2020.rds")


ggplot(out, aes(week)) + 
  stat_smooth(aes(y=deaths, group=as.factor(year)), span=.3, se=F, color="darkgoldenrod", alpha=.8) +
  #geom_point(aes(covid)) + 
  stat_smooth(data=filter(out, covid>1000), aes(y=covid), span=.8, se=F, color="maroon", alpha=.6) + 
  scale_x_continuous(breaks=c(1,13,26, 39, 52),
                     labels=c("Jan","Apr", "Jul", "Oct" ,"Dec")) +
  scale_y_continuous(breaks=seq(1000,11000,1000),
                     labels=comma) + 
  labs(x="",
       y="deaths\n(weekly)",
       title="Excess death associated with COVID-19\nNew York State",
       caption="Reference lines are deaths\nfrom all causes, 2014-2019") + 
  nobord + 
  theme(axis.title.y=element_text(angle=0, vjust=.5)) + 
  gghighlight(max(deaths) > 10000) +
  annotate("text", x=20, y=9500, label="all causes\n(2020)", color="darkgoldenrod", size=3) + 
  annotate("text", x=20, y=4500, label="reported\nCOVID-19\ndeaths", color="maroon", size=3) 

ggsave("viz/excess mortality through 25 Apr, 2020.png",
       device="png",
       type="cairo",
       height=5,
       width=7)

library(gghighlight)


  scale_x_continuous(breaks=c(1,13,52),
                     labels=c("Jan","Apr","Dec"))



scale_x_continuous(breaks=c(1,5,9,13,17,22,26,30,35,39,44,48,52),
                   labels=c(mnth_labs, ""))


ggplot(out, aes(date, deaths, group=as.factor(year), color=as.factor(year))) + 
  #geom_line() + 
  stat_smooth(span=.5, se=F, color="darkgoldenrod3") 

+ 
  scale_x_datetime(date_labels="%b")

?stat_smooth



