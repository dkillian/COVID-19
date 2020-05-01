


# prep --------------------------------------------------------------------

cases <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

names(cases)

dths <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")



# US cases -------------------------------------------------------------------

str(cases)
names(cases)

frq(cases$`Province/State`)
frq(cases$`Country/Region`)

usC <- cases %>%
  filter(`Country/Region`=="US")

usC

byDay <- usC %>%
  pivot_longer(cols=5:ncol(cases),
               names_to="day",
               values_to="cases") %>%
  select(-Lat, -Long) %>%
  group_by(day) %>%
  summarize(cases=sum(cases)) %>%
  mutate(day = as.POSIXct(strptime(day, format="%m/%d/%y")),
         log_cases = log(cases)) %>%
  arrange(day) %>%
  mutate(prcnt_grwth = (cases - lag(cases))/lag(cases)*100) %>%
  as.data.frame

head(byDay)
tail(byDay)
str(byDay)
frq(byDay$day)

ggplot(byDay, aes(day, cases)) +
  annotate("text", x=as.POSIXct("2020-02-08"), y=32200, label="33,272 cases") + 
  geom_point(size=1.5, color="darkblue", alpha=.8) + 
  stat_smooth(se=F, span=.2, color="blue", alpha=.2) + 
  scale_x_datetime(breaks=date_breaks("12 days"),
                   labels=date_format("%b-%d")) +
  scale_y_continuous(labels=comma) + 
  labs(x="",
       y="",
       title="U.S. COVID-19 cases\n22 Jan through 22 March, 2020") + 
  geom_hline(yintercept=33272, color="pink", alpha=.8, size=1) 

ggsave("viz/US cases 22 March 2020 no border.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


ggplot(byDay, aes(day, prcnt_grwth/100)) + 
  #annotate("text", x=as.POSIXct("2020-03-08"), y=2, label="40 percent growth is\na doubling time of five days") + 
  geom_point(size=1.5, color="darkblue", alpha=.8) + 
  stat_smooth(se=F, color="blue", alpha=.2) + 
  scale_x_datetime(breaks=date_breaks("12 days"),
                   labels=date_format("%b-%d")) +
  scale_y_continuous(labels=percent) + 
  labs(x="",
       y="Daily percent growth",
       title="Growth rate in U.S. COVID-19 cases\n22 Jan through 27 March, 2020",
       caption="40 percent daily growth rate\nis a doubling time of two days") + 
  geom_hline(yintercept=.40, color="maroon", alpha=.6, size=1, linetype="dashed") +
  theme(panel.border=element_blank(),
        axis.ticks=element_blank())

ggsave("viz/growth rate in US cases 28 March 2020 no border.png",
       device="png",
       type="cairo",
       height=4,
       width=7)



# cases by country --------------------------------------------------------

head(cases[,1:5])

library(RcppRoll)

byDay <- cases %>%
  pivot_longer(cols=5:ncol(cases),
               names_to="day",
               values_to="cases") %>% # put date columns into 'day' and number to 'cases'
  select(-Lat, -Long) %>%
  group_by(country=`Country/Region`, day) %>%
  summarize(cases=sum(cases)) %>% # in case any sub-national locations, aggregate to country by day
  mutate(day = as.POSIXct(strptime(day, format="%m/%d/%y"))) %>%
  filter(country == "China" | country == "France" | country == "Germany" |# country == "Iran" | 
           country == "Italy" | country == "Spain" | country == "US") %>% # limit to hardest hit countries
  group_by(country) %>%
  mutate(new_cases=cases - lag(cases, order_by=day), # create a growth variable
         week=isoweek(day)) %>% # generate a week integer 1-52 based on the date in case it helps us create a rolling average by week
  group_by(country, week) %>%
  mutate(week_new = round(mean(new_cases, na.rm=T),1)) %>% # rolling average by week, so both forward and backward looking window
  group_by(country) %>%
  mutate(week_new_roll = round(roll_mean(new_cases, 7, align='right', fill=NA),1)) %>% # a rolling average of the current value plus the six previous days
  arrange(desc(country, day)) 

  # visual inspection suggests calendar week average and previous week average are about the same 

str(byDay)
describe(byDay$wk_new)

  # legend

ggplot(byDay, aes(cases, week_new_roll, color=country)) + 
  #geom_point() + 
  stat_smooth(se=F) +
  scale_color_viridis_d() + 
  scale_x_log10(labels=comma) + 
  scale_y_log10(labels=comma) + 
  labs(x="Total confirmed cases",
       y="New cases\n(previous week)",
       title="Trajectory of COVID-19 Confirmed Cases\n22 Jan - 1 Apr, 2020") +
  theme(axis.title.y=element_text(angle=0, vjust=.5))

ggsave("viz/covid trajectory legend.png",
       device="png",
       type="cairo",
       height=5,
       width=9)


  # labels

byDay <- byDay %>%
  mutate(label = ifelse(day==max(day), as.character(country), NA_character_),
         country2 = fct_reorder(country, cases, max))

frq(byDay$label)

?rank
rank(byDay$country)
frq(byDay$country2)

str(byDay)
head(byDay)
tail(byDay)

?distinct
lab <- byDay %>%
  distinct(label) %>%
  na.omit %>%
  unlist

lab

case_ends <- byDay %>%
  group_by(country) %>%
  top_n(1,cases) %>%
  pull(cases)

case_ends


ggplot(byDay, aes(cases, wk_new, color=country)) + 
  #geom_point() + 
  stat_smooth(se=F) +
  scale_color_viridis_d() + 
  scale_x_log10(labels=comma_format(accuracy=2), 
                breaks=c(1,10,100,1000,10000, 100000),
                limits=c(1,250000)) + 
  scale_y_log10(labels=comma) + 
  labs(x="Total confirmed cases",
       y="New confirmed cases\nfor the previous week",
       title="Trajectory of COVID-19 Confirmed Cases\n22 Jan - 1 Apr, 2020") +
  geom_label_repel(aes(label=label)) +
  theme(legend.position="none")

ggsave("viz/covid trajectory legend.png",
       device="png",
       type="cairo",
       height=5,
       width=7)


ggplot(byDay, aes(cases, wk_new, color=country)) + 
  #geom_point() + 
  stat_smooth(se=F) +
  scale_color_viridis_d() + 
  scale_x_log10(labels=comma_format(accuracy=1), 
                breaks=c(1,10,100,1000,10000),
                limits=c(1,150000)) + 
  scale_y_log10(labels=comma) + 
  labs(x="Total confirmed cases",
       y="New confirmed cases\nfor the previous week",
       title="Trajectory of COVID-19 Confirmed Cases\n22 Jan - 1 Apr, 2020")

ggsave("viz/covid trajectory legend.png",
       device="png",
       type="cairo",
       height=5,
       width=7)


  # animate

library(gganimate)

str(byDay)
head(byDay)

p <- ggplot(byDay, aes(cases, wk_new, color=country)) + 
  geom_label(aes(label=country)) + 
  #geom_line() +
  #stat_smooth(se=F, formula = y ~ x) +
  scale_color_viridis_d() + 
  scale_x_log10(labels=comma) + 
  scale_y_log10(labels=comma) + 
  labs(x="Total confirmed cases",
       y="New cases\n(previous week)",
       title="Trajectory of COVID-19 Confirmed Cases\n22 Jan - 1 Apr, 2020") +
  theme(axis.title.y=element_text(angle=0, vjust=.5)) +
  transition_time(day) +
  ease_aes("linear") +
  theme(legend.position="none")

+
  shadow_mark(past=T)

?animate
animate(p,fps=2)

p

anim_save("viz/covid trajectory.gif", p,  width= 1500, height=1000)


ggsave("viz/covid trajectory legend.png",
       device="png",
       type="cairo",
       height=5,
       width=9)




?scale_x_log10
?split

out <- split(byDay, f=list(byDay$country, byDay$week))

new_week <- map(out, function(x) cumsum(x$new_cases)) %>%
  .[7:66]

new_week[31]

out2 <- map_dfr(new_week, as.list)

str(out)
out[66]

  byDay %>%
  group_by(week, country) %>%
  summarize(wk_cases = cumsum(new_cases))
  
cntry <- byDay %>%
  split(country)


out <- split(byDay, byDay$country)


c2 <- lag(byDay$cases)

head(byDay)
tail(byDay)
str(byDay)

describe(byDay$cases)
frq(byDay$country)

byDay <- byDay %>%
  mutate(label = ifelse(day==max(day), as.character(country), NA_character_),
         country2 = fct_reorder(country, cases, max),
         log_cases=ln(cases))

frq(byDay$label)

?rank
rank(byDay$country)
frq(byDay$country2)

str(byDay)
head(byDay)
tail(byDay)

?distinct
lab <- byDay %>%
  distinct(label) %>%
  na.omit %>%
  unlist

lab

case_ends <- byDay %>%
  group_by(country) %>%
  top_n(1,cases) %>%
  pull(cases)

case_ends

tail(byDay)

  # with legend

ggplot(byDay, aes(day, cases, color=fct_rev(country2))) + 
  stat_smooth(size=1, alpha=.2, se=F, span=.1) + 
  scale_color_viridis_d() + 
  scale_x_datetime(limits=c(as.POSIXct("2020-03-01"), as.POSIXct("2020-04-01")),
                   breaks=date_breaks("6 days"),
                   labels=date_format("%b-%d")) +
  scale_y_continuous(limits=c(0,225000),
                     breaks=seq(0,225000,25000),
                     labels=comma) +
  labs(x="",
       y="",
       title="COVID-19 cases for hardest hit countries\n1 March - 1 April, 2020") + 
  spec + 
  theme(legend.title=element_blank(),
        panel.border=element_blank(),
        axis.ticks=element_blank())


ggsave("viz/top country cases 1 April 2020 legend.png",
       device="png",
       type="cairo",
       height=6,
       width=6)

  # with labels

ggplot(byDay, aes(day, cases, color=fct_rev(country2))) + 
  stat_smooth(size=1, alpha=.2, se=F, span=.1) + 
  scale_color_viridis_d() + 
  scale_x_datetime(limits=c(as.POSIXct("2020-03-01"), as.POSIXct("2020-04-02")),
                   breaks=date_breaks("6 days"),
                   labels=date_format("%b-%d")) +
  scale_y_continuous(limits=c(0,235000),
                     breaks=seq(0,225000,25000),
                     labels=comma) +
  labs(x="",
       y="",
       title="COVID-19 cases for hardest hit countries\n25 Feb through 23 March, 2020") +
  theme(legend.title=element_blank()) + 
  geom_label_repel(aes(label=label),
                   #nudge_x=10,
                   direction="both") + 
  spec +
  theme(legend.position="none",
        panel.border = element_blank(),
        axis.ticks=element_blank()) 

ggsave("viz/top country cases 1 April 2020 label.png",
       device="png",
       type="cairo",
       height=6,
       width=6)


  # with secondary y axis

ggplot(byDay, aes(day, cases, color=fct_rev(country2))) + 
  stat_smooth(size=1, alpha=.2, se=F, span=.1) + 
  scale_color_viridis_d() + 
  scale_x_datetime(limits=c(as.POSIXct("2020-03-01"), as.POSIXct("2020-04-01")),
                   breaks=date_breaks("6 days"),
                   labels=date_format("%b-%d")) +
  scale_y_continuous(limits=c(0,225000),
                     breaks=seq(0,225000,25000),
                     labels=comma,
                     sec.axis=sec_axis(~., 
                                       breaks=case_ends,
                                       labels=lab[1:6])) +
  labs(x="",
       y="",
       title="COVID-19 cases for hardest hit countries\nMarch 1 - April 1, 2020") +
  theme(legend.title=element_blank()) + 
  spec +
  theme(legend.position="none",
        panel.border = element_blank(),
        axis.ticks=element_blank()) 


ggsave("viz/top country cases 1 April 2020 sec y.png",
       device="png",
       type="cairo",
       height=9,
       width=7)



  # secondary y axis, log scale

ggplot(byDay, aes(day, cases, color=fct_rev(country2))) + 
  stat_smooth(size=1, alpha=.2, se=F, span=.1, method="lm") + 
  scale_color_viridis_d() + 
  scale_x_datetime(limits=c(as.POSIXct("2020-03-01"), as.POSIXct("2020-04-01")),
                   breaks=date_breaks("6 days"),
                   labels=date_format("%b-%d")) +
  scale_y_log10(labels=comma,
                sec.axis=sec_axis(~.,
                                  breaks=case_ends,
                                  labels=lab[1:6])) +
  labs(x="",
       y="",
       title="COVID-19 cases for hardest hit countries\nMarch 1 - April 1, 2020") +
  theme(legend.title=element_blank()) + 
  spec +
  theme(legend.position="none",
        panel.border = element_blank(),
        axis.ticks=element_blank()) 


ggsave("viz/top country cases 1 April 2020 sec y.png",
       device="png",
       type="cairo",
       height=9,
       width=7)



# US locations ------------------------------------------------------------



# US states ---------------------------------------------------------------

df <- read_csv("http://covidtracking.com/api/states/daily.csv", col_types = cols(date = col_date(format = "%Y%m%d"))) %>%
  mutate(date = as.POSIXct(strptime(date, format="%Y-%m-%d"))) %>%
  as.data.frame

str(df)
 
  mutate(month = lubridate::month(as.Date(date, "%m/%d/%Y"), label = F) # adding these for future analysis
         , day = lubridate::day(date)
  ) 


names(df)
str(df)
head(df)
frq(df$state)
frq(df$state2)

  # DC area series

dc <- df %>%
  filter(state=="DC"| state=="VA") %>%
  group_by(date) %>%
  summarize(positive = sum(positive),
            death=sum(death, na.rm=T)) %>%
  mutate(state="DC area") %>%
  as.data.frame

head(dc)
str(dc)

dat <- df %>%
  filter(state=="WA" | state=="TX" | state=="AZ") %>%
  select(1,3,7,2) %>%
  rbind(dc) %>%
  arrange(desc(death))

dat$death[is.na(dat$death)] <- 0

dat <- dat %>%
  mutate(label = ifelse(date==max(date), as.character(state), NA_character_),
         state2 = fct_reorder(state, positive, max),
         state_death = fct_reorder(state, death, max))

head(dat)  
tail(dat)
frq(dat$state)
frq(dat$state2)
frq(dat$state_death)
str(dat)

lab <- dat %>%
  distinct(label) %>%
  na.omit %>%
  unlist

lab







case_ends <- dat %>%
  group_by(state) %>%
  top_n(1,positive) %>%
  pull(positive)

case_ends

death_ends <- dat %>%
  group_by(state) %>%
  top_n(1,death) %>%
  pull(death)

death_ends

describe(dat)

frq(dat$date)

ggplot(dat, aes(date, positive, color=fct_rev(state2))) + 
  stat_smooth(size=1, alpha=.2, se=F, span=.1) + 
  scale_color_viridis_d() + 
  scale_x_datetime(limits=c(as.POSIXct("2020-03-8"), as.POSIXct("2020-03-27")),
                   breaks=date_breaks("6 days"),
                   labels=date_format("%b-%d")) +
  scale_y_continuous(limits=c(0,3210),
                     breaks=seq(0,3000,500),
                     labels=comma,
                     sec.axis=sec_axis(~., 
                                       breaks=case_ends,
                                       labels=lab)) +
  labs(x="",
       y="",
       title="COVID-19 cases for hardest hit countries\nMarch 8-27, 2020") +
  theme(legend.title=element_blank()) + 
  spec +
  theme(legend.position="none",
        panel.border = element_blank(),
        axis.ticks=element_blank()) 

ggsave("viz/killian states 28 March 2020.png",
       device="png",
       type="cairo",
       height=7,
       width=7)


  # deaths

str(dat)
head(dat)
lab
death_ends

ggplot(dat, aes(date, death, color=fct_rev(state_death))) + 
  stat_smooth(size=1, alpha=.2, se=F, span=.1) + 
  scale_color_viridis_d() + 
  scale_x_datetime(limits=c(as.POSIXct("2020-03-08"), as.POSIXct("2020-03-28")),
                   breaks=date_breaks("5 days"),
                   labels=date_format("%b-%d")) +
  scale_y_continuous(limits=c(0,175),
                     breaks=seq(0,175,25),
                     labels=comma,
                     sec.axis=sec_axis(~., 
                                       breaks=death_ends,
                                       labels=lab)) +
  labs(x="",
       y="",
       title="COVID-19 fatalities for Killian locations\nMarch 8-28, 2020") +
  theme(legend.title=element_blank()) + 
  spec +
  theme(legend.position="none",
        panel.border = element_blank(),
        axis.ticks=element_blank()) 

tail(dat)

ggsave("viz/killian states fatalities 28 March 2020.png",
       device="png",
       type="cairo",
       height=7,
       width=7)






# deaths ------------------------------------------------------------------

str(dths)

DthbyDay <- dths %>%
  pivot_longer(cols=5:ncol(dths),
               names_to="day",
               values_to="deaths") %>% # put date columns into 'day' and number to 'deaths'
  select(-Lat, -Long) %>%
  mutate(date = as.POSIXct(strptime(day, format="%m/%d/%y"))) %>%
  group_by(country=`Country/Region`, date) %>%
  summarize(deaths=sum(deaths)) %>% # in case any sub-national locations, aggregate to country by day
  filter(country == "China" | country == "France" | country == "Germany" |# country == "Iran" | 
           country == "Italy" | country == "Spain" | country == "US") %>% # limit to hardest hit countries
  ungroup() %>% # remove the country grouping
  group_by(country) %>%
  mutate(new_deaths=deaths - lag(deaths, order_by=date), # create a growth variable
         week=isoweek(date)) %>% # generate a week integer 1-52 based on the date in case it helps us create a rolling average by week
  ungroup() %>% # remove the country grouping
  group_by(country, week) %>%
  mutate(week_new = round(mean(new_deaths, na.rm=T),1)) %>% # rolling average by week, so both forward and backward looking window
  ungroup() %>% # remove country and week grouping
  group_by(country) %>%
  arrange(country, date) %>% # 
  mutate(week_new_roll = round(roll_mean(new_deaths, 7, align='right', fill=NA),1)) %>% # a rolling average of the current value plus the six previous days
  ungroup() 


# visual inspection suggests calendar week average and previous week average are about the same 

str(DthbyDay)

# legend

ggplot(DthbyDay, aes(date, week_new_roll, color=country)) + 
  #geom_point() + 
  stat_smooth(se=F) +
  scale_color_viridis_d() + 
  scale_y_log10(labels=comma_format(accuracy=1)) + 
  labs(x="Total confirmed deaths",
       y="New deaths\n(previous week)",
       title="Trajectory of COVID-19 Confirmed Deaths\n22 Jan - 10 Apr, 2020") +
  theme(axis.title.y=element_text(angle=0, vjust=.5)) +
  nobord

ggsave("viz/covid death trajectory legend.png",
       device="png",
       type="cairo",
       height=5,
       width=9)

  # manual table of days since average daily deaths exceed 3

str(DthbyDay)

ave3 <- data.frame(country=c("US", "Spain", "Italy", "Germany", ""),
                   dth3 = c(as.POSIXct("2020-03-13"),
                            as.POSIXct("2020-03-11"),
                            as.POSIXct("2020-02-29"),
                            as.POSIXct("2020-03-17"),
                            as.POSIXct("2020-"))

ave3
