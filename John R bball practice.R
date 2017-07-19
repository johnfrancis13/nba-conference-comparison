# Basketball R Practice
# using tidyverse functions as much as possible

setwd("K:/New Reorganization/RAs/John/John R Practice/Raw data")
install.packages("tidyverse")
library(tidyverse)

# read in 25 years of NBA team statistics
# this creates one large table, not quite what i want, but potentially useful
# Tbl <- list.files(pattern="stats.csv") %>% 
# map_df(~read_csv(., col_types = cols(.default = "c")))

# this creates 75 different dfs 
path <- "K:/New Reorganization/RAs/John/John R Practice/Raw data/"
files <- list.files(path=path, pattern="stats.csv")
for(file in files) {
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
  gsub(" ","",substr(file, 1, perpos-1)), 
  read.csv(paste(path,file,sep=""), header= T))
}

# change column names of oppstats-----------------------------------------------
list_1 <- list(`1992oppstats`,`1993oppstats`,`1994oppstats`,`1995oppstats`,
                `1996oppstats`,`1997oppstats`,`1998oppstats`,`1999oppstats`,
                `2000oppstats`,`2001oppstats`,`2002oppstats`,`2003oppstats`,
                `2004oppstats`,`2005oppstats`,`2006oppstats`,`2007oppstats`,
                `2008oppstats`,`2009oppstats`,`2010oppstats`,`2011oppstats`,
                `2012oppstats`,`2013oppstats`,`2014oppstats`,`2015oppstats`,
                `2016oppstats`)


# this works, how to make it for the whole list of df though?
# colnames(`1992oppstats`)[5:25]<-paste("opp", colnames(`1992oppstats`
#   [, c(5:25)]), sep = "_")

# should be able to do this with a loop, but this doesn't do what i want
# for (i in seq_along(df.list)) {
#   colnames((df.list[i])[5:25])<-paste("opp", colnames((df.list[i])[, c(5:25)]),
#   sep = "_")
# }

# this one actually works fot the whole list of dfs
change_opp<-lapply(list_1, function(df) {
  colnames(df)[5:25]<-paste("opp", colnames(df[, c(5:25)]), sep = "_")
  df
})

# add a column for year---------------------------------------------------------
years <- list(1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004
              ,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
change_opp<-Map(cbind, change_opp, year = years)
opp_stats <- do.call("rbind", change_opp)

# repeat for combining misc stats and nbateamstats
# nbateam
list_2 <- list(`1992nbateamstats`,`1993nbateamstats`,`1994nbateamstats`,
               `1995nbateamstats`,`1996nbateamstats`,`1997nbateamstats`,
               `1998nbateamstats`,`1999nbateamstats`,`2000nbateamstats`,
               `2001nbateamstats`,`2002nbateamstats`,`2003nbateamstats`,
               `2004nbateamstats`,`2005nbateamstats`,`2006nbateamstats`,
               `2007nbateamstats`,`2008nbateamstats`,`2009nbateamstats`,
               `2010nbateamstats`,`2011nbateamstats`,`2012nbateamstats`,
               `2013nbateamstats`,`2014nbateamstats`,`2015nbateamstats`,
               `2016nbateamstats`)
change_team<-Map(cbind, list_2, year = years)
team_stats <- do.call("rbind", change_team)

# misc stats
list_3 <- list(`1992miscstats`,`1993miscstats`,`1994miscstats`,`1995miscstats`,
               `1996miscstats`,`1997miscstats`,`1998miscstats`,`1999miscstats`,
               `2000miscstats`,`2001miscstats`,`2002miscstats`,`2003miscstats`,
               `2004miscstats`,`2005miscstats`,`2006miscstats`,`2007miscstats`,
               `2008miscstats`,`2009miscstats`,`2010miscstats`,`2011miscstats`,
               `2012miscstats`,`2013miscstats`,`2014miscstats`,`2015miscstats`,
               `2016miscstats`)
change_misc<-Map(cbind, list_3, year = years)
misc_stats <-do.call("rbind", change_misc)


# merge data frames-------------------------------------------------------------
nba_stats<-full_join(opp_stats, team_stats, by=c("Team", "year"))

all_stats<-full_join(nba_stats, misc_stats, by=c("Team","year"))

### clean up allstats/create a column for conference----------------------------
names(all_stats)
all_stats[,c(1,27:29,51,74)]<-NULL
all_stats<-all_stats[,c(1,25,48:49,2:3,26:46,4:24,47,50:69)]
all_stats<-arrange(all_stats, year, desc(W))

# adjust team names to modern counterparts
all_stats$Team<-gsub("[*]", "", paste(all_stats$Team))
all_stats$Team[all_stats$Team=="New Orleans Hornets"]<-"New Orleans Pelicans"
all_stats$Team[all_stats$Team=="Seattle SuperSonics"]<-"Oklahoma City Thunder"
all_stats$Team[all_stats$Team=="Washington Bullets"]<-"Washington Wizards"
all_stats$Team[all_stats$Team=="New Jersey Nets"]<-"Brooklyn Nets"
all_stats$Team[all_stats$Team=="Vancouver Grizzlies"]<-"Memphis Grizzlies"
all_stats$Team[all_stats$Team=="Charlotte Bobcats"]<-"Charlotte Hornets"
all_stats$Team[all_stats$Team=="New Orleans/Oklahoma City Hornets"]<-
                               "New Orleans Pelicans"

# Not sure how to do this with mutate, i think there should be a way with mutate
# mutate(all_stats, conference=(allstas$team(c("Boston Celtics", 
#                   "Cleveland Cavaliers","Atlanta Hawks","Milwaukee Bucks", 
#                    "Indiana Pacers", "Chicago Bulls", "Miami Heat",
#                    "Detroit Pistons", "Charlotte Hornets", "New York Knicks", 
#                    "Orlando Magic", "Philadelphia 76ers", "Brooklyn Nets"))<-
#                    "East" &
#   all_stats$Team(c("Golden State Warriors", "San Antonio Spurs", "Houston Rockets", 
#                   "Los Angeles Lakers", "Utah Jazz", "Oklahoma City Thunder", 
#                   "Memphis Grizzlies", "Portland Trailblazers", "Denver Nuggets",
#                   "New Orleans Pelicans", "Dallas Mavericks", "Sacremento Kings",
#                   "Minnesota Timberwolves", "Los Angeles Lakers"))<-"West" & 
#   all_stats$Team("League Average")<-NA))

# this works to create a column for conference, likely a better way though
index <- c("Boston Celtics", "Cleveland Cavaliers","Toronto Raptors", 
           "Washington Wizards","Atlanta Hawks", "Milwaukee Bucks", 
           "Indiana Pacers", "Chicago Bulls", "Miami Heat", "Detroit Pistons", 
           "Charlotte Hornets",  "New York Knicks", "Orlando Magic", 
           "Philadelphia 76ers", "Brooklyn Nets","Golden State Warriors", 
           "San Antonio Spurs", "Houston Rockets",  "Los Angeles Clippers", 
           "Utah Jazz", "Oklahoma City Thunder", "Memphis Grizzlies", 
           "Portland Trail Blazers", "Denver Nuggets","New Orleans Pelicans", 
           "Dallas Mavericks", "Sacramento Kings","Minnesota Timberwolves", 
           "Los Angeles Lakers","Phoenix Suns", "League Average")
values <- c("East","East","East","East","East","East","East","East","East","East",
            "East","East","East","East","East","West","West","West","West","West",
            "West","West","West","West","West","West","West","West","West","West",
            NA)
all_stats$conference <- values[match(all_stats$Team, index)]

# Figure out how you want to compare conferences over time----------------------
# Strength Factor= (win percentage)+(effective fg%-opp effective fg%)+(strength
# of schedule)+(offensive rating/defensive rating); average of top 50% in each
# conference by wins for each year

all_stats$sf<-(all_stats$W/(all_stats$W+all_stats$L))+(all_stats$eFG.-
               all_stats$eFG..1)+(all_stats$ORtg/all_stats$DRtg)+all_stats$SOS

# Make some nice visuals--------------------------------------------------------
upper_half<-all_stats %>% 
  group_by(year, conference) %>% 
  filter(percent_rank(W) >= .46)

upper_avg<-upper_half %>%
  group_by(conference, year) %>%
  summarise_all(funs(mean(., na.rm=TRUE)))

conf_compare<-upper_avg %>% 
  select(conference, year, sf) %>% 
  arrange((year))

ggplot(conf_compare, aes(x=year, y=sf, color=conference)) + 
  geom_point() + 
  geom_line() +
  labs(title = "NBA Conference Strength 1992-2016") +
  scale_x_continuous(limits = c(1992,2016), breaks = c(1992,1996,2000,2004,
                                                       2008,2012,2016)) +
  scale_y_continuous(limits = c(0, 3))


