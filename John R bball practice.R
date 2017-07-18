### Basketball R Practice
### using tidyverse functions as much as possible

install.packages("tidyverse")


setwd("K:/New Reorganization/RAs/John/John R Practice/Raw data")

### read in 25 years of statistics
library(tidyverse)
### creates one large table
# Tbl <- list.files(pattern="stats.csv") %>% 
#  map_df(~read_csv(., col_types = cols(.default = "c")))

### creates 75 different dfs 
path <- "K:/New Reorganization/RAs/John/John R Practice/Raw data/"
files <- list.files(path=path, pattern="stats.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep=""), header= T))
}

nba20161<-read.csv("2016 nba team stats.csv")

### change column names of opp to differentiate those columns
df.list <- list(`1992oppstats`,`1993oppstats`,`1994oppstats`,`1995oppstats`,`1996oppstats`,`1997oppstats`,
                `1998oppstats`,`1999oppstats`,`2000oppstats`,`2001oppstats`,`2002oppstats`,`2003oppstats`,
                `2004oppstats`,`2005oppstats`,`2006oppstats`,`2007oppstats`,`2008oppstats`,`2009oppstats`,
                `2010oppstats`,`2011oppstats`,`2012oppstats`,`2013oppstats`,`2014oppstats`,`2015oppstats`,`2016oppstats`)


### this works, how to make it for the whole list of df though?
#colnames(`1992oppstats`)[5:25]<-paste("opp", colnames(`1992oppstats`[, c(5:25)]), sep = "_")

#for (i in seq_along(df.list)) {
#  colnames((df.list[i])[5:25])<-paste("opp", colnames((df.list[i])[, c(5:25)]), sep = "_")
#}
#hmm<- function(x){colnames(x)[5:25]<-paste("opp", colnames((x)[, c(5:25)]), sep = "_")}
#lapply(df.list, hmm(names(df.list)))

###### this one actually works
data<-lapply(df.list, function(df) {
  colnames(df)[5:25]<-paste("opp", colnames(df[, c(5:25)]), sep = "_")
  df
})

###### add a column for year
years <- list(1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004
              ,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
data1<-Map(cbind, data, year = years)
oppstats <- do.call("rbind", data1)

## repeat for combining misc stats and nbateamstats
## nbateam
df.list2 <- list(`1992nbateamstats`,`1993nbateamstats`,`1994nbateamstats`,`1995nbateamstats`,`1996nbateamstats`,`1997nbateamstats`,
                `1998nbateamstats`,`1999nbateamstats`,`2000nbateamstats`,`2001nbateamstats`,`2002nbateamstats`,`2003nbateamstats`,
                `2004nbateamstats`,`2005nbateamstats`,`2006nbateamstats`,`2007nbateamstats`,`2008nbateamstats`,`2009nbateamstats`,
                `2010nbateamstats`,`2011nbateamstats`,`2012nbateamstats`,`2013nbateamstats`,`2014nbateamstats`,`2015nbateamstats`,`2016nbateamstats`)
data2<-Map(cbind, df.list2, year = years)
teamstats <- do.call("rbind", data2)

## misc stats
df.list3 <- list(`1992miscstats`,`1993miscstats`,`1994miscstats`,`1995miscstats`,`1996miscstats`,`1997miscstats`,
                `1998miscstats`,`1999miscstats`,`2000miscstats`,`2001miscstats`,`2002miscstats`,`2003miscstats`,
                `2004miscstats`,`2005miscstats`,`2006miscstats`,`2007miscstats`,`2008miscstats`,`2009miscstats`,
                `2010miscstats`,`2011miscstats`,`2012miscstats`,`2013miscstats`,`2014miscstats`,`2015miscstats`,`2016miscstats`)
data3<-Map(cbind, df.list3, year = years)
miscstats <- do.call("rbind", data3)


### merge data frames
nbastats<-full_join(oppstats, teamstats, by=c("Team", "year"))

allstats<-full_join(nbastats,miscstats, by=c("Team","year"))

### clean up allstats/create a column for conference
names(allstats)
allstats[,c(1,27:29,51,74)]<-NULL
allstats<-allstats[,c(1,25,48:49,2:3,26:46,4:24,47,50:69)]
allstats<-arrange(allstats, year, desc(W))

### create a column for conference and also adjust team names to modern counterparts
allstats$Team<-gsub("[*]", "", paste(allstats$Team))
allstats$Team[allstats$Team == "New Orleans Hornets"] <-"New Orleans Pelicans"
allstats$Team[allstats$Team == "Seattle SuperSonics"] <-"Oklahoma City Thunder"
allstats$Team[allstats$Team == "Washington Bullets"] <-"Washington Wizards"
allstats$Team[allstats$Team == "New Jersey Nets"] <-"Brooklyn Nets"
allstats$Team[allstats$Team == "Vancouver Grizzlies"] <-"Memphis Grizzlies"
allstats$Team[allstats$Team == "Charlotte Bobcats"] <-"Charlotte Hornets"
allstats$Team[allstats$Team == "New Orleans/Oklahoma City Hornets"] <-"New Orleans Pelicans"

### Not sure how to do this with mutate
###mutate(allstats, conference=(allstas$team(c("Boston Celtics", "Cleveland Cavaliers",
#                                         "Atlanta Hawks", "Milwaukee Bucks", 
#                                          "Indiana Pacers", "Chicago Bulls", "Miami Heat",
#                                          "Detroit Pistons", "Charlotte Hornets", 
#                                          "New York Knicks", "Orlando Magic", 
#                                          "Philadelphia 76ers", "Brooklyn Nets"))<-"East" &
#         allstats$Team(c("Golden State Warriors", "San Antonio Spurs", "Houston Rockets", 
#                       "Los Angeles Lakers", "Utah Jazz", "Oklahoma City Thunder", 
#                       "Memphis Grizzlies", "Portland Trailblazers", "Denver Nuggets",
#                       "New Orleans Pelicans", "Dallas Mavericks", "Sacremento Kings",
#                       "Minnesota Timberwolves", "Los Angeles Lakers"))<-"West" & 
#         allstats$Team("League Average")<-NA))

### this works to create a column for conference, likely a better way though
index <- c("Boston Celtics", "Cleveland Cavaliers",
           "Toronto Raptors", "Washington Wizards",
           "Atlanta Hawks", "Milwaukee Bucks", 
           "Indiana Pacers", "Chicago Bulls", "Miami Heat",
           "Detroit Pistons", "Charlotte Hornets", 
           "New York Knicks", "Orlando Magic", 
           "Philadelphia 76ers", "Brooklyn Nets","Golden State Warriors", "San Antonio Spurs", "Houston Rockets", 
           "Los Angeles Clippers", "Utah Jazz", "Oklahoma City Thunder", 
           "Memphis Grizzlies", "Portland Trail Blazers", "Denver Nuggets",
           "New Orleans Pelicans", "Dallas Mavericks", "Sacramento Kings",
           "Minnesota Timberwolves", "Los Angeles Lakers","Phoenix Suns", "League Average")
values <- c("East","East","East","East","East","East","East","East","East","East",
            "East","East","East","East","East","West","West","West","West","West",
            "West","West","West","West","West","West","West","West","West","West",
            NA)
allstats$conference <- values[match(allstats$Team, index)]

### Figure out how you want to compare conferences over time
# Strength Factor= (win percentage)+(effective fg%-opp effective fg%)+(strength
# of schedule)+(offensive rating/defensive rating); average of top 8 in each
# conference by wins for each year

allstats$sf<-(allstats$W/(allstats$W+allstats$L))+(allstats$eFG.-allstats$eFG..1)+
  (allstats$ORtg/allstats$DRtg)+allstats$SOS

### Make some nice visuals
# going to need new data set Conference, Year, Strength Factor
conference = rep(c("East" , "West"),25 )
year = rep(1992:2016, each=2)
sf = sample(NA,50, replace =TRUE)
confcomparison<-data.frame(conference, year, sf) 

lp<-allstats %>% group_by(year, conference) %>% filter(percent_rank(W) >= .54)

lpp<-lp %>%
  group_by(conference, year) %>%
  summarise_each(funs(mean(., na.rm=TRUE)))

confcomparison<-lpp %>% select(conference, year, sf)
confcomparison<-confcomparison %>% arrange((year))

ggplot(confcomparison, aes(x=year, y=sf, color=conference)) + 
  geom_point() + 
  geom_line() +
  labs(title = "NBA Conference Strength 1992-2016") +
  scale_x_continuous(limits = c(1992,2016), breaks = c(1992,1996,2000,2004,
                                                       2008,2012,2016)) +
  scale_y_continuous(limits = c(0, 3))


