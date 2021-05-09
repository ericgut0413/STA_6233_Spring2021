install.packages("reshape")
#Bring In Libraries
library(rvest)
library(dplyr)
library(plyr)
library(ggplot2)

#The 2012 regular season was shortened from the normal 82 games per team to 66 due to the lockout
#The Nets were "NJN" before 2012 and "BRK" from the 2012-present seasons
#The NBA canceled the game between the Boston Celtics and Indiana Pacers on April 16, 2013 in light 
#of the bombings during the Boston Marathon 
#New Orleans is "NOH" before 2014 and "NOP" from the 2014-present seasons
#Charlotte is "CHA" from before 2015 and "CHO" from the 2015-present seasons
#### First Grab all Teams and the Years####
teams<-c("ATL", "BOS", "NJN", "CHI", "CHA", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM",
         "MIA", "MIL", "MIN", "NOH", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")

#Creating empty dataset to be filled with loop with every team's home game stats
all_games<-data.frame(Result=character())  
#Years<-2015:2019
 
#Creating loop to gather information from the all the regular season from 2015-2019
#for (i in 1:length(Years)){
for(j in 1:length(teams)){
  #Website to scrapms
  #theurl<-paste0("http://www.basketball-reference.com/teams/",teams[j],"/2019/gamelog/")
  theurl<-paste0("http://www.basketball-reference.com/teams/",teams[j],"/2011/gamelog/")
  
  #theurl<-paste0("http://www.basketball-reference.com/teams/",teams[j],"/",Years[i],"/gamelog/") 
  
  #Download/Read the html
  html<- read_html(theurl)
  
  
  #I use CSS selector to figure out what table to read
  games<-html_nodes(html,"#tgl_basic") 
  
  #Make previous object into a table
  table_g<-html_table(games) 
  
  #Keep only desired stats in this table
  stats<-data.frame(table_g[[1]][[4]], table_g[[1]][[6]], table_g[[1]][[7]], table_g[[1]][[8]], 
                    table_g[[1]][[9]], table_g[[1]][[10]], table_g[[1]][[26]], table_g[[1]][[27]],
                    table_g[[1]][[29]], table_g[[1]][[30]], table_g[[1]][[32]], table_g[[1]][[33]],
                    table_g[[1]][[40]])
  stats<-stats %>% dplyr::rename(Place=1, Result=2, Points=3, Opp_Points=4, FG_Made=5, FG_Att=6, 
                                 Opp_FGM=7, Opp_FGA=8, Opp_3PM= 9, Opp_3PA=10, Opp_FTM=11, Opp_FTA=12,
                                 TOV=13)
  stats<-stats[-1, ]
  
  #Get the home game
  stats<-stats[stats$Place=="",]
  
  #Remove rows that are not games but columns
  stats<-stats[stats$Result!="W/L",]
  stats<-stats[stats$Result!="",]
  
  #Add the team to label where each home game was played
 
  #Bind all games from every team
  all_games<-rbind(all_games, stats)
  }
}

  #Check how many home games played in the season (should be 41 per team and 1230 total for a full regular season)
  nrow(all_games)
  
  #Summary statistics for the season
  #Check home winning percentage for the season 
  all_games$wins<-ifelse(all_games$Result=="W", 1, 0) 
  Win_Per<-sum(all_games$wins) / nrow(all_games)
  
  #Differential in points scored versus opponents points scored per game for the season
  all_games$Points<-as.numeric(all_games$Points)
  all_games$Opp_Points<-as.numeric(all_games$Opp_Points)
  PT_DIF= sum(all_games$Points) - sum(all_games$Opp_Points)
  PT_DIF_PG = PT_DIF / nrow(all_games)
  
  #Field Goal percentage at home for the season
  all_games$FG_Made<-as.numeric(all_games$FG_Made)
  all_games$FG_Att<-as.numeric(all_games$FG_Att)
  FG_Percent= sum(all_games$FG_Made) / sum(all_games$FG_Att)
  
  #Opponents field goal percentage away for the season
  all_games$Opp_FGM<-as.numeric(all_games$Opp_FGM)
  all_games$Opp_FGA<-as.numeric(all_games$Opp_FGA)
  Opp_FG_Percent= sum(all_games$Opp_FGM) / sum(all_games$Opp_FGA)
  
  #Opponents three point field goal percentage away for the season
  all_games$Opp_3PM<-as.numeric(all_games$Opp_3PM)
  all_games$Opp_3PA<-as.numeric(all_games$Opp_3PA)
  Opp_3P_Percent= sum(all_games$Opp_3PM) / sum(all_games$Opp_3PA)
  
  #Opponents free throw percentage away for the season
  all_games$Opp_FTM<-as.numeric(all_games$Opp_FTM)
  all_games$Opp_FTA<-as.numeric(all_games$Opp_FTA)
  Opp_FT_Percent= sum(all_games$Opp_FTM) / sum(all_games$Opp_FTA)
  
  #Opponents turnovers per game away for the season
  all_games$TOV<-as.numeric(all_games$TOV)
  Opp_TOV_PG<-sum(all_games$TOV) / nrow(all_games)
  
  #Summary statistics of the different seasons
  #The year "2015" represents the seasons from 2015-2019

require(ggplot2)


season_stats<-data.frame(Season=c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021),
                         Games_Played=c(1230, 990, 1229, 1230, 1230, 1230, 1230, 1230, 1230, 978),
                         Winning_Percent=c(60.41, 58.59, 61.19, 58.05, 57.48, 58.86, 58.37, 57.89, 59.27, 53.37),
                         Point_Differential_PG=c(3.17, 2.82, 3.23, 2.60, 2.41, 2.67, 3.15, 2.11, 2.72, 0.74),
                         Field_Goal_Percent=c(46.53, 45.35, 45.95, 46.08, 45.38, 45.68, 46.33, 46.51, 46.57, 46.70),
                         Opponent_FG_Percent=c(45.19, 44.24, 44.60, 44.80, 44.41, 44.74, 45.11, 45.54, 45.53, 46.46),
                         Opponent_3P_Percent=c(35.50, 34.68, 34.96, 35.81, 34.50, 34.98, 34.81, 36.28, 35.14, 36.45),
                         Opponent_FT_Percent=c(76.28, 75.25, 75.50, 75.25, 74.92, 75.47, 77.26, 76.67, 76.56, 77.73), 
                         Opponent_TOV_PG=c(13.84, 14.09, 14.20, 14.06, 13.79, 13.99, 13.46, 13.73, 13.56, 13.19), stringsAsFactors=F)

#Line Graph for Home Team Winning Percentage Per Season
ggplot(season_stats, aes(Season, Winning_Percent)) + ggtitle("Home Team Winning Percentage") + geom_point(color="blue") + 
xlab("Season") + ylab("Percent (%)") + geom_line(color="blue") + scale_y_continuous()

#Line Graph for Home Team Point Differential Per Game
ggplot(season_stats, aes(Season, Point_Differential_PG)) + ggtitle("Home Team Point Differential Per Game") + 
geom_point(color="red") + xlab("Season") + ylab("Points") + geom_line(color="red") + scale_y_continuous()
.
#Line Graph for Home Team Field Goal Percent Per Season
ggplot(season_stats, aes(Season, Field_Goal_Percent)) + ggtitle("Home Team Field Goal Percentage") + 
geom_point(color="green") + xlab("Season") + ylab("Percent (%)") + geom_line(color="green") + scale_y_continuous()

#Line Graph for Away Team Field Goal Percent Per Season
ggplot(season_stats, aes(Season, Opponent_FG_Percent)) + ggtitle("Away Team Field Goal Percentage") + 
geom_point(color="black") + xlab("Season") + ylab("Percent (%)") + geom_line(color="black") + scale_y_continuous()

#Line Graph for Away Team Free Throw Percent Per season
ggplot(season_stats, aes(Season, Opponent_FT_Percent)) + ggtitle("Away Team Free Throw Percentage") + 
geom_point(color="purple") + xlab("Season") + ylab("Percent (%)") + geom_line(color="purple") + scale_y_continuous()

#Line Graph for Away Team Three Point Percent Per Season
ggplot(season_stats, aes(Season, Opponent_3P_Percent)) + ggtitle("Away Team Three Point Percentage") + 
geom_point(color="red") + xlab("Season") + ylab("Percent (%)") + geom_line(color="red") + scale_y_continuous()

#Line Graph for Away Team Turnovers Per Game Per Season
ggplot(season_stats, aes(Season, Opponent_TOV_PG)) + ggtitle("Away Team Turnovers Per Game") + 
geom_point(color="blue") + xlab("Season") + ylab("Turnovers") + geom_line(color="blue") + scale_y_continuous()

t<-read.csv("C:/Users/ecgut/R/Project2/NBA-Fan-Impact.csv", stringsAsFactors = F)
str(t)

save(t, file="C:/Users/ecgut/R/Project2/NBA-Fan-Impact.RData")
str(t)
