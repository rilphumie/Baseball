# Baseball
#This is an exploratory analysis based on baseball. This project is on what makes a team the best.
# Written in R Language

load("/Users/rilphumie/Downloads/gls2016.RData")
load("/Users/rilphumie/Downloads/pls2016.RData")
load("gls2016.RData")
class(pls2016)
length(pls2016)
length(gls2016)
class(pls2016[[1]])
str(pls2016[[1]])
names(pls2016[[1]])
head(pls2016[[1]]$leagues)
str(pls2016[[1]]$leagues)
head(pls2016[[1]]$playing_positions)
str(pls2016[[1]]$playing_positions)
head(pls2016[[1]]$teams)
str(pls2016[[1]]$teams)
head(pls2016[[1]]$players)
str(pls2016[[1]]$players)
players2016 <-  data.frame()
playing_positions <- data.frame()
teams <- data.frame()
leagues <- data.frame()

for (i in 1:length(pls2016)) {
  players2016 <- rbind(players2016, pls2016[[i]]$players)
  playing_positions <- rbind(playing_positions, pls2016[[i]]$playing_positions)
  teams <- rbind(teams, pls2016[[i]]$teams)
  leagues <- rbind(leagues, pls2016[[i]]$leagues) 
players2016 <- unique(players2016)
playing_positions <- unique(playing_positions)
teams <- unique(teams)
leagues <- unique(leagues)

}

dim(teams)


class(gls2016)
length(gls2016)
class(gls2016[[1]])
str(gls2016[[1]])
names(gls2016[[1]])
head(gls2016[[1]]$leagues)
str(gls2016[[1]]$leagues)
head(gls2016[[1]]$games)
str(gls2016[[1]]$games)
head(gls2016[[1]]$home_teams)
str(gls2016[[1]]$home_teams)
head(gls2016[[1]]$away_teams)
str(gls2016[[1]]$away_teams)
head(gls2016[[1]]$winning_teams)
str(gls2016[[1]]$winning_teams)
head(gls2016[[1]]$seasons)
str(gls2016[[1]]$seasons)
head(gls2016[[1]]$venues)
str(gls2016[[1]]$venues)
head(gls2016[[1]]$officials)
str(gls2016[[1]]$officials)
head(gls2016[[1]]$players)
str(gls2016[[1]]$players)
head(gls2016[[1]]$teams)
str(gls2016[[1]]$teams)
head(gls2016[[1]]$opponents)
str(gls2016[[1]]$opponents)
head(gls2016[[1]]$game_logs)
str(gls2016[[1]]$game_logs)


game2016 <-  data.frame()
home_teams <- data.frame()
teams <- data.frame()
leagues <- data.frame()
games <- data.frame()
away_teams <-  data.frame()
winning_teams <-  data.frame()
seasons <-  data.frame()
venues <-  data.frame()
officials <-  data.frame()
players <-  data.frame()
opponents <-  data.frame()
game_logs <-  data.frame()


for (i in 1:length(gls2016)) {
  print(i)
  game2016 <- rbind(game2016, gls2016[[i]]$games)
  home_teams <- rbind(home_teams, gls2016[[i]]$home_teams)
  teams <- rbind(teams, gls2016[[i]]$teams)
  leagues <- rbind(leagues, gls2016[[i]]$leagues)
  away_teams <- rbind(away_teams, gls2016[[i]]$away_teams)
  winning_teams <- rbind(winning_teams, gls2016[[i]]$winning_teams)
  seasons <- rbind(seasons, gls2016[[i]]$seasons)
  venues <- rbind(venues, gls2016[[i]]$venues)
  officials <- rbind(officials, gls2016[[i]]$officials)
  players <- rbind(players, gls2016[[i]]$players)
  opponents <- rbind(opponents, gls2016[[i]]$opponents)
  game_logs <- rbind(game_logs, gls2016[[i]]$game_logs)
  
  game2016 <- unique(game2016)
  home_teams <- unique(home_teams)
  teams <- unique(teams)
  leagues <- unique(leagues)
  away_teams <- unique(away_teams)
  winning_teams <- unique(winning_teams)
  seasons <- unique(seasons)
  venues <- unique(venues)
  officials <- unique(officials)
  players <- unique(players)
  opponents <- unique(opponents)
  game_logs <- unique(game_logs)
  
}length(playing_positions)
  


colnames(playing_positions)
colnames(winning_teams)
wining <- merge(winning_teams, playing_positions, by.x = "league_id", by.y = "id", all = TRUE)  
colnames(wining) 
colnames(wining) <- c("league_id", "id","created_at.x", "updated_at.x", "color",       
 "colors", "hashtag", "hashtags","location","team_name", "nickname","latitude", "longitude","slug", "division_id", 
 "created_at.y", "updated_at.y", "abbreviation", "description", "formation","position_name", "league_id" )  


gaming <- merge(game_logs, playing_positions, by.x = "id", by.y = "created_at", all = TRUE)  
head(gaming)

# winning percentage
winLoss = aggregate(game_logs[,c("wins","losses")],
                    list(game_logs$team_id),
                    sum,
                    na.rm = TRUE)
names(winLoss)[1] = "team_id"
winLoss$team_name = teams$name[match(winLoss$team_id,teams$id)]
rownames(winLoss) = winLoss$team_name
View(winLoss)
winLoss$win_percent <- winLoss$wins / (winLoss$wins + winLoss$losses)
head(winLoss)

# Plate Appearance
PA_calc = aggregate(game_logs[,c("at_bats","hit_by_pitch","sacrifice_hits","sacrifice_flys","catcher_interferences")],
               list(game_logs$team_id),
               sum,
               na.rm = TRUE)
View(PA_calc)
PA_calc$PA <- rowSums(PA_calc[,2:6])
PA_Final_Metrics <- merge(Final_Metrics, PA_calc, by.x = "team_id.x", by.y = "Group.1" )
PA_Final_Metrics$PASO <- PA_Final_Metrics$PA / PA_Final_Metrics$strikeouts
View(PA_Final_Metrics)
which.max(PA_Final_Metrics$PASO)


sort(PA_Final_Metrics$PASO, decreasing = TRUE)

order_by(PA_Final_Metrics, PASO)
plot_max_metrics <- head(arrange(PA_Final_Metrics, desc(PASO)))


ggplot(PA_Final_Metrics,aes(Group.1, PASO, fill = Group.1)) + 
  geom_bar(stat = "identity") + coord_flip()
hist(PA_Final_Metrics$PASO)


# remove all unwanted rows which is national league
no_national_league <- winLoss[winLoss$team_name != "National League",]
# remove american league
new_winLoss <- no_national_league[no_national_league$team_name != "American League",]
View(new_winLoss)

new_winLoss$strikes <- (game_logs$strikeouts / game_logs$walks )

# This is to remove all zero from my dataframe
na_stikeouts <- na.omit(game_logs$strikeouts)
na_stikeouts1 <- na_stikeouts[na_stikeouts != 0]
na_walks <- na.omit(game_logs$walks)
na_walks1 <- na_walks[na_walks != 0]
strikes <- na_stikeouts / na_walks

zero_null <- subset(game_logs,strikeouts != 0 | walks != 0)
dim(zero_null)


which.max(players$salary)
(players$id)[1298]
players[players$id == "6b8c16f5-369e-4127-b1b6-3ccf411ef700",]

colnames(game_logs)
class("shutouts")
game_logs$s




# this is to groupby 
zero_null %>% subset(select = c(strikeouts, walks))  %>%
  group_by(zero_null$team_id)  %>%
  summarise(strike_walk = sum(zero_null$strikesouts, na.rm = TRUE))

total_strikes <- aggregate(game_logs[,c("strikeouts","walks")],
          list(game_logs$team_id),
          sum,
          na.rm = TRUE)

names(total_strikes)[1] = "team_id"
total_strikes$team_name = teams$name[match(total_strikes$team_id,teams$id)]
rownames(total_strikes) = total_strikes$team_name
View(total_strikes)

# getting the ratio of total strikeouts by walk
total_strikes$ratio <- total_strikes$strikeouts / total_strikes$walks
cor(total_strikes$ratio, new_winLoss$win_percent)


game_logs$on_base_percentage
qqnorm(Final_Metrics$win_percent)
qqline(Final_Metrics$win_percent)


merged_winLoss_ratio <- inner_join(new_winLoss, total_strikes, by = "team_name")
View(merged_winLoss_ratio)

# checking the correlation between winpercent and strikeout ratio
merged_winLoss_ratio$team_name
cor(merged_winLoss_ratio$win_percent, merged_winLoss_ratio$ratio)
summary(lm(merged_winLoss_ratio$win_percent ~ merged_winLoss_ratio$ratio))
# plotting for win % and ratio
plot(lm(merged_winLoss_ratio$win_percent ~ merged_winLoss_ratio$ratio))
summary(lm(merged_winLoss_ratio$win_percent ~ merged_winLoss_ratio$ratio + Final_Metrics$Slug_percent + Final_Metrics$home_runs ))
plot(lm(merged_winLoss_ratio$win_percent ~ merged_winLoss_ratio$ratio + Final_Metrics$Slug_percent + Final_Metrics$home_runs ))


merged_run_win <- merge(game_logs, new_winLoss, by = "team_id")
head(merged_run_win)
dim(merged_run_win)
mp <- na.omit(merged_run_win)
run_win <- cor(mp$home_runs,mp$win_percent)
summary(lm(mp$home_runs ~ mp$win_percent))
abline(lm(mp$home_runs ~ mp$win_percent))
plot(mp$home_runs ~ mp$win_percent)

############# homeruns vs win percent
homerun <- aggregate(merged_run_win[,c("home_runs")],
           list(merged_run_win$team_name),
           sum, na.rm = TRUE)
metrics_df <- merge(homerun, merged_winLoss_ratio, by.x = "Group.1", by.y = "team_name" )
metrics_df$Group.1
############# Remember to change x.y to slug  %
Slug_per$Group.1
Final_Metrics <- merge(metrics_df, Slug_per, by = "Group.1"  )


colnames(Final_Metrics)
colnames(Final_Metrics) <- c("Group.1", "home_runs", "team_id.x", "wins", "losses", "win_percent",
                             "team_id.y", "strikeouts", "walks", "ratio", "Slug_percent" )       
 head(Final_Metrics)                          

plot(Final_Metrics$win_percent, Final_Metrics$Slug_percent)
plot(lm(Final_Metrics$win_percent ~ Final_Metrics$Slug_percent))
summary(lm(Final_Metrics$win_percent~ Final_Metrics$Slug_percent))
# concludes that slugging percentage is not a strong indicator for winning percentage
summary(lm(Final_Metrics$win_percent~ Final_Metrics$home_runs))
# it is a stronger indicator than the slugging percentage for win_percent
plot(lm(Final_Metrics$win_percent ~ Final_Metrics$home_runs))
cor(Final_Metrics$win_percent, Final_Metrics$home_runs)
cor(Final_Metrics$win_percent, Final_Metrics$Slug_percent)
plot(lm(Final_Metrics$win_percent~ Final_Metrics$home_runs))

barplot(Final_Metrics$win_percent, Final_Metrics$home_runs)
barplot(as.matrix(Group.1, ))
barplot(t(as.matrix(Final_Metrics[,c("Group.1", "home_runs")])))
barplot(as.matrix(Final_Metrics[,c("Group.1", "home_runs")]))
class(Final_Metrics)


library("ggplot2")
ggplot(Final_Metrics,aes(Group.1, home_runs, fill = Group.1)) + 
  geom_bar(stat = "identity") + coord_flip()
as.data.frame(Final_Metrics$home_runs, Final_Metrics$Group.1)

ggplot(Final_Metrics,aes(Group.1, win_percent, fill = Group.1)) + 
  geom_bar(stat = "identity") + coord_flip()


sort(Final_Metrics$win_percent, decreasing = TRUE)

?order_by

order_by(Final_Metrics, win_percent)
plot_max_metrics <- head(arrange(Final_Metrics, desc(win_percent)))

ggplot(plot_max_metrics,aes(Group.1, win_percent, fill = Group.1)) + 
  geom_bar(stat = "identity")




library(dplyr)
library(tidyr)
install.packages("tidyr")

winning_teams
game_logs
unique(na.omit(players2016$draft_team_name))
(na.omit(game_logs$team_id,game_logs$wins, game_logs$losses))
#i will need to merge players with gamelogs so i can get teamnames 
  
summary(gaming$batting_average) # it is rounding up to 1
hist(gaming$batting_average) # account for all players that played once by using at_bats
gaming$away_team_outcome # this has to be high
winorloss <- gaming[gaming$team_outcome == "win",]
batting <- gaming$batting_average
mean(cbind(winorloss, batting))

winteam = wining[wining$team_name == "Texas",]


#check top ten batting average and did they win
#merge gamelogs and winning teams
#linear1 = lm(Mortality ~ log_NOx, data = mortality_lab)
hits = gaming$extra_base_hits
bats = gaming$batting_average
mon = lm(bats ~ hits, data = gaming)
abline(plot(mon))


plot(gaming$batting_average, gaming$at_bats)
plot(lm(batting_average ~ at_bats, data = gaming))
abline(plot(gaming$extra_base_hits, gaming$batting_average))

saving = lm(game_logs$slugging_percentage ~ game_logs$saves)
summary(saving)
abline(plot(saving))
#why is cor giving me na
game_logs$runs
plot(game_logs$on_base_percentage, game_logs$runs)
plot(lm(game_logs$runs ~ game_logs$on_base_percentage))
abline(lm(game_logs$runs ~ game_logs$on_base_percentage))


cor(game_logs$runs, game_logs$on_base_percentage)
head(gaming)   

run = na.omit(game_logs$runs)
sl = na.omit(game_logs$slugging_percentage)
obg = na.omit(game_logs$on_base_percentage)
cor(sl, obg) 
cor(sl, run)
cor(obg, run)
bat_ave = na.omit(game_logs$batting_average)
cor(bat_ave,run)
per = na.omit(game_logs$pitcher_runs)
cor(per,run)
abline(plot(lm(run ~ sl)))
max(obg) # this rounds up the values why
max(run) #check the index of max runs compared to slg or obp



which.max(merged_winLoss_ratio$win_percent)
table(merged_winLoss_ratio$win_percent)[21] # whats the difference between this and below
(merged_winLoss_ratio$team_name)[21] #correct

players_info = merge(game_logs, players2016, by.x = "player_id", by.y = "id")
head(players_info)
head(players)
dim(players_info)
colnames(players2016)
colnames(teams)
colnames(game_logs)
team_info <- merge(teams, game_logs, by.x = "id", by.y = "team_id") 
head(team_info)
head(teams)
Washingthon <- na.omit(team_info[team_info$name == "Washington",])
head(Washingthon)


cor(Washingthon$home_runs, Washingthon$wins)
sum(Washingthon$slugging_percentage) # Washington has the best slugging percentage compared to other teams
sum(na.omit(game_logs$slugging_percentage))
# how to plot washington slug% compared to others




which.min(merged_winLoss_ratio$win_percent)
table(merged_winLoss_ratio$win_percent)[25] # whats the difference between this and below
(merged_winLoss_ratio$team_name)[25]
top_n(merged_winLoss_ratio$win_percent)
?top_n
df <- data.frame(c(merged_winLoss_ratio$win_percent))
df %<% top_n(5)


atlanta <- na.omit(team_info[team_info$name == "Atlanta",])
head(atlanta)
cor(atlanta$home_runs, atlanta$wins)
max(atlanta$slugging_percentage) # Washington has the best slugging percentage compared to other teams
max(na.omit(game_logs$slugging_percentage))
max(atlanta$whip)
max(Washingthon$whip)
max(na.omit(game_logs$whip))
max(na.omit(game_logs$pitcher_hits))
max(na.omit(Washingthon$pitcher_hits))
max(na.omit(atlanta$at_bats))

max(na.omit(game_logs$pitcher_stolen_bases))
max(na.omit(Washingthon$pitcher_stolen_bases)) 
max(na.omit(atlanta$pitcher_stolen_bases))

game_logs$slugging_percentage


ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point()

ggplot(Washingthon, aes(hwy, colour = class)) + 
  geom_point()
?ggplot2
ggplot(data = Washingthon) +
  geom_point(mapping = aes(x = on_base_percentage, y = slugging_percentage)) +
  facet_wrap(~ wins)
plot(merged_winLoss_ratio$strikeouts, merged_winLoss_ratio$win_percent)
hist(Final_Metrics$win_percent)
hist(Final_Metrics$ratio)
plot(merged_winLoss_ratio$win_percent, merged_winLoss_ratio$strikeouts)
?barplot
Washingthon$win

Slug_per1 <- tapply(team_info$name, team_info$slugging_percentage, sum)
length(Slug_per1)
length(unique(team_info$name))



Slug_per <- aggregate(team_info[,c("slugging_percentage")],
          list(team_info$name), mean,
          na.rm = TRUE)
class(Slug_per$x)

plot(Slug_per$x, Slug_per$Group.1)
barplot(table(Slug_per))


# vistalization for win %
#slug%
# atlanta vs Washingthon
# at_bats, pitcher hits and whip for both teams
# scatter plot to find winning % and slu% 
#corfor home runs and winning %

install.packages("Hmisc")
library(Hmisc)
rcorr(as.matrix(Washingthon))

cor(Washingthon, method = c("pearson", "kendall", "spearman"))
install.packages("corrplot")
library(corrplot)
M <- data.frame(Washingthon$at_bats, Washingthon$batting_average, Washingthon$home_runs,Washingthon$on_base_percentage, Washingthon$strikeouts, Washingthon$wins, Washingthon$hits, Washingthon$hit_by_pitch, Washingthon$walks, Washingthon$whip, Washingthon$losses)
head(M)
MA <- cor(M)
head(round(MA,2))
corrplot(MA)
corrplot(MA, type = "upper" )
game_logs$


