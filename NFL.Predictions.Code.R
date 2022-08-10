
library(ggplot2)
library(ggrepel)
library(ggimage)
library(nflreadr)
library(tidyverse)
library(DT)
options(scipen = 9999)

## Explore data:
## Read in play-by-play data for 2011 season:
season.2011<-load_pbp(2011)
names(season.2011)
season.2011<-season.2011[season.2011$rush==1|season.2011$pass==1,]
season.2011<-season.2011[season.2011$season_type=='REG',]
season.2011<-season.2011[!is.na(season.2011$epa),]
# Dataframe of completion percentage for short passes in 2011:
season.2011.short.pass<-season.2011[season.2011$air_yards>=0&season.2011$air_yards<10,]
season.2011.short.pass<-season.2011.short.pass[is.na(season.2011.short.pass$play_id)!=T,]
short.pass.cp.2011<-season.2011.short.pass %>% filter(!is.na(cp)) %>% group_by(passer) %>% filter(n()>100) %>%
  summarize(team=first(posteam), short.cp=mean(cp),short.epa=mean(epa)) %>% arrange(-short.cp)
# Dataframe of completion percentage for medium passes in 2011:
season.2011.medium.pass<-season.2011[season.2011$air_yards>=10&season.2011$air_yards<20,]
season.2011.medium.pass<-season.2011.medium.pass[is.na(season.2011.medium.pass$play_id)!=T,]
medium.pass.cp.2011<-season.2011.medium.pass %>% filter(!is.na(cp)) %>% group_by(passer) %>% filter(n()>50) %>%
  summarize(team=first(posteam), medium.cp=mean(cp),medium.epa=mean(epa)) %>% arrange(-medium.cp)
# Dataframe of completion percentage for long passes in 2011:
season.2011.long.pass<-season.2011[season.2011$air_yards>=20,]
season.2011.long.pass<-season.2011.long.pass[is.na(season.2011.long.pass$play_id)!=T,]
long.pass.cp.2011<-season.2011.long.pass %>% filter(!is.na(cp)) %>% group_by(passer) %>% filter(n()>25) %>%
  summarize(team=first(posteam), long.cp=mean(cp),long.epa=mean(epa)) %>% arrange(-long.cp)
# Plot of completion percentage vs EPA for short passes in 2011:
short.pass.cp.2011<-left_join(short.pass.cp.2011,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
short.cp.plot<-ggplot(data=short.pass.cp.2011,aes(x=short.cp,y=short.epa))+
  geom_image(aes(image=team_logo_wikipedia))+geom_text_repel(aes(label=passer))+
  geom_hline(yintercept = mean(short.pass.cp.2011$short.epa), color = "blue", linetype = "dashed", alpha=0.6)+
  geom_vline(xintercept = mean(short.pass.cp.2011$short.cp),color='blue',linetype='dashed',alpha=0.6)+
  stat_smooth(geom='line', alpha=0.75, se=FALSE, method='lm')+
  labs(title = 'Quarterback Completion Percentage vs  EPA on Short Passses (2011)',x='Completion Percentage',
       y=' Expected Points Added (EPA)')
short.cp.plot
# Plot of completion percentage vs EPA for medium passes in 2011:
medium.pass.cp.2011<-left_join(medium.pass.cp.2011,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
medium.cp.plot<-ggplot(data=medium.pass.cp.2011,aes(x=medium.cp,y=medium.epa))+
  geom_image(aes(image=team_logo_wikipedia))+geom_text_repel(aes(label=passer))+
  geom_hline(yintercept = mean(medium.pass.cp.2011$medium.epa), color = "blue", linetype = "dashed", alpha=0.6)+
  geom_vline(xintercept = mean(medium.pass.cp.2011$medium.cp),color='blue',linetype='dashed',alpha=0.6)+
  stat_smooth(geom='line', alpha=0.75, se=FALSE, method='lm')+
  labs(title = 'Quarterback Completion Percentage vs  EPA on Medium Passses (2011)',x='Completion Percentage',
       y=' Expected Points Added (EPA)')
medium.cp.plot
# Plot of completion percentage vs EPA for medium passes in 2011:
long.pass.cp.2011<-left_join(long.pass.cp.2011,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
long.cp.plot<-ggplot(data=long.pass.cp.2011,aes(x=long.cp,y=long.epa))+
  geom_image(aes(image=team_logo_wikipedia))+geom_text_repel(aes(label=passer))+
  geom_hline(yintercept = mean(long.pass.cp.2011$long.epa), color = "blue", linetype = "dashed", alpha=0.6)+
  geom_vline(xintercept = mean(long.pass.cp.2011$long.cp),color='blue',linetype='dashed',alpha=0.6)+
  stat_smooth(geom='line', alpha=0.75, se=FALSE, method='lm')+
  labs(title = 'Quarterback Completion Percentage vs  EPA on Long Passses (2011)',x='Completion Percentage',
       y=' Expected Points Added (EPA)')
long.cp.plot

## Creating the same plots for the 2021 NFL Season
season.2021<-load_pbp(2021)
season.2021<-season.2021[season.2021$rush==1|season.2021$pass==1,]
season.2021<-season.2021[season.2021$season_type=='REG',]
season.2021<-season.2021[!is.na(season.2021$epa),]
# Dataframe of completion percentage for short passes in 2021:
season.2021.short.pass<-season.2021[season.2021$air_yards>=0&season.2021$air_yards<10,]
season.2021.short.pass<-season.2021.short.pass[is.na(season.2021.short.pass$play_id)!=T,]
short.pass.cp.2021<-season.2021.short.pass %>% filter(!is.na(cp)) %>% group_by(passer) %>% filter(n()>100) %>%
  summarize(team=first(posteam), short.cp=mean(cp),short.epa=mean(epa)) %>% arrange(-short.cp)
# Dataframe of completion percentage for medium passes in 2021:
season.2021.medium.pass<-season.2021[season.2021$air_yards>=10&season.2021$air_yards<20,]
season.2021.medium.pass<-season.2021.medium.pass[is.na(season.2021.medium.pass$play_id)!=T,]
medium.pass.cp.2021<-season.2021.medium.pass %>% filter(!is.na(cp)) %>% group_by(passer) %>% filter(n()>50) %>%
  summarize(team=first(posteam), medium.cp=mean(cp),medium.epa=mean(epa)) %>% arrange(-medium.cp)
# Dataframe of completion percentage for long passes in 2021:
season.2021.long.pass<-season.2021[season.2021$air_yards>=20,]
season.2021.long.pass<-season.2021.long.pass[is.na(season.2021.long.pass$play_id)!=T,]
long.pass.cp.2021<-season.2021.long.pass %>% filter(!is.na(cp)) %>% group_by(passer) %>% filter(n()>25) %>%
  summarize(team=first(posteam), long.cp=mean(cp),long.epa=mean(epa)) %>% arrange(-long.cp)
# Plot of completion percentage vs EPA for short passes in 2021:
short.pass.cp.2021<-left_join(short.pass.cp.2021,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
short.cp.plot.2021<-ggplot(data=short.pass.cp.2021,aes(x=short.cp,y=short.epa))+
  geom_image(aes(image=team_logo_wikipedia))+geom_text_repel(aes(label=passer))+
  geom_hline(yintercept = mean(short.pass.cp.2021$short.epa), color = "blue", linetype = "dashed", alpha=0.6)+
  geom_vline(xintercept = mean(short.pass.cp.2021$short.cp),color='blue',linetype='dashed',alpha=0.6)+
  stat_smooth(geom='line', alpha=0.75, se=FALSE, method='lm')+
  labs(title = 'Quarterback Completion Percentage vs  EPA on Short Passses (2021)',x='Completion Percentage',
       y=' Expected Points Added (EPA)')
short.cp.plot.2021
# Plot of completion percentage vs EPA for medium passes in 2021:
medium.pass.cp.2021<-left_join(medium.pass.cp.2021,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
medium.cp.plot.2021<-ggplot(data=medium.pass.cp.2021,aes(x=medium.cp,y=medium.epa))+
  geom_image(aes(image=team_logo_wikipedia))+geom_text_repel(aes(label=passer))+
  geom_hline(yintercept = mean(medium.pass.cp.2021$medium.epa), color = "blue", linetype = "dashed", alpha=0.6)+
  geom_vline(xintercept = mean(medium.pass.cp.2021$medium.cp),color='blue',linetype='dashed',alpha=0.6)+
  stat_smooth(geom='line', alpha=0.75, se=FALSE, method='lm')+
  labs(title = 'Quarterback Completion Percentage vs  EPA on Medium Passses (2021)',x='Completion Percentage',
       y=' Expected Points Added (EPA)')
medium.cp.plot.2021
# Plot of completion percentage vs EPA for medium passes in 2021:
long.pass.cp.2021<-left_join(long.pass.cp.2021,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
long.cp.plot.2021<-ggplot(data=long.pass.cp.2021,aes(x=long.cp,y=long.epa))+
  geom_image(aes(image=team_logo_wikipedia))+geom_text_repel(aes(label=passer))+
  geom_hline(yintercept = mean(long.pass.cp.2021$long.epa), color = "blue", linetype = "dashed", alpha=0.6)+
  geom_vline(xintercept = mean(long.pass.cp.2021$long.cp),color='blue',linetype='dashed',alpha=0.6)+
  stat_smooth(geom='line', alpha=0.75, se=FALSE, method='lm')+
  labs(title = 'Quarterback Completion Percentage vs  EPA on Long Passses (2021)',x='Completion Percentage',
       y=' Expected Points Added (EPA)')
long.cp.plot.2021

## Analyzing running backs in 2011:
run.plays.2011<-season.2011[season.2011$rush==1,]
run.plays.ypc.2011<-run.plays.2011 %>% filter(!is.na(yards_gained)) %>% group_by(rusher) %>% filter(n()>125) %>%
  summarize(team=first(posteam),ypc=mean(yards_gained),epa=mean(epa)) %>% arrange(-ypc)
# Plot of yards per carry vs EPA for running backs in 2011:
run.plays.ypc.2011<-left_join(run.plays.ypc.2011,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
run.ypc.plot.2011<-ggplot(data=run.plays.ypc.2011,aes(x=ypc,y=epa))+
  geom_image(aes(image=team_logo_wikipedia))+geom_text_repel(aes(label=rusher))+
  geom_hline(yintercept = mean(run.plays.ypc.2011$epa), color = "blue", linetype = "dashed", alpha=0.6)+
  geom_vline(xintercept = mean(run.plays.ypc.2011$ypc),color='blue',linetype='dashed',alpha=0.6)+
  stat_smooth(geom='line', alpha=0.75, se=FALSE, method='lm')+
  labs(title = 'Yards Per Carry vs  EPA (2011)',x='Yards Per Carry',
       y=' Expected Points Added (EPA)')
run.ypc.plot.2011

## Analyzing running backs in 2021:
run.plays.2021<-season.2021[season.2021$rush==1,]
run.plays.ypc.2021<-run.plays.2021 %>% filter(!is.na(yards_gained)) %>% group_by(rusher) %>% filter(n()>140) %>%
  summarize(team=first(posteam),ypc=mean(yards_gained),epa=mean(epa)) %>% arrange(-ypc)
# Plot of yards per carry vs EPA for running backs in 2021:
run.plays.ypc.2021<-left_join(run.plays.ypc.2021,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
run.ypc.plot.2021<-ggplot(data=run.plays.ypc.2021,aes(x=ypc,y=epa))+
  geom_image(aes(image=team_logo_wikipedia))+geom_text_repel(aes(label=rusher))+
  geom_hline(yintercept = mean(run.plays.ypc.2021$epa), color = "blue", linetype = "dashed", alpha=0.6)+
  geom_vline(xintercept = mean(run.plays.ypc.2021$ypc),color='blue',linetype='dashed',alpha=0.6)+
  stat_smooth(geom='line', alpha=0.75, se=FALSE, method='lm')+
  labs(title = 'Yards Per Carry vs  EPA (2021)',x='Yards Per Carry',
       y=' Expected Points Added (EPA)')
run.ypc.plot.2021

## Analyzing receivers in 2011:
pass.plays.2011<-season.2011[season.2011$pass==1,]
receivers.ypr.2011<-pass.plays.2011 %>% filter(!is.na(yards_gained), !is.na(receiver)) %>% group_by(receiver) %>% 
  filter(n()>100) %>% summarize(team=first(posteam),ypr=mean(yards_gained),epa=mean(epa)) %>% arrange(-ypr)
receivers.ypr.2011<-left_join(receivers.ypr.2011,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
receivers.ypr.plot.2011<-ggplot(data=receivers.ypr.2011,aes(x=ypr,y=epa))+
  geom_image(aes(image=team_logo_wikipedia))+geom_text_repel(aes(label=receiver))+
  geom_hline(yintercept = mean(receivers.ypr.2011$epa), color = "blue", linetype = "dashed", alpha=0.6)+
  geom_vline(xintercept = mean(receivers.ypr.2011$ypr),color='blue',linetype='dashed',alpha=0.6)+
  stat_smooth(geom='line', alpha=0.75, se=FALSE, method='lm')+
  labs(title = 'Yards Per Reception vs  EPA (2011)',x='Yards Per Reception',
       y=' Expected Points Added (EPA)')
receivers.ypr.plot.2011

## Analyzing receivers in 2021:
pass.plays.2021<-season.2021[season.2021$pass==1,]
receivers.ypr.2021<-pass.plays.2021 %>% filter(!is.na(yards_gained), !is.na(receiver)) %>% group_by(receiver) %>% 
  filter(n()>100) %>% summarize(team=first(posteam),ypr=mean(yards_gained),epa=mean(epa)) %>% arrange(-ypr)
receivers.ypr.2021<-left_join(receivers.ypr.2021,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
receivers.ypr.plot.2021<-ggplot(data=receivers.ypr.2021,aes(x=ypr,y=epa))+
  geom_image(aes(image=team_logo_wikipedia))+geom_text_repel(aes(label=receiver))+
  geom_hline(yintercept = mean(receivers.ypr.2021$epa), color = "blue", linetype = "dashed", alpha=0.6)+
  geom_vline(xintercept = mean(receivers.ypr.2021$ypr),color='blue',linetype='dashed',alpha=0.6)+
  stat_smooth(geom='line', alpha=0.75, se=FALSE, method='lm')+
  labs(title = 'Yards Per Reception vs  EPA (2021)',x='Yards Per Reception',
       y=' Expected Points Added (EPA)')
receivers.ypr.plot.2021

## Analyzing top 20 quarterbacks in terms of EPA from 2000 - 2021:
seasons.2000.2021<-load_pbp(2000:2021)
seasons.2000.2021<-seasons.2000.2021[seasons.2000.2021$rush==1|seasons.2000.2021$pass==1,]
seasons.2000.2021<-seasons.2000.2021[seasons.2000.2021$season_type=='REG',]
seasons.2000.2021<-seasons.2000.2021[!is.na(seasons.2000.2021$epa),]
pass.plays.2000.2021<-seasons.2000.2021[seasons.2000.2021$pass==1,]
qb.cp.2000.2021<-pass.plays.2000.2021 %>% filter(!is.na(cp)) %>% group_by(passer) %>% filter(n()>1000) %>%
  summarize(team=last(posteam),cp=mean(cp),epa=mean(epa)) %>% arrange(-epa)
qb.cp.2000.2021<-left_join(qb.cp.2000.2021,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
qb.cp.plot.2000.2021<-ggplot(data=qb.cp.2000.2021[1:20,],aes(x=cp,y=epa))+
  geom_image(aes(image=team_logo_wikipedia))+geom_text_repel(aes(label=passer))+
  geom_hline(yintercept = mean(qb.cp.2000.2021[1:20,]$epa), color = "blue", linetype = "dashed", alpha=0.6)+
  geom_vline(xintercept = mean(qb.cp.2000.2021[1:20,]$cp),color='blue',linetype='dashed',alpha=0.6)+
  stat_smooth(geom='line', alpha=0.75, se=FALSE, method='lm')+
  labs(title = 'Top 20 QBs in Terms of EPA vs Completion Percentage Between 2000 and 2021',
       x='Completion Percentage', y=' Expected Points Added (EPA)')
qb.cp.plot.2000.2021

## Analyzing top 20 running backs in terms of EPA from 2000 - 2021:
rush.plays.2000.2021<-seasons.2000.2021[seasons.2000.2021$rush==1,]
rb.ypc.2000.2021<-rush.plays.2000.2021 %>% filter(!is.na(yards_gained)) %>% group_by(rusher) %>% filter(n()>300) %>%
  summarize(team=first(posteam),ypc=mean(yards_gained),epa=mean(epa)) %>% arrange(-epa)
rb.ypc.2000.2021<-left_join(rb.ypc.2000.2021,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
rb.ypc.plot.2000.2021<-ggplot(data=rb.ypc.2000.2021[1:20,],aes(x=ypc,y=epa))+
  geom_image(aes(image=team_logo_wikipedia))+geom_text_repel(aes(label=rusher))+
  geom_hline(yintercept = mean(rb.ypc.2000.2021[1:20,]$epa), color = "blue", linetype = "dashed", alpha=0.6)+
  geom_vline(xintercept = mean(rb.ypc.2000.2021[1:20,]$ypc),color='blue',linetype='dashed',alpha=0.6)+
  stat_smooth(geom='line', alpha=0.75, se=FALSE, method='lm')+
  labs(title = 'Top 20 Rushers in Terms of EPA vs Yards per Carry Between 2000 and 2021',
       x='Yards per Carry', y=' Expected Points Added (EPA)')
rb.ypc.plot.2000.2021

## Analyzing top 20 receivers in terms of EPA from 2000 - 2021:
wr.ypr.2000.2021<-pass.plays.2000.2021 %>% filter(!is.na(yards_gained)) %>% group_by(receiver) %>% filter(n()>200) %>%
  summarize(team=first(posteam),ypr=mean(yards_gained),epa=mean(epa)) %>% arrange(-epa)
wr.ypr.2000.2021<-left_join(wr.ypr.2000.2021,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
wr.ypr.plot.2000.2021<-ggplot(data=wr.ypr.2000.2021[1:20,],aes(x=ypr,y=epa))+
  geom_image(aes(image=team_logo_wikipedia))+geom_text_repel(aes(label=receiver))+
  geom_hline(yintercept = mean(wr.ypr.2000.2021[1:20,]$epa), color = "blue", linetype = "dashed", alpha=0.6)+
  geom_vline(xintercept = mean(wr.ypr.2000.2021[1:20,]$ypr),color='blue',linetype='dashed',alpha=0.6)+
  stat_smooth(geom='line', alpha=0.75, se=FALSE, method='lm')+
  labs(title = 'Top 20 Receivers in Terms of EPA vs Yards per Reception Between 2000 and 2021',
       x='Yards per Reception', y=' Expected Points Added (EPA)')
wr.ypr.plot.2000.2021

## Bar graph of TD to INT ratio and EPA for top 10 QBs between 2000 and 2021:
td.int.all<-seasons.2000.2021 %>% filter(pass==1,!is.na(touchdown),!is.na(interception)) %>% group_by(passer) %>% filter(n()>500) %>% 
  summarize(team=first(posteam),td.int=sum(touchdown)/sum(interception),epa=mean(epa)) %>% arrange(-td.int)
td.int.all<-left_join(td.int.all,nflfastR::teams_colors_logos,by=c('team'='team_abbr'))
top.10<-td.int.all[1:10,]
td.int.plot<-ggplot(data=top.10,aes(x=passer,y=td.int,fill=team_color2,label=round(td.int,2)))+geom_bar(stat='identity',alpha=0.75)+
 scale_fill_identity() + geom_image(aes(image=team_logo_wikipedia))+geom_text(vjust=12)+
  labs(title='Top 10 Touchdown-Interception Ratios for QBs between 2000 and 2021',
       x='Quarterback',y='Touchdown-Interception Ratio')
td.int.plot

## Plot of RBs with 250+ carries and PPG:
RB.carries<-seasons.2000.2021 %>% filter(rush==1) %>% group_by(season,rusher) %>% summarize(rush.attempts=n())
RB.carries.2<-RB.carries %>% group_by(season) %>% summarize(power.run=sum(rush.attempts>300))
ppg<-seasons.2000.2021 %>% group_by(season,game_id,home_team) %>% summarize(ppg=last(home_score)+last(away_score))%>%
  group_by(season) %>% summarize(avg.ppg=sum(ppg)/(2*n()))
carries.and.ppg<-left_join(RB.carries.2,ppg,by='season')
carries.vs.ppg<-ggplot(data=carries.and.ppg)+geom_bar(stat='identity',aes(x=season,y=power.run),fill='#3366ff',alpha=0.6) +
  geom_line(stat='identity',aes(x=season,y=0.4*avg.ppg),size=2,color='#CC0000',alpha=0.8)+
  scale_y_continuous(breaks=c(0,3,6,9,12,15), sec.axis=sec_axis(~.*2.5,name="Average Points Per Game"))+
  theme(axis.line.y.right = element_line(color = "#CC0000"), 
        axis.ticks.y.right = element_line(color = "#CC0000"),
        axis.text.y.right = element_text(color = "#CC0000"), 
        axis.title.y.right = element_text(color = "#CC0000")) +
  labs(title='Trends in Power Run Game vs Points Per Game Between 2000 and 2021',
       y='Players With More Than 300 Rushing Attempts',x='Season')
carries.vs.ppg


## Create regression model to predict wins in 2021:
# Offensive and defensive EPA for each team and season:
pbp.2000.2020<-load_pbp(2000:2021)
pbp.2000.2020<-pbp.2000.2020[pbp.2000.2020$rush==1|pbp.2000.2020$pass==1,]
pbp.2000.2020<-pbp.2000.2020[pbp.2000.2020$season_type=='REG',]
pbp.2000.2020<-pbp.2000.2020[!is.na(pbp.2000.2020$epa)&!is.na(pbp.2000.2020$posteam)&pbp.2000.2020$posteam != "",]
pass.epa<-pbp.2000.2020 %>% filter(pass==1) %>% group_by(season,posteam) %>% summarize(pass.epa=mean(epa))
colnames(pass.epa)<-c('season','team','pass.epa')
pass.epa$season<-as.factor(pass.epa$season)
rush.epa<-pbp.2000.2020 %>% filter(rush==1,!is.na(epa)) %>% group_by(season,posteam) %>% summarize(rush.epa=mean(epa))
colnames(rush.epa)<-c('season','team','rush.epa')
rush.epa$season<-as.factor(rush.epa$season)
def.pass.epa<-pbp.2000.2020 %>% filter(pass==1) %>% group_by(season,defteam) %>% summarize(def.pass.epa=mean(epa))
colnames(def.pass.epa)<-c('season','team','def.pass.epa')
def.pass.epa$season<-as.factor(def.pass.epa$season)
def.rush.epa<-pbp.2000.2020 %>% filter(rush==1,!is.na(epa)) %>% group_by(season,defteam) %>% summarize(def.rush.epa=mean(epa))
colnames(def.rush.epa)<-c('season','team','def.rush.epa')
def.rush.epa$season<-as.factor(def.rush.epa$season)
# Win total for each team:
games <- nflreadr::load_schedules()
home <- games %>%filter(game_type == 'REG') %>%select(season, week, home_team, result) %>%rename(team = home_team)
away <- games %>%filter(game_type == 'REG') %>%select(season, week, away_team, result) %>%rename(team = away_team) %>%mutate(result = -result)  
results <- bind_rows(home, away) %>%arrange(week) %>%mutate(win = case_when(result > 0 ~ 1,result < 0 ~ 0,result == 0 ~ 0.5))
win.totals <- results %>%group_by(team, season) %>%summarize(wins = sum(win))
win.totals$season<-as.factor(win.totals$season)
# Merge into one dataframe:
df.list<-list(win.totals,pass.epa,rush.epa,def.pass.epa,def.rush.epa)
wins.and.epa<-df.list %>% reduce(full_join, by=c('season','team'))
wins.and.epa <- wins.and.epa %>% 
  arrange(team, season) %>% group_by(team) %>% 
  mutate(prev.pass.epa = lag(pass.epa),prev.rush.epa = lag(rush.epa),prev.def.pass.epa = lag(def.pass.epa),prev.def.rush.epa = lag(def.rush.epa)) 
# Create regression model to predict wins based on previous season EPA:
wins.and.epa$season<-as.vector(wins.and.epa$season)
wins.and.epa$season<-as.numeric(wins.and.epa$season)
train.data<-wins.and.epa %>% filter(season<2021&season>1999)
win.mod<-lm(wins~prev.pass.epa+prev.rush.epa+prev.def.pass.epa+prev.def.rush.epa,data=train.data)
summary(win.mod)  
# Predicting 2021 season:
prediction<-predict(win.mod,wins.and.epa%>%filter(season==2021))%>%
as_tibble() %>% rename(wins.prediction = value) %>%round(1) %>%bind_cols(wins.and.epa %>% filter(season == 2021) %>% select(team))
wins.2021<-wins.and.epa %>% filter(season==2021) %>% summarize(actual.wins=wins)
prediction.vs.actual<-left_join(prediction,wins.2021,by='team')
prediction.vs.actual<-prediction.vs.actual[,c(2,1,3)]
colnames(prediction.vs.actual)<-c('Team','Predicted 2021 Wins','Actual 2021 Wins')
datatable(prediction.vs.actual)

