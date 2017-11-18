library(ggplot2)
#Adding average point in the data set 
ggplot(aes(x=gender,y=age),data=subset(pseudo_facebook,!is.na(gender)))+geom_boxplot()+stat_summary(fun.y=mean,geom
                                                                                                    ='point',shape=4)
#Adding average median point in the data set 
ggplot(aes(x=gender,y=age),data=subset(pseudo_facebook,!is.na(gender)))+geom_boxplot()+stat_summary(fun.y=median,geom
                                                                                                    ='point',shape=4)
library(dplyr)
age_gender_group=group_by(pseudo_facebook,age,gender)
pf.fc_by_age_gender= summarise(age_gender_group,mean_friend_count=mean(as.numeric(friend_count)), median_friend_count
                               =median(as.numeric(friend_count)),n=n())
 pf.fc_by_age_gender
 
 # Plot for 
 ggplot(aes(x=age,y=mean_friend_count),data=subset(pf.fc_by_age_gender,!is.na(gender)))+geom_line(aes(color=gender))
   
 #Reshape data
 library(reshape2)
pf.fc_by_age_gender.wide=dcast(pf.fc_by_age_gender,age~gender,value.var = 'median_friend_count') 
head(pf.fc_by_age_gender.wide,5)
pf.fc_by_age_gender.wide$ratio=pf.fc_by_age_gender.wide$female/pf.fc_by_age_gender.wide$male
head(pf.fc_by_age_gender.wide,5)
ggplot(aes(x=age,y=ratio),data=pf.fc_by_age_gender.wide)+geom_line()+geom_hline(yintercept = 1,alpha=0.3,linetype=2)

#year of join
pseudo_facebook$yearjoin=floor(2014-pseudo_facebook$tenure/365)

summary(pseudo_facebook$yearjoin)
table(pseudo_facebook$yearjoin)
#Divid data into years
pseudo_facebook$year_join=cut(pseudo_facebook$yearjoin,breaks = c(2004,2009,2011,2012,2014))
pseudo_facebook$year_join


ggplot(aes(x=age,y=friend_count),data=subset(pseudo_facebook,!is.na(year_join)))+geom_line(aes(color=year_join))


ggplot(aes(x=age,y=friend_count),data=subset(pseudo_facebook,!is.na(yearjoin)))+geom_line(aes(color=yearjoin))+stat_summary(fun.y=median,geom
                                                                                                                                       ='point',shape=4)
with(subset(pseudo_facebook,tenure>=1),summary(friend_count/tenure))

ggplot(aes(x=tenure,y=friendships_initiated/tenure),data=subset(pseudo_facebook,tenure>=1))+geom_line(aes(color=year_join))
ggplot(aes(x = 7 * round(tenure / 7), y = friendships_initiated / tenure),
       data = subset(pseudo_facebook, tenure > 0)) +
  geom_smooth(aes(color = year_join))


#/***********Yogurt Data*******************/
  summary(yogurt)
  str(yogurt)
yogurt$id=factor(yogurt$id)
ggplot(aes(price),data=yogurt)+geom_histogram(binwidth = 0.5)
length(unique(yogurt$price))

yogurt$all=transform(yogurt,all_pur=strawberry+blueberry+pina.colada+plain+mixed.berry)
str(yogurt$time)
ggplot(aes(x=price,y=time),data=yogurt)+geom_point()

set.seed(4230)
yogurt$id=factor(yogurt$id)
sample_ids=sample(levels(yogurt$id),16)
ggplot(aes(x=time,y=price),data=subset(yogurt,id %in% sample_ids))+facet_wrap(~id)+geom_line()+geom_point(aes(size=all.all.pur),pch=1)
