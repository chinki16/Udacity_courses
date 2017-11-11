library(ggplot2)
qplot(x=age,y=friend_count,data=pseudo_facebook)
data=pseudo_facebook
ggplot(aes(x=age,y=friend_count),data=data)+geom_point(alpha=1/20)+xlim(13,90)
ggplot(aes(x=age,y=friend_count),data=data)+geom_point(alpha=1/20)+xlim(13,90)+coord_trans(y='sqrt')
ggplot(aes(x=age,y=friendships_initiated),data=data)+geom_point(alpha=1/10,position = position_jitter(h=0))+xlim(13,90)+coord_trans(y='sqrt')
#Gitter to remove negative value 
library(dplyr)
age_group=group_by(data,age)
summary_age_group=summarise(age_group,friend_count_mean=mean(friend_count),friend_count_median=median(friend_count),n=n())
head(summary_age_group)
summary_age_group=arrange(summary_age_group,age)
ggplot(aes(x=age,y=friend_count_mean),data=summary_age_group)+geom_point()
ggplot(aes(x=age,y=friend_count_mean),data=summary_age_group)+geom_line()

ggplot(aes(x=age,y=friendships_initiated),data=data)+geom_point(alpha=1/10,position = position_jitter(h=0),col='green')+xlim(13,90)+coord_trans(y='sqrt')+geom_line(stat='summary',fun.y=mean)+geom_line(stat='summary',fun.y=quantile,probs=.1,col='blue',linetype=2)
cor(data$age,data$friend_count)
round(cor(data$age,data$friend_count),3)
with(subset(data,age<=70),cor.test(data$age,data$friend_count))
ggplot(aes(x=likes_received,y=www_likes_received),data=data)+geom_point()+xlim(0,quantile(data$likes_received,.95)+ylim(0,quantile(data$www_likes_received,0.95)))+geom_smooth(method = 'lm',col='red')

with(data,cor.test(data$www_likes,data$www_likes_received))
cor.test(data$www_likes_received,data$www_likes)
age_in_month=data$age*12
data$age_with_months <- data$age + (1 - data$dob_month / 12)
data$age_with_months <- with(data, age + (1 - dob_month / 12))
age_group_with_month=group_by(data,age_with_months)
summary_age_group_with_month=summarise(age_group_with_month,friend_count_mean=mean(friend_count),friend_count_median=median(friend_count),n=n())
head(summary_age_group)
summary_age_group_with_month=arrange(summary_age_group_with_month,age_with_months)
head(summary_age_group_with_month)

p1=ggplot(aes(x=age_with_months,y=friend_count_mean),data=subset(summary_age_group_with_month,age_with_months>71))+geom_line()

p2=ggplot(aes(x=age,y=friend_count_mean),data=subset(summary_age_group,age>71))+geom_line()

library(gridExtra)

grid.arrange(p2,p1,ncol=1)



#------------------------------------------------------------------------------
library(alr3)
library(car)
?Mitchell
ggplot(aes(x=Month,y=Temp),data=Mitchell)+geom_point()+scale_x_discrete(breaks=seq(0,203,11))
cor(Mitchell$Month,Mitchell$Temp)

