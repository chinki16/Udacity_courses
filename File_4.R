list.files()
Facbook=read.csv('pseudo_facebook.tsv',sep='\t')
#Histogram of users Birthday
library(ggplot2)
names(Facbook)
ggplot(data = Facbook, aes(x = dob_day)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 1:31) +
  facet_wrap(~dob_month)
names(Facbook)
ggplot(data = Facbook,aes(x=friend_count))+geom_histogram(binwidth = 1)
qplot(x=friend_count,data=Facbook,binwidth = 25)+scale_x_continuous(limits = c(0,1000),breaks = seq(0,1000,50))
+facet_wrap(~gender)
table(Facbook$gender)
by(Facbook$friend_count,Facbook$gender,summary)
ggplot(aes(x = tenure), data = Facbook) +
  geom_histogram(binwidth = 30, color = 'black', fill = '#099DD9')

ggplot(aes(x = tenure/365), data = pf) +
  geom_histogram(binwidth = .25, color = 'black', fill = '#F79420')
ggplot(aes(x=age),data=Facbook)+geom_histogram(binwidth = 1,fill = '#5760AB')+scale_x_continuous(breaks = seq(0, 113, 5))
library(gridExtra)
p1=qplot(x=friend_count,data=Facbook)
p2=qplot(x=log10(friend_count),data=Facbook)
p3=qplot(x=sqrt(friend_count),data=Facbook)
grid.arrange(p1,p2,p3,ncol=1)
ggplot(aes(x = friend_count, y = ..count../sum(..count..)),
       data = subset(pf, !is.na(gender))) +
  geom_freqpoly(aes(color = gender), binwidth=10) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  xlab('Friend Count') +
  ylab('Proportion of users with that friend count')
names(Facbook)

qplot(x=www_likes,data = subset(Facbook,!is.na(gender)),geom='freqpoly',col=gender)+scale_alpha_continuous()+scale_x_log10()
by(Facbook$www_likes,Facbook$gender,sum)    

#*****Box Plot*****************
qplot(x=gender,y=friend_count,data = subset(Facbook,!is.na(gender)),geom='boxplot')+ coord_cartesian(ylim = c(0,1000))
