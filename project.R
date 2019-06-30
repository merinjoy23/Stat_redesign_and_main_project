
install.packages("cowplot")
library(ggplot2)

theme_set(theme_bw()))

Elections<-read.csv(file="C:/Users/Merin/2016 Elections.txt",header=TRUE,sep=",")
Elections

chart1<-ggplot(Elections,aes(x=Margin,y=State)) +
  geom_point(shape=21,size=5,aes(fill= ifelse(T==1,"blue","yellow")),color="black")+
  geom_smooth(method=loess, size=1.2, span=0.6) +
  labs(x="Margin of Votes",
       y="States",
       title="Trump Vs Clinton wins")+
scale_x_log10()+
theme(legend.position = "none")
chart1
