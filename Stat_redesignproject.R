--------------------------------------------------
#Title: 2016 Presidential General Election Results"
#Submitted by: Merin Joy and Chandrika Amarkhed
#Date: 03/26/2018
--------------------------------------------------
  
#Setup using the libraries.
  
library(ggplot2)
library(tidyverse)
library(micromapST)
--------------------------------------------------
#Read the csv file.

Elections<-read.csv(file="C:/Users/Merin/2016 Elections2.txt",header=TRUE,sep=",")
Elections
head(Elections)
--------------------------------------------------
#Rename the columns.

colnames(Elections)<- c("State","Trump EV","Clinton EV","Other EV","Total Vote","C",
                        "T","J","Margin","Margin%","Clinton%","Trump%","Johnson%","Other%",
                        "Clinton","Trump","Johnson","Other")
head(Elections)
--------------------------------------------------
#Checking for any NA's.

sum(is.na(Elections))
---------------------------------------------------
#rowTheme
  rowTheme <- theme_gray()+ theme(
    plot.title=element_text(hjust=0.5),
    plot.subtitle=element_text(hjust=0.5),
    plot.caption=element_text(hjust=-.5),
    strip.text.y = element_blank(),
    strip.background=element_rect(fill=rgb(.9,.95,1),
                                  colour=gray(.5), size=.2),
    panel.border=element_rect(fill=FALSE,colour=gray(.75)),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.spacing.x = unit(0.07,"cm"),
    panel.spacing.y = unit(0.07,"cm"),
    axis.ticks=element_blank(),
    axis.text=element_text(colour="black"),
    axis.text.y=element_text(size=rel(.78),
                             margin=margin(0,0,0,3)),
    axis.text.x=element_text(margin=margin(-1,0,3,0))
  )

--------------------------------------------------
#Ploting a Lollipop chart.

ggplot(air,aes(x=air$SEX,y=air$default.payment.next.month))+geom_point(shape=21,size=5, color="black")+
  labs(x="Margin of Victory",
       y="States",
       title="Trump Vs Clinton Wins")+rowTheme
--------------------------------------------------
#Order the rows and columns.
  
Electionsorder<- with(Elections,order(Elections$Trump,decreasing = TRUE))
colorder <- c(1,16,15,17,18,2:14)
colorder
Electionsnew<-Elections[Electionsorder,colorder]
--------------------------------------------------
#Create blocks for ploting rows in the y-axis.

blocks <-as.character(Electionsnew$State)
Electionsnew$State <-factor(blocks,levels=rev(blocks))
levels(Electionsnew$State)
--------------------------------------------------
#Adding groups.
groups <- paste("G",1:11,sep="")
groups
size <- c(5,5,5,5,5,1,5,5,5,5,5)
Electionsnew$Group <- factor(rep(groups, size),level=groups)
Electionsnew$Group
--------------------------------------------------
#Adding row numbers for each group.
order <- rep(1:5,5)
order
seq <- c(order,1,order)
seq
label <- c('1', '2',
          '3', '4', '5')
temp <- labs[seq]
Electionsnew$Record <- factor(temp,levels = label)
Electionsnew$Record
---------------------------------------------------
#5 Differenct colour codes.
recordColor<- rgb(
  red  = c(1.00, 1.00, 0.00, 0.10, 0.80),
  green= c(0.10, 0.50, 0.75, 0.65, 0.45),
  blue = c(0.10, 0.00, 0.00, 1.00, 1.00)
)
---------------------------------------------------
#Facet plot with geom_point.
ggplot(Electionsnew, aes(x=Trump,
                          y=State,fill=Record,group=Group)) +
  labs(x="Trump votes", y="States",
       title="State Vise Trump votes")+
  geom_point(shape=21,size=3)+
  scale_fill_manual(values=recordColor)+
  guides(fill=FALSE)+ 
  facet_grid(Group~., scale="free_y", space="free" )+
  rowTheme
----------------------------------------------------
#Micromaps data
Micromapdata<- Electionsnew1[,1:5]
Micromapdata
Micromapdata <- as.data.frame(Micromapdata)
rownames(Micromapdata) <-Micromapdata[,1]
Micromapdata <-Micromapdata[,c(1:5)]
head(Micromapdata)
----------------------------------------------------
#Creating panels 
panel<- data.frame(
  type=c('mapcum','id','dot','dot','dot','dot'),
  lab1=c('','','Trump Votes','Clinton Votes','Johnson Votes','Other Votes'),
  col1=c(NA,NA,2,3,4,5) 
)
t(panel)
----------------------------------------------------
#Micromap plot is produced and saved in pdf
pdf(file= "2016 Election Results.pdf",width = 14, height = 10)
micromapST(Micromapdata,panel,
           rowNamesCol='State', 
           rowNames='full',
           plotNames = 'full',
           sortVar='Trump', ascend=FALSE,
           title=c("2016 Presidential General Election Results")
)

plot(air$SEX.category, air$default.payment.next.month.category, xlab = "SEX", ylab = "DEFAULT PAYMENT")+xlim(0,100)
mosaicplot(air$SEX, main = "Relationship between BMI, age and gender",
           xlab = "BMI", ylab = "age", cex = 0.75, color = TRUE)

library(ggplot2)
ggplot(air, aes(x = air$default.payment.next.month, y = air$LIMIT_BAL, fill = air$SEX)) + geom_boxplot() +
  facet_wrap(~ air$EDUCATION, ncol = 3)

