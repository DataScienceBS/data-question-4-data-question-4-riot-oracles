# set the directory
setwd("/Users/smitaupadhyay/Desktop/data_science01/repogit/data-question-4-data-question-4-riot-oracles")
#download the packages

library(tidyr)
library(tidyverse)
library(dplyr)
library(gapminder)
library(ggplot2)
library(corrplot)
library(GGally)
library(ggcorrplot)
library(PerformanceAnalytics)
library(MASS)
library(plotly)
library(RColorBrewer)
library(reshape2)
##read the dataframes
scq<- readRDS("data/scq.RDS")
scq1<- readRDS("data/scq1.RDS")
scq2<- readRDS("data/scq2.RDS")
scq3<- readRDS("data/scq3.RDS")
totaltax<-readRDS("data/totaltax.RDS")
scq5<-readRDS("data/scq5.RDS")
ttax_zip<-readRDS("data/ttax_zip.RDS")
merged2<-readRDS("data/merged2.RDS")
merged3<-readRDS("data/merged3.RDS")


#plot the graphs
####pie chart to see the ethic distribution of student in TN high schools
x <-  c(2831.097, 80214.41, 227431.5, 633221.993)
leg <-  c("Native American","Hispanic","African American","White/others")

piepercent<- round(100*x/sum(x), 1)

# Plot the chart.
TN<- pie(x, labels = piepercent, main = "Ethnic Distribution of students in TN hig schools",col = rainbow(length(x)))
legend("left", c("Native American","Hispanic","African American","White/others"), 
       cex = 0.4,
       fill = rainbow(length(x)))
TN


##plots using the scq df
p <- ggplot( scq2, aes(x=mean_expense, y=mean_enrol, 
                     color = CORE_region, size = grad)) + 
  geom_point() + ggtitle("Comparing Enrollment to expense per pupil, the size of the 
                         point is scaled with percent graduation") +
  xlab("Average expense per student") + ylab("Average Enrollment")
p <- ggplotly(p)
p

#####
scq2l <- scq2%>%
  rowwise%>%
  mutate(left = sum(mean_exp +  mean_dropout))
View(scq2l)

p1 <- ggplot( scq2l, aes(x=mean_expense, y=mean_enrol, color = CORE_region, size = BHN)) + 
  geom_point() + ggtitle("Comparing Enrollment to expense per pupil, the size of the 
                         point is scaled with percent BHN") +
  xlab("Average expense per student") + ylab("Average Enrollment")
p1 <- ggplotly(p1)
p1

######
p2<- ggplot( scq2l, aes(x=mean_expense, y= mean_ED, color = CORE_region, size = left)) + 
  geom_point() + ggtitle("Mid Cumberland Core has the highest number of economically 
                         disadvantaged students and also highest number of the students 
                         leaving (expelled + dropout) before graduation") +
  xlab("Average expense per student") + ylab("Average Economically Disadvantage")
p2 <- ggplotly(p2)
p2

#########


scq5.m <- melt(scq5)
#df.m <- rename(df.m, Period = Var1, Region = Var2)

t1 <- ggplot(scq5.m, aes(x = CORE_region, y = value/1e+06,fill = variable)) + 
  ggtitle("Grdaduation and student turdiness in TN high school") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Students") + xlab("")
t1 <- t1 + geom_bar(stat = "identity", position = "stack")

t1 <- ggplotly(t1)
t1
#######
scq2_race<-subset(scq2, select=c(CORE_region, ED, AA, NAm, HS))
scq2_race.m <- melt(scq2_race)

t2 <- ggplot(scq2_race.m, aes(x = CORE_region, y = value,fill = variable)) + 
  ggtitle("Percentage of ethnic distribution in TN high school") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Percentage of Students") + xlab("")
t2 <- t2 + geom_bar(stat = "identity", position = "stack")

t2 <- ggplotly(t2)
t2

####
scq_drop <- scq%>%
  filter(Graduation !=0, ACT_Composite !=0, !is.na(CORE_region))

boxplot(Graduation~CORE_region, data =scq_drop,
        ylab="Percent Graduation",
        main= "Graduation in TN core regions",
        notch=TRUE,
        varwidth=TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
boxplot(ACT_Composite~CORE_region, data =scq_drop,
        ylab="Percent ACT_Composite",
        main= "Graduation in TN core regions",
        notch=TRUE,
        varwidth=TRUE) 
theme(axis.text.x = element_text(angle = 90, hjust = 1))

################
scq_drop_summary <- scq_drop %>% 
  group_by(CORE_region) %>% 
  summarise(ming = min(Graduation),
            maxg = max(Graduation),
            minact = min(ACT_Composite),
            maxact = max(ACT_Composite))
################
p3 <- ggplot(scq_drop, aes(x = CORE_region, y = Graduation, color = ACT_Composite)) +
  geom_boxplot() + 
  geom_jitter(size = 1) +
  ggtitle("Graduation across TN core region") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
 

p3 <- ggplotly(p3)
p3
################

p4 <- ggplot(scq_drop, aes(x = CORE_region, y = ACT_Composite, color = Graduation)) +
  geom_boxplot() + 
  geom_jitter(size = 1) + 
  ggtitle("Graduation across TN core region") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p4<-ggplotly(p4)
p4
################
chloro_ro<- readRDS("data/chloro_data.RDS")
################

saveRDS(tax2015c, file = "data/tax2015c.RDS")
tax2015c_drop<- tax2015c%>%
  filter(zip_code !=0)

p5 <-ggplot(tax2015c_drop, aes(x= agi_a, y= agi_range, size= return_c, col=zip_code))+
  geom_point()+ ggtitle("Agi amopunts filled per zip code sized by the number of returns")
p5<- ggplotly(p5)
p5
################ 
saveRDS(merged4, file="data/merged4.RDS")
 
p6<-ggplot(merged4, aes(y=Per_Pupil_Expenditures, x=agi, 
                         color=county)) + 
  geom_point()+ scale_y_log10() + xlab("agi amount/count") +
  facet_wrap("CORE_region") 
p6<-ggplotly(p6)
p6


p6<-ggplot(merged4, aes(y=Graduation, x=agi, 
                        color=county)) + 
  geom_point()+ xlab("agi amount/cout") +
  facet_wrap("CORE_region") 
p6<-ggplotly(p6)
p6

################

library(RColorBrewer)
zp3 <- ggplot(data = merged4,
              aes(x = latitude, y = longitude)) 
zp3 <- zp3 + geom_polygon(data = merged4,  # This is also a nice example of how to plot
                          aes(x = latitude, y = longitude, fill = CORE_region),  # two superimposed geoms
                          alpha = 1/2)                             # from different data.frames
zp3 <- zp3 + geom_point(size=1)
zp3 <- zp3 + coord_equal()
zp3 <- zp3 + scale_fill_manual(values = colorRampPalette(rev(brewer.pal(11, "Spectral")))(8))
zp3
p7 <- ggplotly(zp3)
p7

################
zp4 <- ggplot(data = merged4,
              aes(x = biz_inc, y = agi)) 
zp4 <- zp4 + geom_polygon(data = merged4,  # This is also a nice example of how to plot
                          aes(x = biz_inc, y = agi, fill = CORE_region),  # two superimposed geoms
                          alpha = 1/2)                             # from different data.frames
zp4 <- zp4 + geom_point(size=1)
zp4 <- zp4 + coord_equal()
zp4 <- zp4 + scale_fill_manual(values = colorRampPalette(rev(brewer.pal(11, "Spectral")))(8))
zp4
p8 <- ggplotly(zp4)
p8
################

p9<- ggplot(merged4, aes(agi, wage, col = CORE_region)) +
  geom_point(aes(size = ACT_Composite, frame = zip_code, ids = county)) 
  

p9 <- ggplotly(p9)
p9
################
tax<-tax2015c_drop%>%
  mutate(agi = agi_a/return_c,
         wage = wage_a/wage_c)
p10<- ggplot(tax, aes(agi, wage, col = agi_range)) +
  geom_point(aes(size= farm_c, frame = zip_code, ids = zip_code)) 


p10 <- ggplotly(p10)
p10
######
zp5 <- ggplot(data = merged4,
              aes(x = wage, y = agi)) 
zp5 <- zp5 + geom_polygon(data = merged4,  # This is also a nice example of how to plot
                          aes(x = wage, y = agi, fill = CORE_region),  # two superimposed geoms
                          alpha = 1/2)                             # from different data.frames
zp5 <- zp5 + geom_point(size=1)
zp5 <- zp5 + coord_equal()
zp5 <- zp5 + scale_fill_manual(values = colorRampPalette(rev(brewer.pal(11, "Spectral")))(8))
zp5
p11 <- ggplotly(zp5)
p11
########
scq7<- scq2_race%>%
  rowwise()%>%
  mutate(Others = 100 - (sum(AA + NAm + HS)))
scq7a<-subset(scq7, select = -c(ED))

scq7a.m<- melt(scq7a)
rnames <- scq7a[,1]
mat_scq7a <- data.matrix(scq7a[,2:ncol(scq7a)])
rownames(mat_scq7a) <- rnames
################

pie(mat_scq7a, labels = mat_scq7a, main = "Ethnic Distribution of students in TN hig schools",col = rainbow(length(x)))
legend("left", c("Native American","Hispanic","African American","White/others"), 
       cex = 0.4,
       fill = rainbow(length(x)))+facet_wrap("")
########
pie(EastTN.m$value, labels = EastTN$value,
    main = "Ethnic Distribution of students in East TN hig schools",
    col = rainbow(length(x)))
legend("left", c("Native American","Hispanic","African American","White/others"), 
       cex = 0.4,
       fill = rainbow(length(x)))
#############

EastTN<- mat_scq7a[1, ]
EastLab<- c(8.59,  0.38,  6.06, 84.97)
FirstTN<- mat_scq7a[2, ]
FirstTNLab <- c(4.88,  0.37,  5.98, 88.77)
MidCumb<- mat_scq7a[3, ]
MidCumbLab <- c(22.95, 0.39, 12.14, 64.52)
Northwest<- mat_scq7a[4, ]
NorthwestLab<- c(16.89,  0.16,  4.25, 78.71)
Southcen <- mat_scq7a[5, ]
SouthcenLab<- c(9.86,  0.33,  7.23, 82.59)
Southeast<- mat_scq7a[6, ]
SoutheastLab<-c(18.52,  0.26,  7.96, 73.26)
Southwest<- mat_scq7a[7, ]
SouthwestLab <- c(58.72,  0.18,  8.01, 33.09)
UpperCum<- mat_scq7a[8, ]
UpperCumLab <- c(2.98,  0.33,  7.09, 89.59)
##########
pie(EastTN, labels = EastLab,
    main = "Ethnic Distribution of students in East TN hig schools",
    col = rainbow(length(x)))
legend("left", c("African American","Native American","Hispanic","White/others"), 
       cex = 0.4,
       fill = rainbow(length(x)))

pie(FirstTN, labels = FirstTNLab,
    main = "Ethnic Distribution of students in First TN hig schools",
    col = rainbow(length(x)))
legend("left", c("African American","Native American","Hispanic","White/others"), 
       cex = 0.4,
       fill = rainbow(length(x)))

pie(MidCumb, labels = MidCumbLab,
    main = "Ethnic Distribution of students in Mid Cumberland TN hig schools",
    col = rainbow(length(x)))
legend("left", c("African American","Native American","Hispanic","White/others"), 
       cex = 0.4,
       fill = rainbow(length(x)))

pie(Northwest, labels = NorthwestLab,
    main = "Ethnic Distribution of students in Northwest TN hig schools",
    col = rainbow(length(x)))
legend("left", c("African American","Native American","Hispanic","White/others"), 
       cex = 0.4,
       fill = rainbow(length(x)))

pie(Southcen, labels = SouthcenLab,
    main = "Ethnic Distribution of students in South Central TN hig schools",
    col = rainbow(length(x)))
legend("left", c("African American","Native American","Hispanic","White/others"), 
       cex = 0.4,
       fill = rainbow(length(x)))

pie(Southeast, labels = SoutheastLab,
    main = "Ethnic Distribution of students in Southeast TN hig schools",
    col = rainbow(length(x)))
legend("left", c("African American","Native American","Hispanic","White/others"), 
       cex = 0.4,
       fill = rainbow(length(x)))

pie(Southwest, labels = SouthwestLab,
    main = "Ethnic Distribution of students in Southwest/Memphis TN hig schools",
    col = rainbow(length(x)))
legend("left", c("African American","Native American","Hispanic","White/others"), 
       cex = 0.4,
       fill = rainbow(length(x)))

pie(UpperCum, labels = UpperCumLab,
    main = "Ethnic Distribution of students in Upper Cumberland TN hig schools",
    col = rainbow(length(x)))
legend("left", c("African American","Native American","Hispanic","White/others"), 
       cex = 0.4,
       fill = rainbow(length(x)))

##############

