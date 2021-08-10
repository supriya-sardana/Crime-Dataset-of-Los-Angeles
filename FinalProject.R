library(tidyverse)
library(dplyr)
library(ggplot2)
library(ISLR)
library(aod)
library(Zelig)
library(caret)
library(AUC)
library(ROCR)
library(Metrics)

# Reading dataset
Crime_data = read.csv('/Users/supriyasardana/Documents/George Mason/AIT-580/Final Project/Crime_Data_from_2010_to_2019_clean_data2.csv')

#Dropping extra column from dataset
Crime_data<-subset(Crime_data,select = c(-X))

head(Crime_data)
View(Crime_data)

##1. Summary
summary(Crime_data)
sapply(Crime_data, class)
colnames(Crime_data)
dim(Crime_data)

#Removing columns that are not required
Crime_data <- subset(Crime_data, select = -c(Premis.Cd,Premis.Desc,X))

#Removing missing values if any
Crime_data<-na.omit(Crime_data)

#Dropping duplicate values
Crime_data <- Crime_data[!duplicated(Crime_data),]


##2. Sub setting numeric variable
var<-sapply(Crime_data, is.numeric)
var
Crime_data_num <- Crime_data[, var]
head(Crime_data_num)

names(Crime_data_num)


##3. Variable/ Univariate analysis

#a.Area Name
Crime_data_Area_Name <- Crime_data %>% group_by(AREA.NAME) %>% summarise(Count=n())

ggplot(Crime_data_Area_Name, aes(AREA.NAME,Count))+
  geom_bar(stat = "identity" , fill='cadetblue4') +
  scale_fill_brewer(palette =  "Blues")+
  theme_minimal()+
  geom_text(aes(label= Count),vjust=-0.25,size=2)+
  xlab('Area Name where crime committed in LA') +
  ylab('Frequency') + 
  ggtitle('Area Name where most crime was reported') + 
  theme(axis.text.x = element_text(angle = 90, color = 'red', hjust = .5, vjust = .3),
        axis.text.y = element_text(color = 'red', hjust = .5, vjust = .3),
        plot.title = element_text(color="black", face = "bold", hjust=.5,vjust=2),
        title=element_text(size = 9,face='bold'))


###Type of crime in 77th street

Crime_data_Area_Name1 <- Crime_data %>% group_by(AREA.NAME,Crm.Cd.Desc) %>% summarise(Count=n())
head(Crime_data_Area_Name1[Crime_data_Area_Name1$AREA.NAME=='77th Street',],5)
Crime_data_Area_Name1_TopCrime=Crime_data_Area_Name1[Crime_data_Area_Name1$AREA.NAME=='77th Street',]

Crime_data_Area_Name1_TopCrime<-head(Crime_data_Area_Name1_TopCrime[order(-Crime_data_Area_Name1_TopCrime$Count),],5)

###Pie Chart
ggplot(Crime_data_Area_Name1_TopCrime, aes("",Count, fill=Crm.Cd.Desc))+
  geom_bar(stat = "identity" ) +
  coord_polar("y", start=0)

  scale_fill_brewer(palette =  "Blues")+
  theme_minimal()+
  geom_text(aes(label= Count),vjust=-0.25,size=2)+
  xlab('Area Name where crime committed in LA') +
  ylab('Frequency') + 
  ggtitle('Top 20 frequency of ares codes where most crime was reported') + 
  theme(axis.text.x = element_text(angle = 90, color = 'red', hjust = .5, vjust = .3),
        axis.text.y = element_text(color = 'red', hjust = .5, vjust = .3),
        plot.title = element_text(color="black", face = "bold", hjust=.5,vjust=2),
        title=element_text(size = 9,face='bold'))


## 4. Multivariate Analysis
  
###1. Is any particular ethnicity being targeted by criminals?

unique(Crime_data$Vict.Descent)
table(Crime_data$Vict.Descent)

  # Cleaning and correcting year in YYYY format
Crime_data$Crime_date <- substr(Crime_data$Date.Rptd,7,10)
Crime_data$Crime_date <- replace(Crime_data$Crime_date,Crime_data$Crime_date=="10 0",2010)
Crime_data$Crime_date <- replace(Crime_data$Crime_date,Crime_data$Crime_date=="11 0",2011)
Crime_data$Crime_date <- replace(Crime_data$Crime_date,Crime_data$Crime_date=="12 0",2012)
Crime_data$Crime_date <- replace(Crime_data$Crime_date,Crime_data$Crime_date=="13 0",2013)
Crime_data$Crime_date <- replace(Crime_data$Crime_date,Crime_data$Crime_date=="14 0",2014)
Crime_data$Crime_date <- replace(Crime_data$Crime_date,Crime_data$Crime_date=="15 0",2015)
Crime_data$Crime_date <- replace(Crime_data$Crime_date,Crime_data$Crime_date=="16 0",2016)
Crime_data$Crime_date <- replace(Crime_data$Crime_date,Crime_data$Crime_date=="17 0",2017)
Crime_data$Crime_date <- replace(Crime_data$Crime_date,Crime_data$Crime_date=="18 0",2018)
Crime_data$Crime_date <- replace(Crime_data$Crime_date,Crime_data$Crime_date=="19 0",2019)
Crime_data$Crime_date <- replace(Crime_data$Crime_date,Crime_data$Crime_date=="20 0",2020)
Crime_data$Crime_date <- replace(Crime_data$Crime_date,Crime_data$Crime_date=="21 0",2021)


###Grouping based on crime date and ethnicity
Crime_data_ethinicity <- Crime_data %>% group_by(Crime_date,Vict.Descent,Vict.Sex) %>% summarise(count = n())

head(Crime_data_ethinicity)

###Removing ethinicity withh NULL values
Crime_data_ethinicity<-subset(Crime_data_ethinicity,Crime_data_ethinicity$Vict.Descent!="" & Crime_data_ethinicity$Vict.Descent!= '-')

###Subsetting data based on gender,ethinicity and year
Crime_data_ethinicity<-subset(Crime_data_ethinicity,Crime_data_ethinicity$Vict.Sex=="M" | Crime_data_ethinicity$Vict.Sex== "F")
Crime_data_ethinicity_1<-subset(Crime_data_ethinicity,Crime_data_ethinicity$Vict.Descent =='H' | Crime_data_ethinicity$Vict.Descent =='A' | Crime_data_ethinicity$Vict.Descent =='B' | Crime_data_ethinicity$Vict.Descent =='O' | Crime_data_ethinicity$Vict.Descent =='W')
Crime_data_ethinicity_1<-subset(Crime_data_ethinicity_1,Crime_data_ethinicity_1$Crime_date =='2010' | Crime_data_ethinicity_1$Crime_date =='2012' | Crime_data_ethinicity_1$Crime_date =='2014' | Crime_data_ethinicity_1$Crime_date =='2016' | Crime_data_ethinicity_1$Crime_date =='2018' | Crime_data_ethinicity_1$Crime_date =='2020')

Crime_data_freq <- Crime_data %>% group_by(Crime_date) %>% summarise(n())
names(Crime_data_freq)[1]<-"Year"
names(Crime_data_freq)[2]<-"Frequency"

## Plotting graph
ggplot(Crime_data_ethinicity_1,aes(Vict.Descent,count, fill=Vict.Sex))+
  geom_bar(stat = 'identity')+
  facet_grid(.~Crime_date)+
  labs(title = "Crime frequency of Male and Female \namong the ethinicity in LA(2010-2020)", x='Ethinicity(Year wise)', y='Crime Frequency' , fill='Victim\nGender')+
  scale_fill_manual(values = c("skyblue" , "pink3"))+
  theme(title = element_text(size = 9,face='bold'),plot.title = element_text(hjust = 0.5), axis.text.x = element_text(color = 'red'), axis.text.y = element_text(color = 'red'))


# 5.Logistic Regression

unique(Crime_data_1$Crm.Cd.1)
unique(Crime_data_1$Crm.Cd.2)
unique(Crime_data_1$Crm.Cd.3)
union(Crime_data_1$Crm.Cd, Crime_data_1$Crm.Cd.2)

#Identifying most serious crime codes and creating variable with serious crime as 1 and non serious as 0
serious_crime <- unique(Crime_data_1$Crm.Cd.1)[!(unique(Crime_data_1$Crm.Cd.1) %in% unique(Crime_data_1$Crm.Cd.2))]
serious_crime

##Adding serious crime column that indicates 1 for serious crime and 0 for non serious crime
for (i in 1:dim(Crime_data)[1]) {
  if(Crime_data$Crm.Cd[i] %in% serious_crime==TRUE){
    Crime_data$SeriousCrime[i] = 1
  }
  else
  {
   Crime_data$SeriousCrime[i] = 0
  }
}

# Writing file to csv with serious crime indicator
write.csv(Crime_data,'/Users/supriyasardana/Documents/George Mason/AIT-580/Final Project/Crime_Data_from_2010_to_2019_clean_data1.csv')

head(Crime_data[Crime_data$SeriousCrime==1,],90000)

Crime_data_subset <- Crime_data[Crime_data$Crime_date %in% c(2010),]
dim(Crime_data_subset)

typeof(Crime_data$Crime_date)
Crime_data$Crime_date<-as.integer(Crime_data$Crime_date)

## Removing records with Victim sex= -,X,H
Crime_data_lr1 <- subset(Crime_data[c(Crime_data$Vict.Sex != '-' & Crime_data$Vict.Sex != 'X' & Crime_data$Vict.Sex != 'H'),])
dim(Crime_data_lr1)

## Building logistic model

## rare event 
set.seed(12345)
dt_1<-(Crime_data_lr1[Crime_data_lr1$SeriousCrime == 1,])
dt_2 <- Crime_data_lr1[Crime_data_lr1$SeriousCrime == 0,]
## Taking 22,000 dataset with 2000 records =1 (6000 rows) nd 20000 with value =0

dt_3<- sample_n(dt_1, size = 2000)
dt_4<- sample_n(dt_2, 20000)
dt <- rbind(dt_3,dt_4)
rows<-sample(nrow(dt))
dt <- dt[rows,]
dt_5 <- sample(nrow(dt), nrow(dt)*.6)


#dividing training and testing data
train = dt[dt_5,]
test = dt[-dt_5,]
      
#Model building
mylogit <- glm(SeriousCrime~Vict.Age+Vict.Sex+Hour_Crime+Crime_date, data = train, family = binomial(link="logit"))

summary(mylogit)

plot(mylogit)

#confidence interval
confint(mylogit)

##Confusion matrix for training set
mylogit.prob1 <- predict(mylogit, train, type="response")
mylogit.pred1 <- ifelse(mylogit.prob1> 0.5, "1", "0")
table(mylogit.pred1, train$SeriousCrime) 
confusionMatrix(table(mylogit.pred1, train$SeriousCrime))


##Confusion matrix for testing set
mylogit.prob2 <- predict(mylogit, test, type="response")
mylogit.pred2 <- ifelse(mylogit.prob2> 0.5, "1", "0")
table(mylogit.pred2, test$SeriousCrime)

confusionMatrix(table(mylogit.pred2, test$SeriousCrime))
data.frame(mylogit.pred1, train$SeriousCrime)




## Anova test on logistic model
anova(mylogit, test='Chisq')


pt <- prediction(mylogit.prob2, test$SeriousCrime)
prf <- performance(pt, measure = "tpr", x.measure = "fpr")

plot(prf, title="ROC of testing dataset") 

auc(test$SeriousCrime, mylogit.prob2)



##6. Clustering

dt$Vict.Age.cat <- cut(dt$Vict.Age, breaks = c(-1,10,20,30,40,60,70,80,90,100,110), labels=c(1,2,3,4,5,6,7,8,9,10))
View(dt)
dt$Vict.Age.cat <-as.integer(dt$Vict.Age.cat)

var1<-sapply(dt, is.numeric)
var1
Crime_data_num1 <- dt[, var1]
head(Crime_data_num1)
Crime_data_num1<-drop_na(Crime_data_num1)

# Taking columns hour when crime is commiteed and age
Crime_data_num_cluster1<- Crime_data_num1[,c(10,11)]
Crime_data_num_cluster1<-na.omit(Crime_data_num_cluster1)

# Model building with K=3
km2 <- kmeans(Crime_data_num_cluster1, centers=4, nstart =1)
Crime_data_num_cluster1$cluster <- factor(km2$cluster)
View(Crime_data_num_cluster1)
Crime_data_num_cluster1<- drop_na(Crime_data_num_cluster1)
centers <- data.frame(cluster=factor(1:4),km2$centers)
centers         

# plotting cluster graph      
ggplot(Crime_data_num_cluster1,aes(Vict.Age.cat,Hour_Crime, color=cluster, shape=cluster))+
  geom_point(alpha=.3)+
  scale_shape_manual(values = 1:4,
                      guide = guide_legend(override.aes=aes(size=1))) + 
  geom_point(data=centers,  aes(x=Vict.Age.cat,y=Hour_Crime), size=2, stroke=2)  +
  labs(x='Victim Age group', y='Time(in hour)', title = 'Scatterplot of Time(in hour) vs victim age group')+
  theme_bw()+
  theme(axis.text.x = element_text(color = 'red', hjust = .5, vjust = .3),
      axis.text.y = element_text(color = 'red', hjust = .5, vjust = .3),
      plot.title = element_text(color="black", face = "bold", hjust=.5,vjust=2),
      title=element_text(size = 10,face='bold'))+
  scale_x_continuous(limits = c(0,10), breaks = c(1,3,5,7,9))+
  scale_y_continuous(limits = c(0,24), breaks = c(1,4,7,10,13,16,19,22))


##7. 	Relationship of crime with victim gender
head(dt)

male_female <- dt %>% group_by(Vict.Sex, SeriousCrime) %>% summarise(count=n())
male_female <- male_female[male_female$Vict.Sex != "",]

ggplot(male_female, aes(Vict.Sex,count , fill= as.factor(SeriousCrime)))+geom_bar(stat="identity") +
  geom_text(aes(label= count),vjust=1.5,size=2) +
  scale_fill_manual(values = c("lightpink1","darkseagreen")) +
  theme_minimal() +
  xlab('Victim Gender') +
  ylab('Frequency') + 
  ggtitle('Relationship of type of crime with gender') + 
  labs(fill="Serious Crime")+
  theme(axis.text.x = element_text(color = 'red', hjust = .5, vjust = .3),
        axis.text.y = element_text(color = 'red', hjust = .5, vjust = .3),
        plot.title = element_text(color="black", face = "bold", hjust=.5),
        title=element_text(size = 9,face='bold'))


##8. Status of the case analysis

dt_7 <-Crime_data %>% group_by(Status.Desc,SeriousCrime) %>% summarise(Count=n())
dt_7_0 <- subset(dt_7, dt_7$SeriousCrime %in% '0')
dt_7_1 <- subset(dt_7, dt_7$SeriousCrime %in% '1')

#1. Non serious
ggplot(dt_7_0, aes(Status.Desc, Count)) +
  geom_linerange(
    aes(x = Status.Desc, ymin = 0, ymax = Count), 
    color = "lightgray", size = 1.5
  )+
  geom_point(aes(color = Status.Desc), size = 2)+
  xlab('Status Description') +
  ylab('Frequency') + 
  ggtitle('Status of the case for non serious crime') + 
  theme(axis.text.x = element_text(color = 'red', hjust = .5, vjust = .3, angle =90),
        axis.text.y = element_text(color = 'red', hjust = .5, vjust = .3),
        plot.title = element_text(color="black", face = "bold", hjust=.5),
        title=element_text(size = 9,face='bold'))+
  ggpubr::color_palette("jco")


#2. Serious
ggplot(dt_7_1, aes(Status.Desc, Count)) +
  geom_linerange(
    aes(x = Status.Desc, ymin = 0, ymax = Count), 
    color = "lightgray", size = 1.5
  )+
  geom_point(aes(color = Status.Desc), size = 2)+
  xlab('Status Description') +
  ylab('Frequency') + 
  ggtitle('Status of the case for non serious crime') + 
  theme(axis.text.x = element_text(color = 'red', hjust = .5, vjust = .3, angle=90),
        axis.text.y = element_text(color = 'red', hjust = .5, vjust = .3),
        plot.title = element_text(color="black", face = "bold", hjust=.5),
        title=element_text(size = 9,face='bold'))+
  ggpubr::color_palette("jco")


