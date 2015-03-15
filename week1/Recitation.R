setwd("C:\\Users\\Carlos\\Documents\\Statistics\\The analytics edge\\week1")
USDA <- read.csv('USDA.csv')

str(USDA) ## structure of the data set
summary(USDA)

#Find the food with the highest level of sodium

USDA$Description[which.max(USDA$Sodium)]

names(USDA)


HighSodium<- subset(USDA,Sodium>10000)
nrow(HighSodium)
HighSodium$Description

#match gives the positions of the first argument in the second vector
USDA$Sodium[match("CAVIAR",USDA$Description)]

mean(USDA$Sodium,na.rm=TRUE)
sd(USDA$Sodium,na.rm=TRUE)


plot(USDA$Protein,USDA$TotalFat,xlab="Protein",ylab='Fat',main ='Protein vs Fat',col='red')
hist(USDA$VitaminC,xlab='Vitamin C(mg)',main = 'Histogram of Vitamin C Levels',xlim=c(0,100),breaks=2400)
boxplot(USDA$Sugar,main = "Box plot of sugar levels",ylab = "Sugar(g)")

#Adding new variables
##Create new variable, check for the boolean operation on the right
HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium,na.rm=TRUE))
str(HighSodium)
##add it
USDA$HighProtein <-as.numeric(USDA$Protein > mean(USDA$Protein,na.rm=TRUE))
USDA$HighTotalFat <-as.numeric(USDA$TotalFat > mean(USDA$TotalFat,na.rm=TRUE))
USDA$Carbohydrate <-as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate,na.rm=TRUE))

table(USDA$HighProtein,USDA$HighTotalFat)
tapply(USDA$Sodium,USDA$HighProtein,mean,na.rm=TRUE)
tapply(USDA$VitaminC,USDA$Carbohydrate,max,na.rm=TRUE)
tapply(USDA$VitaminC,USDA$Carbohydrate,summary,na.rm=TRUE)
  