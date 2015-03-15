
#Intro R lecture

Sys.setlocale("LC_ALL", "C")


Country <- c("Brazil","China")
LifeExpectancy <- c(76,85)

CountryData <- data.frame(Country,LifeExpectancy)
CountryData$Population <- c(199000,1390000)

#Combine dataframes with the same number of columns with function rbind(dataframe1,datafram2,...,dataframen)
setwd('C:\\Users\\Carlos\\Documents\\Statistics\\The analytics edge\\week1')
WHO = read.csv('WHO.csv')
str(WHO) #structure of the data
summary(WHO)
WHO_Europe = subset(WHO,Region== "Europe")
WHO_Europe2 = WHO[WHO$Region == "Europe",]
write.csv(WHO_Europe,'WHO_Europe.csv')

mean(WHO$Over60)
WHO$Country[which.min(WHO$Over60)]
WHO$Country[which.max(WHO$LiteracyRate)]

plot(WHO$GNI,WHO$FertilityRate)
Outliers <- subset(WHO,GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers)
Outliers[c("Country","GNI","FertilityRate")]

hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region,xlab = "",ylab = "Life Expectancy",main="Life Expectancy of Countries by Region")

table(WHO$Region)
#merges the date by the 2nd variable and computes the 3rd argument using the 1st argument
tapply(WHO$Over60,WHO$Region,mean)

tapply(WHO$LiteracyRate,WHO$Region,min,na.rm=TRUE)

WHO$Region[which.min(tapply(WHO$ChildMortality,WHO$Region,mean))]
