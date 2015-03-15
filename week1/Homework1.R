
mvtData<-read.csv('mvtWeek1.csv')

#1
nrow(mvtData)
#2
str(mvtData)
ncol(mvtData)
##1.3
max(mvtData$ID)
##1.4
min(mvtData$Beat)
##1.5
summary(mvtData$Arrest)[3]
##1.6
nrow(subset(mvtData,LocationDescription == 'ALLEY'))

#2
##2.1
mvtData$Date[1]
##2.2
DateConvert = as.Date(strptime(mvtData$Date,"%m/%d/%y %H:%M"))
summary(DateConvert)
##2.3
mvtData$Month = months(DateConvert)
mvtData$WeekDay = weekdays(DateConvert)
mvtData$Date = DateConvert
names(mvtData)
table(mvtData$Month)
##2.4
table(mvtData$WeekDay)
##2.5
table(mvtData$Month,mvtData$Arrest)

#3
##3.1

pdf('Hist.pdf')
hist(mvtData$Date,breaks = 100)
dev.off()

##3.2
boxplot(Date~Arrest,mvtData)

##3.3
table(mvtData$Year,mvtData$Arrest)[1,2]

#4
#4.1

T <- sort(table(mvtData$LocationDescription))

#4.2
Top5Names = names(T)[c(73:75,77:78)]
Top5 <- subset(mvtData,LocationDescription %in% Top5Names)
str(Top5)
#4.3
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription,Top5$Arrest)
#4.4
table(Top5$LocationDescription,Top5$WeekDay)


#Second Tab
##Data
IBM <- read.csv('IBMStock.csv')
GE <- read.csv('GEStock.csv')
ProcterGamble <- read.csv('ProcterGambleStock.csv')
CocaCola <- read.csv('CocaColaStock.csv')
Boeing <- read.csv('BoeingStock.csv')

IBM$Date <- as.Date(IBM$Date,"%m/%d/%y")
GE$Date <- as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date <- as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date <- as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date <- as.Date(Boeing$Date, "%m/%d/%y")


#1
summary(IBM$Date)
summary(IBM$StockPrice)
summary(GE$StockPrice)
summary(CocaCola$StockPrice)
summary(Boeing$StockPrice)
sd(ProcterGamble$StockPrice)

#2
plot(StockPrice~Date,CocaCola,type='l',col='red')
lines(StockPrice~Date,ProcterGamble,lty=2,col='blue')
abline(v=as.Date(c("2000-03-01")),lwd=2)
abline(v=as.Date(c("1983-02-01")),lwd=2)

#3
plot(StockPrice~Date,CocaCola[301:432,],type='l',col='red',ylim=c(0,210))
lines(StockPrice~Date,GE[301:432,],col='blue',lty=2)
lines(StockPrice~Date,IBM[301:432,],col='green',lty=3)
lines(StockPrice~Date,ProcterGamble[301:432,],col='orange',lty=4)
lines(StockPrice~Date,Boeing[301:432,],col='purple',lty=5)

abline(v=as.Date(c("1997-09-01")),lwd=1)
abline(v=as.Date(c("1997-11-10")),lwd=1)

#4
IBMStockMonths<-tapply(IBM$StockPrice,months(IBM$Date),mean)
IBMStockMonths[IBMStockMonths>mean(IBM$StockPrice)]

tapply(GE$StockPrice,months(GE$Date),mean)
tapply(CocaCola$StockPrice,months(CocaCola$Date),mean)
       

#3 Demographic and employment in the US

#1
CPS <- read.csv('CPSData.csv')
str(CPS)
summary(CPS)
sort(summary(CPS$Industry))
sort(table(CPS$Industry))

sort(table(CPS$State))
table(CPS$Citizenship)


table(CPS$Hispanic,CPS$Race)
#2

summary(CPS)

table(CPS$Region,is.na(CPS$Married))
table(CPS$Sex,is.na(CPS$Married))
table(CPS$Age,is.na(CPS$Married))

table(CPS$State,is.na(CPS$MetroAreaCode))
table(CPS$Region,is.na(CPS$MetroAreaCode))

sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean) )

MetroAreaMap <- read.csv('MetroAreaCodes.csv')
CountryMap <- read.csv('CountryCodes.csv')

str(CountryMap)
str(MetroAreaMap)

CPS = merge(CPS,MetroAreaMap,by.x="MetroAreaCode",by.y="Code",all.x=TRUE)

sort(table(CPS$MetroArea))

sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))
T <- tapply(CPS$Race == "Asian",CPS$MetroArea,mean)
T1<-T[T>=0.2]
length(T1[!is.na(T1)])

sort(tapply(CPS$Education == "No high school diploma",CPS$MetroArea,mean,na.rm=TRUE))[1]

#4

CPS = merge(CPS,CountryMap,by.x="CountryOfBirthCode",by.y="Code",all.x=TRUE)

summary(CPS$Country)

sort(table(CPS$Country))

tapply(!CPS$Country=="United States",CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA",mean,na.rm=TRUE)

sort(tapply(CPS$Country=="Brazil",CPS$MetroArea,sum,na.rm=TRUE))
