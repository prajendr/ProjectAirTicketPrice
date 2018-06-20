# ProjectAirTicketPrice
Analysis to find parameters affecting price of air tickets
Companies with a robust data expertise and capable technology are optimizing the savings they can gain from travel suppliers. At the same time there is increase expectation among the corporate travelers to book and manage the travel in the same way they do the personal travel. This leads efficient way to optimize the business travel expense at the same time meeting the customized demand of business travelers. The purpose of this paper is to identify the ways to optimize air travel expenses by identifying the predictors and suggesting recommendations to the travel policy that can help the company to save money. One of the main purpose of this research is to identify the best time to book the tickets. There are several factors considered and a regression analysis is done to establish the significance and the dependence. The regression is done by city pair wise and the results of most prominent route in the company is analyzed in detail.

Code

dat<-dplyr::filter(amex9, TITAMT>0)
dat<-dplyr::filter(dat, ind_Field_Service!="Y")



##Adjust dates
dat$bookingdate<-as.Date(as.character(dat$TIBDTE), "%Y%m%d")
dat$traveldate<-as.Date(as.character(dat$TISTDT), "%Y%m%d")
dat$arrivaldate<-as.Date(as.character(dat$TIENDT), "%Y%m%d")

##dat$invoicedate<-as.Date(as.character(dat$<U+FEFF>TIIDTE), "%Y%m%d")

#add date diff
dat$date_diff <-dat$traveldate-dat$bookingdate
dat$dd <- as.numeric(as.character(dat$date_diff))

#adjustfare
dat$fare<-dat$TITAMT/100

#extractyear/month/day from dates
dat$byear<-format(dat$bookingdate,'%Y')
dat$bmonth<-format(dat$bookingdate,'%b')
dat$bday<-strftime(dat$bookingdate,'%A')
dat$tyear<-format(dat$traveldate,'%Y')
dat$tmonth<-format(dat$traveldate,'%b')
dat$tday<-strftime(dat$traveldate,'%A')
dat$ayear<-format(dat$arrivaldate,'%Y')
dat$amonth<-format(dat$arrivaldate,'%b')
dat$aday<-strftime(dat$arrivaldate,'%A')

#14more
dat$adv14 <- ifelse(dat$date_diff >=14, "14+", "<14")

#weekadv

dat$weekadv <- ifelse(dat$date_diff >14, "14+",ifelse(dat$date_diff >7, "7-14", "<7") )


##splitdatasets
dat2015<-dplyr::filter(dat, byear=='2015')
dat2016<-dplyr::filter(dat, byear=='2016')
dat2017<-dplyr::filter(dat, byear=='2017')
datUS<-dplyr::filter(dat, country=='US')
datCN<-dplyr::filter(dat, country=='CN')
datGB<-dplyr::filter(dat, country=='GB')

##airline code
dat$FareAirline = paste(dat$TIVAIR,dat$ClassFare,sep="_")


# count of bookings <10days entire data
ggplot(dat,aes(dat$date_diff))+geom_histogram(fill="red",alpha=0.5)+scale_x_continuous(limits = c(0, 150))+theme_minimal()+labs(x="# of days advance", y="Count")+ labs(title="Count of bookings")+geom_freqpoly()
ggplot(datUS,aes(datUS$date_diff))+geom_histogram(fill="red",alpha=0.5)+scale_x_continuous(limits = c(0, 150))+theme_minimal()+labs(x="# of days advance", y="Count")+ labs(title="Count of bookings")
ggplot(datCN,aes(datCN$date_diff))+geom_histogram(fill="red",alpha=0.5)+scale_x_continuous(limits = c(0, 150))+theme_minimal()+labs(x="# of days advance", y="Count")+ labs(title="Count of bookings")
ggplot(datGB,aes(datGB$date_diff,fill=TVAIR))+geom_histogram(fill="red",alpha=0.5)+scale_x_continuous(limits = c(0, 150))+theme_minimal()+labs(x="# of days advance", y="Count")+ labs(title="Count of bookings")



# booking day entire data
ggplot(dat,aes(dat$bday))+geom_histogram(stat="count",fill="Blue")+theme_minimal()
ggplot(dat,aes(dat$bday,colour=country))+geom_histogram(stat="count")+theme_minimal()


##ret1
ret1<-dplyr::filter(dat, TIAIR=='SAN/SFO/SAN')

##fare vs days in advance
ggplot(ret1,aes(ret1$dd,ret1$fare))+geom_point()+geom_smooth(method=lm)+scale_y_continuous(limits = c(0, 1200))+scale_x_continuous(limits = c(0, 100))
ggplot(ret1,aes(ret1$dd,ret1$fare))+geom_point()+scale_y_continuous(limits = c(0, 1200))+scale_x_continuous(limits = c(0, 100))



###Fare by days in advance
ggplot(ret1, aes(x=factor(dd), y=fare)) + stat_summary(fun.y="mean", geom="bar")+xlab("Days in advance")

###count by days in advance
ggplot(ret1,aes(ret1$dd))+geom_bar(stat="count",fill='red',alpha=0.5)+xlab("Days in advance")

##fare boxplot booking day
ggplot(ret1,aes(ret1$bday,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))
ggplot(ret1,aes(ret1$bday))+geom_bar(stat="count")

## fare boxplot travel day
ggplot(ret1,aes(ret1$tday,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))
ggplot(ret1,aes(ret1$tday))+geom_bar(stat="count")

## fare boxplot by <14, 14+ days
ggplot(ret1,aes(ret1$adv14,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))+labs(x="# of days advance", y="Fare$")

ggplot(ret1,aes(ret1$weekadv,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))+labs(x="# of days advance", y="Fare$")

ggplot(ret1,aes(ret1$ClassFare,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))+labs(x="# of days advance", y="Fare$")



ggplot(ret1,aes(ret1$fare))+geom_bar()+scale_y_continuous(limits = c(0, 300))+labs(x="# of days advance", y="Fare$")+scale_x_continuous(limits = c(0, 500))
summary(ret1)

##fare boxplot by year
ggplot(ret1,aes(ret1$tyear,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))

##Fare boxplot by month
ggplot(ret1,aes(ret1$tmonth,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))+geom_jitter(alpha=0.1)

##fare boxplot by compliance
ggplot(ret1,aes(ret1$Compliance,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))+geom_jitter(alpha=0.2)+theme_minimal()

##fare boxplot by classfare
ggplot(ret1,aes(ret1$ClassFare,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))+geom_jitter(alpha=0.2,fill="yellow")+theme_minimal()

##fare average by month
ggplot(ret1, aes(x=factor(tmonth), y=fare)) + stat_summary(fun.y="mean", geom="point")+scale_y_continuous(limits = c(0, 500))

##arrival day 
ggplot(ret1,aes(ret1$aday))+geom_bar(stat="count")

##travel month && booking month
ggplot(ret1,aes(ret1$tmonth))+geom_bar(stat="count")
ggplot(ret1,aes(ret1$bmonth))+geom_bar(stat="count")

## lm model advance days and fare
ggplot(dat,aes(dat$date_diff,dat$fare))+geom_point()+scale_x_continuous(limits = c(0, 250))+scale_y_continuous(limits = c(0, 40000))+geom_smooth(method=lm)

summary(lm(formula=fare~adv14+tday+bday+aday+tmonth,ret1))

as.data.frame(dat$adv14)

##Month
ggplot(ret1,aes(ret1$tmonth,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))+geom_jitter(alpha=0.1,fill='green')+theme_minimal()
ggplot(ret1, aes(x=factor(tmonth), y=fare)) + stat_summary(fun.y="mean", geom="bar")+scale_y_continuous(limits = c(0, 500))
ggplot(ret1,aes(ret1$tmonth,fill="blue9"))+geom_bar(stat="count")
ggplot(ret1,aes(ret1$bmonth))+geom_bar(stat="count")



summary(lm(formula=fare~adv14+tday+bday,ret1))
summary(lm(formula=fare~ClassAmex+adv14+tday+bday+TIVAIR,ret1))
summary(lm(formula=fare~ClassAmex+date_diff+tday+bday+TIVAIR,ret1))
summary(lm(formula=fare~ClassAmex+adv14+tday+bday+TIVAIR+tmonth,ret1))
summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassAmex+TIVAIR,ret1))
summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassFare+TIVAIR,ret1))
summary(lm(formula=fare~adv14+bday+adv14*bday,ret1))
summary(lm(formula=fare~date_diff+bday+date_diff*bday,ret1))

sqldf('select distinct TIVAIR,ClassFare,count (*),sum(fare)/count (*) from ret1 group by TIVAIR,ClassFare')

summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassAmex+TIVAIR,ret1))
summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassFare+TIVAIR,ret1))
summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassFare+TIVAIR+Compliance,ret1))

summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassFare+TIVAIR,ret1))
summary(lm(formula=fare~weekadv+tday+bday+ClassFare+TIVAIR,ret1))

summary(lm(formula=TITAMT/100~weekadv+tday+bday+ClassFareBasis+TIVAIR,ret1))


summary(lm(formula=fare~adv14+ClassFare+weekadv*ClassFare,ret1))


###Ret 2
ret2<-dplyr::filter(dat, TIAIR=='SFO/SAN/SFO')
summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassAmex+TIVAIR,ret2))
summary(lm(formula=fare~weekadv+tday+bday+ClassFare+TIVAIR,ret2))
summary(lm(formula=TITAMT/100~weekadv+tday+bday+ClassFareBasis+TIVAIR,ret2))
sqldf('select distinct byear,count (*),sum(fare) from ret2 group by byear')
summary(ret2)


##ret3

ret3<-dplyr::filter(dat, TIAIR=='SFO/BOS/SFO')
summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassAmex+TIVAIR,ret3))
summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassFare+TIVAIR,ret3))
summary(lm(formula=TIAFAR/100~weekadv+tday+bday+ClassFareBasis+TIVAIR,ret3))

summary(lm(formula=TITAMT/100~weekadv+tday+bday+ClassAmex+TIVAIR+tmonth+bmonth+tmonth*ClassAmex,ret3))
sqldf('select distinct byear,count (*),sum(fare) from ret3 group by byear')




sqldf('select distinct ClassFare,count (*),sum(fare)/count (*) from ret3 group by ClassFare')

##ret4
ret4<-dplyr::filter(dat, TIAIR=='BOS/SFO/BOS')
summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassAmex+TIVAIR+Compliance,ret4))
summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassFare+TIVAIR,ret4))
summary(lm(formula=TIAFAR/100~weekadv+tday+bday+ClassFare+TIVAIR+TIREAS,ret4))

summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassFare+TIVAIR,ret4))
summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassFareBasis+TIVAIR,ret4))


##ret5
ret5<-dplyr::filter(dat, TIAIR=='LHR/BOS/LHR')
summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassAmex+TIVAIR+Compliance,ret5))
summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassFare+TIVAIR,ret5))
summary(lm(formula=TIAFAR/100~weekadv+tday+bday+ClassAmex+TIVAIR,ret5))

summary(lm(formula=TIAFAR/100~weekadv+tday+bday+ClassFareBasis+TIVAIR+tmonth,ret5))


##ret6
ret6<-dplyr::filter(dat, TIAIR=='PEK/SHA')
ggplot(ret6,aes(ret6$date_diff))+geom_histogram(fill="red",alpha=0.5)+scale_x_continuous(limits = c(0, 50))+theme_minimal()+labs(x="# of days advance", y="Count")+ labs(title="Count of bookings")+theme_minimal()
ggplot(ret6,aes(ret6$dd,ret6$fare))+geom_point()+geom_smooth(method=lm)+scale_y_continuous(limits = c(0, 1200))+scale_x_continuous(limits = c(0, 100))+xlab("Days in advance")+ylab("fare")+theme_minimal()
ggplot(ret6,aes(ret6$dd,ret6$fare))+geom_point()+scale_y_continuous(limits = c(0, 1200))+scale_x_continuous(limits = c(0, 100))
ggplot(ret6, aes(x=factor(dd), y=fare)) + stat_summary(fun.y="mean", geom="bar")+xlab("Days in advance")
ggplot(ret6,aes(ret6$dd))+geom_bar(stat="count",fill='red',alpha=0.5)+xlab("Days in advance")
ggplot(ret6,aes(ret6$bday,ret6$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))
ggplot(ret6,aes(ret6$bday))+geom_bar(stat="count")
ggplot(ret6,aes(ret6$tday,ret6$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))
ggplot(ret6,aes(ret6$tday))+geom_bar(stat="count")
ggplot(ret1,aes(ret1$adv14,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))+labs(x="# of days advance", y="Fare$")
ggplot(ret1,aes(ret1$tyear,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))
ggplot(ret1,aes(ret1$tmonth,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))+geom_jitter(alpha=0.1)
ggplot(ret1,aes(ret1$Compliance,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))+geom_jitter(alpha=0.2)+theme_minimal()
ggplot(ret1,aes(ret1$ClassFare,ret1$fare))+geom_boxplot()+scale_y_continuous(limits = c(0, 1200))+geom_jitter(alpha=0.2,fill="yellow")+theme_minimal()
ggplot(ret6, aes(x=factor(tmonth), y=fare)) + stat_summary(fun.y="mean", geom="point")+scale_y_continuous(limits = c(0, 500))
ggplot(ret1,aes(ret1$aday))+geom_bar(stat="count")
ggplot(ret1,aes(ret1$tmonth))+geom_bar(stat="count")
ggplot(ret1,aes(ret1$bmonth))+geom_bar(stat="count")

summary(lm(formula=TIAFAR/100~adv14+tday+bday+ClassAmex+TIVAIR+Compliance,ret6))
summary(lm(formula=TIAFAR/100~adv14+tday+bday+TIVAIR+ClassFare+Compliance,ret6))


###ret7
ret6<-dplyr::filter(dat, TIAIR=='SHA/PEK')
summary(lm(formula=TIAFAR/100~weekadv+tday+bday+TIVAIR+ClassFare,ret6))


##Random Forest Methodology
ret2<-ret1
ret2$TITKTS<-NULL
ret2$TITRFE<-NULL
ret2$TIEXTN<-NULL
ret2$`Job Family Group`<-NULL
ret2$`Job Family`<-NULL
ret2$Group<-NULL
ret2$Division<-NULL
ret2$`Business Unit`<-NULL
ret2$`Cost Center`<-NULL
ret2$TripPurpose<-NULL
ret2$TripPurposeCode<-NULL
ret2$`Email - Primary Work`<-NULL

# Write CSV in R
write.csv(ret2, file = "MyData.csv")
getwd()

model1 <- randomForest(Fare ~ ., data = MyData, importance = TRUE)
plot(model1)
importance(model1)
varImpPlot(model1) 
model1






