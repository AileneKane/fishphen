#read in return data from the albion test fishery on the fraser river
#available at http://www.pac.dfo-mpo.gc.ca/fm-gp/species-especes/salmon-saumon/research-recherche/testfishery-pechedessai-eng.html
d1 <- read.csv("data/AlbionTestData_fromEric.csv")#frmo 1980-2010
d1<-d1[,1:9]
#add more recent data
d2 <- read.csv("data/AlbionTestData_2011.csv")
d3 <- read.csv("data/AlbionTestData_2012.csv")
d4 <- read.csv("data/AlbionTestData_2013.csv")
d5 <- read.csv("data/AlbionTestData_2014.csv")
d6 <- read.csv("data/AlbionTestData_2015.csv")
d7 <- read.csv("data/AlbionTestData_2016.csv")
d8 <- read.csv("data/AlbionTestData_2017.csv")
d9 <- read.csv("data/AlbionTestData_2018.csv")
d10 <- read.csv("data/AlbionTestData_2019.csv")
dall<-rbind(d2,d3,d4,d5,d6,d7,d8,d9,d10)
#remove the week summary rows
dall<-dall[-which(substr(dall$Date,1,8)=="Statweek"),]
dall$date<-as.Date(dall$Date,"%d%b%y")
strsplit("dall$Date", "-")
dall<-dall %>% tidyr::separate(Date, 
                c("day", "month","year"))

dall$year<-paste("20",dall$year,sep="")
dall$month[dall$month=="Apr"]<-4
dall$month[dall$month=="May"]<-5
dall$month[dall$month=="Jun"]<-6
dall$month[dall$month=="Jul"]<-7
dall$month[dall$month=="Aug"]<-8
dall$month[dall$month=="Sep"]<-9
dall$month[dall$month=="Oct"]<-10
dall$calDay<-strftime(strptime(paste(dall$year,dall$month,dall$day,sep="-"), format = "%Y-%m-%d"),format = "%j") 
colnames(dall)[5:9]<-c("net.length","catch","sets","effort","cpue")
d<-subset(dall,select=c(month,day,year,calDay,net.length,catch,sets,effort,cpue))
d$calDay<-as.integer(d$calDay)
d1$calDay[which(is.na(d1$calDay))]<-strftime(strptime(paste(d1$year[which(is.na(d1$calDay))],d1$month[which(is.na(d1$calDay))],d1$day[which(is.na(d1$calDay))],sep="-"), format = "%Y-%m-%d"),format = "%j") 

d<-rbind(d1,d)
#some rows have NA for effort, even though there is CPUE. We can back-calculate effort from this
d$effort[which(is.na(as.numeric(d$effort)))]<- as.numeric(gsub(",","",d$effort[which(is.na(as.numeric(d$effort)))]))
write.csv(d,"analyses/output/albionddat.csv",row.names = FALSE)
