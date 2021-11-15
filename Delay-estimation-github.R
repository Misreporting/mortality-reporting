library(stringr)
library(gridExtra)
library(grid)
library(lubridate) 
library(tidyverse) 
library(plyr)
library(data.table)
library(xlsx)
library(MASS)

#Initialization Steps

setwd("C:/Users/Marco/Desktop/OneDriveCOVIDdata/Releases of FluView data/states by release date")
filenames <- list.files(path=getwd())  
numfiles <- length(filenames) 
area<-c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming","New York City")
numarea <- length(area)
release.date<-c("April 10","April 17","April 24","May 01","May 08","May 15","May 22","May 29",
                "June 05","June 12","June 19","June 26","July 03","July 10","July 17","July 24",
                "July 31","August 07","August 14","August 21","August 28","September 04","September 11",
                "September 18","September 25","October 02","October 16","October 23","October 30",
                "November 13","November 20","November 27","December 04")
seasons<-c("2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19","2019-20","2020-21")
numseasons <- length(seasons)
week <-c(1:52)
numweek <-length(week)
startDate <- as.Date("2020-01-04")
date <- seq(startDate, by="7 days", length.out=47)
length.date <- length(date)

pca<-list()
pcaw<-list()
sum.total.death<-array(data = NA, dim = c(length.date,numfiles,numarea), dimnames = list(date,release.date,area))
Z_saved1<-list()
Z_saved2<-list()
Z_saved3<-list()
weekly.delay<-array(data = NA, dim = c(numarea,length.date), dimnames = list(area,date))

#PART 1: PLOTTING STATE LEVEL TOTAL MORTALITY DATA

for (j in 1:numarea){
  loc<-area[j] 
  ny_all<-NULL
  A<-NULL
  A2<-NULL
  B<-NULL
  le<-NULL
  for (i in 1:numfiles){
    A <- read.csv(filenames[i], header=T)
    A <- A%>%filter(SUB.AREA==loc)
    A$TOTAL.DEATHS<-as.numeric(gsub(",", "", A$TOTAL.DEATHS))
    A$NUM.PNEUMONIA.DEATHS<-as.numeric(gsub(",", "", A$NUM.PNEUMONIA.DEATHS))
    A$NUM.INFLUENZA.DEATHS<-as.numeric(gsub(",", "", A$NUM.INFLUENZA.DEATHS))
    A2<-arrange(A, SEASON)
    lastweek<-A2$WEEK[nrow(A2)]
    A2 <- A2[which((A2$SEASON==("2015-16"))|A2$SEASON==("2016-17")|A2$SEASON==("2017-18")|A2$SEASON==("2018-19")|A2$SEASON==("2019-20")|A2$SEASON==("2020-21")),]
    A2$year<-c(rep(2015,times=13),rep(2016,times=52),rep(2017,times=52),rep(2018,times=52),rep(2019,times=52),rep(2020,times=lastweek))
    A2$date<-as.Date(paste(A2$year, A2$WEEK, 6, sep="-"), "%Y-%U-%u")-7
    who<-which(is.na(A2$date)==T) 
    A2$date[who]<-c(ymd("2015-12-26"),ymd("2018-12-29"),ymd("2019-12-28"))
    ny_all<-rbind.fill(ny_all,A2)
    s1 <-substr(filenames[i], start = 18, stop = 23) 
    con<-data.frame(c(rep(s1,times=nrow(A2))))
    B<-rbind.fill(B,con)
    le<-cbind(le,nrow(A2))
  }
  names(B)<-"Release1"
  ny_all<-cbind(ny_all,B)
  ny_all$Release<-c(rep("April 10",times=le[1]),rep("April 17",times=le[2]),
                    rep("April 24",times=le[3]),rep("May 01",times=le[4]),
                    rep("May 08",times=le[5]),rep("May 15",times=le[6]),
                    rep("May 22",times=le[7]),rep("May 29",times=le[8]),
                    rep("June 05",times=le[9]),rep("June 12",times=le[10]),
                    rep("June 19",times=le[11]), rep("June 26",times=le[12]),
                    rep("July 03",times=le[13]),rep("July 10",times=le[14]), 
                    rep("July 17",times=le[15]),rep("July 24",times=le[16]),
                    rep("July 31",times=le[17]),rep("August 07",times=le[18]),
                    rep("August 14",times=le[19]),rep("August 21",times=le[20]),
                    rep("August 28",times=le[21]),rep("September 04",times=le[22]),
                    rep("September 11",times=le[23]),rep("September 18",times=le[24]),
                    rep("September 25",times=le[25]),rep("October 02",times=le[26]),
                    rep("October 16",times=le[27]),rep("October 23",times=le[28]),
                    rep("October 30",times=le[29]),rep("November 13",times=le[30]),
                    rep("November 20",times=le[31]),rep("November 27",times=le[32]),
                    rep("December 04",times=le[33]))
  pca[[j]]<-ggplot(ny_all, aes(x=date, y=TOTAL.DEATHS, group=Release)) +    
    geom_line(aes(color=Release))+geom_point(aes(color=Release))+ 
    xlab('Time')+ylab('Weekly Mortality')+ggtitle(paste(loc),"All-Cause")
  pcaw[[j]]<-ggplot(ny_all, aes(x=date, y=TOTAL.DEATHS, group=Release)) +
    geom_line(aes(color=Release))+guides(color = FALSE)+
    geom_point(aes(color=Release))+ xlab('Year 2020')+
    ylab('Weekly Mortality')+ggtitle(paste(loc),"All-Cause")+
    scale_x_date( date_labels = " %b %d" , 
                  limits = as.Date(c("2020-01-04","2020-12-05")))
  
}

#All plots in grid form
for (j in 1:13){
  grid.arrange(pcaw[[(4*j)-3]],pcaw[[(4*j)-2]],pcaw[[(4*j)-1]],pcaw[[4*j]], nrow=2, ncol=2) #, pcapw[[j]], pcaiw[[j]], nrow=3)
}
#Select plots of interest in grid form
grid.arrange(pcaw[[7]],pcaw[[33]],pcaw[[31]],pcaw[[39]], nrow=2, ncol=2) 
grid.arrange(pcaw[[34]],pcaw[[49]],pcaw[[40]],pcaw[[19]],pcaw[[20]],pcaw[[13]], nrow=2, ncol=3) 

#PART 2: GATHERING DELAY DATA (IN WEEKS) ON STATE LEVEL MORTALITY COUNTS
for (j in 1:numarea){
  loc<-area[j]
  timeframe1<-seasons[8]
  timeframe2<-seasons[9]
  for (i in 1:numfiles){
    Y <- read.csv(filenames[i], header=T)
    Y_inter1 <- Y%>%filter(((SUB.AREA==loc)&(SEASON==timeframe1)&(WEEK<=39)))
    Y_inter2 <- Y%>%filter(((SUB.AREA==loc)&(SEASON==timeframe2)&(WEEK>=40)&(WEEK<=47)))
    Y<-rbind(Y_inter1,Y_inter2)
    Y$TOTAL.DEATHS<-as.numeric(gsub(",", "", Y$TOTAL.DEATHS))
    dead<-Y$TOTAL.DEATHS
    length(dead)<-length.date
    dead.final<-as.data.frame(dead)
    sum.total.death[,i,j]<-dead.final$dead
  }
}
##################################################
sum.total.death2<-as.data.frame(sum.total.death)
rownames(sum.total.death2)<-date
#the variable sum.total.death (and by extension, sum.total.death2) compiles the total weekly mortality counts for 
#(1) each week of the year 2020 [through December 4th], (2) each weekly updated FluView dataset and (3) each state/region. 
##################################################

##Using the sum.total.death variable, for each state/region, we can look at: 
###(a) the absolute difference in total mortality count for a given week, from one dataset release to the subsequent dataset's release the following week (represented by the variable Z_saved1)
###(b) the percentage change in in total mortality count for a given week, from one dataset release to the subsequent dataset's release the following week (represented by the variable Z_saved2)
###(c) the categorized version of Z_saved2, whereby an percentage change greater than 1% is coded as a "1", or otherwise coded with a "0" (represented by the variable Z_saved3)
####### each one in Z_saved3 represents an additional week's delay in which consensus has not been reached on the total mortality count for a given week  
##The variable weekly.delay is based off of Z_saved3 & represents the total number of 1s for a given week, which we equate to the total delay (in weeks) for a given week's total mortality count.

for (p in 1:numarea){
  X<-as.data.frame(sum.total.death[,,p])
  rownames(X)<-date
  num.datasets<-ncol(sum.total.death[,,p])
  num.dates<-nrow(sum.total.death[,,p])
  weekly.change<-array(data = NA, dim = c(num.dates,num.datasets-1,1))
  weekly.change.percent<-array(data = NA, dim = c(num.dates,num.datasets-1,1))
  
  for (l in 1:num.dates){
    for (m in 2:num.datasets) {
      weekly.change[l,m-1,1]<-(X[l,m]-X[l,m-1]) 
      weekly.change.percent[l,m-1,1]<-(X[l,m]-X[l,m-1])/(X[l,m-1])
    }
  }
  
  Z1<-as.data.frame(weekly.change)
  Z2<-as.data.frame(weekly.change.percent)
  rownames(Z1)<-date
  rownames(Z2)<-date
  date.ranges<-c("April 17-April 10","April 24-April 17","May 01-April 24","May 08-May 01",
                 "May 15-May 08","May 22-May 15","May 29-May 22",
                 "June 05-May 29","June 12-June 05","June 19-June 12",
                 "June 26-June 19","July 03-June 26","July 10-July 03",
                 "July 17-July 10","July 24-July 17","July 31-July 24",
                 "August 07-July 31","August 14-August 07","August 21-August 14",
                 "August 28-August 21","September 04-August 28","September 11-September 04",
                 "September 18-September 11","September 25-September 18","October 02-September 25",
                 "October 16-October 02","October 23-October 16","October 30-October 23",
                 "November 13-October 30","November 20-November 13","November 27-November 20",
                 "December 04-November 27")
  colnames(Z1)<-date.ranges
  colnames(Z2)<-date.ranges
  Z_saved1[[p]]<-Z1
  Z_saved2[[p]]<-Z2
  
  Z3<-as.data.frame(t(Z2))
  for (n in 1:ncol(Z3)) {
    for (o in 1:nrow(Z3)) {
      Z3[o,n] <- ifelse(Z3[o,n]>0.01, 1, 0)
    }
  }
  Z_saved3[[p]]<-Z3
  
  for (h in 1:ncol(Z3)) {
    weekly.delay[p,h]<-sum(Z_saved3[[p]][,h],na.rm = TRUE)
  }
}

names(Z_saved1)<-area
names(Z_saved2)<-area
names(Z_saved3)<-area

###################################################################################################
#Create of a concise spreadsheet (rdata) with total mortality count delays & pertinent weekly mortality counts (total and per million of state population) by state 
weekly.delay<-as.data.frame(weekly.delay)
colnames(weekly.delay)<-date
weekly.delay.cleaned<-weekly.delay[,14:36]
weekly.delay.cleaned<-cbind(weekly.delay.cleaned,rowMeans(weekly.delay.cleaned))
names(weekly.delay.cleaned)[names(weekly.delay.cleaned) == "rowMeans(weekly.delay.cleaned)"] <- "Average Delay"
countrywide.average.delay<-mean(weekly.delay.cleaned$`Average Delay`)
print(countrywide.average.delay)

rdata<-NULL
capita<-read.xlsx("c:/Users/Marco/Desktop/draft_rosenbaum_COVID/percapita.xls",1)
for (q in 1:numarea) {
  int<-cbind((rep(rownames(weekly.delay.cleaned[q,]),23)),rownames(sum.total.death2[14:36,]),t(weekly.delay.cleaned[q,1:23]),sum.total.death[14:36,numfiles,q])
  int<-as.data.frame(int)
  row.names(int)<-NULL
  colnames(int)<-c("State","Date","Weeks Delayed","Deaths")
  rdata<-rbind(rdata,int)
}
for (z in 1:nrow(rdata)) {
  rdata$PPM[z]<-capita$PopPerMillion[capita$State==rdata$State[z]]
}

rdata$`Weeks Delayed`<-as.numeric(as.character(rdata$`Weeks Delayed`))
rdata$Deaths<-as.numeric(as.character(rdata$Deaths))
rdata$deathperPPM<-rdata$Deaths/rdata$PPM
colnames(rdata)<-c("State","Date","Weeks Delayed","Deaths (as recorded in last released dataset)","State Population (in millions)","Deaths per Million of State Population")

###################################################################################################
#AUGMENTED "weekly.daily.cleaned" DELAY DATA (which accounts for submission of missing counts from states)
####unfortunately, as it is calculated, the delay counts (in weeks) in rdata do not add in the delay in preliminary counts. 
######in some states, many weeks pass between when a given week's initial mortality count should be recorded vs when it is actually is recorded.
####to compensate for the resulting potential understimation in delay counts, Z_saved3 was manually assessed in every state for delays in the preliminary counts (in weeks), which were subsequently added to the delay counts recorded in rdata. 
######this augmented data was saved under weekly.daily.cleaned.FINAL2.csv, which was then read in below, in order to create weekly.delay.cleaned2 & untimately rdata2.

missing.count<-read.csv("c:/Users/Marco/Desktop/draft_rosenbaum_COVID/missing.datasets.ALT3.csv")
weekly.delay.cleaned2<-(weekly.delay.cleaned[,1:23])+(missing.count[,2:24])
weekly.delay.cleaned2<-cbind(weekly.delay.cleaned2,rowMeans(weekly.delay.cleaned2))
names(weekly.delay.cleaned2)[names(weekly.delay.cleaned2) == "rowMeans(weekly.delay.cleaned2)"] <- "Average Delay"
countrywide.average.delay2<-mean(weekly.delay.cleaned2$`Average Delay`)
print(countrywide.average.delay2)

rdata2<-NULL
capita<-read.xlsx("c:/Users/Marco/Desktop/draft_rosenbaum_COVID/percapita.xls",1)
for (q in 1:numarea) {
  int2<-cbind((rep(rownames(weekly.delay.cleaned2[q,]),23)),rownames(sum.total.death2[14:36,]),t(weekly.delay.cleaned2[q,1:23]),sum.total.death[14:36,numfiles,q])
  int2<-as.data.frame(int2)
  row.names(int2)<-NULL
  colnames(int2)<-c("State","Date","Weeks Delayed","Deaths")
  rdata2<-rbind(rdata2,int2)
}
for (z in 1:nrow(rdata2)) {
  rdata2$PPM[z]<-capita$PopPerMillion[capita$State==rdata2$State[z]]
}

rdata2$`Weeks Delayed`<-as.numeric(as.character(rdata2$`Weeks Delayed`))
rdata2$Deaths<-as.numeric(as.character(rdata2$Deaths))
rdata2$deathperPPM<-rdata2$Deaths/rdata2$PPM
colnames(rdata2)<-c("State","Date","Weeks Delayed","Deaths (as recorded in last released dataset)","State Population (in millions)","Deaths per Million of State Population")

################################################################################################

setwd("C:/Users/Marco/Desktop")
write.csv(rdata,"weekly.daily.cleaned.FINAL.csv")
write.csv(rdata2,"weekly.daily.cleaned2.FINAL.csv")