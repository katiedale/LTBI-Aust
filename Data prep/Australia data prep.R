

### This script imports the Australian census data by cob, yoa and age (not sex), 
### tidies it, and the TB risks are calculated for each census group.
### The output: six lists with three lists within each, 
### corresponding to each census year (2006,2011,2016):

# Arisk5k - 5000 estimates of the LTBI risk of residents 
# born in China, India, Philippines, Vietnam or Great Britain.

# Anum5k - 5000 estimates of the number of residents with LTBI
# born in China, India, Philippines, Vietnam or Great Britain.

# Arisk200 - 200 estimates of the LTBI risk of residents 
# born in overseas-born countries other than
# China, India, Philippines, Vietnam or Great Britain.

# Anum200 - 200 estimates of the number of residents with LTBI
# born in overseas-born countries other than
# China, India, Philippines, Vietnam or Great Britain.

# Abrisk5k - 5000 estimates of the LTBI risk of Australian-born residents 

# Abnum5k - 5000 estimates of the of the number of Australian-born residents with LTBI


library(TTR)
library(zoo)
library(plyr)
library(data.table)
library(stringr)
library(quantmod)
library(dplyr)
library(sm)
library(reshape2)
library(countrycode)
library(ggplot2)
library(reshape2)
library(bindr)
library(stringi)
library(tidyr)
library(tidyselect)


#Loading the the Australian Census data - choose relevant location
####################################################################
Aust06 <- read.csv("H:\\Katie\\PhD\\ABS data\\Australia 2006\\Australia 2006.csv",skip=9,header=T)
Aust11 <- read.csv("H:\\Katie\\PhD\\ABS data\\Australia 2011\\Australia 2011.csv",skip=9,header=T)
Aust16 <- read.csv("H:\\Katie\\PhD\\ABS data\\Australia 2016\\Australia 2016.csv",skip=10,header=T)

Aust<-list(Aust06,Aust11,Aust16)
rm(Aust06,Aust11,Aust16)


#####CENSUS DATASET CLEANING

#Function for filling down the blank rows in the age column
filltheblanks <- function(x, missing=""){
  rle <- rle(as.character(x))
  empty <- which(rle$value==missing)
  rle$values[empty] <- rle$value[empty-1] 
  inverse.rle(rle)
}


Aust<-lapply(Aust, function(dt) {
  #Turn the dataframes into datatables
  dt<-as.data.table(dt)
  #Replace the first two column names with age and year of arrival
  setnames(dt,1,"age") 
  setnames(dt,2,"yoa")
  #Fill down the blank rows in the age column
  dt$age<-filltheblanks(dt$age)
  #Get rid of rows that aren't needed, or replace them
  dt <- dt[age!="Age in Single Years (AGEP)"]
  dt <- dt[age!="AGEP - Age in Single Years"]
  dt <- dt[age!="AGEP Age"]
  dt <- dt[age!="(c) Commonwealth of Australia 2016", ]
  dt <- dt[age!="(c) Commonwealth of Australia 2017", ]
  dt <- dt[yoa!="YARP Year of Arrival in Australia", ]
  dt <- dt[yoa!="Year of Arrival in Australia (YARP)", ]
  dt <- dt[age!="Total", ]
  dt <- dt[age!="Data Source: 2006 Census of Population and Housing", ]
  dt <- dt[age!="Data Source: 2011 Census of Population and Housing", ]
  dt <- dt[age!="INFO", ]
  dt[dt$yoa=="Arrived 1 Jan 2011 - 9 August 2011",yoa:="2011"]
  #Get rid of the characters in age column
  dt$age <- gsub("\\D","",dt$age)
  #Make the age column numeric
  dt$age<-as.numeric(dt$age)
  #Find the highest year of arrival (i.e.2006/2011/2016) 
  # so I can make useful object called census year 
  dt$yoa<-as.character(dt$yoa)
  yoalist<-unique(dt$yoa)
  yoalist <- gsub("\\D","",yoalist)
  yoalist <-as.numeric(yoalist)
  censusyear<-max(yoalist, na.rm=T)
  #Make all "Overseas visitor" having arrived in the year
  #prior to the census
  #I think this is only included as a category in 2016
  if(censusyear==2006) {
    dt$yoa[dt$yoa=="Overseas visitor"]<-"Arrived 2005"
  } else if (censusyear==2011) {
    dt$yoa[dt$yoa=="Overseas visitor"]<-"Arrived 2010"
  } else if (censusyear==2016) {
    dt$yoa[dt$yoa=="Overseas visitor"]<-"Arrived 2015"
  }
  dt$yoa <- gsub("\\D","",dt$yoa)
  dt$yoa<-as.numeric(dt$yoa)
  #Reshape, of country of birth becomes one column
  dt<- melt(dt, id=c("age", "yoa"))
  ##Renaming country of birth (cob) variable
  setnames(dt,"variable","cob") 
  setnames(dt,"value","pop") 
  #unique(dt$cob)
  dt <- dt[dt$cob!="Total", ]
  dt$pop<-as.numeric(dt$pop)
  #Create year of birth (yob) column
  dt$yob<-censusyear-dt$age
  ##Creating a column of iso3 codes 
  dt$iso3<-countrycode(dt$cob, "country.name", "iso3c")
  dt
}) 

#Total Australian population
sumpop<-lapply(Aust, function(dt) {
  sum(dt$pop,na.rm=TRUE)
}) 
sumpop
#2006 19,855,249
#2011 21,507,670
#2016 23,612,655

##Fixing iso3 codes for those that didn't convert, and changing those
## that don't appear in Houben and Dodd's dataset
setwd("H:/Katie/PhD/LTBI to active TB project/R/")
source('ISO3 fix functions.R')
Aust<-lapply(Aust,iso3fixfunc)

##Removing any rows with no population 
Aust<-lapply(Aust, function(dt) {
  dt <- dt[pop!=0]
  dt
}) 

##Checking if there are any countries
# that don't have an 
# iso3 match in ARI data
#loading the ARTI/hazard data from Houben and Dodd
load("H:/Katie/PhD/LTBI project/R/Houben and Dodd/200repLARI.Rdata")
tbhaz<-as.data.table(rundata)
rm(rundata)
iso3list<-unique(tbhaz$iso3)
'%!in%' <- function(x,y)!('%in%'(x,y))
missingiso3check<- function(DT){
  DT<-DT[DT$iso3 %!in% iso3list,]
  unique(DT$cob)
}
lapply(Aust,missingiso3check)
rm(tbhaz)


#A bit more tidying of country names
Aust<-lapply(Aust, function(dt) {
  dt[dt$iso3 == "GBR","cob":="Great Britain"]
  dt[dt$iso3 == "CHN","cob":="China"]
  dt[dt$iso3 == "PHL","cob":="Philippines"]
  dt[dt$iso3 == "ZAF","cob":="South Africa"]
  dt[dt$iso3 == "NZL","cob":="New Zealand"]
  dt[dt$iso3 == "SDN","cob":="Sudan"]
  dt[dt$iso3 == "IND","cob":="India"]
  dt[dt$iso3 == "ZAF","cob":="South Africa"]
  dt[dt$iso3 == "MYS","cob":="Malaysia"]
  dt[dt$iso3 == "SOM","cob":="Philippines"]
  dt[dt$iso3 == "SOM","cob":="Vietnam"]
  dt[dt$iso3 == "SOM","cob":="Somalia"]
  dt[dt$iso3 == "RUS","cob":="Russian Federation"]
  dt[dt$iso3 == "COD","cob":="Democratic Republic of Congo"]
  dt[dt$iso3 == "THA","cob":="Thailand"]
  dt[dt$iso3 == "PNG","cob":="Papua New Guinea"]
  dt[dt$iso3 == "HKG","cob":="Hong Kong"]
  dt[dt$iso3 == "LKA","cob":="Sri Lanka"]
  dt[dt$iso3 == "KOR","cob":="South Korea"]
  dt
}) 

#aggregate all the rows that have ended being the same
Aust<-lapply(Aust, function(dt) {
  dt <- dt[, list(pop=sum(pop)), 
               by=c("age", "yoa", "cob", "yob", "iso3")]
  dt
}) 

#Recheck population
lapply(Aust, function(dt) {
  sum(dt$pop)
}) 
#total population
#2006 - 19,855,249
#2011 - 21,507,670
#2016 - 23,612,655


#####MISSING DATA CHECK

lapply(Aust, function(dt) {
  sum(dt$pop[is.na(dt$yoa)&(dt$iso3!="AUS")& !is.na(dt$iso3)],na.rm=TRUE)
})
#Missing yoa only, not Australian: 
#2006 - 210,725
#2011 - 234,766
#2016 - 207,978

lapply(Aust, function(dt) {
  sum(dt$pop[is.na(dt$iso3)& !is.na(dt$yoa)],na.rm=TRUE)
})
#Missing iso3 only: 
#2006 - 9,184
#2011 - 8318
#2016 - 327,069

lapply(Aust, function(dt) {
  sum(dt$pop[is.na(dt$iso3)& is.na(dt$yoa)],na.rm=TRUE)
})
#Missing both yoa and iso3 only: 
#2006 - 1,367,763
#2011 - 1,197,204
#2016 - 1,624,443

##Creating separate Australian-born data
Austborn <-lapply(Aust, function(dt) {
  dt<- subset(dt, dt$iso3=="AUS")
  dt$yoa<-NULL
  dt
})
lapply(Austborn, function(dt) {
  sum(dt$pop)
})
#2006 - 14,072,762
#2011 - 15,021,457
#2016 - 15,615,550
#str(Austborn)

##Subsetting all rows with NA in yoa, but a country of birth, 
#and all those with yoa, but not country of birth
#i.e. they were born overseas but we don't know when 
#they migrated, or where they migrated from.
AustNA <-lapply(Aust, function(dt) {
  dt<-subset(dt, (is.na(dt$yoa)& dt$iso!="AUS")|is.na(dt$iso3))
})
lapply(AustNA, function(dt) {
  sum(dt$pop)
})
#2006 - 1,587,672
#2011 - 1,440,288
#2016 - 2,159,490



##Removing all rows with anything missing 
Aust <-lapply(Aust, function(dt) {
  dt<-subset(dt,!is.na(dt$yoa) & !is.na(dt$iso3))
})
lapply(Aust, function(dt) {
  sum(dt$pop,na.rm=TRUE)
})  
#2006 - 4,194,815
#2011 - 5,045,925
#2016 - 5,837,615
rm(AustNA)


######CALCULATING ANNUAL RISKS OF INFECTION (ARTI)
###THE JUDICIOUS MERGE SOLUTION

#AUSTRALIAN BORN - 5000 replicate DATASET
#Sourcing the ARTIcalc functions
setwd("H:/Katie/PhD/LTBI to active TB project/R/")
source('ARTIcalc functions.R')
Austborn <-lapply(Austborn, AustbornTBriskcalc, 5000)


#THE REST OF THE WORLD
#5000 replicate ones
#Sourcing the ARTIcalc functions
setwd("H:/Katie/PhD/LTBI to active TB project/R/")
source('ARTIcalc functions.R')
#### DT needs nine columns: "year","iso3","age","yoa","sex",
###"pop", "yob", "mani", "cob"
#### Or 8 columns without "yob"
Aust <-lapply(Aust, function(dt) {
  censusyear<-max(dt$yob, na.rm=T)
  dt$year<-censusyear
  dt$mani<-1
  dt$sex<-1
  dt
})

#I need to perform aricalc function 
#on each data table separately 
#or I reach memory limits:
Aust06<-Aust[[1]]
Aust11<-Aust[[2]]
Aust16<-Aust[[3]]
rm(Aust)

TBriskcalc_5K200rep<- function(DT){
  DT<-as.data.table(DT)
  year<-DT$year[1]
  censusyear<-DT$year[1]
  DT$yob<-year-DT$age
  ##Create the 5000 rep and 200 rep datasets 
  newcnty<-c("CHN", "GBR", "IND", "PHL", "VNM")
  DT5k<-DT[DT$iso3%in% newcnty,]
  '%!in%' <- function(x,y)!('%in%'(x,y))
  DT200<-DT[DT$iso3%!in% newcnty,]
  ###calculate the risks using 5000 an 200 rep dataset
  #Sourcing the functions
  setwd("H:/Katie/PhD/LTBI to active TB project/R/")
  source('ARTIcalc functions.R')
  ##################################################################################
  ####CHOOSE WHICH ARTI CALC IS NEEDED
  ####NORMAL:TBriskcalc
  ####ARTI RISK AT ARRIVAL (i.e. Asutralian ARTIs are zero: TBriskcalc_onarrival
  ####FOR RECENT RISK ANALYSIS: TBriskcalc_allbutrecent
  ##################################################################################
  DT5k<-TBriskcalc(DT5k,5000)
  DT200<-TBriskcalc(DT200,200)
  #DT5k<-TBriskcalc_onarrival(DT5k,5000)
  #DT200<-TBriskcalc_onarrival(DT200,200)
  #DT5k<-TBriskcalc_allbutrecent(DT5k,200)
  #DT200<-TBriskcalc_allbutrecent(DT200,200)
  DT200[,year:=year]
  DT5k[,year:=year]
  DT<-list(DT200,DT5k)
}

Austrisk06<-TBriskcalc_5K200rep(Aust06)
Austrisk11<-TBriskcalc_5K200rep(Aust11)
Austrisk16<-TBriskcalc_5K200rep(Aust16)


gc()

#Get rid of the columns we don't need
Austrisk06 <-lapply(Austrisk06, function(dt) {
  dt[, "mani":=NULL]
  dt[, "sex":=NULL]
  dt
})

Austrisk11 <-lapply(Austrisk11, function(dt) {
  dt[, "mani":=NULL]
  dt[, "sex":=NULL]
  dt
})

Austrisk16 <-lapply(Austrisk16, function(dt) {
  dt[, "mani":=NULL]
  dt[, "sex":=NULL]
  dt
})

Aust06ex <- Austrisk06[[2]]
Aust11ex <- Austrisk11[[2]]
Aust16ex <- Austrisk16[[2]]

Aust06 <- Austrisk06[[1]]
Aust11 <- Austrisk11[[1]]
Aust16 <- Austrisk16[[1]]

Austborn06 <- Austborn[[1]]
Austborn11 <- Austborn[[2]]
Austborn16 <- Austborn[[3]]

rm(Austborn, sumpop, Austrisk06,Austrisk11,Austrisk16)

#Tidying datatables and putting them into lists
Austborn06$yoa<-NA
Austborn11$yoa<-NA
Austborn16$yoa<-NA
Austborn06<-Austborn06[,c(1,2,5007,3:5006)]
Austborn11<-Austborn11[,c(1,2,5007,3:5006)]
Austborn16<-Austborn16[,c(1,2,5007,3:5006)]

Abrisk5k <- list("Ab06"<-Austborn06,"Ab11"<-Austborn11,"Ab16"<-Austborn16)
Arisk5k <- list("Ax06"<-Aust06ex,"Ax11"<-Aust11ex,"Ax16"<-Aust16ex)
rm(Ab06)
rm(Ab11)
rm(Ab16)
rm(Ax06)
rm(Ax11)
rm(Ax16)

Arisk200 <- list("A06"<-Aust06,"A11"<-Aust11,"A16"<-Aust16)
rm(A06)
rm(A11)
rm(A16)

Abrisk5k<-lapply(Abrisk5k, function(df) {
  DT<-as.data.table(df)
  DT[, "id":=NULL]
  DT
}) 

Arisk5k<-lapply(Arisk5k, function(df) {
  DT<-as.data.table(df)
  DT[, "id":=NULL]
  DT
}) 

Arisk200<-lapply(Arisk200, function(df) {
  DT<-as.data.table(df)
  DT[, "id":=NULL]
  DT
}) 

# Turn the LTBI risks into numbers with LTBI
risktonumfunction<-function(dt) {
  if(ncol(dt)>4999) {
    repnumber <- "5000"
  } else {
    repnumber <- "200"
  }
  firstrep<-"V1"
  lastrep<-paste0(c("V"),repnumber)
  g<-which(colnames(dt)==firstrep)
  h<-which(colnames(dt)==lastrep)
  dt[ , (g:h) :=lapply(.SD, "*", dt$pop), .SDcols =c(g:h)]
  dt
}
Anum5k<-copy(Arisk5k)
Abnum5k<-copy(Abrisk5k)
Anum200<-copy(Arisk200)

Anum5k<-lapply(Anum5k,risktonumfunction)
Abnum5k<-lapply(Abnum5k,risktonumfunction)
Anum200<-lapply(Anum200,risktonumfunction)

rm(Austborn06,Austborn11,Austborn16)
rm(Aust06ex,Aust11ex,Aust16ex)
rm(Aust06,Aust11,Aust16)

#Saving files to disk is impossible. 
#They are too large.







