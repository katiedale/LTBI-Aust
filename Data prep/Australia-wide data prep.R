#install.packages("swirl")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages ("quantmod")
#install.packages ("sm")
#install.packages("countrycode")
#install.packages("stringr")
#install.packages("reshape2")
#install.packages("zoo")
#install.packages("data.table")
#install.packages("plyr")
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

#############################################################################
##NEED TO CHANGE THE CENSUS YEAR VALUES BELOW AND IN THE BIG ANALYSIS LOOPS##
#############################################################################


#Loading the the Australian Census data - pick a year and choose relevant location
####################################################################
#Aust <- read.csv("H:\\Katie\\PhD\\ABS data\\Australia 2006\\Australia 2006.csv",skip=9,header=T)
#Aust <- read.csv("H:\\Katie\\PhD\\ABS data\\Australia 2011\\Australia 2011.csv",skip=9,header=T)
#Aust <- read.csv("H:\\Katie\\PhD\\ABS data\\Australia 2016\\Australia 2016.csv",skip=10,header=T)
#Aust<-read.csv('data/Australia 2006.csv',skip=9,header=T)
#Aust<-read.csv('data/Australia 2011.csv',skip=9,header=T)
####################################################################

#Replace the first two column names to become age and yoa
colnames(Aust)[1] <- "age"
colnames(Aust)[2] <- "yoa"

# Function for filling down the blank rows in the age column
filltheblanks <- function(x, missing=""){
  rle <- rle(as.character(x))
  empty <- which(rle$value==missing)
  rle$values[empty] <- rle$value[empty-1] 
  inverse.rle(rle)
}
Aust$age <- filltheblanks(Aust$age)

#Getting rid of rows that aren't needed
Aust <- Aust[Aust$age!="AGEP - Age in Single Years", ]
Aust <- Aust[Aust$age!="(c) Commonwealth of Australia 2016", ]
Aust <- Aust[Aust$yoa!="Year of Arrival in Australia (YARP)", ]
Aust <- Aust[Aust$age!="Total", ]
Aust <- Aust[Aust$age!="Data Source: 2006 Census of Population and Housing", ]
Aust <- Aust[Aust$age!="Data Source: 2011 Census of Population and Housing", ]
Aust <- Aust[Aust$age!="INFO", ]

#Getting rid of the characters in age and yoa and pop columns and make them all numeric
Aust$age <- gsub("\\D","",Aust$age)
Aust$age<-as.numeric(Aust$age)
Aust$yoa<-as.character(Aust$yoa)
Aust$yoa[Aust$yoa=="Arrived 1 Jan 2011 - 9 August 2011"]<-"2011"
Aust$yoa <- gsub("\\D","",Aust$yoa)
Aust$yoa<-as.numeric(Aust$yoa)


#Reshaping
Aust<- melt(Aust, id=c("age", "yoa"))

##Renaming cob variable
names(Aust)[names(Aust)=="variable"] <- "cob"
names(Aust)[names(Aust)=="value"] <- "pop"
unique(Aust$cob)
Aust <- Aust[Aust$cob!="Total", ]
Aust$pop<-as.numeric(Aust$pop)

##Creating a variable with 2006/2011/2016 in it, 
# so I can minus the age values from it to obtain year of birth (yob)
censusyear<-max(Aust$yoa, na.rm=T)
  if(censusyear==2006) {
    Aust$censusdate<-2006
  } else if (censusyear==2011) {
    Aust$censusdate<-2011
  } else if (censusyear==2016) {
    Aust$censusdate<-2016
  }

Aust$yob<-Aust$censusdate-Aust$age
Aust$censusdate<-NULL
str(Aust)

unique(Aust$cob)

##Sorting cob column 
Aust$cob<-as.character(Aust$cob)
Aust$cob[Aust$cob == "Not.stated"]<-NA
Aust$cob[Aust$cob == "Not applicable"]<-NA
Aust$cob[Aust$cob == "Overseas visitor"]<-NA
Aust$cob[Aust$cob == "Overseas.visitor"]<-NA
Aust$cob[Aust$cob == "Not.elsewhere.classified"]<-NA
Aust$cob[Aust$cob == "Inadequately.described"]<-NA
Aust$cob[Aust$cob == "At.sea"]<-NA

##Creating a column of iso3 codes
Aust$iso3<-countrycode(Aust$cob, "country.name", "iso3c")

##Checking that all countries were converted okay
Aust$cob[is.na(Aust$cobiso)]
unique(Aust$cob)

##Fixing those that didn't convert to iso3 codes
Aust$iso3[Aust$cob == "China..excludes.SARs.and.Taiwan.Province."] <- "CHN"
Aust$iso3[Aust$cob == "Japan.and.the.Koreas..nfd"] <- "JPN"
Aust$iso3[Aust$cob == "United.Kingdom..Channel.Islands.and.Isle.of.Man..nfd"] <- "GBR"
Aust$iso3[Aust$cob == "Virgin.Islands..United.States"] <- "USA"
Aust$iso3[Aust$cob == "Americas..nfd"]<-"USA"
Aust$iso3[Aust$cob == "British.Antarctic.Territory"]<-"GBR"
Aust$iso3[Aust$cob == "Caribbean..nfd"]<-"BHS"
Aust$iso3[Aust$cob == "Central.America..nfd"]<-"CRI"
Aust$iso3[Aust$cob == "Central.and.West.Africa..nfd"]<-"CMR"
Aust$iso3[Aust$cob == "Central.Asia..nfd"]<-"KAZ"
Aust$iso3[Aust$cob == "Eastern.Europe..nfd"]<-"POL"
Aust$iso3[Aust$cob == "England"]<-"GBR"
Aust$iso3[Aust$cob == "Japan.and.the.Koreas..nfd"]<-"JPN"
Aust$iso3[Aust$cob == "Kosovo"]<-"SRB"
Aust$iso3[Aust$cob == "Mainland.South.East.Asia..nfd"]<-"THA"
Aust$iso3[Aust$cob == "Maritime.South.East.Asia..nfd"]<-"PHL"
Aust$iso3[Aust$cob == "Melanesia..nfd"]<-"VUT"
Aust$iso3[Aust$cob == "Micronesia..nfd"]<-"KIR"
Aust$iso3[Aust$cob == "Middle.East..nfd"]<-"SAU"
Aust$iso3[Aust$cob == "North.Africa..nfd"]<-"MAR"
Aust$iso3[Aust$cob == "North.Africa.and.the.Middle.East..nfd"]<-"EGY"
Aust$iso3[Aust$cob == "North.East.Asia..nfd"]<-"MNG"
Aust$iso3[Aust$cob == "North.West.Europe..nfd"]<-"FRA"
Aust$iso3[Aust$cob == "Northern.America..nfd"]<-"USA"
Aust$iso3[Aust$cob == "Northern.Europe..nfd"]<-"NLD"
Aust$iso3[Aust$cob == "Polynesia..excludes.Hawaii...nec"]<-"WSM"
Aust$iso3[Aust$cob == "Polynesia..excludes.Hawaii...nfd"]<-"WSM"
Aust$iso3[Aust$cob == "Scotland South.America..nec"]<-"GBR"
Aust$iso3[Aust$cob == "South.America..nfd"]<-"BRA"
Aust$iso3[Aust$cob == "South.East.Asia..nfd"]<-"THA"
Aust$iso3[Aust$cob == "South.Eastern.Europe..nfd"]<-"MKD"
Aust$iso3[Aust$cob == "Southern.and.Central.Asia..nfd"]<-"AFG"
Aust$iso3[Aust$cob == "Southern.and.East.Africa..nec"]<-"SOM"
Aust$iso3[Aust$cob == "Southern.and.East.Africa..nfd"]<-"SOM"
Aust$iso3[Aust$cob == "Southern.and.Eastern.Europe..nfd"]<-"MKD"
Aust$iso3[Aust$cob == "Southern.Asia..nfd"]<-"IND"
Aust$iso3[Aust$cob == "Southern.Europe..nfd"]<-"GRC"
Aust$iso3[Aust$cob == "Spanish.North.Africa"]<-"ESP"
Aust$iso3[Aust$cob == "Sub.Saharan.Africa..nfd"]<-"SOM"
Aust$iso3[Aust$cob == "United.Kingdom..Channel.Islands.and.Isle.of.Man..nf"]<-"GBR"
Aust$iso3[Aust$cob == "Scotland"]<-"GBR"
Aust$iso3[Aust$cob == "Wales"]<-"GBR"
Aust$iso3[Aust$cob == "Western.Europe..nfd"]<-"FRA"
Aust$iso3[Aust$cob == "South.America..nec"]<-"BRA"
Aust$iso3[Aust$cob == "Not.stated"]<-NA
Aust$iso3[Aust$cob == "Overseas.visitor"]<-NA
Aust$iso3[Aust$cob == "Inadequately.described"]<-NA
Aust$iso3[Aust$cob == "At.sea"]<-NA
Aust$iso3[Aust$cob == "China..excludes.SARs.and.Taiwan."]<-"CHN"
Aust$iso3[Aust$cob == "Australia..includes.External.Territories...nfd"]<-"AUS" 
Aust$iso3[Aust$cob == "Norfolk.Island"]<-"AUS" 
Aust$iso3[Aust$cob == "Australian.External.Territories..nec"]<-"AUS" 
unique(Aust$cob)

##Removing any rows with no population 
Aust <- Aust[Aust$pop!= 0, ]

#Loading the hazard data - choose relevant location
####################################################################
#load("H:\\Katie\\PhD\\Houben and Dodd\\AAA_rundata-1.Rdata")
#load('data/AAA_rundata-1.Rdata')
####################################################################
tbhaz<-rundata
rm(rundata)

##Create an FOI variable
tbhaz$FOI <- exp(tbhaz$lari)
tbhaz ["lari"]<- NULL

##For 2016 data only subset all 2014 rows so I can create 
# 2015 and 2016 data with the same data and then append them on
if(censusyear==2016) {
  tbhaz2014 <- subset(tbhaz, tbhaz$year==2014)
  tbhaz2015 <- tbhaz2014
  tbhaz2015$year[tbhaz2015$year==2014] <- 2015
  tbhaz2016 <- tbhaz2014
  tbhaz2016$year[tbhaz2016$year==2014] <- 2016
  rm(tbhaz2014)
  tbhaz1516<-rbind(tbhaz2015,tbhaz2016)
  tbhaz<-rbind(tbhaz1516,tbhaz)
  tbhaz<-tbhaz[order(tbhaz$iso3,tbhaz$replicate,tbhaz$year), ]
  rownames(tbhaz) <- seq(length=nrow(tbhaz))
  rm(tbhaz1516)
  rm(tbhaz2015)
  rm(tbhaz2016)
}


##Comparing ISO3 codes in TBhaz versus census
missinginours<-setdiff(tbhaz$iso3, Aust$iso3)
missinginours<-countrycode(missinginours, "iso3c","country.name")
missinginours

missingintheirs<-setdiff(Aust$iso3,tbhaz$iso3)
missingintheirs<-countrycode(missingintheirs, "iso3c","country.name")
missingintheirs

rm(missinginours)
rm(missingintheirs)

##Converting cob not represented in Dodd's data to others that are
Aust$iso3[Aust$iso3 == "IMN"] <- "GBR" #Isle of Man
Aust$iso3[Aust$iso3 == "GGY"] <- "GBR" #Guernsey
Aust$iso3[Aust$iso3 == "JEY"] <- "GBR" #Jersey
Aust$iso3[Aust$iso3 == "GIB"] <- "ESP" #Gibraltar
Aust$iso3[Aust$iso3 == "TWN"] <- "CHN" #Taiwan
Aust$iso3[Aust$iso3 == "SHN"] <- "GBR" #Saint Helena
Aust$iso3[Aust$iso3 == "FLK"] <- "GBR" #Falkland Islands (Malvinas)
Aust$iso3[Aust$iso3 == "REU"] <- "FRA" #Reunion
Aust$iso3[Aust$iso3 == "NFK"] <- "AUS" #Norfolk Island
Aust$iso3[Aust$iso3 == "ATA"] <- "GBR" #Antarctica

Aust$iso3[Aust$iso3 == "LIE"] <- "AUT" #Liechtenstein
Aust$iso3[Aust$iso3 == "FRO"] <- "NOR" #Faroe Islands
Aust$iso3[Aust$iso3 == "VAT"] <- "ITA" #Holy See (Vatican City State)
Aust$iso3[Aust$iso3 == "ESH"] <- "MAR" #Western Sahara
Aust$iso3[Aust$iso3 == "SPM"] <- "FRA" #Saint Pierre and Miquelon
Aust$iso3[Aust$iso3 == "TWN"] <- "CHN" #Taiwan, Province of China
Aust$iso3[Aust$iso3 == "GLP"] <- "FRA" #Guadeloupe
Aust$iso3[Aust$iso3 == "GUF"] <- "FRA" #French Guiana
Aust$iso3[Aust$iso3 == "MTQ"] <- "FRA" #Martinique
Aust$iso3[Aust$iso3 == "MYT"] <- "FRA" #Mayotte
Aust$iso3[Aust$iso3 == "SHN"] <- "GBR" #Saint Helena, Ascension and Tristan da Cunha


sum(Aust$pop,na.rm=TRUE)
#2006 - 19,855,249
#2011 - 21,507,670
#2016 - 23,612,655

unique(Aust$cob)

#Finding out what's missing
sum(Aust$pop[(is.na(Aust$yoa)&(Aust$iso3!="AUS"))|is.na(Aust$iso3)|(!is.na(Aust$yoa)& Aust$iso=="AUS"& Aust$cob!="Norfolk.Island")],na.rm=TRUE)
#Missing either yoa if not Australian, or iso3
#2006 - 1,589,616
#2011 - 1,440,288
#2016 - 2,159,490

sum(Aust$pop[is.na(Aust$yoa)&(Aust$iso3!="AUS")& !is.na(Aust$iso3)],na.rm=TRUE)
#Missing yoa only, not Australian: 
#2006 - 210,634
#2011 - 234,766
#2016 - 207,978
sum(Aust$pop[is.na(Aust$iso3)& !is.na(Aust$yoa)],na.rm=TRUE)
#Missing iso3 only: 
#2006 - 11,128
#2011 - 8,318
#2016 - 11,553
sum(Aust$pop[is.na(Aust$iso3)&is.na(Aust$yoa) ],na.rm=TRUE)
#Missing iso3 and yoa: 
#2006 - 1,367,854
#2011 - 1,197,204
#2016 - 1,939,959

#Changing the FOI for the census year so that it represents 
# only part of the year to the date of the census
tbhaz$testari<-tbhaz$FOI*(220/365.2425)
if(censusyear==2006) {
  tbhaz$FOI[tbhaz$year==2006]<-tbhaz$testari[tbhaz$year==2006]
  tbhaz["testari"]<- NULL
} else if (censusyear==2011) {
  tbhaz$FOI[tbhaz$year==2011]<-tbhaz$testari[tbhaz$year==2011]
  tbhaz["testari"]<- NULL
} else if (censusyear==2016) {
  tbhaz$FOI[tbhaz$year==2016]<-tbhaz$testari[tbhaz$year==2016]
  tbhaz["testari"]<- NULL
}


#Creating an FOI variable that is halved, to apply to year of birth and year of arrival
tbhaz$halfFOI<- tbhaz$FOI/2

#Creating an FOI variable that is quartered, to apply to year of birth and year of arrival
tbhaz$quartFOI<- tbhaz$FOI/4

##Subsetting all rows with NA in yoa and those born in Ausralia 
Austborn <- subset(Aust, Aust$iso3=="AUS")
Austborn$yoa<-NULL
sum(Austborn$pop)
#2006 - 14,072,762
#2011 - 15,021,457
#2016 - 15,072,762
str(Austborn)

##Subsetting all rows with NA in yoa, but a country of birth, 
#and all those with yoa, but not country of birth
#i.e. they were born overseas but we don't know when 
#they migrated, or where they migrated from.
AustNA <- subset(Aust, (is.na(Aust$yoa) & Aust$iso3!="AUS")|(Aust$iso3=="NA"))
sum(AustNA$pop,na.rm=TRUE)
#2006 - 
#2011 - 234,766
#2016 - 

##Removing all rows with NA in yoa, i.e. those from Australia mostly
Aust <- subset(Aust,!is.na(Aust$yoa) & Aust$iso3!="AUS" & !is.na(Aust$iso3))
sum(Aust$pop,na.rm=TRUE)
#2006 - 4,192,871
#2011 - 5,045,925
#2016 - 5,837,615
sum(Austborn$pop,na.rm=TRUE)
#2006 - 14,072,762
#2011 - 15,021,457
#2016 - 15,615,550
#altogether 2006 -  18,265,633
#altogether 2011 -  20,067,382
#altogether 2016 -  21,453,165

#Reorder columns by country (alphabetical), yob (ascending), yoa (ascending) 
Aust<-Aust[order(Aust$iso3,Aust$yob,Aust$yoa), ]
Austborn<-Austborn[order(Austborn$yob), ]
##Creating unique IDs
Aust$id <- seq.int(nrow(Aust))
Austborn$id <- seq.int(nrow(Austborn))


#Filling in the risk values based on the Australia wide data set
#for overseasborn
#Austt<-lapply(Aust, function(df) {
#  df<-merge(df, [!duplicated(Austrisk06t[c("yoa","iso3","yob")]),], by=c("yoa","iso3","yob"),  all.x = TRUE)
#  df
#})

#for Australian born
#AustAustt<-merge(Austborn, AustriskAustt, by=c("yob"))



### AUSTRALIAN BIG REPLICATE LOOP
##FUNCTIONS THAT WILL BE NEEDED TO ADD HAZARDS
## Full haz function
haz.calc<-function(tbhz, fyear,fhaz) {
  haz<-tbhz[(tbhz$year==fyear), "FOI"]
  haz<-fhaz+haz
  return(haz)
}
## Half haz function 
halfhaz.calc<-function(tbhz,fyear,fhaz) {
  halfhaz<-tbhz[(tbhz$year==fyear), "halfFOI"]
  haz<-fhaz+halfhaz
  return(haz)
}
## Quart haz function
quarthaz.calc<-function(tbhz,fyear,fhaz) {
  quarthaz<-tbhz[(tbhz$year==fyear), "quartFOI"]
  haz<-fhaz+quarthaz
  return(haz)
}

##Creating matrices to accept all of the hazard values
numrwA <- nrow(Austborn)
rawhazAust<- matrix(data=0, nrow = numrwA, ncol = 200)

##Hazard calculation for Austborn
tbhzAust <- tbhaz[tbhaz$iso3=="AUS",]  

lapply(1:200,function(x){
  tbhz <- tbhzAust[tbhzAust$replicate==x,]
  for(i in 1:numrwA) {
    yobt<-Austborn[i,]$yob
    hazt<-0
    #set the year of haz to the year following the year of birth
    yearofhaz<-yobt+1
    ####################################################################
    #If they were born and arrived in 2006 then apply halfari for both cob and Aust
    if(yobt==2006) {
      hazt<-halfhaz.calc(tbhz,2006,hazt) 
      ####################################################################
      #Set the haz as the half FOI in the year of birth
    } else if (yobt<1934) {
      #for <1934 group - setting the hazard to the halfFOI in the year of birth:
      hazt<-halfhaz.calc(tbhz,1934,hazt)
    } else {
      hazt<-halfhaz.calc(tbhz,yobt,hazt)
    }
    ####################################################################
    #within each row, for years between year of birth and 2006:
    for (k in yearofhaz:2006) {  
      if(yearofhaz>2006){
        ####################################################################
        next
      } else if(yearofhaz<1934){
        hazt<-haz.calc(tbhz,1934,hazt)
        yearofhaz<-yearofhaz+1
      } else {
        hazt<-haz.calc(tbhz,yearofhaz,hazt)
        yearofhaz<-yearofhaz+1
      }
    }
    rawhazAust[i,x]<<-hazt
  }
})


#Create function to turn the hazards into risks
haztorisk<-function(v) {
  1-(exp(-v))
}

#Apply the above function to every element of the matrix and make it 
#a dataframe again
rawhazAust<- as.data.frame(rawhazAust)
rawriskAust <- sapply(rawhazAust[,1:200], haztorisk)
rawriskAust<- as.data.frame(rawriskAust)

##Creating unique IDs for rawhazAust
rawriskAust$id <- seq.int(nrow(rawriskAust))

#Merging it to the Australian census data information
Austbornrisk <- merge(Austborn, rawriskAust,by=c('id'))



##FOR THE REST OF THE WORLD

##Creating matrices to accept all of the hazard values
numrw <- nrow(Aust)
rawhaz <- matrix(data=0, nrow = numrw, ncol = 200)

##Creating a list of the countries that the loops have to cycle through.
ctrylist <- unique(as.character(Aust$iso3))

for (l in ctrylist) {
  Austcn <- Aust[Aust$iso3==l,]
  tbhzcn <- tbhaz[tbhaz$iso3==l,]
  numRowscn <- nrow(Austcn)
  lapply(1:200,function(x){
    ##Creating dataframes only with replicate x
    tbhz <- tbhzcn[tbhzcn$replicate==x,]
    tbhzA <- tbhzAust[tbhzAust$replicate==x,]
    for(i in 1:numRowscn) {
      yobt<-Austcn[i,]$yob[1]
      yoat<-Austcn[i,]$yoa[1]
      id<-Austcn[i,]$id[1]
      hazt<-0
      #set the years before arrival to the number of years between yob and yoa
      yearsbeforearrival<-yoat-yobt-1
      #set the year of haz to the year following the year of birth
      yearofhaz<-yobt+1
      ##YOB
      #If they were born and arrived in the same year then set the hazard in that year as quartfhaz for both cob and AUS
      if ((yobt==yoat) & (yobt>1933)) {
        hazt<-quarthaz.calc(tbhz,yobt,hazt)
        hazt<-quarthaz.calc(tbhzA,yoat,hazt)
      } else if ((yobt==yoat) & (yobt<1934)){
        hazt<-quarthaz.calc(tbhz,1934,hazt)
        hazt<-quarthaz.calc(tbhzA,1934,hazt)  
        #Otherwise set the hazard in the year of birth as the half FOI 
      } else if ((yobt!=yoat) & (yobt<1934)) {
        hazt<-halfhaz.calc(tbhz,1934, hazt)
      } else {
        hazt<-halfhaz.calc(tbhz,yobt, hazt)
      }
      ##YEARS BETWEEN YOB AND YOA
      if(yearsbeforearrival>0) {
        for (j in 1:yearsbeforearrival) { 
          if (yearofhaz<1934){
            hazt<-haz.calc(tbhz,1934,hazt)
            yearofhaz<-yearofhaz+1
          } else {        
            hazt<-haz.calc(tbhz,yearofhaz,hazt)
            yearofhaz<-yearofhaz+1        
          }
        }
      }
      ##YOA
      #within each row, for year of arrival:
      #If they were born and arrived in the same year then I calculated 
      #their haz in yoa above already.
      if(yobt!=yoat){
        if (yoat<1934){
          hazt<-halfhaz.calc(tbhz,1934,hazt)
          hazt<-halfhaz.calc(tbhzA,1934,hazt)
          yearofhaz<-yoat+1
        } else {        
          hazt<-halfhaz.calc(tbhz,yoat,hazt)
          hazt<-halfhaz.calc(tbhzA,yoat,hazt)
          yearofhaz<-yoat+1
        }
      }
      ####################################################################
      ##Between YOA and 2006
      #within each row, for years between year of arrival and 2006:
      if(yearofhaz<2007){
        for (k in yearofhaz:2006) {  
          ####################################################################
          if (yearofhaz<1934){
            hazt<-haz.calc(tbhzA,1934,hazt)
            yearofhaz<-yearofhaz+1
          } else {
            hazt<-haz.calc(tbhzA,yearofhaz,hazt)
            yearofhaz<-yearofhaz+1
          }
        }
      }
      rawhaz[id,x]<<-hazt
    }
  })
}


#Create function to turn the hazards into risks
haztorisk<-function(v) {
  1-(exp(-v))
}

#Apply the above function to every element of the matrix and make it 
# a dataframe again
rawhazdf<- as.data.frame(rawhaz)
rawrisk <- sapply(rawhazdf[,1:200], haztorisk)
rawriskdf<- as.data.frame(rawrisk)

##Creating unique IDs for rawriskdf, so I can merge it with census data
rawriskdf$id <- seq.int(nrow(rawriskdf))

#Merging it to the Australia census data
Austrisk <- merge(Aust, rawriskdf,by=c('id'))

#Combining overseasborn and Australian born data sets
Austbornrisk$id<-NULL
Austrisk$id<-NULL
Austbornrisk$yoa<-NA
Austbornrisk$cob<-"Australia"

Austrisk<-rbind(Austrisk,Austbornrisk)


#Export it to csv
write.csv(Austrisk,file="Austrisk.csv") # keeps the rownames
