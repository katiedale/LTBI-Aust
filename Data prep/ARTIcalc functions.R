

####Functions for calculating ARI values for census groups


### AUSTRALIAN BORN - 5000 or 200 replicate DATASET
#### This takes a dataframe of Australian-botn residents and 
## calculates their ARTI's with either the 200 or 5000 replicate hazard data
## depending on what is specified in the function
#### DT needs five columns: "iso3","age","pop","yob", "cob" 
AustbornTBriskcalc <-function(dt,repnumber) {
  reps<-as.character(c("1":repnumber))
  firstrep<-"1"
  lastrep<-repnumber
  censusyear<-max(dt$yob, na.rm=T)
  dt<-as.data.table(dt)
  dt<-dt[order(dt$yob), ]
  ##Creating unique IDs
  dt$id <- seq.int(nrow(dt))
  #Loading the hazard data - choose relevant location
  ####################################################################
  precede <- paste(repnumber,'repLARI.Rdata',sep = "")
  mm<-"Data/"
  filename <- paste(mm,precede,sep = "") 
  loading <- function(dataset){
    merchants <- load(dataset)
    return(get(merchants))
  }
  tbhaz<-as.data.table(loading(filename))
  ##Create an FOI variable from the log ARI (lari) variable.
  tbhaz[, "FOI":=exp(lari)]
  tbhaz[, "lari":=NULL]
  tbhaz<-subset(tbhaz,iso3=="AUS")
  ##Subset all 2014 rows so I can create 
  # 2015 and 2016 data with the same data and then append them on
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
  ##For pre-1934 data only, subset all 1934 rows so I can create 
  # pre-1934 data and then append them on
  tbhaz1934 <- subset(tbhaz, tbhaz$year==1934)
  tbhaz1934<-tbhaz1934[,1:4]
  df<-tbhaz1934
  for(i in 1:45) {
    dfnew<-tbhaz1934
    dfnew$year[dfnew$year==1934] <- 1934-i
    df<-rbind(dfnew,df)
  }
  df<-df[df$year!=1934,]
  tbhaz<-rbind(df,tbhaz)
  tbhaz<-tbhaz[order(tbhaz$iso3,tbhaz$replicate,tbhaz$year), ]
  rownames(tbhaz) <- seq(length=nrow(tbhaz))
  rm(tbhaz1934)
  rm(df)
  rm(dfnew)
  #Changing the FOI for the census year so that it represents 
  # only part of the year to the date of the census
  tbhaz$testari<-tbhaz$FOI*(220/365.2425)
  if(censusyear==2006) {
    tbhaz$FOI[tbhaz$year==2006]<-tbhaz$testari[tbhaz$year==2006]
  } else if (censusyear==2011) {
    tbhaz$FOI[tbhaz$year==2011]<-tbhaz$testari[tbhaz$year==2011]
  } else if (censusyear==2016) {
    tbhaz$FOI[tbhaz$year==2016]<-tbhaz$testari[tbhaz$year==2016]
  }
  tbhaz[, "testari":=NULL]
  tbhaz[, cumhaz := cumsum(FOI), by=list(iso3, replicate)]
  tbhzAust<-as.data.table(tbhaz)
  ##Order them all properly
  dt<-dt[order(yob)]
  #Creating the year of birth data table (and dividing the hazards in half)...
  tbhazl<-tbhzAust[,c("year","iso3","replicate","FOI")]
  tbhazl<-dcast(tbhazl, year+iso3~replicate)
  yob<-merge(dt, tbhazl, by.x = c("yob","iso3"), 
             by.y = c("year","iso3"), all.x=TRUE, all.y=FALSE)
  yob<-data.table(yob)
  yob[ , as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.5), .SDcols =reps]
  yob<-yob[order(iso3,yob)]
  #Years between year of birth and census year ...
  # Subtracting the cumulative hazard in the census year from the 
  # cumulative hazard in the year of birth
  tbhazl<-tbhzAust[,c("year","iso3","replicate","cumhaz")]
  tbhazl<-dcast(tbhazl, year~replicate)
  # creating data table with cumulative hazard in the year of birth
  yobcmsm<-merge(dt, tbhazl, by.x = c("yob"), by.y = c("year"), 
                 all.x=TRUE, all.y=FALSE)
  yobcmsm<-yobcmsm[order(iso3,yob)]
  # creating data table with cumulative hazard in the censusyear
  dt<-dt[, censyear := censusyear]
  censcmsm<-merge(dt, tbhazl, by.x = c("censyear"), 
                  by.y = c("year"), all.x=TRUE, all.y=FALSE)
  censcmsm<-censcmsm[order(iso3,yob)]
  dt[, "censyear":=NULL]
  #subtracting one from the other
  a<-censcmsm[, reps, with=FALSE]
  b<-yobcmsm[, reps, with=FALSE]
  yobtocens<-a- b
  rm(a)
  rm(b)
  rm(censcmsm)
  rm(yobcmsm)
  #Merging it to the Australian census data information
  yobtocens$id <- seq.int(nrow(yobtocens))
  yobtocens <- merge(dt[,c("age","cob","yob","iso3","pop","id")], 
                     yobtocens,by=c('id'))
  ##Adding up all of the hazards!!! i.e. yob+yobtocens
  a<-yob[, reps, with=FALSE]
  b<-yobtocens[, reps, with=FALSE]
  dtrisk5<-a+b
  rm(a)
  rm(b)
  #Merging it to the Australian census data information
  dtrisk5$id <- seq.int(nrow(dtrisk5))
  dtrisk5 <- merge(dt[,c("age","cob","yob","iso3","pop","id")], 
                   dtrisk5,by=c('id'))
  rm(yob)
  rm(yobtocens)
  rm(tbhazl)
  #Create function to turn the hazards into risks
  haztorisk<-function(v) {
    1-(exp(-v))
  }
  #Apply the above function to every element of the datatable
  dtrisk5[ , as.character(c(firstrep:lastrep)) :=lapply(.SD,haztorisk), .SDcols =reps]
  #Rename column variables in Austrisk
  Vcolnam <- paste0(c("V"),1:repnumber)
  firstcol<-which(colnames(dtrisk5)==firstrep)
  lastcol<-which(colnames(dtrisk5)==lastrep)
  colnames(dtrisk5)[firstcol:lastcol] <- Vcolnam
  dtrisk5
}

#### This takes a dataframe of overseasborn residents and 
## calculates their ARI's with either the 200 or 5000 replicate hazard data
## depending on what is specified in the function
### DT needs nine columns: "year","iso3","age","yoa","sex","pop", "yob", "mani", "cob"
#### Or 8 columns without "yob"
## The output is the same datatable with added columns V1:V200 or V5000 
## which are the estimated risks of infection
TBriskcalc <-function(DT,repnumber) {
  reps<-as.character(c("1":repnumber))
  firstrep<-"1"
  lastrep<-repnumber
  censusyear<-DT$year[1]
  #Loading the hazard data - choose relevant location
  ####################################################################
  precede <- paste(repnumber,'repLARI.Rdata',sep = "")
  mm<-"Data/"
  filename <- paste(mm,precede,sep = "") 
  loading <- function(dataset){
    merchants <- load(dataset)
    return(get(merchants))
  }
  tbhaz<-as.data.table(loading(filename))
  ##Create an FOI variable from the log ARI (lari) variable.
  tbhaz[, "FOI":=exp(lari)]
  tbhaz[, "lari":=NULL]
  ##Subset all 2014 rows so I can create 
  # 2015 and 2016 data with the same data and then append them on
  tbhaz2015 <- subset(tbhaz, tbhaz$year==2014)
  tbhaz2015[,"year":=2015]
  tbhaz2016 <- subset(tbhaz, tbhaz$year==2014)
  tbhaz2016[,"year":=2016]
  tbhaz1516<-rbind(tbhaz2015,tbhaz2016)
  tbhaz<-rbind(tbhaz1516,tbhaz)
  tbhaz<-tbhaz[order(tbhaz$iso3,tbhaz$replicate,tbhaz$year), ]
  rownames(tbhaz) <- seq(length=nrow(tbhaz))
  rm(tbhaz1516,tbhaz2015,tbhaz2016)
  ##For pre-1934 data only, subset all 1934 rows so I can create 
  # pre-1934 data and then append them on
  tbhaz1934 <- subset(tbhaz, tbhaz$year==1934)
  df<-tbhaz1934
  for(i in 1:45) {
    dfnew<-tbhaz1934
    dfnew$year[dfnew$year==1934] <- 1934-i
    df<-rbind(dfnew,df)
  }
  df<-df[df$year!=1934,]
  tbhaz<-rbind(df,tbhaz)
  tbhaz<-tbhaz[order(tbhaz$iso3,tbhaz$replicate,tbhaz$year), ]
  rownames(tbhaz) <- seq(length=nrow(tbhaz))
  rm(tbhaz1934,df,dfnew)
  #Changing the FOI for the census year so that it represents 
  # only part of the year to the date of the census
  #First find the censys year
  tbhaz$testari<-tbhaz$FOI*(220/365.2425)
  if(censusyear==2006) {
    tbhaz$FOI[tbhaz$year==2006]<-tbhaz$testari[tbhaz$year==2006]
  } else if (censusyear==2011) {
    tbhaz$FOI[tbhaz$year==2011]<-tbhaz$testari[tbhaz$year==2011]
  } else if (censusyear==2016) {
    tbhaz$FOI[tbhaz$year==2016]<-tbhaz$testari[tbhaz$year==2016]
  }
  tbhaz[, "testari":=NULL]
  ###Create cumulative sum variable for the hazards
  tbhaz[, cumhaz := cumsum(FOI), by=list(iso3, replicate)]
  tbhzAust <- tbhaz[tbhaz$iso3=="AUS",]  
  tbhzAust<-as.data.table(tbhzAust)
  DT<-as.data.table(DT)
  #Sort and order them and add id columns
  DT<-DT[order(iso3,yob,yoa)]
  DT$id <- seq.int(nrow(DT))
  #ARIcalc bit
  #Creating the year of birth data table (and dividing the hazards in half)...
  tbhazl<-tbhaz[,c("year","iso3","replicate","FOI")]
  tbhazl<-dcast(tbhazl, year+iso3~replicate)
  yob<-merge(DT, tbhazl, by.x = c("yob","iso3"),
             by.y = c("year","iso3"), all.x=TRUE, all.y=FALSE)
  yob[ , as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.5), .SDcols =reps]
  ## half the values again in the rows where yob equals yoa 
  yob[yob==yoa, as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.5), .SDcols =reps]
  ##subset those rows
  subyob <- subset(yob, yob==yoa)
  ##create a new datatable containing a quarter 
  #of the Australia hazard for year of arrival
  tbhazl<-tbhzAust[,c("year","replicate","FOI")]
  tbhazl<-dcast(tbhazl, year~replicate)
  yoaA<-merge(DT, tbhazl, by.x = c("yoa"), by.y = c("year"), all.x=TRUE, all.y=FALSE)
  yoaA[ , as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.25), .SDcols =reps]
  #subset this data table and add it to the yob datatable I subsetted above
  subyoaA <- subset(yoaA, yob==yoa)
  e<-which(colnames(subyob)==firstrep)
  f<-which(colnames(subyob)==lastrep)
  g<-which(colnames(subyoaA)==firstrep)
  h<-which(colnames(subyoaA)==lastrep)
  subyob<-subyob[,e:f]+subyoaA[,g:h]
  subyob$id2 <- seq.int(nrow(subyob))
  subyoaA$id2 <- seq.int(nrow(subyoaA))
  #Merging it to the Australian census data information
  subyob <- merge(subyoaA[,c("yoa","age","cob","year",
                             "yob","iso3","pop",
                             "mani","sex","id2","id")], subyob,by=c('id2'))
  subyob[, "id2":=NULL]
  # bind it back onto the original datatable for yob and order
  yob <- yob[yob!=yoa]
  yob<- rbind(yob, subyob)
  yob<-yob[order(iso3,yob,yoa)]
  rm(subyob)
  rm(subyoaA)
  rm(yoaA)
  #Years between year of birth and year of arrival ...
  # Subtracting the cumulative hazard in the year of birth from the 
  # cumulative hazard in the year before arrival
  tbhazl<-tbhaz[,c("year","iso3","replicate","cumhaz")]# extract the cumhaz column from tbhaz
  tbhazl<-dcast(tbhazl, year+iso3~replicate)
  # creating data table with cumulative hazard in the year of birth
  yobcmsm<-merge(DT, tbhazl, by.x = c("yob","iso3"), 
                 by.y = c("year","iso3"), all.x=TRUE, all.y=FALSE)
  yobcmsm<-yobcmsm[order(iso3,yob,yoa)]
  # creating data table with cumulative hazard in the year before arrival
  DT[, yoam1 := yoa-1]
  yoam1cmsm<-merge(DT, tbhazl, by.x = c("yoam1","iso3"), 
                   by.y = c("year","iso3"), all.x=TRUE, all.y=FALSE)
  yoam1cmsm<-yoam1cmsm[order(iso3,yob,yoa)]
  #subtracting one from the other
  a<-yoam1cmsm[, reps, with=FALSE]
  b<-yobcmsm[, reps, with=FALSE]
  yobtoyoa<-a-b
  rm(a)
  rm(b)
  rm(yoam1cmsm)
  rm(yobcmsm)
  DT[, "yoam1":=NULL]
  yobtoyoa$id <- seq.int(nrow(yobtoyoa))
  #Merging it to the Australian census data information
  yobtoyoa <- merge(DT[,c("age","yoa","cob","yob","iso3","year",
                          "pop","mani","sex","id")], yobtoyoa,by=c('id'))
  ## remove the hazard values in the rows where yob equals yoa, 
  #because I sorted their hazard values above
  yobtoyoa[yob==yoa, as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0), .SDcols =reps]
  ##Year of arrival hazard in the country of birth (and dividing the hazards in half)...
  tbhazl<-tbhaz[,c("year","iso3","replicate","FOI")]
  tbhazl<-dcast(tbhazl, year+iso3~replicate)
  yoa<-merge(DT, tbhazl, by.x = c("yoa","iso3"),
             by.y = c("year","iso3"), all.x=TRUE, all.y=FALSE)
  yoa<-data.table(yoa)
  yoa[,as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.5),.SDcols =reps]
  yoa<-yoa[order(iso3,yob,yoa)]
  ##Year of arrival hazard in Australia (and dividing the hazards in half)...
  tbhazl<-tbhzAust[,c("year","replicate","FOI")]
  tbhazl<-dcast(tbhazl, year~replicate)
  yoaA<-merge(DT, tbhazl, by.x = c("yoa"),
              by.y = c("year"), all.x=TRUE, all.y=FALSE)
  yoaA<-data.table(yoaA)
  yoaA[ , as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.5), .SDcols =reps]
  yoaA<-yoaA[order(iso3,yob,yoa)]
  ##Adding the two hazards in the year of arrival 
  a<-yoa[, reps, with=FALSE]
  b<-yoaA[, reps, with=FALSE]
  yoa<-a+b
  rm(a)
  rm(b)
  rm(yoaA)
  #Merging it to the Australian census data information
  yoa$id <- seq.int(nrow(yoa))
  yoa <- merge(DT[,c("age","yoa","cob","yob","iso3","year",
                     "pop","mani","sex","id")], yoa,by=c('id'))
  ## remove the hazard values in the rows where yob equals yoa, 
  #because I sorted their hazard values above
  yoa[yob==yoa, as.character(c(firstrep:lastrep)):=lapply(.SD, "*", 0), .SDcols =reps]
  ##Adding up all of the hazards!!! i.e. yob+yobtoyoa+yoa
  a<-yob[, reps, with=FALSE]
  b<-yobtoyoa[, reps, with=FALSE]
  rm(yob)
  rm(yobtoyoa)
  e<-a+b
  rm(a)
  rm(b)
  c<-yoa[, reps, with=FALSE]
  rm(yoa)
  f<-e+c
  rm(c)
  rm(e)
  #Years between year of arrival and census year ...
  # Subtracting the cumulative hazard in the census year from the 
  # cumulative hazard in the year of arrival
  tbhazl<-tbhzAust[,c("year","iso3","replicate","cumhaz")]
  tbhazl<-dcast(tbhazl, year~replicate)
  # creating data table with cumulative hazard in the year of arrival
  yoacmsm<-merge(DT, tbhazl, by.x = c("yoa"), by.y = c("year"), all.x=TRUE, all.y=FALSE)
  yoacmsm<-yoacmsm[order(iso3,yob,yoa)]
  # creating data table with cumulative hazard in the censusyear
  DT<-DT[, censyear := censusyear]
  censcmsm<-merge(DT, tbhazl, by.x = c("censyear"), by.y = c("year"), all.x=TRUE, all.y=FALSE)
  rm(tbhazl)
  censcmsm<-censcmsm[order(iso3,yob,yoa)]
  DT[, "censyear":=NULL]
  gc()
  #subtracting one from the other
  a<-censcmsm[, reps, with=FALSE]
  b<-yoacmsm[, reps, with=FALSE]
  gc()
  yoatocens<-a-b
  rm(a)
  rm(b)
  rm(censcmsm)
  rm(yoacmsm)
  yoatocens$id <- seq.int(nrow(yoatocens))
  #Merging it to the Australian census data information
  yoatocens <- merge(DT[,c("age","yoa","cob","yob","iso3","year",
                           "pop","mani","sex","id")], yoatocens,by=c('id'))
  ##Adding up all of the hazards!!! i.e. f + yoatocens
  d<-yoatocens[, reps, with=FALSE]
  rm(yoatocens)
  yobtoyoa<-f+d
  rm(d)
  rm(f)
  #Merging it to the Australian census data information
  yobtoyoa$id <- seq.int(nrow(yobtoyoa))
  DT <- merge(DT[,c("age","yoa","cob","yob","iso3","year",
                    "pop","mani","sex","id")], yobtoyoa,by=c('id'))
  #Create function to turn the hazards into risks
  haztorisk<-function(v) {
    1-(exp(-v))
  }
  #Apply the above function to every element of the matrix and make it 
  # a dataframe again
  rm(yobtoyoa)
  DT[ , as.character(c(firstrep:lastrep)):=lapply(.SD,haztorisk), .SDcols =reps]
  #Rename columns with replicate numbers
  Vcolnam <- paste0(c("V"),1:repnumber)
  g<-which(colnames(DT)==firstrep)
  h<-which(colnames(DT)==lastrep)
  colnames(DT)[g:h] <- Vcolnam
  DT
}


#### ARTI ON ARRIVAL 
#### This takes a dataframe of overseasborn residents and 
## calculates their ARI's with either the 200 or 5000 replicate hazard data
## depending on what is specified in the function
### Assuming no ARTI accumulation after arrival
### DT needs nine columns: "year","iso3","age","yoa","sex","pop", "yob", "mani", "cob"
#### Or 8 columns without "yob"
## The output is the same datatable with added columns V1:V200 or V5000 
## which are the estimated risks of infection
TBriskcalc_onarrival <-function(DT,repnumber) {
  reps<-as.character(c("1":repnumber))
  firstrep<-"1"
  lastrep<-repnumber
  censusyear<-DT$year[1]
  #Loading the hazard data - choose relevant location
  ####################################################################
  precede <- paste(repnumber,'repLARI.Rdata',sep = "")
  mm<-"Data/"
  filename <- paste(mm,precede,sep = "") 
  loading <- function(dataset){
    merchants <- load(dataset)
    return(get(merchants))
  }
  tbhaz<-as.data.table(loading(filename))
  ##Create an FOI variable from the log ARI (lari) variable.
  tbhaz[, "FOI":=exp(lari)]
  tbhaz[, "lari":=NULL]
  #########################################################
  tbhaz[iso3=="AUS","FOI":=0]
  ##########################################################
  ##Subset all 2014 rows so I can create 
  # 2015 and 2016 data with the same data and then append them on
  tbhaz2015 <- subset(tbhaz, tbhaz$year==2014)
  tbhaz2015[,"year":=2015]
  tbhaz2016 <- subset(tbhaz, tbhaz$year==2014)
  tbhaz2016[,"year":=2016]
  tbhaz1516<-rbind(tbhaz2015,tbhaz2016)
  tbhaz<-rbind(tbhaz1516,tbhaz)
  tbhaz<-tbhaz[order(tbhaz$iso3,tbhaz$replicate,tbhaz$year), ]
  rownames(tbhaz) <- seq(length=nrow(tbhaz))
  rm(tbhaz1516,tbhaz2015,tbhaz2016)
  ##For pre-1934 data only, subset all 1934 rows so I can create 
  # pre-1934 data and then append them on
  tbhaz1934 <- subset(tbhaz, tbhaz$year==1934)
  df<-tbhaz1934
  for(i in 1:45) {
    dfnew<-tbhaz1934
    dfnew$year[dfnew$year==1934] <- 1934-i
    df<-rbind(dfnew,df)
  }
  df<-df[df$year!=1934,]
  tbhaz<-rbind(df,tbhaz)
  tbhaz<-tbhaz[order(tbhaz$iso3,tbhaz$replicate,tbhaz$year), ]
  rownames(tbhaz) <- seq(length=nrow(tbhaz))
  rm(tbhaz1934,df,dfnew)
  #Changing the FOI for the census year so that it represents 
  # only part of the year to the date of the census
  #First find the censys year
  tbhaz$testari<-tbhaz$FOI*(220/365.2425)
  if(censusyear==2006) {
    tbhaz$FOI[tbhaz$year==2006]<-tbhaz$testari[tbhaz$year==2006]
  } else if (censusyear==2011) {
    tbhaz$FOI[tbhaz$year==2011]<-tbhaz$testari[tbhaz$year==2011]
  } else if (censusyear==2016) {
    tbhaz$FOI[tbhaz$year==2016]<-tbhaz$testari[tbhaz$year==2016]
  }
  tbhaz[, "testari":=NULL]
  ###Create cumulative sum variable for the hazards
  tbhaz[, cumhaz := cumsum(FOI), by=list(iso3, replicate)]
  tbhzAust <- tbhaz[tbhaz$iso3=="AUS",]  
  tbhzAust<-as.data.table(tbhzAust)
  DT<-as.data.table(DT)
  #Sort and order them and add id columns
  DT<-DT[order(iso3,yob,yoa)]
  DT$id <- seq.int(nrow(DT))
  #ARIcalc bit
  #Creating the year of birth data table (and dividing the hazards in half)...
  tbhazl<-tbhaz[,c("year","iso3","replicate","FOI")]
  tbhazl<-dcast(tbhazl, year+iso3~replicate)
  yob<-merge(DT, tbhazl, by.x = c("yob","iso3"),
             by.y = c("year","iso3"), all.x=TRUE, all.y=FALSE)
  yob[ , as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.5), .SDcols =reps]
  ## half the values again in the rows where yob equals yoa 
  yob[yob==yoa, as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.5), .SDcols =reps]
  ##subset those rows
  subyob <- subset(yob, yob==yoa)
  ##create a new datatable containing a quarter 
  #of the Australia hazard for year of arrival
  tbhazl<-tbhzAust[,c("year","replicate","FOI")]
  tbhazl<-dcast(tbhazl, year~replicate)
  yoaA<-merge(DT, tbhazl, by.x = c("yoa"), by.y = c("year"), all.x=TRUE, all.y=FALSE)
  yoaA[ , as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.25), .SDcols =reps]
  #subset this data table and add it to the yob datatable I subsetted above
  subyoaA <- subset(yoaA, yob==yoa)
  e<-which(colnames(subyob)==firstrep)
  f<-which(colnames(subyob)==lastrep)
  g<-which(colnames(subyoaA)==firstrep)
  h<-which(colnames(subyoaA)==lastrep)
  subyob<-subyob[,e:f]+subyoaA[,g:h]
  subyob$id2 <- seq.int(nrow(subyob))
  subyoaA$id2 <- seq.int(nrow(subyoaA))
  #Merging it to the Australian census data information
  subyob <- merge(subyoaA[,c("yoa","age","cob","year",
                             "yob","iso3","pop",
                             "mani","sex","id2","id")], subyob,by=c('id2'))
  subyob[, "id2":=NULL]
  # bind it back onto the original datatable for yob and order
  yob <- yob[yob!=yoa]
  yob<- rbind(yob, subyob)
  yob<-yob[order(iso3,yob,yoa)]
  rm(subyob)
  rm(subyoaA)
  rm(yoaA)
  #Years between year of birth and year of arrival ...
  # Subtracting the cumulative hazard in the year of birth from the 
  # cumulative hazard in the year before arrival
  tbhazl<-tbhaz[,c("year","iso3","replicate","cumhaz")]# extract the cumhaz column from tbhaz
  tbhazl<-dcast(tbhazl, year+iso3~replicate)
  # creating data table with cumulative hazard in the year of birth
  yobcmsm<-merge(DT, tbhazl, by.x = c("yob","iso3"), 
                 by.y = c("year","iso3"), all.x=TRUE, all.y=FALSE)
  yobcmsm<-yobcmsm[order(iso3,yob,yoa)]
  # creating data table with cumulative hazard in the year before arrival
  DT[, yoam1 := yoa-1]
  yoam1cmsm<-merge(DT, tbhazl, by.x = c("yoam1","iso3"), 
                   by.y = c("year","iso3"), all.x=TRUE, all.y=FALSE)
  yoam1cmsm<-yoam1cmsm[order(iso3,yob,yoa)]
  #subtracting one from the other
  a<-yoam1cmsm[, reps, with=FALSE]
  b<-yobcmsm[, reps, with=FALSE]
  yobtoyoa<-a-b
  rm(a)
  rm(b)
  rm(yoam1cmsm)
  rm(yobcmsm)
  DT[, "yoam1":=NULL]
  yobtoyoa$id <- seq.int(nrow(yobtoyoa))
  #Merging it to the Australian census data information
  yobtoyoa <- merge(DT[,c("age","yoa","cob","yob","iso3","year",
                          "pop","mani","sex","id")], yobtoyoa,by=c('id'))
  ## remove the hazard values in the rows where yob equals yoa, 
  #because I sorted their hazard values above
  yobtoyoa[yob==yoa, as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0), .SDcols =reps]
  ##Year of arrival hazard in the country of birth (and dividing the hazards in half)...
  tbhazl<-tbhaz[,c("year","iso3","replicate","FOI")]
  tbhazl<-dcast(tbhazl, year+iso3~replicate)
  yoa<-merge(DT, tbhazl, by.x = c("yoa","iso3"),
             by.y = c("year","iso3"), all.x=TRUE, all.y=FALSE)
  yoa<-data.table(yoa)
  yoa[,as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.5),.SDcols =reps]
  yoa<-yoa[order(iso3,yob,yoa)]
  ##Year of arrival hazard in Australia (and dividing the hazards in half)...
  tbhazl<-tbhzAust[,c("year","replicate","FOI")]
  tbhazl<-dcast(tbhazl, year~replicate)
  yoaA<-merge(DT, tbhazl, by.x = c("yoa"),
              by.y = c("year"), all.x=TRUE, all.y=FALSE)
  yoaA<-data.table(yoaA)
  yoaA[ , as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.5), .SDcols =reps]
  yoaA<-yoaA[order(iso3,yob,yoa)]
  ##Adding the two hazards in the year of arrival 
  a<-yoa[, reps, with=FALSE]
  b<-yoaA[, reps, with=FALSE]
  yoa<-a+b
  rm(a)
  rm(b)
  rm(yoaA)
  #Merging it to the Australian census data information
  yoa$id <- seq.int(nrow(yoa))
  yoa <- merge(DT[,c("age","yoa","cob","yob","iso3","year",
                     "pop","mani","sex","id")], yoa,by=c('id'))
  ## remove the hazard values in the rows where yob equals yoa, 
  #because I sorted their hazard values above
  yoa[yob==yoa, as.character(c(firstrep:lastrep)):=lapply(.SD, "*", 0), .SDcols =reps]
  ##Adding up all of the hazards!!! i.e. yob+yobtoyoa+yoa
  a<-yob[, reps, with=FALSE]
  b<-yobtoyoa[, reps, with=FALSE]
  rm(yob)
  rm(yobtoyoa)
  e<-a+b
  rm(a)
  rm(b)
  c<-yoa[, reps, with=FALSE]
  rm(yoa)
  f<-e+c
  rm(c)
  rm(e)
  #Years between year of arrival and census year ...
  # Subtracting the cumulative hazard in the census year from the 
  # cumulative hazard in the year of arrival
  tbhazl<-tbhzAust[,c("year","iso3","replicate","cumhaz")]
  tbhazl<-dcast(tbhazl, year~replicate)
  # creating data table with cumulative hazard in the year of arrival
  yoacmsm<-merge(DT, tbhazl, by.x = c("yoa"), by.y = c("year"), all.x=TRUE, all.y=FALSE)
  yoacmsm<-yoacmsm[order(iso3,yob,yoa)]
  # creating data table with cumulative hazard in the censusyear
  DT<-DT[, censyear := censusyear]
  censcmsm<-merge(DT, tbhazl, by.x = c("censyear"), by.y = c("year"), all.x=TRUE, all.y=FALSE)
  rm(tbhazl)
  censcmsm<-censcmsm[order(iso3,yob,yoa)]
  DT[, "censyear":=NULL]
  gc()
  #subtracting one from the other
  a<-censcmsm[, reps, with=FALSE]
  b<-yoacmsm[, reps, with=FALSE]
  gc()
  yoatocens<-a-b
  rm(a)
  rm(b)
  rm(censcmsm)
  rm(yoacmsm)
  yoatocens$id <- seq.int(nrow(yoatocens))
  #Merging it to the Australian census data information
  yoatocens <- merge(DT[,c("age","yoa","cob","yob","iso3","year",
                           "pop","mani","sex","id")], yoatocens,by=c('id'))
  ##Adding up all of the hazards!!! i.e. f + yoatocens
  d<-yoatocens[, reps, with=FALSE]
  rm(yoatocens)
  yobtoyoa<-f+d
  rm(d)
  rm(f)
  #Merging it to the Australian census data information
  yobtoyoa$id <- seq.int(nrow(yobtoyoa))
  DT <- merge(DT[,c("age","yoa","cob","yob","iso3","year",
                    "pop","mani","sex","id")], yobtoyoa,by=c('id'))
  #Create function to turn the hazards into risks
  haztorisk<-function(v) {
    1-(exp(-v))
  }
  #Apply the above function to every element of the matrix and make it 
  # a dataframe again
  rm(yobtoyoa)
  DT[ , as.character(c(firstrep:lastrep)):=lapply(.SD,haztorisk), .SDcols =reps]
  #Rename columns with replicate numbers
  Vcolnam <- paste0(c("V"),1:repnumber)
  g<-which(colnames(DT)==firstrep)
  h<-which(colnames(DT)==lastrep)
  colnames(DT)[g:h] <- Vcolnam
  DT
}

#### All but recent
#### This takes a dataframe of overseasborn residents and 
## calculates their ARI's with either the 200 or 5000 replicate hazard data
## depending on what is specified in the function
### Assuming no ARTI in the two years prior to each census
### DT needs nine columns: "year","iso3","age","yoa","sex","pop", "yob", "mani", "cob"
#### Or 8 columns without "yob"
## The output is the same datatable with added columns V1:V200 or V5000 
## which are the estimated risks of infection
TBriskcalc_allbutrecent<- function(DT,repnumber){
  reps<-as.character(c("1":repnumber))
  firstrep<-"1"
  lastrep<-repnumber
  censusyear<-DT$year[1]
  #Loading the hazard data - choose relevant location
  ####################################################################
  precede <- paste(repnumber,'repLARI.Rdata',sep = "")
  mm<-"Data/"
  filename <- paste(mm,precede,sep = "") 
  loading <- function(dataset){
    merchants <- load(dataset)
    return(get(merchants))
  }
  tbhaz<-as.data.table(loading(filename))
  ##Create an FOI variable from the log ARI (lari) variable.
  tbhaz[, "FOI":=exp(lari)]
  tbhaz[, "lari":=NULL]
  ##Subset all 2014 rows so I can create 
  # 2015 and 2016 data with the same data and then append them on
  tbhaz2015 <- subset(tbhaz, tbhaz$year==2014)
  tbhaz2015[,"year":=2015]
  tbhaz2016 <- subset(tbhaz, tbhaz$year==2014)
  tbhaz2016[,"year":=2016]
  tbhaz1516<-rbind(tbhaz2015,tbhaz2016)
  tbhaz<-rbind(tbhaz1516,tbhaz)
  tbhaz<-tbhaz[order(tbhaz$iso3,tbhaz$replicate,tbhaz$year), ]
  rownames(tbhaz) <- seq(length=nrow(tbhaz))
  rm(tbhaz1516,tbhaz2015,tbhaz2016)
  ##For pre-1934 data only, subset all 1934 rows so I can create 
  # pre-1934 data and then append them on
  tbhaz1934 <- subset(tbhaz, tbhaz$year==1934)
  df<-tbhaz1934
  for(i in 1:45) {
    dfnew<-tbhaz1934
    dfnew$year[dfnew$year==1934] <- 1934-i
    df<-rbind(dfnew,df)
  }
  df<-df[df$year!=1934,]
  tbhaz<-rbind(df,tbhaz)
  tbhaz<-tbhaz[order(tbhaz$iso3,tbhaz$replicate,tbhaz$year), ]
  rownames(tbhaz) <- seq(length=nrow(tbhaz))
  rm(tbhaz1934,df,dfnew)
  #Changing the FOI for the census year so that it represents 
  # only part of the year to the date of the census
  #First find the censys year
  
  ###Recent years function....
  #Changing the FOI for the year two years prior to census to represent
  # only part of the year and changing the FOI for two census year 
  # and the one previously to zero
  tbhaz$testari<-tbhaz$FOI*(220/365.2425)
  if(censusyear==2006) {
    tbhaz$FOI[tbhaz$year==2004]<-tbhaz$testari[tbhaz$year==2004]
    tbhaz$FOI[tbhaz$year==2005]<-0
    tbhaz$FOI[tbhaz$year==2006]<-0
  } else if (censusyear==2011) {
    tbhaz$FOI[tbhaz$year==2009]<-tbhaz$testari[tbhaz$year==2009]
    tbhaz$FOI[tbhaz$year==2010]<-0
    tbhaz$FOI[tbhaz$year==2011]<-0
  } else if (censusyear==2016) {
    tbhaz$FOI[tbhaz$year==2014]<-tbhaz$testari[tbhaz$year==2014]
    tbhaz$FOI[tbhaz$year==2015]<-0
    tbhaz$FOI[tbhaz$year==2016]<-0
  }
  tbhaz[, "testari":=NULL]
  ###Create cumulative sum variable for the hazards
  tbhaz[, cumhaz := cumsum(FOI), by=list(iso3, replicate)]
  tbhzAust <- tbhaz[tbhaz$iso3=="AUS",]  
  tbhzAust<-as.data.table(tbhzAust)
  DT<-as.data.table(DT)
  #Sort and order them and add id columns
  DT<-DT[order(iso3,yob,yoa)]
  DT$id <- seq.int(nrow(DT))
  #ARIcalc bit
  #Creating the year of birth data table (and dividing the hazards in half)...
  tbhazl<-tbhaz[,c("year","iso3","replicate","FOI")]
  tbhazl<-dcast(tbhazl, year+iso3~replicate)
  yob<-merge(DT, tbhazl, by.x = c("yob","iso3"),
             by.y = c("year","iso3"), all.x=TRUE, all.y=FALSE)
  yob[ , as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.5), .SDcols =reps]
  ## half the values again in the rows where yob equals yoa 
  yob[yob==yoa, as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.5), .SDcols =reps]
  ##subset those rows
  subyob <- subset(yob, yob==yoa)
  ##create a new datatable containing a quarter 
  #of the Australia hazard for year of arrival
  tbhazl<-tbhzAust[,c("year","replicate","FOI")]
  tbhazl<-dcast(tbhazl, year~replicate)
  yoaA<-merge(DT, tbhazl, by.x = c("yoa"), by.y = c("year"), all.x=TRUE, all.y=FALSE)
  yoaA[ , as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.25), .SDcols =reps]
  #subset this data table and add it to the yob datatable I subsetted above
  subyoaA <- subset(yoaA, yob==yoa)
  e<-which(colnames(subyob)==firstrep)
  f<-which(colnames(subyob)==lastrep)
  g<-which(colnames(subyoaA)==firstrep)
  h<-which(colnames(subyoaA)==lastrep)
  subyob<-subyob[,e:f]+subyoaA[,g:h]
  subyob$id2 <- seq.int(nrow(subyob))
  subyoaA$id2 <- seq.int(nrow(subyoaA))
  #Merging it to the Australian census data information
  subyob <- merge(subyoaA[,c("yoa","age","cob","year",
                             "yob","iso3","pop",
                             "mani","sex","id2","id")], subyob,by=c('id2'))
  subyob[, "id2":=NULL]
  # bind it back onto the original datatable for yob and order
  yob <- yob[yob!=yoa]
  yob<- rbind(yob, subyob)
  yob<-yob[order(iso3,yob,yoa)]
  rm(subyob)
  rm(subyoaA)
  rm(yoaA)
  #Years between year of birth and year of arrival ...
  # Subtracting the cumulative hazard in the year of birth from the 
  # cumulative hazard in the year before arrival
  tbhazl<-tbhaz[,c("year","iso3","replicate","cumhaz")]# extract the cumhaz column from tbhaz
  tbhazl<-dcast(tbhazl, year+iso3~replicate)
  # creating data table with cumulative hazard in the year of birth
  yobcmsm<-merge(DT, tbhazl, by.x = c("yob","iso3"), 
                 by.y = c("year","iso3"), all.x=TRUE, all.y=FALSE)
  yobcmsm<-yobcmsm[order(iso3,yob,yoa)]
  # creating data table with cumulative hazard in the year before arrival
  DT[, yoam1 := yoa-1]
  yoam1cmsm<-merge(DT, tbhazl, by.x = c("yoam1","iso3"), 
                   by.y = c("year","iso3"), all.x=TRUE, all.y=FALSE)
  yoam1cmsm<-yoam1cmsm[order(iso3,yob,yoa)]
  #subtracting one from the other
  a<-yoam1cmsm[, reps, with=FALSE]
  b<-yobcmsm[, reps, with=FALSE]
  yobtoyoa<-a-b
  rm(a)
  rm(b)
  rm(yoam1cmsm)
  rm(yobcmsm)
  DT[, "yoam1":=NULL]
  yobtoyoa$id <- seq.int(nrow(yobtoyoa))
  #Merging it to the Australian census data information
  yobtoyoa <- merge(DT[,c("age","yoa","cob","yob","iso3","year",
                          "pop","mani","sex","id")], yobtoyoa,by=c('id'))
  ## remove the hazard values in the rows where yob equals yoa, 
  #because I sorted their hazard values above
  yobtoyoa[yob==yoa, as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0), .SDcols =reps]
  ##Year of arrival hazard in the country of birth (and dividing the hazards in half)...
  tbhazl<-tbhaz[,c("year","iso3","replicate","FOI")]
  tbhazl<-dcast(tbhazl, year+iso3~replicate)
  yoa<-merge(DT, tbhazl, by.x = c("yoa","iso3"),
             by.y = c("year","iso3"), all.x=TRUE, all.y=FALSE)
  yoa<-data.table(yoa)
  yoa[,as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.5),.SDcols =reps]
  yoa<-yoa[order(iso3,yob,yoa)]
  ##Year of arrival hazard in Australia (and dividing the hazards in half)...
  tbhazl<-tbhzAust[,c("year","replicate","FOI")]
  tbhazl<-dcast(tbhazl, year~replicate)
  yoaA<-merge(DT, tbhazl, by.x = c("yoa"),
              by.y = c("year"), all.x=TRUE, all.y=FALSE)
  yoaA<-data.table(yoaA)
  yoaA[ , as.character(c(firstrep:lastrep)) :=lapply(.SD, "*", 0.5), .SDcols =reps]
  yoaA<-yoaA[order(iso3,yob,yoa)]
  ##Adding the two hazards in the year of arrival 
  a<-yoa[, reps, with=FALSE]
  b<-yoaA[, reps, with=FALSE]
  yoa<-a+b
  rm(a)
  rm(b)
  rm(yoaA)
  #Merging it to the Australian census data information
  yoa$id <- seq.int(nrow(yoa))
  yoa <- merge(DT[,c("age","yoa","cob","yob","iso3","year",
                     "pop","mani","sex","id")], yoa,by=c('id'))
  ## remove the hazard values in the rows where yob equals yoa, 
  #because I sorted their hazard values above
  yoa[yob==yoa, as.character(c(firstrep:lastrep)):=lapply(.SD, "*", 0), .SDcols =reps]
  ##Adding up all of the hazards!!! i.e. yob+yobtoyoa+yoa
  a<-yob[, reps, with=FALSE]
  b<-yobtoyoa[, reps, with=FALSE]
  rm(yob)
  rm(yobtoyoa)
  e<-a+b
  rm(a)
  rm(b)
  c<-yoa[, reps, with=FALSE]
  rm(yoa)
  f<-e+c
  rm(c)
  rm(e)
  #Years between year of arrival and census year ...
  # Subtracting the cumulative hazard in the census year from the 
  # cumulative hazard in the year of arrival
  tbhazl<-tbhzAust[,c("year","iso3","replicate","cumhaz")]
  tbhazl<-dcast(tbhazl, year~replicate)
  # creating data table with cumulative hazard in the year of arrival
  yoacmsm<-merge(DT, tbhazl, by.x = c("yoa"), by.y = c("year"), all.x=TRUE, all.y=FALSE)
  yoacmsm<-yoacmsm[order(iso3,yob,yoa)]
  # creating data table with cumulative hazard in the censusyear
  DT<-DT[, censyear := censusyear]
  censcmsm<-merge(DT, tbhazl, by.x = c("censyear"), by.y = c("year"), all.x=TRUE, all.y=FALSE)
  rm(tbhazl)
  censcmsm<-censcmsm[order(iso3,yob,yoa)]
  DT[, "censyear":=NULL]
  gc()
  #subtracting one from the other
  a<-censcmsm[, reps, with=FALSE]
  b<-yoacmsm[, reps, with=FALSE]
  gc()
  yoatocens<-a-b
  rm(a)
  rm(b)
  rm(censcmsm)
  rm(yoacmsm)
  yoatocens$id <- seq.int(nrow(yoatocens))
  #Merging it to the Australian census data information
  yoatocens <- merge(DT[,c("age","yoa","cob","yob","iso3","year",
                           "pop","mani","sex","id")], yoatocens,by=c('id'))
  ##Adding up all of the hazards!!! i.e. f + yoatocens
  d<-yoatocens[, reps, with=FALSE]
  rm(yoatocens)
  yobtoyoa<-f+d
  rm(d)
  rm(f)
  #Merging it to the Australian census data information
  yobtoyoa$id <- seq.int(nrow(yobtoyoa))
  DT <- merge(DT[,c("age","yoa","cob","yob","iso3","year",
                    "pop","mani","sex","id")], yobtoyoa,by=c('id'))
  #Create function to turn the hazards into risks
  haztorisk<-function(v) {
    1-(exp(-v))
  }
  #Apply the above function to every element of the matrix and make it 
  # a dataframe again
  rm(yobtoyoa)
  DT[ , as.character(c(firstrep:lastrep)):=lapply(.SD,haztorisk), .SDcols =reps]
  #Rename columns with replicate numbers
  Vcolnam <- paste0(c("V"),1:repnumber)
  g<-which(colnames(DT)==firstrep)
  h<-which(colnames(DT)==lastrep)
  colnames(DT)[g:h] <- Vcolnam
  DT
}  


