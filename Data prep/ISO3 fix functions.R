
####Function for tidying countries of birth 
## and adjusting iso3 values to ones
## that are in Houben and Dodd's ARTI data:

####Set up: DT needs at least columns "iso3" and "cob", and "yoa":
iso3fixfunc<- function(DT){
  DT<-as.data.table(DT)
  DT$cob<-gsub("..", ".", DT$cob, fixed=TRUE)
  DT$cob <- gsub(",", ".", DT$cob, fixed=TRUE)
  DT$cob<-gsub(".", " ", DT$cob, fixed=TRUE)
  DT$cob<-gsub("-", "", DT$cob, fixed=TRUE)
  DT$cob<-gsub("nec", "", DT$cob, fixed=TRUE)
  DT$cob<-gsub("nfd", "", DT$cob, fixed=TRUE)
  DT$cob<-gsub("nf", "", DT$cob, fixed=TRUE)
  DT$cob<-gsub("  ", " ", DT$cob, fixed=TRUE) 
  
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  DT$cob<-trim(DT$cob)
  DT[DT$cob == "At sea","iso3":=NA]
  DT[DT$cob == "Not stated","iso3":= NA]
  DT[DT$cob == "Not applicable","iso3":= NA]
  DT[DT$cob == "Not elsewhere classified","iso3":= NA]
  DT[DT$cob == "Overseas visitor","iso3":=NA]
  DT[DT$cob == "Inadequately described","iso3":=NA]
  DT[DT$cob == "No data available","iso3":=NA]
  DT[DT$cob == "Overseas  Country unknown","iso3":= NA]
  DT[DT$cob == "Unknown","iso3":= NA]
  
  #Australian regions
  DT[DT$cob == "Australia includes External Territories","iso3":="AUS"] 
  DT[DT$cob == "Norfolk Island","iso3":="AUS"]
  DT[DT$cob == "Australian External Territories","iso3":="AUS"]
  DT[DT$cob == "Oceania and Antarctica","iso3":="NZL"]
  
  #Asian countrie
  DT[DT$cob == "China (excludes SARs and Taiwan)","iso3":= "CHN"]
  DT[DT$cob == "China excludes SARs and Taiwan Province","iso3":= "CHN"]
  DT[DT$cob == "Japan and the Koreas","iso3":= "JPN"]
  DT[DT$cob == "China excludes SARs and Taiwan","iso3":="CHN"]
  DT[DT$iso3 == "TWN","iso3":= "CHN" ] #Taiwan
  
  #Regions
  DT[DT$cob == "Middle East","iso3":="SAU"]
  DT[DT$cob == "North Africa","iso3":="MAR"]
  DT[DT$cob == "North Africa and the Middle East","iso3":="EGY"]
  DT[DT$cob == "Southern and East Africa","iso3":="SOM"]
  DT[DT$cob == "Spanish North Africa","iso3":="ESP"]
  DT[DT$cob == "Sub Saharan Africa","iso3":="SOM"]
  DT[DT$cob == "Sub-Saharan Africa, ","iso3":="SOM"]
  DT[DT$cob == "SubSaharan Africa","iso3":="SOM"] 
  DT[DT$cob == "Central and West Africa","iso3":="CMR"]
  
  DT[DT$cob == "Eastern Europe","iso3":="POL"]
  DT[DT$cob == "North West Europe","iso3":="FRA"]
  DT[DT$cob == "NorthWest Europe","iso3":="FRA"]
  DT[DT$cob == "Southern Europe","iso3":="GRC"]
  DT[DT$cob == "South Eastern Europe","iso3":="MKD"]
  DT[DT$cob == "Southern and Eastern Europe","iso3":="MKD"]
  DT[DT$cob == "Northern Europe","iso3":="NLD"]
  DT[DT$cob == "Western Europe","iso3":="FRA"]
  
  DT[DT$cob == "Americas","iso3":="USA"]
  DT[DT$cob == "Polynesia excludes Hawaii","iso3":="WSM"]
  DT[DT$cob == "Polynesia (excludes Hawaii)","iso3":="WSM"]
  
  DT[DT$cob == "South America","iso3":="BRA"]
  DT[DT$cob == "Northern America","iso3":="USA"]
  DT[DT$cob == "Central America","iso3":="CRI"]
  DT[DT$cob == "Virgin Islands United States","iso3":= "USA"]
  DT[DT$cob == "Caribbean","iso3":="BHS"]
  DT[DT$cob == "Virgin Islands United States","iso3":= "USA"]
  
  DT[DT$cob == "Central Asia","iso3":="KAZ"]
  DT[DT$cob == "South East Asia","iso3":="THA"]
  DT[DT$cob == "SouthEast Asia","iso3":="THA"]
  DT[DT$cob == "Southern Asia","iso3":="IND"]
  DT[DT$cob == "North East Asia","iso3":="MNG"]
  DT[DT$cob == "NorthEast Asia","iso3":="MNG"]
  
  DT[DT$cob == "Mainland South East Asia","iso3":="THA"]
  DT[DT$cob == "Mainland SouthEast Asia","iso3":="THA"]
  DT[DT$cob == "Maritime South East Asia","iso3":="PHL"]
  DT[DT$cob == "Maritime SouthEast Asia","iso3":="PHL"]
  DT[DT$cob == "Melanesia","iso3":="VUT"]
  DT[DT$cob == "Micronesia","iso3":="KIR"]
  DT[DT$cob == "Southern and Central Asia","iso3":="AFG"]

  #Great Britain and surrounding
  DT[DT$cob == "Northern Ireland","iso3":="IRL"]
  DT[DT$cob == "Scotland","iso3":="GBR"]
  DT[DT$cob == "Wales","iso3":="GBR"]
  DT[DT$cob == "Channel Islands","iso3":="GBR"]
  DT[DT$cob == "Pitcairn Islands","iso3":="GBR"]  
  DT[DT$cob == "England","iso3":="GBR"]
  DT[DT$cob == "United Kingdom","iso3":="GBR"]
  DT[DT$cob == "Channel Islands","iso3":="GBR"]
  DT[DT$cob == "Channel Islands and Isle of Man","iso3":="GBR"]
  DT[DT$cob == "United Kingdom Channel Islands and Isle of Man","iso3":= "GBR"]  
  DT[DT$cob == "United Kingdom  Channel Islands and Isle of Man","iso3":= "GBR"] 
  DT[DT$cob == "United Kingdom, Channel Islands and Isle of Man","iso3":= "GBR"] 
  DT[DT$cob == "British Antarctic Territory","iso3":="GBR"]
  

  #Other
  DT[DT$cob == "Kosovo","iso3":="SRB"]
  DT[DT$iso3 == "ESH","iso3":= "MRT"]
  DT[DT$cob == "Yugoslavia Federal Republic of","iso3":= "MKD"]
  DT[DT$cob == "SFR Yugoslavia","iso3":= "MKD"]
  DT[DT$iso3 == "YUG","iso3":= "MRT"]
  DT[DT$cob == "Yugoslavia, Federal Republic of","iso3":= "MKD"]
  
  
  #European territories
  DT[DT$cob == "RÃ union","iso3":="FRA"]
  DT[DT$cob == "Réunion","iso3":="FRA"] 
  DT[DT$cob == "R� union","iso3":="FRA"]
  DT[DT$cob == "St Barthelemy","iso3":="FRA"]
  DT[DT$cob == "St Martin French part","iso3":="FRA"]
  DT[DT$cob == "Bonaire Sint Eustatius and Saba","iso3":="NLD"]
  DT[DT$cob == "Aland Islands","iso3":="SWE"]
  DT[DT$cob == "Netherlands Antilles","iso3":="NLD"]
  
  #Removing yoa for Norfolk Islanders since I've made their iso3 Australia anyway
  DT[DT$cob == "Norfolk Island","yoa":=NA]
  DT[DT$iso3 == "IMN","iso3":= "GBR" ] #Isle of Man
  DT[DT$iso3 == "GGY","iso3":= "GBR" ] #Guernsey
  DT[DT$iso3 == "JEY","iso3":= "GBR" ] #Jersey
  DT[DT$iso3 == "GIB","iso3":= "ESP" ] #Gibraltar
  DT[DT$iso3 == "TWN","iso3":= "CHN" ] #Taiwan
  DT[DT$iso3 == "SHN","iso3":= "GBR" ] #Saint Helena
  DT[DT$iso3 == "FLK","iso3":= "GBR" ] #Falkland Islands (Malvinas)
  DT[DT$iso3 == "REU","iso3":= "FRA" ] #Reunion
  DT[DT$iso3 == "NFK","iso3":= "AUS" ] #Norfolk Island
  DT[DT$iso3 == "ATA","iso3":= "GBR" ] #Antarctica
  DT[DT$iso3 == "LIE","iso3":= "AUT" ] #Liechtenstein
  DT[DT$iso3 == "FRO","iso3":= "NOR" ] #Faroe Islands
  DT[DT$iso3 == "VAT","iso3":= "ITA" ] #Holy See (Vatican City State)
  DT[DT$iso3 == "ESH","iso3":= "MAR" ] #Western Sahara
  DT[DT$iso3 == "SPM","iso3":= "FRA" ] #Saint Pierre and Miquelon
  DT[DT$iso3 == "TWN","iso3":= "CHN" ] #Taiwan, Province of China
  DT[DT$iso3 == "GLP","iso3":= "FRA" ] #Guadeloupe
  DT[DT$iso3 == "GUF","iso3":= "FRA" ] #French Guiana
  DT[DT$iso3 == "MTQ","iso3":= "FRA" ] #Martinique
  DT[DT$iso3 == "MYT","iso3":= "FRA" ] #Mayotte
  DT[DT$iso3 == "SHN","iso3":= "GBR" ] #Saint Helena, Ascension and Tristan da Cunha
  DT
}