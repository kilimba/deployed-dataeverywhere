#library(RODBC)
library(lubridate)

# connUR <- odbcConnect("unifiedReports")
#conn <- odbcConnect("postgres")

#language <- sqlFetch(conn,"language")
language <- read.csv("//acs-fs02/Shared/RDM/Datasets/Data Everywhere/Public Engagement Portal/language/language.csv")
hhvariables <- read.csv("//acs-fs02/Shared/RDM/Datasets/Data Everywhere/Public Engagement Portal/household/hhvariable.csv")

for(i in language$language){
  hhvariables[,i] <- as.character(hhvariables[,i])
} 

hhresponse <- read.csv("//acs-fs02/Shared/RDM/Datasets/Data Everywhere/Public Engagement Portal/household/hhresponse.csv")

# Change to lowercase all language responses
for(i in language$language){
  hhresponse[,i] <- tolower(hhresponse[,i])
}

indvariables <- read.csv("//acs-fs02/Shared/RDM/Datasets/Data Everywhere/Public Engagement Portal/individual/indvariable.csv")

for(i in language$language){
  indvariables[,i] <- as.character(indvariables[,i])
} 

indresponse <- read.csv("//acs-fs02/Shared/RDM/Datasets/Data Everywhere/Public Engagement Portal/individual/indresponse.csv")

# Change to lowercase all language responses
for(i in language$language){
  indresponse[,i] <- tolower(indresponse[,i])
}

getHSEData <- function(){
  if(file.exists("//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Households/RD06-99 ACDIS HSE-H All.rds")){
    return(readRDS("//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Households/RD06-99 ACDIS HSE-H All.rds"))
  }else{
    data <- read.csv("//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Households/RD06-99 ACDIS HSE-H All.csv")
    saveRDS(data,"//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Households/RD06-99 ACDIS HSE-H All.rds")
    return(data)
  }
  return(read.csv())
}

getIndividualData <- function(){
  if(file.exists("//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Individuals/RD07-99 ACDIS HSE-I All.rds")){
    return(readRDS("//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Individuals/RD07-99 ACDIS HSE-I All.rds"))
  }else{
    data <- read.csv("//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Individuals/RD07-99 ACDIS HSE-I All.csv")
    saveRDS(data,"//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Individuals/RD07-99 ACDIS HSE-I All.rds")
    return(data)
  }
  
}

hhdata <- getHSEData()
inddata <- getIndividualData()


addYearCol <- function(data) {
  data$Year <- year(data$VisitDate)
  return(data)
}

getTable <- function(data, variable){  
  return(table(data[,"Year"],data[,variable]))
}

getYearlyTotals <- function(table){
  Total <- margin.table(table,1)
  table <- cbind(table,Total)
  total <- as.data.frame(table[,"Total"])
  names(total) <- "Total"
  yearlyTotals <<- total
} 

getDataFrame <- function(table){
  return(as.data.frame(table,stringsAsFactors = F))
}

getData <- function(variable,type){
  #browser()
  responsevar <- variable
  if(type == "household"){
    data <- hhdata
    validresponse <- subset(hhresponse,hhresponse$variable == responsevar)
  }else if(type == "individual"){
    data <- inddata
    validresponse <- subset(indresponse,indresponse$variable == responsevar)
  }
  
  
  data <- addYearCol(data)
  table <- getTable(data,variable)
  getYearlyTotals(table)
  df <- getDataFrame(table)
  df$total <- yearlyTotals[df$Var1,]
  df$rate <- (df$Freq/df$total)*100
  df <- merge(df,validresponse,by.x = "Var2",by.y = "validresponse")
  df <- df[order(df$Var2,df$Var1),]
  return(df)
}

