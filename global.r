suppressMessages(library(shinydashboard))
#library(data.table)
suppressMessages(library(openxlsx))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(ggmap))
suppressMessages(library(data.table))
suppressMessages(library(plotly))
suppressMessages(library(leaflet))
suppressMessages(library(htmltools))
suppressMessages(library(RColorBrewer))
suppressMessages(library(threejs))
suppressMessages(library(DT))
suppressMessages(library(shiny))
suppressMessages(library(lubridate))
suppressMessages(library(stringr))
suppressMessages(library(fuzzyjoin))
suppressMessages(library(tidyverse))
suppressMessages(library(flexdashboard))
Sys.setlocale("LC_TIME", "C")
# Folder for my computer
tfpath="D:/STUDIES/01_DataAnalytics/CBAnalytics/Data"
if (dir.exists(tfpath))
{
  setwd(tfpath)
  fpath=tfpath
}
# Folder for my laptop
tfpath="/Users/Chimbu/Desktop/Work/Personal/DataAnalytics/CBAnalytics/Data"
if (dir.exists(tfpath))
{
  setwd(tfpath)
  fpath=tfpath
}

if (!exists("AccDB"))
{
  f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
  KeywordMapping = read.xlsx("KeywordMapping.xlsx",sheet = 1,startRow = 1, colNames = TRUE,detectDates = TRUE,rowNames = FALSE)
  CitiAccDB = read.xlsx("Citibank.xlsx",sheet = 1,startRow = 1, colNames = TRUE,detectDates = TRUE,rowNames = FALSE)
  files <- list.files(path =fpath,pattern = ".CSV")
  temp <- lapply(files, fread,sep=",",na.strings="")
  AccDB <- rbindlist(temp)
  #Column based duplicate remove because special CHARS in booking text makes difference
  # Remove rows with 0
  AccDB<-AccDB[!(AccDB$Amount==0),]
  #Change column names
  OldNames=c("Transaction date","Value date","Transaction type","Booking text")
  NewNames=c("TransactionDate","ValueDate","TransactionType","BookingText")
  colnames(AccDB)[which(colnames(AccDB) %in% OldNames )] <- NewNames
  # temp comment - 08apr
  AccDB = AccDB %>%
    distinct(TransactionDate,ValueDate,Amount, .keep_all = TRUE)
  #remove temp names
  rm(OldNames,NewNames)
  #
  #Change Dates
  #as.numeric()
  CitiAccDB$Withdrawals=as.numeric(CitiAccDB$Withdrawals)
  CitiAccDB$Deposits=as.numeric(CitiAccDB$Deposits)
  AccDB$TransactionDate=as.Date(AccDB$TransactionDate, "%d.%m.%Y")
  AccDB$ValueDate=as.Date(AccDB$ValueDate, "%d.%m.%Y")
  #CitiAccDB$Date= parse_date_time(x = CitiAccDB$Date,orders = c("Y-m-d","d/m/Y"),locale = "eng")
  #nzd$date <- format(as.Date(nzd$date, format = "%d/%m/%Y"), "%Y-%m-%d")
  #CitiAccDB$Date <-format(as.Date(CitiAccDB$Date, format = "%d.%m.%Y"), "%Y-%m-%d")
  CitiAccDB$Date <-format(as.Date(CitiAccDB$Date, format = "%Y-%m-%d"), "%d.%m.%Y")
  tempDate=dmy(AccDB$TransactionDate)
  AccDB$TransactionYear= year(AccDB$TransactionDate)
  AccDB$TransactionMonth= month(AccDB$TransactionDate,label = TRUE)
  AccDB$TransactionDay<- weekdays(AccDB$TransactionDate)
  rm(tempDate)
  #
  # InsuranceExpenses=
  #
  AccDB$Category=ifelse(AccDB$Amount < 0,'Expense','Income')
  CitiAccDB$Category=ifelse(CitiAccDB$Withdrawals > 0,"Expense","Income")
  AccDB$Amount=abs(AccDB$Amount)
 # AccDB=AccDB %>% fuzzy_inner_join(KeywordMapping, by = c("BookingText" = "SearchKeyword"), match_fun = str_detect)
  #AccDB=AccDB %>% fuzzy_left_join(KeywordMapping, by = c("BookingText" = "SearchKeyword"), match_fun = str_detect ,ignore_case=TRUE)
  AccDB=regex_left_join(AccDB,KeywordMapping, by = c("BookingText" = "SearchKeyword"),ignore_case=TRUE)
   #AccDB=merge(AccDB, KeywordMapping, by.x = "BookingText", by.y = "SearchKeyword")
  # 
  # Exceptions
  # Reporting ticket amount
  AccDB$Company=ifelse(AccDB$Amount==1490.55,'Lufthansa',AccDB$Company)
  #Estimate Inputs
  ValTransactionYear=unique(AccDB$TransactionYear[!is.na(AccDB$TransactionYear)])
  ValTransactionMonth=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  ValTransactionDay=c("Mon","Tue","Wed","Thu","Fri","Sar","Sun")
  StandardExpenses=c('G+B Housing','India Citibank','Commerz Bank','HOME Internet','VATTENFALL','Deutsche Bank','Post Bank','KITA','BVG')
  ValSegment = AccDB %>% select(Segment) %>% unique()
  tmp = AccDB %>% filter(!Company %in% StandardExpenses) %>% select(Company) %>% unique() %>% arrange(Company)
  IrregularExpenses=tmp$Company
  #
  # To remove mentioned columns
dropscol=c("Account of initiator","IBAN of account of initiator","Bank code of account of initiator","SearchKeyword")
AccDB=AccDB[ , !(names(AccDB) %in% dropscol)]
rm(tmp)
rm(temp)
rm(dropscol)
 #
  #
  
}
