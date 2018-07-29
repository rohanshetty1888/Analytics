# Graded Assignment 1
# Topic     : Data Visualization in R
# Solution  : Rohan shetty (Jig4010)

# Step 1    : Data cleaning / Manipulation

# Activate the libraries
library(XML)
library(RCurl)

# Assigning source URL "The World's Most Valuable Brands List - Forbes.html" to "url"
url <- "file:///D:/EPBA/GradedAssignments/Assignment1/Dataset/The%20World's%20Most%20Valuable%20Brands%20List%20-%20Forbes.html"

# Getting the data
urldata <- getURL(url)

# Reading the HTML Table
table <- readHTMLTable(urldata,which=18)
head(table)

#Parsing the data in the html table to a data frame
data <- data.frame(table)
str(data)

#Removing unwanted column Var.1
data <- data[2:8]

View(data)

#EXporting to CSV file
setwd("D:\\EPBA\\GradedAssignments\\Assignment1")
getwd()

write.csv(data,file="brands_data.csv")

#Parsing data by removing unwanted symbols
View(data)
data$Rank=gsub("#","",data$Rank)

data$Brand.Value=gsub("\\$| B","",data$Brand.Value)

data$X1.Yr.Value.Change=gsub("%","",data$X1.Yr.Value.Change)

data$Brand.Revenue=gsub("\\$| B","",data$Brand.Revenue)

data$Company.Advertising=gsub("\\$| B","",data$Company.Advertising)

# Exporting modified data object into csv for further manual data cleaning
write.csv(data,file="brands_data_gsub.csv")

#NOTE: Post cleanup I have saved the "brands_data_gsub.csv" as "brands_data_parsed.csv"

## Mainly I have performed the following in excel as a part of cleanup
## Removed M and converted million to billion (1M = 0.001B)
## Also removed certain special characters from entries of Brands Column such as ', -, é
## Changed column name X1.Yr.Value.Change to One.Yr.Value.Change

# Importing the brands_data_parsed.csv file post cleanup
BrandsData <- read.csv("brands_data_parsed.csv",sep = ",", header = TRUE, stringsAsFactors = FALSE)
#View(BrandsData)

# Reassigned the data object to remove unwanted xtra columns
#BrandsData <- BrandsData[1:7]
#head(BrandsData)

#Count the number of rows
nrow(BrandsData)

#Verifying the Structure of data
str(BrandsData)

# Replacing Missing Value "-" with 0
BrandsData$Brand.Revenue <- gsub("-",0,BrandsData$Brand.Revenue)
BrandsData$Company.Advertising <- gsub("-",0,BrandsData$Company.Advertising)

# Converting datatype
BrandsData$Brand.Revenue <- as.numeric(BrandsData$Brand.Revenue)
BrandsData$Company.Advertising <- as.numeric(BrandsData$Company.Advertising)
#BrandsData$Brand <- as.character(BrandsData$Brand)

#Viewing cleaned Dataset
View(BrandsData)

# Exporting final cleaned dataset into csv
write.csv(BrandsData,file="brands_cleaned_data.csv",row.names=F)

#Loading dplyr library
library(dplyr)

#Filter by Technology Industry
BrandsData%>%filter(Industry=="Technology")->TechData
head(TechData)

#Counting the number of records for Technology Industry
TechData%>%nrow()


#Filter by Luxury Industry
BrandsData%>%filter(Industry=="Luxury")->LuxData
head(LuxData)

#Counting the number of records for Luxury Industry
LuxData%>%nrow()


#Filter by Financial Services Industry
BrandsData%>%filter(Industry=="Financial Services")->FinData
head(FinData)

#Counting the number of records for Financial Services Industry
FinData%>%nrow()

#Filter by Automotive Industry
BrandsData%>%filter(Industry=="Automotive")->AutoData
head(AutoData)

#Counting the number of records for Automotive Industry
AutoData%>%nrow()
