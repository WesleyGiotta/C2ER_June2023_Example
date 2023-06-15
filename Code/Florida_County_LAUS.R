# Query BLS API for Local Area Unemployment Statistics (LAUS) data
# https://www.bls.gov/lau/

# clear workspace
rm(list=ls())

# # install packages
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("rjson")
# install.packages("httr")
# install.packages("openxlsx")
# install.packages("data.table")
# install.packages("rstudioapi")
# install.packages("tigris")
# install.packages("ggplot2")
# install.packages("officer")

# load required libraries
library(dplyr)        # Data transformation
library(tidyr)        # Data transformation
library(rjson)        # Using BLS API
library(httr)         # Using BLS API
library(openxlsx)     # Creating an excel workbook
library(data.table)   # Read in census data
library(rstudioapi)   # Get script location
library(tigris)       # Get FL county shapefile
library(ggplot2)      # Graph data
library(officer)      # write word documents

# set working directory based on file location (it should be the Code folder)
# Note: only works with RStudio
setwd(dirname(getActiveDocumentContext()$path))

# # Set up bls API package or use the functions copied from the GitHub page
# library(devtools)
# install_github('mikeasilva/blsAPI')
# library(blsAPI)
# # Import BLS funcations
source('BLS_functions.R')


# Pull LAUS data from BLS API ---------------------------------------------

# Florida counties (2020 geographic codes)
# https://www.census.gov/library/reference/code-lists/ansi.2020.html#cou
county_fl <- fread("https://www2.census.gov/geo/docs/reference/codes2020/cou/st12_fl_cou2020.txt",sep="|")
county_fl$areacode <- paste0("12",sprintf("%03d", county_fl$COUNTYFP))
county_fips <- sprintf("%03d", county_fl$COUNTYFP)

# County LAUS, unemployment rate, not seasonally adjusted
unempr_counties <- paste0("LAUCN12",county_fips,"0000000003")

# State LAUS, unemployment rate, not seasonally adjusted
laus_state <- "LAUST120000000000003"

# National Labor Force Statistics, unemployment rate, not seasonally adjusted
# using the Current Population Survey (CPS)
cps_nation <- "LNU04000000"

# BLS series IDs for API 
blsSeriesFull <- c(unempr_counties, laus_state, cps_nation)

# if api_version=1 we can only pull 10 years of data and 25 series at a time
# for more years or series at a time, you need to register and get a key for api_version=2
# BLS_KEY <- "Your key here"
BLS_KEY <- ""

# empty dataframe to load data into
laus_data <- data.frame()

# subset series requests to conform with API limit: https://www.bls.gov/developers/api_faqs.htm#register1
for (i in 1:ceiling(length(blsSeriesFull)/25)){

  blsSeries <- blsSeriesFull[(i*25-24):min(c(i*25,length(blsSeriesFull)))]
  
  ## What to pull from BLS API
  payload <- list('seriesid'=blsSeries,
                  'startyear'=as.numeric(format(Sys.Date(),'%Y')),
                  'endyear'=as.numeric(format(Sys.Date(),'%Y')),
                  'registrationKey'=BLS_KEY)
  
  # pull data from API as a nested list
  response <- blsAPI(payload, api_version = 1)
  json <- fromJSON(response)
  
  # data columns to join data to
  laus_bls <- apiDF(json$Results$series[[1]]$data)
  laus_bls$value <- NULL
  
  # pull data from nested list
  for (j in 1:length(blsSeries)){
    intermidiate <- apiDF(json$Results$series[[j]]$data)
    colnames(intermidiate) <- c('year','period','periodName',blsSeries[j])
    laus_bls <- laus_bls %>%
      left_join(intermidiate, by=c('year','period','periodName'))
  }
  
  if (i==1){
    laus_data <- rbind(laus_data,laus_bls)
  } else {
    laus_data <- left_join(laus_data,laus_bls, by=c('year','period','periodName'))
  }
}

# clean data
cleaned_data <- laus_data %>%
  # create date column
  mutate(Date = as.Date(paste0(year,gsub('M','',period),"01"), "%Y%m%d")) %>%
  # filter for the most recent date
  group_by() %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  # pivot data from each column is a BLS series to each row
  pivot_longer(-c(year,period,Date,periodName),
               names_to="seriescode",
               values_to='Unemployment Rate') %>%
  # pull out FIPS codes from BLS series ID
  mutate(areacode = substr(seriescode,6,10),
         # convert unemployment rate to decimal
         `Unemployment Rate` = as.numeric(`Unemployment Rate`)/100) %>%
  # join county names to FIPS codes
  left_join(county_fl, by="areacode") %>%
  # create column of region names
  mutate(Region = case_when(
    areacode == "12000" ~ "Florida",
    areacode == "00000" ~ "United States",
    TRUE ~ COUNTYNAME)) %>%
  # sort data
  arrange(areacode) %>%
  # select columns to use
  select(Date, Region, `Unemployment Rate`)


# Write workbook ----------------------------------------------------------

#create workbook
wb <- createWorkbook()

# header
header1 <- "Florida Local Area Unemployment Statistics By County"
header2 <- format(cleaned_data$Date[1],"%B %Y")
header3 <- "(Not Seasonally Adjusted)"

# foot notes
footer <- "Source: U.S. Department of Labor, Bureau of Labor Statistics, Local Area Unemployment Statistics Program, in cooperation with the Florida Department of Economic Opportunity, Bureau of Workforce Statistics and Economic Research."
notefooter <- "Note: Items may not add to totals or compute to displayed percentages due to rounding.  All data are subject to revision."
updatedate <- paste0("Last Updated: ",format(Sys.Date(),"%B %d, %Y"))

# add a worksheet for county data
addWorksheet(wb, "County")

# set column widths and row heights
setColWidths(wb, "County", cols = c(1,2), widths = c(30,25))
setRowHeights(wb, "County", rows = 1:85, heights = 16.75)
showGridLines(wb,"County",showGridLines = FALSE)
mergeCells(wb, "County", cols = 1:2, rows = 1)
mergeCells(wb, "County", cols = 1:2, rows = 2)
mergeCells(wb, "County", cols = 1:2, rows = 3)

# title and headers
writeData(wb, "County", header1, startCol = 1, startRow = 1)
writeData(wb, "County", header2, startCol = 1, startRow = 2)
writeData(wb, "County", header3, startCol = 1, startRow = 3)
writeData(wb, "County", "Unemployment Rate", startCol = 2, startRow = 5, colNames = FALSE)
writeData(wb, "County", "Region", startCol = 1, startRow = 5, colNames = FALSE)

# county unemployment rate data
writeData(wb, "County", cleaned_data[c("Region","Unemployment Rate")], startCol = 1, startRow = 6, colNames = FALSE)

# add footnotes
writeData(wb, "County", footer, startCol = 1, startRow = (nrow(cleaned_data)+7))
writeData(wb, "County", notefooter, startCol = 1, startRow = (nrow(cleaned_data)+9))

# add updated date to spread sheet
writeData(wb, "County", updatedate, startCol = 1, startRow = (nrow(cleaned_data)+11))

# add font and formatting
hs <- createStyle(fontName = "Arial", fontSize = 10, halign = "CENTER", valign = "BOTTOM") 
hs1 <- createStyle(fontName = "Arial", fontSize = 10, halign = "LEFT", valign = "BOTTOM")
fs <- createStyle(fontName = "Arial", fontSize = 10, valign = "BOTTOM")
pct <- createStyle(numFmt = "0.0%")

addStyle(wb, "County", hs, rows=1:5, cols=1:2, gridExpand = TRUE)
addStyle(wb, "County", hs1, rows=6:(nrow(cleaned_data)+5), cols=1, gridExpand = TRUE)
addStyle(wb, "County", hs, rows=6:(nrow(cleaned_data)+5), cols=2, gridExpand = TRUE)
addStyle(wb, "County", pct, rows=6:(nrow(cleaned_data)+5), cols=2, stack = TRUE)
addStyle(wb, "County", fs, rows=(nrow(cleaned_data)+7):(nrow(cleaned_data)+11), cols=1, gridExpand = TRUE)

# add borders
borderstyle1 <- createStyle(border = "BOTTOM")
borderstyle2 <- createStyle(border = "RIGHT")

addStyle(wb, "County", borderstyle1,rows = 5, cols = 1:2, stack = TRUE)
addStyle(wb, "County", borderstyle1,rows = 7, cols = 1:2, stack = TRUE)
addStyle(wb, "County", borderstyle1,rows = (nrow(cleaned_data)+5), cols = 1:2, stack = TRUE)
addStyle(wb, "County", borderstyle2,rows = 5:(nrow(cleaned_data)+5), cols = 1, stack = TRUE)
addStyle(wb, "County", borderstyle2,rows = 5:(nrow(cleaned_data)+5), cols = 2, stack = TRUE)

#save to desired location
saveWorkbook(wb,"../Output/Florida County Unemployment Rates.xlsx", overwrite = TRUE)


# Florida County Map ------------------------------------------------------

# Read in shapefile
fl_shape <- counties(state="12", year=2022)

# join unemployment data
fl_shape <- fl_shape %>%
  left_join(cleaned_data, by=c("NAMELSAD"="Region"))

# create Florida county heat map
FLheat <- ggplot() +
  geom_sf(data=fl_shape, color="gray75", aes(fill=`Unemployment Rate`*100)) +
  coord_sf(datum = NA) +
  theme_minimal() +
  labs(title = "Florida County Unemployment Rates",
       subtitle = paste0(format(cleaned_data$Date[1],"%B %Y"),", Not Seasonally Adjusted"),
       fill = "Unemployment Rate")

# Save image
ggsave("Florida Heat Map.pdf", plot=FLheat, path="../Output")


# Write a word document ---------------------------------------------------

# separate county unemployment rates
ur_counties <- cleaned_data[!cleaned_data$Region %in% c("United States","Florida"),]

# US and FL unemployment rates
usrate <- sprintf("%.1f",cleaned_data$`Unemployment Rate`[cleaned_data$Region=="United States"] * 100)
flrate <- sprintf("%.1f",cleaned_data$`Unemployment Rate`[cleaned_data$Region=="Florida"] * 100)

# list of counties with the highest and lowest unemployment rate
topCounty <- ur_counties$Region[ur_counties$`Unemployment Rate`==max(ur_counties$`Unemployment Rate`)]
bottomCounty <- ur_counties$Region[ur_counties$`Unemployment Rate`==min(ur_counties$`Unemployment Rate`)]

countymax <- paste0(topCounty,collapse=', ')
countymax <- ifelse(length(topCounty) == 2,
                    sub(",([^,]*)$", " and\\1", countymax), 
                    sub(",([^,]*)$", ", and\\1", countymax))

countymin <- paste0(bottomCounty,collapse=', ')
countymin <- ifelse(length(bottomCounty) == 2,
                    sub(",([^,]*)$", " and\\1", countymin), 
                    sub(",([^,]*)$", ", and\\1", countymin))


maxUR <- sprintf("%.1f",max(ur_counties$`Unemployment Rate`) * 100)
minUR <- sprintf("%.1f",min(ur_counties$`Unemployment Rate`) * 100)

# Month of data
URmonth <- format(cleaned_data$Date[1],"%B %Y")

# function to write the percentage point difference between two rates
high_low <- function(x){
  if (x > 1){return(paste0(sprintf('%.1f',x),' percentage points above'))}
  else if (x > 0 & x <= 1){return(paste0(sprintf('%.1f',x),' percentage point above'))}
  else if (x < -1){return(paste0(sprintf('%.1f',abs(x)),' percentage points lower than'))}
  else if (x < 0 & x >= -1){return(paste0(sprintf('%.1f',abs(x)),' percentage point lower than'))}
  else if(x == 0){return('equal to')}
}

# FL vs US
fl_us <- high_low(as.numeric(flrate) - as.numeric(usrate))

# first write up about US and FL unemployment rates
writeup1 <- paste0("The unemployment rate in Florida was ",flrate," percent in ",URmonth,
                   ", which was ",fl_us," the United States unemployment rate",
                   ifelse(fl_us=="equal to",".",paste0(" of ",usrate," percent.")))

# second write up about the FL county unemployment rates
writeup2 <- paste0(countymin," had the lowest unemployment rate (",minUR,
                   " percent) in Florida. Meanwhile, ",countymax,
                   " had the highest unemployment rate (",maxUR,
                   " percent) in Florida.")

# create word document
wd <- read_docx() %>%
  # Add a title
  body_add_par(paste0("Florida County Unemployment Rates for ",
                      format(cleaned_data$Date[1],"%B %Y"),
                      ", Not Seasonally Adjusted"), style="centered") %>%
  # Skip a line
  body_add_par("") %>%
  # first write up about US and FL unemployment rates
  body_add_par(writeup1, style="Normal") %>%
  # second write up about the FL county unemployment rates
  body_add_par(writeup2, style="Normal") %>%
  # add previously made heat map
  body_add_gg(value = FLheat, width=6.5, height=6, style="centered")

# output word document
print(wd, target="../Output/Florida County Unemployment Rates Write-Up.docx")
