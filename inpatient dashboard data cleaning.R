## load in packages 
library(tidyverse)
library(readxl)
library(janitor)
library(gsubfn)

## set the file as the working directory
setwd("N:/Performance&Transformation/BusinessIntelligence/NCL Analytics/Information/3_Adhoc_Requests/NCL Wide/001909 CAMHS dashboard/Datasets/Inpatient Data (Jim Pursell - Betty)")

## list all xlsx files in the wd 
xlsx <- file.info(list.files(pattern="*.xlsx"))

## select the most recent file 
ipfile <-rownames(xlsx)[which.max(xlsx$mtime)]

## load in the most recent file and clean 

ip <- read_excel(ipfile
                 ,sheet = "Dashboard"
                 ,col_names = TRUE
                 ,trim_ws = TRUE
                 ) %>% 
      clean_names() 



test<- ip[4:56, ]

test[1,1] <- "site"

test[1,2] <- "indicator"

## ad dwhittington to blank rows 
test[3:17,1] <- test[2,1]

## add royal free to blank rows 
test[19:24,1] <- test[18,1]

## add UCLH to blank rows 
test[26:31,1] <- test[25,1]

## add UCLH to blank rows 
test[33:38,1] <- test[32,1]

## add barnet to blank rows 
test[40:45,1] <- test[39,1]

## add chase farm to blank rows 
test[47,1] <- test[46,1]

## add ncl to blank rows 
test[49:53,1] <- test[48,1]


## turn top row to header and then drop
names(test) <- test[1,]

test <- test[-1,]


test <- test %>% 
        gather(
               value = "value"
               ,key = "date"
               ,3:812
        ) %>% 
        mutate(
              date = as.integer(date)
              ,date = as.Date(date, origin = "1899-12-30")
              ,datakey = format(date, "%Y%m%d")
              )

write.csv(test
          ,"inpatient_dashboard_data.csv"
          ,row.names = FALSE
          )