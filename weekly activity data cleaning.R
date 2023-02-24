## load in packages 
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(gsubfn)
library(knitr)

## set the file as the working directory
setwd("N:/Performance&Transformation/BusinessIntelligence/NCL Analytics/Information/3_Adhoc_Requests/NCL Wide/001909 CAMHS dashboard/Datasets/Inpatient Data (Polly Lee)")

## list all xlsx files in the wd 
xlsx <- list.files(pattern="*.xlsx")

## get all files named weekly case tracker 
track <- xlsx %>% 
              str_subset("Weekly case tracker")

## create blank borough template
borougho <- data.frame(ward= character()
                       ,borough = character()
                       ,value = as.numeric(character())
                       ,date = character()
                       ,stringsAsFactors=FALSE)

## run script to stitch data together 
for( i in 1:length(track)){

## read by borough data

boroughn <- read_excel(
                      track[i]
                     ,sheet = "by borough data"
                     ,range = "A1:P44"
                     ,col_names = TRUE
                     ,trim_ws = TRUE
                    ) %>% 
          clean_names() %>% 
          select_if(function(x) !(all(is.na(x)) | all(x==""))) %>% 
##          row_to_names(1) %>% 
          rename(
                  ward = 1
                  )  %>% 
          filter_all(any_vars(!is.na(.))) %>% 
          gather(value = "value"
                 ,key = "borough"
                 ,2:15
                 ) %>%  
          mutate(
##                 date = strapplyc(track[i], "[0-9-]{8,}", simplify = TRUE)
##                 ,date = as.Date(date, format = "%d-%m-%y")
##                 ,datakey = format(date, "%Y%m%d")
                 extractiondate = ifelse(nchar(strapplyc(track[i], "[0-9/]{8,}", simplify = TRUE)) != 12
                                          ,strapplyc(track[i], "[0-9/]{8,}", simplify = TRUE)
                                          ,ifelse(
                                            nchar(strapplyc(track[i], "[0-9-]{8,}", simplify = TRUE)) != 12
                                            ,strapplyc(track[i], "[0-9-]{8,}", simplify = TRUE)
                                            ,strapplyc(track[i], "[0-9 \\.]{8,}", simplify = TRUE)))
                 ,extractiondate =gsub("[^0-9]", "",extractiondate) 
                 ,extractiondate = ifelse(
                                           nchar(extractiondate) < 8 
                                           ,paste0(substring(extractiondate,1,4), "20",substring(extractiondate,5,6),sep = "")
                                          ,extractiondate)
                 ,extractiondate = ifelse(as.numeric(substr(extractiondate,1,4)) > "2012" &  substr(extractiondate,1,2) == "20"
                                            ,as.Date(extractiondate, format ="%Y%m%d")
                                            ,as.Date(extractiondate, format ="%d%m%Y"))
                 ,extractiondate =as.Date(extractiondate, origin = "1970-01-01")
                 ,datakey = format(extractiondate, "%Y%m%d")
                 ,date = extractiondate
                )

## bind new data ti file
 borougho = rbind(borougho,boroughn)


}



## write data to csv 
write.csv(borougho,"admission_rates.csv",row.names = FALSE)


