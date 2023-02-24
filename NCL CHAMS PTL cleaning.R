## load in packages 
library(tidyverse)
library(readxl)
library(janitor)

## global options  (remove scientific notation)
options(scipen=999)
## set working directory to NCL waiting times report
setwd ("C:/Users/DavidEgan/OneDrive - NHS North Central London ICB/Documents/Ad Hoc/20221129 CAMHS Dashboard/Local Data")

## create empty data frame
pltcom <- data_frame()

## identify the file containing the waiting times report
captl <- list.files(pattern="*.xlsx") %>% 
  str_subset("NCL CAMHS Monthly PTL")  

for(i in 1:length(captl)){

ptl <- read_excel(
                   captl[i]
                   ,sheet = "NCL all v2"
                   ,trim_ws = TRUE
                   ,col_names = FALSE
                  ) %>% 
        clean_names() %>% 
         separate(x1, c("Trust","LAD"),"-") %>% 
          filter(is.na(x5) == FALSE & is.na(x2) == FALSE ) %>% 
           fill(c(Trust,LAD),.direction = "down") %>% 
            select(
                   -x10
                   ,-x11
                   ) %>% 
             rename(
                    Service = x2
                    ,Description = x3
                    ,"0-4"  = x4
                    ,"4-8"  = x5
                    ,"8-18" = x6
                    ,"18-52" = x7
                    ,"52+"  = x8
                    ,Total  = x9 
                   ) %>% 
              mutate( 
                     Date = str_remove(captl[i], "001918 NCL CAMHS Monthly PTL Waiting Report")
                     ,Date = str_remove(Date, ".xlsx")
                     ,Date = paste("01",Date,sep= "")
                     ,Date = as.Date(Date, format= "%d %b %Y")
                     ,`0-4` = as.numeric(`0-4`)
                     ,`4-8` = as.numeric(`4-8`)
                     ,`8-18` = as.numeric(`8-18`)
                     ,`18-52` = as.numeric(`18-52`)
                     ,`52+` = as.numeric(`52+`)
                     ,`more than 4 weeks` = `4-8` + `8-18` + `18-52` + `52+`
                     ,`more than 18 weeks` =  `18-52` + `52+`
                     ,`under 18 weeks` = `4-8` + `8-18` + `0-4`
                     )

pltcom <- rbind(ptl,pltcom)
}

write.csv( 
            pltcom
           ,"NCL_CAMHS_PTL.csv"
           ,row.names = FALSE
          )
