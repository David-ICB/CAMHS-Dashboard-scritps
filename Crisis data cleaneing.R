library(tidyverse)
library(readxl)
library(janitor)
library(naniar)

## set working directory to CDP data files
setwd ("C:/Users/DavidEgan/OneDrive - NHS North Central London ICB/Documents/Ad Hoc/20221129 CAMHS Dashboard/Local Data")

## look at all xlsx files in the folder 
xlsx <- list.files(pattern="*.xlsx")

## identify the file containing crisis data 
crisisdata <- xlsx %>% 
              str_subset("Crisis Data")  

## Hubs data 
hubs <- read_excel(crisisdata
                   ,sheet = "Hubs"
                   ,col_names = TRUE
                   ,trim_ws = TRUE) %>% 
        clean_names() %>% 
        mutate(
                referral_date_clean = as.Date(ifelse(nchar(referral_date) == 5 
                                             ,as.Date(as.numeric(referral_date), origin = "1899-12-30")
                                             ,as.Date(referral_date, format = "%d/%m/%Y")
                                             ), origin =  "1970-01-01")
                ,datekey = format(referral_date_clean, "%Y%m%d")
                ,pts = 1
                ,referral_source = case_when(
                                              (str_detect(referrer,"A&E") | str_detect(referrer, " ED "))  ~ "A&E"
                                              ,(str_detect(referrer,"Whit") | str_detect(referrer, "RF") | str_detect(referrer, "Royal Free")
                                               | str_detect(referrer, "UCLH") | str_detect(referrer, "NHUH")) ~"Trust"
                                              ,is.na(referrer) == TRUE ~"Unknown"
                                              ,TRUE == TRUE ~"Other"
                  
                                             )
               ) %>% 
        select(
                 location
                ,referral_date_clean   
                ,datekey
                ,referral_source
                ,pts
               )

write.csv(hubs,"Crisis_hub.csv",row.names = FALSE)

##Crisis line 
line <- read_excel(crisisdata
                   ,sheet = "CrisisLine"
                   ,col_names = TRUE
                   ,trim_ws = TRUE) %>% 
        clean_names() %>%  
        mutate(
                date_clean = as.Date(ifelse(nchar(date) == 5 
                                                     ,as.Date(as.numeric(date), origin = "1899-12-30")
                                                     ,as.Date(date, format = "%d/%m/%Y")
                 ), origin =  "1970-01-01")
                ,datekey = format(date_clean, "%Y%m%d")
                ,pts = 1
                  
              ) %>%  
       select(
         date_clean
         ,datekey
         ,call_source
         ,call_type
         ,outcome
         ,team
         ,pts
       )

write.csv(line,"Crisis_line.csv",row.names = FALSE)

## crisis team 
team <- read_excel(crisisdata
                   ,sheet = "Crisis team"
                   ,col_names = TRUE
                   ,trim_ws = TRUE) %>% 
       clean_names()  %>%  
  mutate(
         date_clean = as.Date(ifelse(nchar(date_referred) == 5 
                                ,as.Date(as.numeric(date_referred), origin = "1899-12-30")
                                 ,as.Date(date_referred, format = "%d/%m/%Y")
                              ), origin =  "1970-01-01")
         ,datekey = format(date_clean, "%Y%m%d")
         ,pts = 1
         ,time_seen_clean = format(as.POSIXct(as.Date(as.numeric(time_seen),origin="1899-12-30")), format = "%H:%M:%S")
         ,time_seen_group = case_when(
                                is.na(time_seen) == TRUE   ~ "Unkown"
                                ,format(as.POSIXct(as.Date(as.numeric(time_seen),origin="1899-12-30")), format = "%H:%M:%S") >= format(as.POSIXct("09:00:00", format = "%H:%M:%S"), format = "%H:%M:%S")
                                 &
                                 format(as.POSIXct(as.Date(as.numeric(time_seen),origin="1899-12-30")), format = "%H:%M:%S") < format(as.POSIXct("17:00:00", format = "%H:%M:%S"), format = "%H:%M:%S")
                                 ~ "Morning"
                                ,TRUE == TRUE  ~ "Evening"
                               )
         ,local_camhs_team = case_when(
                                        str_detect(local_camhs,"Barnet")         ~ "Barnet"
                                        ,str_detect(local_camhs,"Enfield")       ~ "Enfield"
                                        ,str_detect(local_camhs,"Camden")        ~ "Camden"
                                        ,str_detect(local_camhs,"Islington")     ~ "Islington"
                                        ,str_detect(local_camhs,"Haringey")      ~ "Haringey"
                                        ,str_detect(local_camhs,"OOA")           ~ "OOA"
                                        ,str_detect(local_camhs,"Out of area")   ~ "OOA"
                                        ,is.na(local_camhs)                      ~ "Unknown"
                                        ,TRUE == TRUE                            ~ "Other"      
                                        
                                       )
        
         ) %>% 
  select(
         date_clean
         ,datekey
         ,time_seen_group
         ,primary_reason_for_referral
         ,local_camhs_team
         ,pts
         )
    
write.csv(team,"Crisis_team.csv",row.names = FALSE)
