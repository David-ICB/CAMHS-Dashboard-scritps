library(tidyverse)
library(readxl)
library(janitor)
library(naniar)

## set working directory to CDP data files
setwd ("C:/Users/DavidEgan/OneDrive - NHS North Central London ICB/Documents/Ad Hoc/20221129 CAMHS Dashboard/Local Data")

xlsx <- list.files(pattern="*.xlsx") %>% 
          str_subset("CDP backing")  

## create empty data frame 
tableau <- data.frame(mapping= character(),
                  reporting_period = as.Date(character()) 
                  ,org_type = character()
                  ,data_source = character()
                  ,org_code = character()
                  ,org_name = character()
                  ,region_code = character()
                  ,region_name = character()
                  ,stp_code = character()
                  ,stp_name = character()
                  ,plan = character()
                  ,percent_trajectory_achieved = character()
                  ,percent_plan_achieved = character()
                  ,metric = character()
                  ,measurement = as.numeric(character())
                  ,stringsAsFactors=FALSE)


## read in and stich data  together 

for (i in 1:length(xlsx)){

sheet1 <- read_excel(xlsx[i]
                      ,sheet = "tableau"
                      ,col_names = TRUE
                      ,trim_ws = TRUE) %>% 
           clean_names() %>% 
           gather(key = "metric"
                  ,value = "measurement"
                  ,11:53
                  
                  ) %>% 
           filter(
             measurement != "NULL"
           ) %>% 
           mutate(
             measurement = as.numeric(measurement)
           )

tableau <- rbind(tableau,sheet1)
}

##clean tableau data 
tableau <- tableau %>% 
           mutate(
                   plan = as.numeric(ifelse(
                                  plan == "NULL"
                                  ,""
                                  ,plan
                                  ))
                   ,target = as.numeric(ifelse(
                                   target == "NULL"
                                   ,""
                                   ,target
                                  ))
                   ,percent_plan_achieved = as.numeric(ifelse(
                                                    percent_plan_achieved == "NULL"
                                                   ,""
                                                   ,percent_plan_achieved
                                                 ))
                   ,percent_trajectory_achieved = as.numeric(ifelse(
                                                         percent_trajectory_achieved == "NULL"
                                                         ,""
                                                         ,percent_trajectory_achieved
                                                         ))
                  ,datekey = format(reporting_period, "%Y%m%d")
                )
## save dataset
write.csv(tableau
          ,"CDP data.csv"
          ,row.names = FALSE
          )
## E1 Data 

e1 <- tableau %>%
      filter(
                ## select NCL STP
                org_type == 'STP'
                ,org_code == 'QMJ'
                ,data_source == "CYP 1+ Actual"
                 ) %>% 
     select(
       reporting_period
       ,measurement
       ,plan
       ,target
     ) %>% 
     arrange(
     reporting_period
     )



##write e1 tata 
 write.csv(e1
           ,"cyp_e1_data.csv"
            ,row.names = FALSE)