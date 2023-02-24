## load in packages 
library(tidyverse)
library(readxl)
library(janitor)

## global options  (remove scientific notation)
options(scipen=999)
## set working directory to NCL waiting times report
setwd ("C:/Users/DavidEgan/OneDrive - NHS North Central London ICB/Documents/Ad Hoc/20221129 CAMHS Dashboard/Local Data")


## identify the file containing the waiting times report
wait <- list.files(pattern="*.xlsx") %>% 
         str_subset("NCL Waiting Times Report")  


wtd <-  read_excel(wait
                   ,sheet = "All"
                   ,col_names = TRUE
                   ,trim_ws = TRUE) %>% 
          clean_names()  %>% 
           filter(is.na(x3) == FALSE)  %>%
            mutate(rownum = 1:n()
                   ,measure = case_when( (rownum >= 2 & rownum <=6)     ~ "Active Caseload (end of month)"
                                         ,(rownum >= 8 & rownum <=14)   ~ "Patients waiting to be seen- Assessment Appointment"
                                         ,(rownum >= 16 & rownum <=22)  ~ "Patients waiting to be seen- Treatment Appointment"
                                         ,(rownum >= 24 & rownum <=30)  ~ "patients seen- Assessment Appointment"
                                         ,(rownum >= 32 & rownum <=38)  ~ "patients seen- Treatment Appointment"
                                         ,(rownum >= 40 & rownum <=43)  ~ "DNA"
                                         ,(rownum >= 45 & rownum <=48)  ~ "Cancelled by Patient"
                                         ,(rownum >= 50 & rownum <=53)  ~ "Cancelled by Provider"
                                         ,(rownum >= 55 & rownum <=58)  ~ "Referrals received"
                                         ,(rownum >= 60 & rownum <=63)  ~ "Referrals rejected"
                                         , rownum == 1                  ~ "measure"
                                         ,TRUE == TRUE                  ~ "NA"
                                       )
                  ,summary_all_providers = ifelse(rownum == 1,"metric",summary_all_providers)
                  ,x2 = ifelse(rownum == 1,"Apr-2021",x2)
                  ,x3 = ifelse(rownum == 1,"May-2021",x3)
                  ,x4 = ifelse(rownum == 1,"Jun-2021",x4)
                  ,x5 = ifelse(rownum == 1,"Jul-2021",x5)
                  ,x6 = ifelse(rownum == 1,"Aug-2021",x6)
                  ,x7 = ifelse(rownum == 1,"Sep-2021",x7)
                  ,x8 = ifelse(rownum == 1,"Oct-2021",x8)
                  ,x9 = ifelse(rownum == 1,"Nov-2021",x9)
                  ,x10 = ifelse(rownum == 1,"Dec-2021",x10)
                  ,x11 = ifelse(rownum == 1,"Jan-2022",x11)
                  ,x12 = ifelse(rownum == 1,"Feb-2022",x12)
                  ,x13 = ifelse(rownum == 1,"Mar-2022",x13)
                  ,x14 = ifelse(rownum == 1,"Apr-2022",x14)
                  ,x15 = ifelse(rownum == 1,"May-2022",x15)
                  ,x16 = ifelse(rownum == 1,"Jun-2022",x16)
                  ,x17 = ifelse(rownum == 1,"Jul-2022",x17)
                  ,x18 = ifelse(rownum == 1,"Aug-2022",x18)
                  ,x19 = ifelse(rownum == 1,"Sep-2022",x19)
                  ,x20 = ifelse(rownum == 1,"Oct-2022",x20)
                  ,x21 = ifelse(rownum == 1,"Nov-2022",x21)
                  ,x22 = ifelse(rownum == 1,"Dec-2022",x22)
                  ,x23 = ifelse(rownum == 1,"Jan-2023",x23)
                  ,x24 = ifelse(rownum == 1,"Feb-2023",x24)
                  ,x25 = ifelse(rownum == 1,"Mar-2023",x25)
                  
                   ) %>%
             mutate(rownum = ifelse(rownum == 1,"rownum" , rownum)) %>% 
              row_to_names(1,remove_row = TRUE,remove_rows_above = TRUE) %>% 
               select(-rownum) %>% 
                filter(measure != "NA") %>% 
                 gather(key = "date"
                        ,value = "value"
                        ,c(-metric,-measure)
                        ) %>% 
                  mutate(
                         value = round(as.numeric(value),3)
                         ,date = paste("01-",date,sep ="")
                         ,date = as.Date(date, format = "%d-%b-%Y")
                         ,datakey = format(date, "%Y%m%d")
                         )
         
write.csv(wtd,"NCL_Waiting_Times.csv",row.names = FALSE)             

