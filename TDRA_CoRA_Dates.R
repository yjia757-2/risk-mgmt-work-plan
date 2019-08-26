library(dplyr)
library(openxlsx)
library(janitor)
library(reshape2)
library(lubridate) 
library(tidyverse)
library(plyr)

# narrow the report scope - start from next week Monday - the following four weeks 
today <- Sys.Date()
nextMonday <- function(date) {
  date <- ymd(date)
  .day <- wday(date, label = TRUE)
  shift <- ifelse(as.numeric(.day) < 2, 0, 7)
  date + days(2 - as.numeric(.day) + shift)
}
startDate <- nextMonday(today)
endDate <- startDate + days(28-2)

# get the original dataset
setwd("C:/Users/320024909/OneDrive - Philips/Team_Members/Phil/2019 LSP DB Schedules/TDRA")
control_tower <- read.csv("Control_Tower.csv", stringsAsFactors = FALSE, na.strings="")
control_tower <- cbind(control_tower[c(1:15, 26:39)], stack(control_tower[16:25]))
colnames(control_tower)[c(30,31)] <- c("Deal_Board_Date", "Deal_Board")

# prepare for the data - we only need stage 01-05 LSPs and their Valid Planned Dealboard which will happen in the next four weeks
control_tower <- control_tower %>% 
  mutate_at(vars(TDRA.Date, 
                 TDRA.Slides.Date, 
                 CoRA.Date, 
                 CoRA.Slides.Date,
                 Deal_Board_Date), as.Date, format="%m/%d/%Y") %>% 
  filter(Stage %in% c("01 - Pre-Qual", 
                      "04 - Propose", 
                      "02 - Qualify", 
                      "03 - Develop", 
                      "05 - Negotiation"),
         Deal_Board %in% c("Planned.DB.2",
                           "Planned.DB.3"), # only include DB2/3 since this report is only for Lorinzo's workload on DB2/3
         !is.na(Deal_Board_Date),
         between(Deal_Board_Date, startDate, endDate)) %>% 
  select(Opportunity.Name, 
         Complexity.Level, 
         Capture.Manager,
         Stage,
         TDRA.Date, 
         TDRA.Slides.Date, 
         CoRA.Date, 
         CoRA.Slides.Date,
         Deal_Board_Date,
         Deal_Board)

# easier name the deal board name by getting rid of "planned", bcz they are all planned
control_tower$Deal_Board <- str_replace(control_tower$Deal_Board, "Planned.","")
control_tower <- data.frame(apply(control_tower, MARGIN = c(1,2), function(x){
  gsub("[^[:alnum:][:blank:],-]", "", x)
}))

# deal with less compless dealboard - they usually has db2/3 combined
control_tower <- left_join(control_tower,ddply(control_tower,.(Opportunity.Name, Deal_Board_Date),nrow), 
                           by = c("Opportunity.Name", "Deal_Board_Date"))
control_tower$Deal_Board <- as.character(control_tower$Deal_Board)
for(i in 1:nrow(control_tower)){
  if(control_tower[i,"Deal_Board"] == "DB2" && control_tower[i,"V1"] == 2){
    control_tower[i,"Deal_Board"] <- "DB2/3"
  }
}
control_tower <- control_tower %>% filter(Deal_Board != "DB3" | V1 != 2)


# now combine TDRA dates and CoRA dates
tdra <- control_tower %>% select(1:6,9:10) %>% filter(Deal_Board != "DB3") %>% 
  mutate(Type_Date = "TDRA") # TDRA is required for both High & Less deals' DB 2 or DB 2/3
cora <- control_tower %>% select(1:4,7:10) %>% filter(Complexity.Level == "High Complex", Deal_Board == "DB3") %>% 
  mutate(Type_Date = "CoRA") # CoRA is only requried for High deals' DB3
colnames(tdra)[5:6] <- c("Start_Date", "End_Date")
colnames(cora)[5:6] <- c("Start_Date", "End_Date")
tdra_cora <- rbind(tdra, cora)



for(i in 1:nrow(tdra_cora)) {
  if(is.na(tdra_cora[i,"Start_Date"]) && tdra_cora[i,"Type_Date"] == "TDRA" && tdra_cora[i,"Stage"] == "03 - Develop"){
    print(tdra_cora[i,])
  }
  if(is.na(tdra_cora[i,"End_Date"]) && tdra_cora[i,"Type_Date"] == "TDRA" && tdra_cora[i,"Stage"] == "03 - Develop"){
    print(tdra_cora[i,])
  }
  if(is.na(tdra_cora[i,"Start_Date"]) && tdra_cora[i,"Type_Date"] == "CoRA" && tdra_cora[i,"Stage"] == "04 - Propose"){
    print(tdra_cora[i,])
  }
  if(is.na(tdra_cora[i,"End_Date"]) && tdra_cora[i,"Type_Date"] == "CoRA" && tdra_cora[i,"Stage"] == "04 - Propose"){
    print(tdra_cora[i,])
  }
}


write.csv(tdra_cora, "TDRA_CoRA.csv", row.names = FALSE)



