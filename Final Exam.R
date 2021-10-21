
# Final Exam Script
library(tidyquant)
library(tidyverse)
library(lubridate)
library(readr)
library(readxl)




# Task 1: Creating Certificate Grades -------------------------------------


cdata <- read.delim("certificates.txt", header = T, sep = ";")

cdata <- cdata %>% group_by(Firstname,Lastname) %>% 
  mutate(Avg.S=sum(COIN,REMA,ACVM)/3)

quant.Avg.S <- quantile(cdata$Avg.S,probs = c(.20,.40,.60,.80))

cdata <- cdata %>% group_by(Firstname,Lastname) %>% mutate(Grade=case_when(
  Avg.S>quantile(cdata$Avg.S,probs = .80)~"A",
  Avg.S>quantile(cdata$Avg.S,probs = .60)~"B",
  Avg.S>quantile(cdata$Avg.S,probs = .40)~"C",
  Avg.S>quantile(cdata$Avg.S,probs = .20)~"D",
  Avg.S<=quantile(cdata$Avg.S,probs = .20)~"F")
  )

cdata <- cdata %>% arrange(Lastname,Firstname)



# Task 2: Flirting --------------------------------------------------------







