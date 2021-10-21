
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

chatPerson <- read.delim("Chat-Up-Lines_Person.txt", header = T, sep = ",")
chatSuccess <- read.delim("Chat-Up-Lines_Success.txt", header = T, sep = ",")
ChatPer_Succ <- merge(chatPerson,chatSuccess, by="id", all = T)

# Who is flirted with more (in percentage) - men or women?
# Who is addressed with funnier chat-up lines on average - men or women?
# Who is approached with more suggestive chat-up lines on average - men or women?
# Contingency table for the success




# Task 3: Nobel Prizes ----------------------------------------------------

nobel <- read.csv("http://api.nobelprize.org/v1/laureate.csv")

