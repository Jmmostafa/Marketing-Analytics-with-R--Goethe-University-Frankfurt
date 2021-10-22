
# Final Exam Script for Marketing Analytics with R
# ========================================================================


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

# (1) Who is flirted with more (in percentage) - men or women?

ChatPer_Succ %>%
  group_by(Gender) %>% 
  summarise(percent=(sum(Success)/n())*100)

# Answer (1): Men were flirted with 52.3%.

# (2) Who is addressed with funnier chat-up lines on average - men or women?

ChatPer_Succ %>%
  group_by(Gender) %>% 
  summarise(Average=mean(Funny))

# Answer (2): Women were on an average funnier than men (4.56>3.71).

# (3) Who is approached with more suggestive chat-up lines on average - men or women?

ChatPer_Succ %>%
  group_by(Gender) %>% 
  summarise(Average=mean(Sex))

# Answer (3): Women were on an average more suggestive chat-up line than men (6.08>5.93).

# Contingency table for the success

ChatPer_Succ %>% select(Gender, Success) %>% 
  group_by(Gender,Success) %>% 
  table() %>% prop.table()


# Task 3: Nobel Prizes ----------------------------------------------------

nobel <- read.csv("http://api.nobelprize.org/v1/laureate.csv")


# (1) Bar plot for noble prize winner by gernder

barplot((prop.table(table(nobel$gender,nobel$category)))*100, beside = T,
        main = "Nobel Prizes per Category and Gender",
        ylab = "Frequency(%)",xlab = "Category",
        col = c("darkblue", "steelblue","yellow"),
        legend.text = c("Male","Female","Oragnization"),
        ylim = c(0,100),xpd = T)

# (2) Nobel Prize Shares per Category (ggplot)

nobel %>% group_by(category, share) %>% 
  summarise(Mean=mean(share)) %>% 
  ungroup() %>% group_by(category) %>% 
  summarise(Means=mean(Mean)) %>% 
  ggplot(aes(x=category,y=Means)) +
  geom_col()+
  coord_cartesian(ylim = c(0,3))+
  labs(title = "Nobel Prize Shares per Category",
        x = "Category",
        y = "Average Number of Laureates per Prize")
  
  
# The end -----------------------------------------------------------------


