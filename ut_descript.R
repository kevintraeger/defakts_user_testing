##########################
# DESCRIPTIVE STATISTICS # 
##########################

source("ut_import.R")



# DISTRIBUTION TREATMENT
d_clean %>%
  ggplot(aes(x=factor(participant.treat))) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = participant.treat)) +
  scale_y_continuous(labels=percent)+
  labs(title = "Verteilung Treatments", x = "Treatment", y = "Percentage")+
  theme(legend.position = "none")



  

# DISTRIBUTION GENDER
d_clean %>%
  ggplot(aes(x=factor(participant.gender))) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = participant.gender)) +
  scale_y_continuous(labels=percent)+
  labs(title = "Verteilung Gender", x = "Sex", y = "Percentage")+
  theme(legend.position = "none")

str(d_clean)



# FIRST GLIMPSE
str(d_raw, list.len=ncol(d_raw))
str(d_filt$survey_1.1.player.pid)


# SUBSETS (WEITERMACHEN)
# d_treat1 <- subset(d_filt, participant.survey == 1, !select= ColA:ColB)


