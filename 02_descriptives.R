##########################
# DESCRIPTIVE STATISTICS # 
##########################



# LOADING DATA & PACKAGES
source("01_import.R")



# SUMMARY STATISTICS
summary(d_full)



# DISTRIBUTION TREATMENT
table(d_full$treat)

d_full %>%
  ggplot(aes(factor(treat))) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = treat))+
  labs(title = "Verteilung Treatments", x = "Treatment", y = "Percentage")+
  scale_y_continuous(labels=percent)+
  theme(legend.position = "none")


# DISTRIBUTION GENDER
d_full %>%
  ggplot(aes(factor(ud1))) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = ud1))+
  labs(title = "Verteilung Treatments", x = "Treatment", y = "Percentage")+
  scale_y_continuous(labels=percent)+
  theme(legend.position = "none")


# VERTEILUNG NUMERISCHE VARIABLEN
d_full %>%
  filter(treat == 1) %>%
  select(ud1:pt4) %>%
  multi.hist()








