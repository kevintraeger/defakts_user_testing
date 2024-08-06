##########################
# DESCRIPTIVE STATISTICS # 
##########################

rm(list = ls())

# LOADING DATA & PACKAGES
source("01_import.R")

# SUMMARY STATISTICS
summary(d)


# DISTRIBUTION TREATMENT
table(d$treat)

d %>%
  ggplot(aes(factor(treat))) +
  geom_bar(aes(y = after_stat(count)/sum(..count..), fill = treat))+
  labs(title = "Distribution Treatments", x = "Treatment", y = "Percentage")+
  scale_y_continuous(labels=percent)+
  theme(legend.position = "none")


# DISTRIBUTION GENDER
d %>%
  ggplot(aes(factor(ud1))) +
  geom_bar(aes(y = after_stat(count)/sum(..count..), fill = ud1))+
  labs(title = "Distribution Gender", x = "Treatment", y = "Percentage")+
  scale_y_continuous(labels=percent)+
  theme(legend.position = "none")


# DISTRIBUTION NUMERIC VARIABLES
d %>%
  filter(treat == 2) %>%
  select(ud1:da1, da5:da6) %>%
  multi.hist()





