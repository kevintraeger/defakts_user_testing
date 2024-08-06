#-----------------------#
#########################
## FURTHER INFORMATION ##
#########################
#-----------------------#

###############
# IMPORT DATA #
###############


files_o <- list.files(path = "data/data_otree/", pattern = "*.csv", full.names = T)
d_otree <- sapply(files_o, read_csv, simplify=FALSE, show_col_types = FALSE) %>% 
  rbind.fill() %>% 
  filter(session.code %in% c("9xeb44bb","qje6ie6p", "e3vr0ego"),              # removed unused rows
         !participant._current_app_name == "") %>%
  unite(pid, ends_with(".pid"),sep = "", na.rm = TRUE) %>%
  mutate(treat = dplyr::recode(participant._current_app_name,            # recode treatment
                               "survey_1" = 1,
                               "survey_2" = 2,
                               "survey_3" = 3,
                               .default = 99))




################################### 
# TABLE OF DUPLICATE PARTICIPANTS #
###################################


d_duplic <- d_otree[duplicated(d_otree$pid) | duplicated(d_otree$pid, fromLast=TRUE),] %>%
  filter(!pid =="") %>%
  select(pid,
         participant.code,
         treat) %>%
  arrange(pid)
write.xlsx(d_duplic, 'table_duplicate_pid.xlsx') # export as xlsx



################################### 
# TABLE OF DUPLICATE PARTICIPANTS #
###################################


d_ac_fail <- d_otree %>%
  filter(!survey_1.1.player.ac1 == 3 | !survey_2.1.player.ac1 == 3 | !survey_3.1.player.ac1 == 3 |
         !survey_1.1.player.ac2 == 5 | !survey_2.1.player.ac2 == 5 | !survey_3.1.player.ac2 == 5,
         !pid == 45) %>%
  mutate(item.ac1 = coalesce(survey_1.1.player.ac1, survey_2.1.player.ac1, survey_3.1.player.ac1)) %>%
  mutate(item.ac2 = coalesce(survey_1.1.player.ac2, survey_2.1.player.ac2, survey_3.1.player.ac2)) %>%
  select(participant.id_in_session,
         participant.code,
         pid,
         item.ac1,
         item.ac2,
         treat)
write.xlsx(d_ac_fail, 'table_ac_fails.xlsx') # export as xlsx

#d_ac_fail_treat2 <- d_add2 %>%
#  filter(!survey_2.1.player.ac1 == 3 | !survey_2.1.player.ac2 == 5,
#         !participant.code == "tx60xj4n") %>%
#  dplyr::rename(prolific_id = survey_2.1.player.pid,
#         ac1 = survey_2.1.player.ac1,
#         ac2 = survey_2.1.player.ac2) %>%
#  select(participant.id_in_session,
#         participant.code,
#         prolific_id,
#         ac1,
#         ac2)
#write.xlsx(d_ac_fail_treat2, 'user_testing_table_ac_fails_treat2.xlsx') # export as xlsx

# d_ac_fail_treat3 <- d_add3 %>%
#   filter(!survey_3.1.player.ac1 == 3 | !survey_3.1.player.ac2 == 5,
#          !participant.code == c("zwidfqtx","l6722sn9")) %>%
#   dplyr::rename(prolific_id = survey_3.1.player.pid,
#                 ac1 = survey_3.1.player.ac1,
#                 ac2 = survey_3.1.player.ac2) %>%
#   select(participant.id_in_session,
#          participant.code,
#          prolific_id,
#          ac1,
#          ac2)
# write.xlsx(d_ac_fail_treat3, 'user_testing_table_ac_fails_treat3.xlsx') # export as xlsx
