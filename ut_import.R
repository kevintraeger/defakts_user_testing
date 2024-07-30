#---------------------------#
#############################
## IMPORT & PRE-PROCESSING ##
#############################
#---------------------------#


############
# PACKAGES #
############

chooseCRANmirror(ind=37)
.packages <- c("readr", "data.table", "dplyr", "plyr", "tidyverse", "ggplot2", "scales", "openxlsx", "psych")
.inst     <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
.loading  <- lapply(.packages, require, character.only=TRUE)
if(any(!unlist(.loading))) stop("looks like some library did not load")





###############
# IMPORT DATA #
###############

d_otree <- read.csv("~/experiment/user_testing_2/data/raw/data_otree_plain.csv",
                     sep = ",",
                     header = TRUE)

d_prol <- read.csv("~/experiment/user_testing_2/data/raw/prolific_demographic_data.csv",
                   sep = ",",
                   header = TRUE) %>% 
  select(!c("Custom.study.tncs.accepted.at","Reviewed.at", "Submission.id", "Status", "Completion.code")) %>% # nicht importierte columns
  dplyr::rename(prol_id = Participant.id)




##################
# PRE-PROCESSING #
##################

# LIST OF REMOVED COLUMNS
col_list_rm <- c("participant.label",
                 "participant.visited",
                 "participant._current_page_name",
                 "participant.mturk_worker_id",
                 "participant.mturk_assignment_id",
                 "participant.payoff",
                 "participant._is_bot",
                 "participant._index_in_pages",
                 "participant._max_page_index",
                 "participant.participiant_field_1",
                 "session.code",
                 "session.label",
                 "session.mturk_HITId",
                 "session.mturk_HITGroupId",
                 "session.comment",
                 "session.is_demo",
                 "session.config.participation_fee",
                 "session.config.real_world_currency_per_point",
                 "session.config.name",
                 "session.session_field_1",
                 "welcome.1.player.id_in_group",
                 "welcome.1.player.role",
                 "welcome.1.player.payoff",
                 "welcome.1.group.id_in_subsession",
                 "welcome.1.subsession.round_number",
                 "survey_1.1.player.id_in_group",
                 "survey_2.1.player.id_in_group",
                 "survey_3.1.player.id_in_group",
                 "survey_1.1.player.role",
                 "survey_2.1.player.role",
                 "survey_3.1.player.role",
                 "survey_1.1.player.payoff",
                 "survey_2.1.player.payoff",
                 "survey_3.1.player.payoff",
                 "survey_1.1.group.id_in_subsession",
                 "survey_2.1.group.id_in_subsession",
                 "survey_3.1.group.id_in_subsession",
                 "survey_1.1.subsession.round_number",
                 "survey_2.1.subsession.round_number",
                 "survey_3.1.subsession.round_number")


############  
d_proc <- d_otree %>%
  filter(participant.visited %in% 1,                # get rid of unused rows
         session.code %in% "9xeb44bb",              # get rid of wrong session
         participant.visited %in% 1,
         !participant._current_app_name == "",
         ) %>% 
  unite(prol_id, c(survey_1.1.player.pid,       # create one prolific_id
                       survey_2.1.player.pid,
                       survey_3.1.player.pid),
                       sep = "") %>%
  unite(ud1, c(survey_1.1.player.ud1,
               survey_2.1.player.ud1,
               survey_3.1.player.ud1),
        na.rm = T,  sep = "") %>%
  unite(ud2, c(survey_1.1.player.ud2,
               survey_2.1.player.ud2,
               survey_3.1.player.ud2),
        na.rm = T) %>%
  unite(ud3, c(survey_1.1.player.ud3,
               survey_2.1.player.ud3,
               survey_3.1.player.ud3),
        na.rm = T) %>%
  unite(ud4, c(survey_1.1.player.ud4,
               survey_2.1.player.ud4,
               survey_3.1.player.ud4),
        na.rm = T) %>%
  unite(ud5, c(survey_1.1.player.ud5,
               survey_2.1.player.ud5,
               survey_3.1.player.ud5),
        na.rm = T) %>%
  unite(ud6, c(survey_1.1.player.ud6,
               survey_2.1.player.ud6,
               survey_3.1.player.ud6),
        na.rm = T) %>%
  unite(ca1, c(survey_1.1.player.ca1,
               survey_2.1.player.ca1,
               survey_3.1.player.ca1),
        na.rm = T) %>%
  unite(t1, c(survey_1.1.player.t1,
               survey_2.1.player.t1,
               survey_3.1.player.t1),
        na.rm = T) %>%
  unite(t2, c(survey_1.1.player.t2,
              survey_2.1.player.t2,
              survey_3.1.player.t2),
        na.rm = T) %>%
  unite(t3, c(survey_1.1.player.t3,
              survey_2.1.player.t3,
              survey_3.1.player.t3),
        na.rm = T) %>%
  unite(t4, c(survey_1.1.player.t4,
              survey_2.1.player.t4,
              survey_3.1.player.t4),
        na.rm = T) %>%
  unite(t5, c(survey_1.1.player.t5,
              survey_2.1.player.t5,
              survey_3.1.player.t5),
        na.rm = T) %>%
  unite(t6, c(survey_1.1.player.t6,
              survey_2.1.player.t6,
              survey_3.1.player.t6),
        na.rm = T) %>%
  unite(pu1, c(survey_1.1.player.pu1,
               survey_2.1.player.pu1,
               survey_3.1.player.pu1),
        na.rm = T) %>%
  unite(pu2, c(survey_1.1.player.pu2,
               survey_2.1.player.pu2,
               survey_3.1.player.pu2),
        na.rm = T) %>%
  unite(pu3, c(survey_1.1.player.pu3,
               survey_2.1.player.pu3,
               survey_3.1.player.pu3),
        na.rm = T) %>%
  unite(pu4, c(survey_1.1.player.pu4,
               survey_2.1.player.pu4,
               survey_3.1.player.pu4),
        na.rm = T) %>%
  unite(pu5, c(survey_1.1.player.pu5,
               survey_2.1.player.pu5,
               survey_3.1.player.pu5),
        na.rm = T) %>%
  unite(us1, c(survey_1.1.player.us1,
               survey_2.1.player.us1,
               survey_3.1.player.us1),
        na.rm = T) %>%
  unite(us2, c(survey_1.1.player.us2,
               survey_2.1.player.us2,
               survey_3.1.player.us2),
        na.rm = T) %>%
  unite(us3, c(survey_1.1.player.us3,
               survey_2.1.player.us3,
               survey_3.1.player.us3),
        na.rm = T) %>%
  unite(us4, c(survey_1.1.player.us4,
               survey_2.1.player.us4,
               survey_3.1.player.us4),
        na.rm = T) %>%
  unite(us5, c(survey_1.1.player.us5,
               survey_2.1.player.us5,
               survey_3.1.player.us5),
        na.rm = T) %>%
  unite(pt1, c(survey_1.1.player.pt1,
               survey_2.1.player.pt1,
               survey_3.1.player.pt1),
        na.rm = T) %>%
  unite(pt2, c(survey_1.1.player.pt2,
               survey_2.1.player.pt2,
               survey_3.1.player.pt2),
        na.rm = T) %>%
  unite(pt3, c(survey_1.1.player.pt3,
               survey_2.1.player.pt3,
               survey_3.1.player.pt3),
        na.rm = T) %>%
  unite(pt4, c(survey_1.1.player.pt4,
               survey_2.1.player.pt4,
               survey_3.1.player.pt4),
        na.rm = T) %>%
  unite(da1, c(survey_2.1.player.da1,
               survey_3.1.player.da1),
        na.rm = T) %>%
  unite(da5, c(survey_2.1.player.da5,
               survey_3.1.player.da5),
        na.rm = T) %>%
  unite(da6, c(survey_2.1.player.da6,
               survey_3.1.player.da6),
        na.rm = T) %>%
  unite(ac1, c(survey_1.1.player.ac1,
               survey_2.1.player.ac1,
               survey_3.1.player.ac1),
        na.rm = T) %>%
  unite(ac2, c(survey_1.1.player.ac2,
               survey_2.1.player.ac2,
               survey_3.1.player.ac2),
        na.rm = T) %>%
  unite(af1, c(survey_1.1.player.af1,
               survey_2.1.player.af1,
               survey_3.1.player.af1),
        na.rm = T, sep = "") %>%
  dplyr::rename(da2 = survey_3.1.player.da2,
                da3 = survey_3.1.player.da3,
                da4 = survey_3.1.player.da4) %>%
  mutate(treat = dplyr::recode(participant._current_app_name,
                               "survey_1" = 1,
                               "survey_2" = 2,
                               "survey_3" = 3,
                               .default = 99),
         ud1 = dplyr::recode(ud1,
                             "Männlich"= 1,
                             "Weiblich"= 2,
                             "Divers" = 3,
                             "m" = 1,
                             "w" = 2,
                             "d" = 3,
                             .default = 99)) %>%
  dplyr::rename(id_session = participant.id_in_session,
                code = participant.code) %>%
  select(!c(participant.label:survey_1.1.player.payoff,
            survey_1.1.group.id_in_subsession:survey_2.1.player.payoff,
            survey_2.1.group.id_in_subsession:survey_3.1.player.payoff,
            survey_3.1.group.id_in_subsession:survey_3.1.subsession.round_number)) %>%
  relocate(c(prol_id, code, treat), .before = ud1) %>%
  relocate(c(da5, da6, af1), .after = da4) %>%
  mutate_at(c(6:39), as.numeric)
  
  




#########################
# MERGING & FILTERING #
#######################

# D_FULL == merged dataset without technically faulty observations
d_full <- d_prol %>% full_join(d_proc, by = join_by(prol_id)) %>%
  filter(!code %in% c("fihql90t", "f3kx1fp4", "4jinqv0q",                  # filter test participants
                      "k1ydrtzh", "5fglq7lh", "uc3s755q", "8dkkaw00",      # filter erroneous participants (see below)
                      "ebz9etbg",                                          # filter one "returned"
                      "3lgpbk8e", "1mo5amku",                              # filter gender inconsistencies
                      "v46i9113"),                                         # filter one participant with missing responses
         
         ac1 == 3 | ac2 == 5 ) %>%   #remove ac1/2-fails
  dplyr::rename(time = Time.taken,
         age  = Age,
         ethnic = Ethnicity.simplified,
         country_birth = Country.of.birth,
         country_resid = Country.of.residence,
         nationality = Nationality,
         language = Language,
         student = Student.status,
         employment_prol = Employment.status) %>%
  mutate(age = as.numeric(age)) %>%
  select(!c(Started.at:Archived.at, Total.approvals, Sex))



##### ZU KLÄREN

#Employment Status
#Prolific-Daten umkodieren und in Codebook einfügen?



###########
# SUBSETS #
###########

#d_treat1 <- d_clean[d_clean$participant.treat == 1,]
#d_treat2 <- d_clean[d_clean$participant.treat == 2,]
#d_treat3 <- d_clean[d_clean$participant.treat == 3,]


