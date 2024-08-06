#---------------------------#
#############################
## IMPORT & PRE-PROCESSING ##
#############################
#---------------------------#


############
# PACKAGES #
############

chooseCRANmirror(ind=37)
.packages <- c("readr", "data.table", "dplyr", "plyr", "tidyverse", "ggplot2", "scales", "openxlsx", "psych", "stringr")
.inst     <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
.loading  <- lapply(.packages, require, character.only=TRUE)
if(any(!unlist(.loading))) stop("looks like some library did not load")



###############
# IMPORT DATA #
###############

# OTREE #

files_o <- list.files(path = "data/data_otree/", pattern = "*.csv", full.names = T)    # Load Directory
d_otree <- sapply(files_o, read_csv, simplify=FALSE, show_col_types = FALSE) %>%       # Import Data
  rbind.fill() %>% 
  filter(session.code %in% c("9xeb44bb","qje6ie6p", "e3vr0ego"),                       # removed unused lines
         !participant._current_app_name == "") %>%
  set_names(~str_replace_all(., c("[.]t1" = "[.]tr1",                                  # necessary renaming of Trust-Items (t1 - t6)
                                    "[.]t2" = "[.]tr2",
                                    "[.]t3" = '[.]tr3',
                                    "[.]t4" = '[.]tr4',
                                    "[.]t5" = '[.]tr5',
                                    "[.]t6" = '[.]tr6')))


# PROLIFIC #

files_p <- list.files(path = "data/data_prolific/", pattern = "*.csv", full.names = T) # Load Directory
d_prol <- sapply(files_p, read_csv, simplify=FALSE, show_col_types = FALSE) %>%        # Import Data
  rbind.fill() %>%
  select(!c("Custom study tncs accepted at",                                           # leave unused columns
            "Reviewed at",
            "Submission id",
            "Status",
            "Completion code")) %>% 
  dplyr::rename(pid = "Participant id")                                                # rename Prolific ID




##################
# PRE-PROCESSING #
##################


col_names <- d_otree %>% select(33:63, 69:102, 108:144) %>% names() %>% str_sub(-3,-1) %>% unique()   # extract names of columns to unify

d_otree <- col_names %>%
  lapply(function(x){unite(d_otree, x,  grep(x, names(d_otree), value = TRUE),                        # extract names of columns to unify
                            sep = '', remove = T, na.rm = T) %>% select(x)})  %>%
  bind_cols() %>%
  setNames(col_names) %>%
  mutate(id_session = d_otree$participant.id_in_session,                                              # re-import and modification of not-unified columns
         code = d_otree$participant.code,
         treat = dplyr::recode(d_otree$participant._current_app_name,
                               "survey_1" = 1,
                               "survey_2" = 2,
                               "survey_3" = 3,
                               .default = 99),
         ud1 = (case_when(ud1 %in% c("Männlich","m","1") ~ 1, 
                          ud1 %in% c("Weiblich","w","2") ~ 2, 
                          ud1 %in% c("Divers","d","3") ~ 1,
                          .default = 99))) %>%
  select(30, 38:40, 1, 29, 2:28, 32, 35:37, 33:34, 31)                                                # set order of columns

         



#############
# FILTERING #
#############
  
v_filter_codes <- c("fihql90t", "f3kx1fp4", "4jinqv0q", "tx60xj4n", "zwidfqtx", "l6722sn9",      # remove test participants
                    "k1ydrtzh", "5fglq7lh", "uc3s755q", "8dkkaw00",                              # remove erroneous participants (see below)
                    "ebz9etbg", "mrbqff67",                                                      # remove one "returned" / empty rows
                    "3lgpbk8e", "1mo5amku",                                                      # remove gender inconsistencies
                    "v46i9113",                                                                  # remove one participant with missing responses
                    "imx0iuo0", "lwlfpfby", "yv0ivd2x", "v0oqbnqr", "4aqdhfiz", "aicrnrhq", "l4f72091", "bb82czdx", "kx4m8mmb", "bh40khso", "vggno1zk", "qe32kuwd", "1pmmge9a",  "y2j0x92j", "geuywgqy", "8wvwpqs4", "kf6xfq98", "ejtx6o45", "1plwimao", "i4z9825t", "embblbhq", "psu3yro1", "e0wbzpky", "gox6xf8s", "ql6hxtb7","x7xpypuz", "ixd3yxas", "9a8cjgyu", "fusohgb0", "j3jbtfsi", "95hg3416")   # Remove Participants with multiple occurence

d_otree <- d_otree %>%                                       
  filter(!code %in% v_filter_codes,                                                             # Remove by v_filter_codes
         ac1 == 3, ac2 == 5)                                                                    # Remove AC-Fails

# manually chosen removals of duplicate participants:
# T1: imx0iuo0, lwlfpfby, yv0ivd2x, v0oqbnqr, 4aqdhfiz, aicrnrhq, l4f72091, bb82czdx, kx4m8mmb, bh40khso, vggno1zk, qe32kuwd, 1pmmge9a
# T2: y2j0x92j, geuywgqy, 8wvwpqs4, kf6xfq98, ejtx6o45, 1plwimao, i4z9825t, embblbhq, psu3yro1, e0wbzpky, gox6xf8s, ql6hxtb7
# T3: x7xpypuz, ixd3yxas, 9a8cjgyu, fusohgb0, j3jbtfsi, 95hg3416  



###########
# MERGING #
###########
  
# D_FULL == merged dataset without technically faulty observations
d <- d_prol %>% right_join(d_otree, by = join_by(pid)) %>%
  select(!c("Started at":"Total approvals", Sex)) %>%
  dplyr::rename(age  = Age,                                              # unify variable names
         ethnic = "Ethnicity simplified",
         country_birth = "Country of birth",
         country_resid = "Country of residence",
         nationality = Nationality,
         language = Language,
         student = "Student status",
         employment_prol = "Employment status") %>%
  distinct(pid, .keep_all = T) %>%                                      # leave duplicate pid
  mutate_at(c(2,12:47), as.numeric) %>%
  select(!pid) %>%                                                      # remove PID due to data security policy
  suppressWarnings()                                                    # NAs intentionally produced

rm(list=setdiff(ls(), "d"))                                             # delete superfluos objects
           
################
# OPT: SUBSETS #
################

#d_treat1 <- d_full[d_full$treat == 1,]
#d_treat2 <- d_full[d_full$treat == 2,]
#d_treat3 <- d_full[d_full$treat == 3,]

