
# Check duplicates --------------------------------------------------------

#Start by defining duplicate rows into a single list of multiple dfs.
#ids var defines the vars for which dups have been checked
#These will need to be checked by hand
ids <- c("fs_id", "consent_id", "sample_collection_id_a", "sample_collection_id_c")

#function to select duplicates
select_dups <- function(df, var){
    dup <- df[[var]][duplicated(df[[var]])]
    df <- df[df[[var]] %in% dup & !is.na(df[[var]]),]
    df$dup_problem <- var
    df <- df[c(ncol(df), 2:ncol(df)-1)]
    df
}

# duplicate_df <- map_df(ids, ~ select_dups(fs_pcr_elisa, .x))


# Join PCR ----------------------------------------------------------------

#Need to group_by sample_collection_id and then join to reduce duplicates
pcr_final <- pcr_definitive %>% 
  group_by(sample_collection_id_acute) %>% 
  summarise(sample_codes = str_c(sample_code_acute, collapse = "--"),
            result_final = str_c(result_final, collapse = "--")) 


fs_pcr <- fs_samples_raw %>% 
  select(-consent_id, -case_date, -first_symptom_date, -prior_dengue) %>% 
  left_join(pcr_final, by = c("sample_collection_id_a" = "sample_collection_id_acute"))



# Join ELISA --------------------------------------------------------------

#Create list of lists. 2 Lists IgM IgG: Each with individual pathogens
elisa_list <- elisa_raw %>% 
  select(sample_collection_id, sample_code, target_pathogen, result, titer, immunoglobulin) %>% 
  split(.$immunoglobulin) %>% 
  map(~ .x %>% split(.$target_pathogen))
  
#Rejoin all Igs  
elisa_all_igm <- Reduce(function(...) full_join(..., by='sample_code', all.x=TRUE), elisa_list$IgM)

#Group by sample_collection_id:summarise all IgM DENV to a row per sample_collection_id. (same sample may have different codes)
elisa_igm_den <- elisa_list$IgM$DENV %>% 
    group_by(sample_collection_id) %>% 
    summarise(result = str_c(result, collapse = "--"),
              titer = str_c(titer, collapse = "--")) 
  
titre_tidy <- function(x){str_replace(x, "1:|1/", "")}
fs_pcr_elisa <- fs_pcr %>% 
  left_join(elisa_igm_den %>% select(sample_collection_id, result_igm_acute = result, titer_igm_acute = titer), 
            by = c("sample_collection_id_a" = "sample_collection_id")) %>% 
  left_join(elisa_igm_den %>% select(sample_collection_id, result_igm_con = result, titer_igm_con = titer), 
            by = c("sample_collection_id_c" = "sample_collection_id"))  %>% 
  mutate_at(vars(matches("titer")), titre_tidy) %>%
  mutate(conv_doi = second_specimen_date - first_specimen_date, 
          result_igm_acute = str_replace(result_igm_acute, "negative--negative", "negative"),
         result_igm_con = str_replace(result_igm_con, "negative--negative", "negative"),
         titer_igm_acute = str_replace(titer_igm_acute, "negative--negative", "negative"),
         titer_igm_con = str_replace(titer_igm_con, "negative--negative", "negative"),
         titer_igm_acute = str_replace(titer_igm_acute, "positive--positive", "positive"),
         titer_igm_con = str_replace(titer_igm_con, "positive--positive", "positive"),
         result_igm_acute = str_replace(result_igm_acute, "positive--positive", "positive"),
         result_igm_con = str_replace(result_igm_con, "positive--positive", "positive"),
        serol = case_when(is.na(result_igm_con) & is.na(result_igm_acute) ~ "NA",
                           (result_igm_acute == "negative" & result_igm_con == "negative") | 
                             (titer_igm_acute  == "negative" & titer_igm_con  == "negative") ~  "NEG",
                           titer_igm_acute == "100" & titer_igm_con == "100" ~ "100",
                           (titer_igm_acute == "100" & is.na(titer_igm_con)) | (titer_igm_acute  == "100" & titer_igm_con == "negative") ~ "100A",
                           (titer_igm_con == "100" & is.na(titer_igm_acute)) | (titer_igm_acute  == "negative" & titer_igm_con == "100") ~ "100C",
                           as.numeric(titer_igm_con)/as.numeric(titer_igm_acute) >= 4 | 
                             titer_igm_acute  == "negative" & as.numeric(titer_igm_con) > 100 |
                             result_igm_acute  == "negative" & as.numeric(titer_igm_con) > 100 ~ "SEROC",
                           as.numeric(titer_igm_acute) > 100 & is.na(titer_igm_con) ~ "ACUTE",
                           as.numeric(titer_igm_acute) > as.numeric(titer_igm_con) |
                             (as.numeric(titer_igm_acute) > 100 & titer_igm_con == "negative") ~ "DECLINE",
                           as.numeric(titer_igm_acute) < as.numeric(titer_igm_con) ~ "INCREASE",
                           titer_igm_acute == titer_igm_con ~ "SAME",
                           result_igm_acute == "negative" & titer_igm_con == "negative" ~ "NEG",
                           titer_igm_acute == "negative" & result_igm_con == "negative" ~ "NEG",
                           (result_igm_acute == "negative"  | titer_igm_acute == "negative") & is.na(result_igm_con) ~ "NEG_NO_CON",
                          result_igm_con == "negative" & is.na(result_igm_acute) ~ "NEG_NO_ACU",
                          str_detect(titer_igm_con, "\\d") ~ "CON",
                          str_detect(titer_igm_acute, "100") ~ "100A",
                          str_detect(titer_igm_acute, "\\d") ~ "CON")) %>% 
  mutate(combo = case_when(str_detect(result_final, "DEN|ZIK") ~ result_final,
                           serol %in% c("ACUTE", "CON", "SEROC") ~ "SEROC",
                           result_final == "NEG" | serol == "NEG" ~ "NEG"),
         combo = case_when(combo == "DEN2--DEN2" ~ "DEN2",
                           combo == "DEN3--DEN3" ~ "DEN3",
                           combo == "DEN4--DEN4" ~ "DEN4",
                           TRUE ~ combo)) %>% 
  # filter(!is.na(combo)) %>% 
  group_by(fs_id) %>% 
  sample_n(1) %>% 
  select(sex, birthdate, fs_id, result = combo, participant_codes)





    

# NOT CLEAN ---------------------------------------------------------------



test <- fs_pcr_elisa %>% 
  inner_join(fs_raw, by = "fs_id") %>% 
  mutate(doi = date - first_symptom_date) %>% 
  arrange(fs_id, doi) 
  

# Dummify symptom_daily ---------------------------------------------------

#Create vector of unique possible symptoms
fs_symptoms_vec <- str_split(test$symptoms_daily, "; ")
fs_symptoms_vec <- sort(unique(unlist(fs_symptoms_vec)))

fs_symptoms_vec <- fs_symptoms_vec[fs_symptoms_vec != "Asymptomatic" & fs_symptoms_vec != "None"]
#str_detect doens't like bracketrs as special character so remove these
fs_symptoms_vec <- str_trim(str_replace_all(fs_symptoms_vec, "\\(.*\\).*", ""))

#Function to create list logicals. Each element of subsequent list is a single logical columne stating 
#whether or not they had experienced that specific symtpom 
symp_list <- map(fs_symptoms_vec, function(x){
  str_detect(test$symptoms_daily, x)
})

#Name eachelement of the list so the relevant symptom is asigned to the relvant variable (new more succinct names)
symp_names <- {c("abdominal_pain", "back_pain", "gum_bleed", 
                 "blood_stools", "body_pain", "chest_pain", 
                 "chills", "cough", "diarrhea", "dizziness", 
                 "bad_taste", "ear_pain", "nose_bleed", "sputum", 
                 "retro_orbital_pain", "fever", "headache", 
                 "hematemesis", "hepatomegaly", "itch", 
                 "jaundice", "joint_pain", "anorexia", 
                 "maculopapular_rash", "muscle_pain", "rhinorrhea", 
                 "nausea",  "petechiae", "photophobia", "purpura", 
                 "erysipelas", "sore_throat", "splenomegaly", 
                 "vaginal_bleeding", "vomiting", "weakness_malaise")}
names(symp_list) <- symp_names

#Rejoin individual elements of list into df
symp_df <- do.call(bind_cols, symp_list)  
pd_symptoms_dummy <- bind_cols(test, symp_df) %>% 
  mutate(day_illness = as.integer(date - first_symptom_date)) 
  


# Create matrix template --------------------------------------------------
#Day 1 to 20 matrix
consent_id <- unique(pd_symptoms_dummy$consent_id) #list of all participants
consent_id_rep <- rep(consent_id, each = 20) #vector of repeat study (joining var 1)
day_illness <- rep(1:20, length(consent_id)) #vector of illness days (joining var 2)
fs_matrix_template <- data_frame(consent_id = consent_id_rep, 
                                 day_illness = day_illness) %>% 
  left_join(test %>% select(consent_id, result) %>% distinct(), by = "consent_id")

#Rejoin original data to template. There may be missing days as forms were not always completed up to 
#the final date
fs_matrix <- fs_matrix_template %>% 
  left_join(pd_symptoms_dummy, by = c("consent_id", "day_illness"))

#Fill after values with false (Surveys are no longer completed after symptoms are not reported)
fs_matrix[symp_names] <- map(fs_matrix[symp_names], function(x){
  case_when(is.na(x) ~ FALSE, TRUE ~ x)
})


#Any symptom variable added (whether any synptom was experienced on this day)
fs_matrix$any_symp <- apply(fs_matrix[symp_names], MARGIN = 1, any, na.rm= TRUE)
#Num symptom var added (total number of symptoms experienced on a certain day)
fs_matrix$total_symp <- apply(fs_matrix[symp_names], MARGIN = 1, sum, na.rm= TRUE)
#filter out those who have no symptoms on the first day
no_symptoms_day1 <- fs_matrix$consent_id[fs_matrix$day_illness == 1 & fs_matrix$total_symp == 0]
fs_matrix <- fs_matrix %>% 
  filter(!consent_id %in% no_symptoms_day1)

