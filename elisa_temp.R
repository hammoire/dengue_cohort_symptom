#Currently working on dengue ELisa
elisa_deng <- elisa_raw %>% 
  filter(str_detect(target_pathogen, "DEN"), immunoglobulin != "IgG") 

 


test <- fs_samples_results_pcr %>% 
  left_join(elisa_deng %>%     
              select(sample_code, result_igm_acute = result, titer_igm_acute = titer), 
            by = c("sample_code_acute" = "sample_code")) %>% 
  left_join(elisa_deng %>%     
              select(sample_code, result_igm_con = result, titer_igm_con = titer), 
            by = c("sample_code_con" = "sample_code")) %>% 
  mutate(titer_igm_acute = as.numeric(str_replace(titer_igm_acute, "1:|1/", "")),
         titer_igm_con = as.numeric(str_replace(titer_igm_con, "1:|1/", "")),
         interval = sample_date_con - sample_date_acute)



write_csv(test, "~/Desktop/elsia.csv")


#Process elisas
#Break into IGM and IGG lists and then of each pathogen so a list of 2 with multiple sublist per pathogen

elisa_ig_path_list <- elisa_raw %>% 
  filter(sample_code %in% fs_samples_results_pcr$sample_code_acute) %>% 
  select(sample_code, target_pathogen, result, titer, immunoglobulin) %>% 
  split(.$immunoglobulin) %>% 
  map(~ .x %>% split(.$target_pathogen))

#Then serial full joins using the reduce function to bind all dfs
###########
####IGM####
###########
elisa_igm_path_df <- Reduce(function(...) full_join(..., by='sample_code', all.x=TRUE), elisa_ig_path_list$IgM) 

#rename the vars 
igm_paths <- names(elisa_ig_path_list$IgM) #use names of paths in original list and bind these to current vars
names(elisa_igm_path_df) <- c("sample_code" , str_c(rep(igm_paths, each = 4), "_", c("target_pathogen", "result", "titer", "immunoglobulin"), "_IGM"))

#Deselect redundant variables
elisa_igm_path_df <- elisa_igm_path_df %>% 
  # select(sample_code, matches("result|titer")) %>% #All pathogens
  # filter(sample_code != "POA0364" | VEEV_result_IGM != "negative") %>% #filter this out using all pathogens
  select(sample_code, matches("DEN.*IGM")) %>% #just dengue
  distinct()

###########
####IGG####
###########

elisa_igg_path_df <- Reduce(function(...) full_join(..., by='sample_code', all.x=TRUE), elisa_ig_path_list$IgG) 

#rename the vars 
igg_paths <- names(elisa_ig_path_list$IgG) #use names of paths in original list and bind these to current vars
names(elisa_igg_path_df) <- c("sample_code" , str_c(rep(igg_paths, each = 4), "_", c("target_pathogen", "result", "titer", "immunoglobulin"), "_IGG"))

#Deselect redundant variables
elisa_igm_path_df <- elisa_igg_path_df %>% 
  # select(sample_code, matches("result|titer")) %>% #All pathogens
  select(sample_code, matches("DEN.*IGM")) %>% 
  distinct()

#Join IGM to original data
elsia_deng <- fs_samples_results_pcr %>% 
  left_join(elisa_igm_path_df, by = c("sample_code_acute" = "sample_code")) %>% View()
      left_join(elisa_igm_path_df, by = c("sample_code_con" = "sample_code")) 
  



fs_samples_results_pcr_elisa[str_subset(names(fs_samples_results_pcr_elisa), "titer")] <- map(fs_samples_results_pcr_elisa[str_subset(names(fs_samples_results_pcr_elisa), "titer")], ~ as.numeric(str_replace(.x, "1:", "")))


function(df, res, acute, conv){
  case_when(df[[res]] == "positive" | df[[conv]]/df[[res]])
}

case_when(ALLV_result_IGM.x == "positive" | (ALLV_titer_IGM.y/ALLV_titer_IGM.x) > 4 ~ "positive",
          TRUE ~ NA_character_))

fs_samples_results_pcr_elisa %>% 
  mutate(ALL_final = case_when(ALLV_result_IGM.x == "positive" | (ALLV_titer_IGM.y/ALLV_titer_IGM.x) > 4 ~ "positive",
                               TRUE ~ NA_character_))



fs_samples_results_pcr_elisa_vars <- c("participant_id", "sex", "birthdate", "participant_codes", 
  "project_code", "study", "fs_id", "consent_id", "case_date", 
  "first_symptom_date", "first_specimen_date", "second_specimen_date", 
  "prior_dengue", "sample_code_acute", "sample_date_acute", "sample_collection_id_acute", 
  "sample_code_con", "sample_date_con", "sample_collection_id_con", 
  "age", "result_final", "ALLV_result_IGG.x", "ALLV_result_IGG.y", "ALLV_result_IGM.x", 
                           "ALLV_result_IGM.y", "ALLV_titer_IGG.x", "ALLV_titer_IGG.y", 
                           "ALLV_titer_IGM.x", "ALLV_titer_IGM.y", "ANDV_result_IGG.x", 
                           "ANDV_result_IGG.y", "ANDV_result_IGM.x", "ANDV_result_IGM.y", 
                           "ANDV_titer_IGG.x", "ANDV_titer_IGG.y", "ANDV_titer_IGM.x", "ANDV_titer_IGM.y", 
                           "CARV_result_IGM.x", "CARV_result_IGM.y", "CARV_titer_IGM.x", 
                           "CARV_titer_IGM.y", "CATV_result_IGG.x", "CATV_result_IGG.y", 
                           "CATV_result_IGM.x", "CATV_result_IGM.y", "CATV_titer_IGG.x", 
                           "CATV_titer_IGG.y", "CATV_titer_IGM.x", "CATV_titer_IGM.y", "CDUV_result_IGG.x", 
                           "CDUV_result_IGG.y", "CDUV_result_IGM.x", "CDUV_result_IGM.y", 
                           "CDUV_titer_IGG.x", "CDUV_titer_IGG.y", "CDUV_titer_IGM.x", "CDUV_titer_IGM.y", 
                           "DENV_result_IGG.x", "DENV_result_IGG.y", "DENV_result_IGM.x", 
                           "DENV_result_IGM.y", "DENV_titer_IGG.x", "DENV_titer_IGG.y", 
                           "DENV_titer_IGM.x", "DENV_titer_IGM.y", "EEEV_result_IGM.x", 
                           "EEEV_result_IGM.y", "EEEV_titer_IGM.x", "EEEV_titer_IGM.y", 
                           "EMCV_result_IGM.x", "EMCV_result_IGM.y", "EMCV_titer_IGM.x", 
                           "EMCV_titer_IGM.y", "ESCV_result_IGG.x", "ESCV_result_IGG.y", 
                           "ESCV_result_IGM.x", "ESCV_result_IGM.y", "ESCV_titer_IGG.x", 
                           "ESCV_titer_IGG.y", "ESCV_titer_IGM.x", "ESCV_titer_IGM.y", "GROV_result_IGM.x", 
                           "GROV_result_IGM.y", "GROV_titer_IGM.x", "GROV_titer_IGM.y", 
                           "ILHV_result_IGM.x", "ILHV_result_IGM.y", "ILHV_titer_IGM.x", 
                           "ILHV_titer_IGM.y", "LANV_result_IGM.x", "LANV_result_IGM.y", 
                           "LANV_titer_IGM.x", "LANV_titer_IGM.y", "LEP_result_IGM.x", "LEP_result_IGM.y", 
                           "LEP_titer_IGM.x", "LEP_titer_IGM.y", "MACV_result_IGM.x", "MACV_result_IGM.y", 
                           "MACV_titer_IGM.x", "MACV_titer_IGM.y", "MAYV_result_IGG.x", 
                           "MAYV_result_IGG.y", "MAYV_result_IGM.x", "MAYV_result_IGM.y", 
                           "MAYV_titer_IGG.x", "MAYV_titer_IGG.y", "MAYV_titer_IGM.x", "MAYV_titer_IGM.y", 
                           "MLOV_result_IGG.x", "MLOV_result_IGG.y", "MLOV_result_IGM.x", 
                           "MLOV_result_IGM.y", "MLOV_titer_IGG.x", "MLOV_titer_IGG.y", 
                           "MLOV_titer_IGM.x", "MLOV_titer_IGM.y", "MURV_result_IGM.x", 
                           "MURV_result_IGM.y", "MURV_titer_IGM.x", "MURV_titer_IGM.y", 
                           "OROV_result_IGG.x", "OROV_result_IGG.y", "OROV_result_IGM.x", 
                           "OROV_result_IGM.y", "OROV_titer_IGG.x", "OROV_titer_IGG.y", 
                           "OROV_titer_IGM.x", "OROV_titer_IGM.y", "QFG_result_IGG.x", "QFG_result_IGG.y", 
                           "QFG_titer_IGG.x", "QFG_titer_IGG.y", "RIOMV_result_IGG.x", "RIOMV_result_IGG.y", 
                           "RIOMV_result_IGM.x", "RIOMV_result_IGM.y", "RIOMV_titer_IGG.x", 
                           "RIOMV_titer_IGG.y", "RIOMV_titer_IGM.x", "RIOMV_titer_IGM.y", 
                           "ROCV_result_IGM.x", "ROCV_result_IGM.y", "ROCV_titer_IGM.x", 
                           "ROCV_titer_IGM.y", "RRI_result_IGG.x", "RRI_result_IGG.y", "RRI_titer_IGG.x", 
                           "RRI_titer_IGG.y", "RTY_result_IGG.x", "RTY_result_IGG.y", "RTY_titer_IGG.x", 
                           "RTY_titer_IGG.y", "SLEV_result_IGM.x", "SLEV_result_IGM.y", 
                           "SLEV_titer_IGM.x", "SLEV_titer_IGM.y", "SNV_result_IGG.x", "SNV_result_IGG.y", 
                           "SNV_titer_IGG.x", "SNV_titer_IGG.y", "STGV_result_IGG.x", "STGV_result_IGG.y", 
                           "STGV_titer_IGG.x", "STGV_titer_IGG.y", "TCRV_result_IGG.x", 
                           "TCRV_result_IGG.y", "TCRV_result_IGM.x", "TCRV_result_IGM.y", 
                           "TCRV_titer_IGG.x", "TCRV_titer_IGG.y", "TCRV_titer_IGM.x", "TCRV_titer_IGM.y", 
                           "UNAV_result_IGG.x", "UNAV_result_IGG.y", "UNAV_result_IGM.x", 
                           "UNAV_result_IGM.y", "UNAV_titer_IGG.x", "UNAV_titer_IGG.y", 
                           "UNAV_titer_IGM.x", "UNAV_titer_IGM.y", "VEEV_result_IGG.x", 
                           "VEEV_result_IGG.y", "VEEV_result_IGM.x", "VEEV_result_IGM.y", 
                           "VEEV_titer_IGG.x", "VEEV_titer_IGG.y", "VEEV_titer_IGM.x", "VEEV_titer_IGM.y", 
                           "WNV_result_IGM.x", "WNV_result_IGM.y", "WNV_titer_IGM.x", "WNV_titer_IGM.y", 
                           "YFV_result_IGM.x", "YFV_result_IGM.y", "YFV_titer_IGM.x", "YFV_titer_IGM.y")





# New notes ---------------------------------------------------------------

elisa_igm_list <- elisa_raw %>% 
  filter(sample_collection_id %in% fs_samples_raw$sample_collection_id_a | 
           sample_collection_id %in% fs_samples_raw$sample_collection_id_c,
         immunoglobulin == "IgM") %>% 
  mutate(titer = as.numeric(str_replace(titer, "1:", ""))) %>% 
  select(c("sample_collection_id", "sample_code", 
           "target_pathogen", "result", "titer")) %>% 
  split(.$target_pathogen) %>% 
  discard(~ nrow(.x) < 5)

path_names <- names(elisa_igm_list)

rename_f <- function(path, list) {
  df <- list[[path]]
  df <- df %>% select(-target_pathogen)
  temp <- str_c(path, "_", names(df))
  temp[2] <- "sample_code"
  names(df) <- temp
  df
}

elisa_igm_list <- map(path_names, rename_f, elisa_igm_list)

elisa_igm_unprocessed <- Reduce(function(...) full_join(..., by='sample_code', all.x=TRUE), elisa_igm_list) %>% select(namage) 

igm_acute <- elisa_igm_unprocessed
names(igm_acute) <- str_c(names(igm_acute), "_a")
igm_conv <- elisa_igm_unprocessed
names(igm_conv) <- str_c(names(igm_conv), "_c")


elisa_igm_joined <- fs_samples_raw %>% 
  select(sample_collection_id_a, sample_collection_id_c) %>% 
  left_join(igm_acute, by = "sample_collection_id_a") %>% 
  mutate(sample_collection_id_c = ifelse(is.na(sample_collection_id_c), "NA", sample_collection_id_c)) %>% 
  left_join(igm_conv, by = "sample_collection_id_c") %>% 
  select(c( "sample_collection_id_a", "sample_collection_id_c", "sample_code_a", "sample_code_c",
            "ALLV_result_a", "ALLV_result_c", "ALLV_titer_a", "ALLV_titer_c", 
           "ANDV_result_a", "ANDV_result_c", "ANDV_titer_a", "ANDV_titer_c", 
           "CARV_result_a", "CARV_result_c", "CARV_titer_a", "CARV_titer_c", 
           "CATV_result_a", "CATV_result_c", "CATV_titer_a", "CATV_titer_c", 
           "CDUV_result_a", "CDUV_result_c", "CDUV_titer_a", "CDUV_titer_c", 
           "DENV_result_a", "DENV_result_c", "DENV_titer_a", "DENV_titer_c", 
           "EEEV_result_a", "EEEV_result_c", "EEEV_titer_a", "EEEV_titer_c", 
           "EMCV_result_a", "EMCV_result_c", "EMCV_titer_a", "EMCV_titer_c", 
           "GROV_result_a", "GROV_result_c", "GROV_titer_a", "GROV_titer_c", 
           "ILHV_result_a", "ILHV_result_c", "ILHV_titer_a", "ILHV_titer_c", 
           "LANV_result_a", "LANV_result_c", "LANV_titer_a", "LANV_titer_c", 
           "LEP_result_a", "LEP_result_c", "LEP_titer_a", "LEP_titer_c", 
           "MACV_result_a", "MACV_result_c", "MACV_titer_a", "MACV_titer_c", 
           "MAYV_result_a", "MAYV_result_c", "MAYV_titer_a", "MAYV_titer_c", 
           "MURV_result_a", "MURV_result_c", "MURV_titer_a", "MURV_titer_c", 
           "OROV_result_a", "OROV_result_c", "OROV_titer_a", "OROV_titer_c", 
           "RIOMV_result_a", "RIOMV_result_c", "RIOMV_titer_a", "RIOMV_titer_c", 
           "ROCV_result_a", "ROCV_result_c", "ROCV_titer_a", "ROCV_titer_c", 
           "SLEV_result_a", "SLEV_result_c", "SLEV_titer_a", "SLEV_titer_c", 
           "TCRV_result_a", "TCRV_result_c", "TCRV_titer_a", "TCRV_titer_c", 
           "UNAV_result_a", "UNAV_result_c", "UNAV_titer_a", "UNAV_titer_c", 
           "VEEV_result_a", "VEEV_result_c", "VEEV_titer_a", "VEEV_titer_c", 
           "WNV_result_a", "WNV_result_c", "WNV_titer_a", "WNV_titer_c", 
           "YFV_result_a", "YFV_result_c", "YFV_titer_a", "YFV_titer_c"))


elisa_process <- function(pathogen, df) {
  
  res_a <- str_c(pathogen, "_result_a")
  res_c <- str_c(pathogen, "_result_c")
  tit_a <- str_c(pathogen, "_titer_a")
  tit_c <- str_c(pathogen, "_titer_c")
  
  temp <- case_when(is.na(df[[res_a]]) & is.na(df[[res_c]]) & is.na(df[[tit_a]]) & is.na(df[[tit_c]]) ~ "NT",
                    (df[[tit_c]]/df[[tit_a]]) >= 4 | (df[[tit_c]] > 100 & is.na(df[[tit_a]])) ~ "SC",
                    (df[[res_a]] == "negative" | df[[tit_a]] == "negative") &
                      (df[[res_c]] == "negative" | df[[tit_c]] == "negative") ~ "NEG",
                    df[[tit_a]] > 100 | df[[res_a]] == "positive" | df[[tit_a]] ~ "ACUTE",
                    (df[[res_a]] == "negative" | df[[tit_a]] == "negative" | df[[res_c]] == "negative") ~ "NEG",
                    TRUE ~ "ID")
   temp
}


elisa_igm_processed_list <- map(path_names, elisa_process, elisa_igm_joined)
names(elisa_igm_processed_list) <- path_names
elisa_igm_processed_raw <- do.call(bind_cols, elisa_igm_processed_list)
elisa_igm_processed <- elisa_igm_joined %>% 
  select(sample_collection_id_a) %>% 
  bind_cols(elisa_igm_processed_raw)


table_NAS <- function(x){
  t <- table(x)
  NAS <- sum(is.na(x))
  y <- append(t, NAS)
  names(y) <- append(names(t), "NAs")
  y
}


# elisa_igm_processed_binary <- map_df(elisa_igm_processed[2:ncol(elisa_igm_processed)], ~ ifelse(.x %in% c("SC", "ACUTE"), TRUE, FALSE)) %>% bind_cols(elisa_igm_processed[1])

test <- fs_samples_raw %>% 
  inner_join(pcr_definitive_26june2019, by = c("sample_collection_id_a" = "sample_collection_id_acute")) %>% 
  left_join(elisa_igm_processed, by = "sample_collection_id_a") %>% 
  rowwise() %>% 
  mutate()

# New notes ---------------------------------------------------------------

elisa_igg_list <- elisa_raw %>% 
  filter(sample_collection_id %in% fs_samples_raw$sample_collection_id_a | 
           sample_collection_id %in% fs_samples_raw$sample_collection_id_c,
         immunoglobulin == "IgG") %>% 
  mutate(titer = as.numeric(str_replace(titer, "1:", ""))) %>% 
  select(c("sample_collection_id", "sample_code", 
           "target_pathogen", "result", "titer")) %>% 
  split(.$target_pathogen) %>% 
  discard(~ nrow(.x) < 5)

path_names <- names(elisa_igg_list)

rename_f <- function(path, list) {
  df <- list[[path]]
  df <- df %>% select(-target_pathogen)
  temp <- str_c(path, "_", names(df))
  temp[2] <- "sample_code"
  names(df) <- temp
  df
}

elisa_igg_list <- map(path_names, rename_f, elisa_igg_list)

elisa_igg_unprocessed <- Reduce(function(...) full_join(..., by='sample_code', all.x=TRUE), elisa_igg_list)  

igm_acute <- elisa_igm_unprocessed
names(igm_acute) <- str_c(names(igm_acute), "_a")
igm_conv <- elisa_igm_unprocessed
names(igm_conv) <- str_c(names(igm_conv), "_c")


elisa_igg_joined <- fs_samples_raw %>% 
  select(sample_collection_id_a, sample_collection_id_c) %>% 
  left_join(igm_acute, by = "sample_collection_id_a") %>% 
  mutate(sample_collection_id_c = ifelse(is.na(sample_collection_id_c), "NA", sample_collection_id_c)) %>% 
  left_join(igm_conv, by = "sample_collection_id_c")  
  select(c( "sample_collection_id_a", "sample_collection_id_c", "sample_code_a", "sample_code_c",
            "ALLV_result_a", "ALLV_result_c", "ALLV_titer_a", "ALLV_titer_c", 
            "ANDV_result_a", "ANDV_result_c", "ANDV_titer_a", "ANDV_titer_c", 
            "CARV_result_a", "CARV_result_c", "CARV_titer_a", "CARV_titer_c", 
            "CATV_result_a", "CATV_result_c", "CATV_titer_a", "CATV_titer_c", 
            "CDUV_result_a", "CDUV_result_c", "CDUV_titer_a", "CDUV_titer_c", 
            "DENV_result_a", "DENV_result_c", "DENV_titer_a", "DENV_titer_c", 
            "EEEV_result_a", "EEEV_result_c", "EEEV_titer_a", "EEEV_titer_c", 
            "EMCV_result_a", "EMCV_result_c", "EMCV_titer_a", "EMCV_titer_c", 
            "GROV_result_a", "GROV_result_c", "GROV_titer_a", "GROV_titer_c", 
            "ILHV_result_a", "ILHV_result_c", "ILHV_titer_a", "ILHV_titer_c", 
            "LANV_result_a", "LANV_result_c", "LANV_titer_a", "LANV_titer_c", 
            "LEP_result_a", "LEP_result_c", "LEP_titer_a", "LEP_titer_c", 
            "MACV_result_a", "MACV_result_c", "MACV_titer_a", "MACV_titer_c", 
            "MAYV_result_a", "MAYV_result_c", "MAYV_titer_a", "MAYV_titer_c", 
            "MURV_result_a", "MURV_result_c", "MURV_titer_a", "MURV_titer_c", 
            "OROV_result_a", "OROV_result_c", "OROV_titer_a", "OROV_titer_c", 
            "RIOMV_result_a", "RIOMV_result_c", "RIOMV_titer_a", "RIOMV_titer_c", 
            "ROCV_result_a", "ROCV_result_c", "ROCV_titer_a", "ROCV_titer_c", 
            "SLEV_result_a", "SLEV_result_c", "SLEV_titer_a", "SLEV_titer_c", 
            "TCRV_result_a", "TCRV_result_c", "TCRV_titer_a", "TCRV_titer_c", 
            "UNAV_result_a", "UNAV_result_c", "UNAV_titer_a", "UNAV_titer_c", 
            "VEEV_result_a", "VEEV_result_c", "VEEV_titer_a", "VEEV_titer_c", 
            "WNV_result_a", "WNV_result_c", "WNV_titer_a", "WNV_titer_c", 
            "YFV_result_a", "YFV_result_c", "YFV_titer_a", "YFV_titer_c"))


elisa_process <- function(pathogen, df) {
  
  res_a <- str_c(pathogen, "_result_a")
  res_c <- str_c(pathogen, "_result_c")
  tit_a <- str_c(pathogen, "_titer_a")
  tit_c <- str_c(pathogen, "_titer_c")
  
  temp <- case_when(is.na(df[[res_a]]) & is.na(df[[res_c]]) & is.na(df[[tit_a]]) & is.na(df[[tit_c]]) ~ "NT",
                    (df[[tit_c]]/df[[tit_a]]) >= 4 | (df[[tit_c]] > 100 & is.na(df[[tit_a]])) ~ "SC",
                    (df[[res_a]] == "negative" | df[[tit_a]] == "negative") &
                      (df[[res_c]] == "negative" | df[[tit_c]] == "negative") ~ "negative",
                    df[[tit_a]] > 100 | df[[res_a]] == "positive" | df[[tit_a]] ~ "ACUTE",
                    (df[[res_a]] == "negative" | df[[tit_a]] == "negative" | df[[res_c]] == "negative") ~ "negative",
                    TRUE ~ "ID")
  temp
}


elisa_igm_processed_list <- map(path_names, elisa_process, elisa_igm_joined)
names(elisa_igm_processed_list) <- path_names
elisa_igm_processed_raw <- do.call(bind_cols, elisa_igm_processed_list)
elisa_igm_processed <- elisa_igm_joined %>% 
  select(sample_collection_id_a, sample_collection_id_c, sample_code_a, sample_code_c) %>% 
  bind_cols(elisa_igm_processed_raw)


table_NAS <- function(x){
  t <- table(x)
  NAS <- as.character(sum(is.na(x)))
  y <- append(t, NAS)
  names(y) <- append(names(t), "NAs")
  y
}

map(elisa_igm_processed_raw, table_NAS)
path_names

View(elisa_igm_processed %>% select(contains("DEN")))
