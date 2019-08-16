#Outlier detection
#No 0 day of illness
#When non cohort 
pd_symptoms_dummy$num_symp <- apply(pd_symptoms_dummy[symp_names], MARGIN = 1, sum, na.rm= TRUE)

goodies <- pd_symptoms_dummy %>% 
  nest(-fs_id) %>% 
  mutate(lm = map(data, ~ if(nrow(.x) < 2){
    rep(99, nrow(.x))
  }else{
    mod <- lm(num_symp ~ doi, data = .x)
    rep(coef(mod)[2], nrow(.x))
  })) %>% 
  unnest() %>% 
  group_by(fs_id) %>% 
  mutate(diff_doi = c(NA, diff(doi)),
    test = first(doi == 0) & 
           last(doi) >= 12 &
           last(doi) < 45 &
           nth(doi, 1) + nth(doi, 2) + nth(doi, 3) + nth(doi, 4) == 6 &
         n() > 4 & 
         first(num_symp) > 0 & 
         sum(num_symp) > 5 &
         lm <= 0 &
         (sum(diff_doi > 1, na.rm = TRUE) == 1 | sum(diff_doi == 2, na.rm = TRUE) < 3)) %>%
  split(.$test)

#Plot good ones

samp <- sample(unique(goodies[[1]]$fs_id[goodies[[1]]$result != "NEG"]), 5)

 goodies[[1]] %>% 
   filter(fs_id %in% samp) %>%
   filter(result != "NEG") %>%
   filter(doi >= 0, doi < 45) %>% 
   ggplot(aes(as.integer(doi), num_symp)) +
   geom_point(alpha = 0.3, color = "blue", size = 0.5) +
   geom_line(aes(group = fs_id, color = fs_id), size = 0.2, alpha = 0.8) +
   # geom_text(aes(label = doi), size = 4) +
   theme(legend.position = "none")

  goodies
  