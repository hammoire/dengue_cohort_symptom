
library(lubridate)
#Error checking script
a[[2]] %>% View()

positivos <- fs_pcr_elisa %>% 
  inner_join(fs_raw, by = "fs_id") %>% 
  arrange(case_date, fs_id, date) %>% 
  mutate(doi = as.integer(date - first_symptom_date),
         dif_doi = c(NA, diff(doi))) %>% 
  group_by(fs_id) %>% 
  mutate(doi30 = max(doi, na.rm = TRUE) > 30 & max(doi, na.rm = TRUE) < 45) %>% 
  filter(doi30) %>% 
  select(c("fs_id", "result",
           "fsfu_id","doi",  "case_date", "date", 
           "first_symptom_date", "first_specimen_date", "second_specimen_date",  "dif_doi", "symptoms_daily", "temperature", "pulse", 
           "respiration", "diastole", "systole"))

pos_id <- unique(positivos$fs_id)
(id <- pos_id[1])
  positivos %>% 
    # filter(fs_id == id) %>% 
    group_by(fs_id) %>% 
    summarise(dois = str_c(doi, collapse = "-")) %>% View()





# Day 0 no symptoms -------------------------------------------------------
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

pd_symptoms_dummy$num_symp <- apply(pd_symptoms_dummy[symp_names], MARGIN = 1, sum, na.rm= TRUE)
  

pd_symptoms_dummy %>% 
  filter(doi == 0, symptoms_daily == "Missing") %>% View()

x[[100]]

# Symptoms daily ----------------------------------------------------------

x <- pd_symptoms_dummy %>% 
  mutate(month_s = floor_date(case_date, unit = "months")) %>% 
  filter(doi > -1, doi < 14) %>% 
  split(.$month_s) %>% 
  map(~ ggplot(.x, aes(doi, num_symp, color = result)) +
        geom_point() + geom_line() + facet_wrap(~fs_id) + theme(legend.position = "top"))

pd_symptoms_dummy %>% 
  mutate(month_s = floor_date(case_date, unit = "months")) %>% 
  filter(doi > -1, doi < 14) %>% 
  ggplot(aes(doi, num_symp, color = fs_id)) +
        geom_point() + geom_line() + theme(legend.position = "top")


str(x)
x[[51]]



negatives_doi <- fs_raw %>% 
  filter(fs_id %in% fs_pcr_elisa$fs_id) %>% 
  mutate(doi = as.numeric(date - first_symptom_date)) %>% 
  group_by(fs_id) %>% 
  mutate(n = row_number(), max_n = max(n)) %>% 
  filter(max_n > 2, doi < 0) %>%
  arrange(desc(n)) %>% 
  select(c("fs_id", "fsfu_id", "case_date",  "doi",
           "date", "first_symptom_date", "first_specimen_date", "second_specimen_date", 
           "pregnant", "symptom_all", "symptoms_daily", "temperature", "pulse", 
           "respiration", "diastole", "systole", "torniquet_test", 
           "n", "max_n"))


  dput(names(negatives_doi))



dput(str_c("record = CohortSymptomsForm.objects.get(id=‘", ids, "’)"))

{
  # record = CohortSymptomsForm.objects.get(id='847f9c73-f8ad-4a92-a075-7010c667c5c2') DONE
  # record = CohortSymptomsForm.objects.get(id='2062a212-7011-4dba-99cc-9ff703a1b491') PAPER PAPER
  # record = CohortSymptomsForm.objects.get(id='e1725e7c-0bd9-4f6b-ae37-95972ec56d66') PAPER PAPER
  # record = CohortSymptomsForm.objects.get(id='17582dcb-ad8b-43e3-9e3b-ec292f2bc1f6') DONE
  # record = CohortSymptomsForm.objects.get(id='09eb32b0-b0ef-4edc-aa47-be75da85f21b') DONE
  # record = CohortSymptomsForm.objects.get(id='70d28f22-0d93-4b8a-9d09-d84723a7c5ae') DONE
  # record = CohortSymptomsForm.objects.get(id='8c27edf3-f7a3-4213-a3f7-e98abcbbadc6') PAPER PAPER
  # record = CohortSymptomsForm.objects.get(id='b7365423-8a98-4463-b00f-d96abb521546') DONE
  # record = CohortSymptomsForm.objects.get(id='2bbeabd8-1a0c-4622-989b-71e0fcfbe315') DONE
  # record = CohortSymptomsForm.objects.get(id='6c54c02d-9aad-4a17-be1f-4d04eacb9aa0') DONE
  # record = CohortSymptomsForm.objects.get(id='79e54b15-ff0a-4a1c-b182-699f34dd1ede') DONE
  # record = CohortSymptomsForm.objects.get(id='b00cd0c4-23ba-4dcc-a891-c838638ae6ac') DONE
  # record = CohortSymptomsForm.objects.get(id='b26552d0-0a93-480f-8ffb-e948fe933793') DONE
  # record = CohortSymptomsForm.objects.get(id='ff5a8683-4bab-473f-a58b-f87208256e06') DONE
  # record = CohortSymptomsForm.objects.get(id='0fc83896-df72-481e-a11d-130ad3c29e7b') DONE
  # record = CohortSymptomsForm.objects.get(id='a1c4a404-8767-4d1c-be9f-7fb4f4dd2f6f') DONE
  # record = CohortSymptomsForm.objects.get(id='d3768af5-5785-40ae-8248-831313174e78') DONE
  # record = CohortSymptomsForm.objects.get(id='e074d993-ec3b-4586-8715-d09e0bb2417a') DONE
  # record = CohortSymptomsForm.objects.get(id='00d9fd3e-e750-438d-8c46-f1027d8a1829') DONE
  # record = CohortSymptomsForm.objects.get(id='0bf9d507-cf4d-461c-b8ba-da95680de4e4') DONE
  # record = CohortSymptomsForm.objects.get(id='1dffefdd-1979-4ee8-8f06-99bc9fac17a7') DONE
  # record = CohortSymptomsForm.objects.get(id='512ec717-942f-48d7-b411-43407057bf98') DONE
  # record = CohortSymptomsForm.objects.get(id='7b03b7e7-24c6-499b-93c4-f2a5262b426d') DONE
  # record = CohortSymptomsForm.objects.get(id='9d5113e1-1e85-49da-b239-48d88e0ee0af') DONE
  # record = CohortSymptomsForm.objects.get(id='bebdea6a-d94e-448a-aa54-98aff5615e8e') DONE
  # record = CohortSymptomsForm.objects.get(id='d7dfc490-cb11-4e76-a255-5809b784e89f') DONE


# Done --------------------------------------------------------------------


  # record = CohortSymptomsForm.objects.get(id='df401eb9-2e15-400e-a9d2-76a3fd92c817') DONE
  # record = CohortSymptomsForm.objects.get(id='ee56ac4b-bd7b-4c73-b62d-5524cb53c350') DONE
  # record = CohortSymptomsForm.objects.get(id='f4fd8d69-da64-4d2b-bc12-1786a395b940') DONE
  # record = CohortSymptomsForm.objects.get(id='009235ae-31fe-4235-be78-3a89337d3239') DONE
  # record = CohortSymptomsForm.objects.get(id='09daaf1b-a48e-4730-87b1-e92ab98295f4') 13385425-119e-40ca-be4c-1e1a60342eb2 DELETE THIS FORM 2011-07-11
  # record = CohortSymptomsForm.objects.get(id='1362fe22-7a3d-49b0-90d9-aa9f4670fc16') DONE
  # record = CohortSymptomsForm.objects.get(id='16c5b45e-f146-4bb9-b28d-b76985e22021') DONE
  # record = CohortSymptomsForm.objects.get(id='187610fc-f4a7-4f5b-a39a-ccb0df1adfb9') DONE
  # record = CohortSymptomsForm.objects.get(id='1a5a0893-b9f1-4721-a2e7-c126e0d1976d') DONE
  # record = CohortSymptomsForm.objects.get(id='1b09456c-6d52-41d2-ac65-b74ce339b0a3') CHECK 
  # record = CohortSymptomsForm.objects.get(id='1b6adc28-a7a1-40c6-9a7e-ec649b4b314e') DONE
  # record = CohortSymptomsForm.objects.get(id='1b7f4b4a-35a2-4dbc-b70e-1bc102a6a091') DONE
  # record = CohortSymptomsForm.objects.get(id='2018b6d4-3d98-4922-bd9c-d82aa42f60e8') DONE
  # record = CohortSymptomsForm.objects.get(id='265fa119-f756-412c-8aa3-ce673e2ad3a5') c103c99f-03f7-4df4-b4e5-6c53b95252fb DELETE THIS FORM 2008-10-25
  # record = CohortSymptomsForm.objects.get(id='2deaf14e-8536-436b-8368-6aa948d2491d') CHECK
  # record = CohortSymptomsForm.objects.get(id='2fa5029d-1c72-4366-a944-1bfd62f92513') DONE
  # record = CohortSymptomsForm.objects.get(id='36a52001-5fe2-41c9-b399-462731047a40') DONE
  # record = CohortSymptomsForm.objects.get(id='374f4744-7db2-4edc-95bd-4ba9827b06ef') DONE
  # record = CohortSymptomsForm.objects.get(id='37958f58-1f62-4339-b663-bf735230b705') DONE
  # record = CohortSymptomsForm.objects.get(id='39e520eb-93ba-479e-a62a-1efa195736bf') CHECK
  # record = CohortSymptomsForm.objects.get(id='3ec49bc8-915e-4db6-a3f1-f48f9ad83c8a') DONE
  # record = CohortSymptomsForm.objects.get(id='3f893125-1706-4e46-b5c7-9dfcf809f56f') DONE
  # record = CohortSymptomsForm.objects.get(id='4077752e-269f-40e8-b0af-c4dd654d2d75') DONE
  # record = CohortSymptomsForm.objects.get(id='43844d41-1be9-44ec-9420-124239bb01fb') DONE
  # record = CohortSymptomsForm.objects.get(id='45d3fc87-bae7-49c4-ace8-51933b9e26ad') DONE
  # record = CohortSymptomsForm.objects.get(id='48b86e34-906f-45a1-91ba-36f26a018626') DONE
  # record = CohortSymptomsForm.objects.get(id='50e2b63b-f997-4bb7-86d2-c4402cb24ac3') DONE
  # record = CohortSymptomsForm.objects.get(id='57f8f046-1494-404f-8258-8bec459c8da7') 3763f566-24f2-46f4-9a03-e3f8a7c6eba4 delete form 2012-10-29
  # record = CohortSymptomsForm.objects.get(id='593fa01d-bbf5-4014-87b4-e24f303cfc4a') DONE
  # record = CohortSymptomsForm.objects.get(id='59b17d3e-0401-4d88-a0a1-0c186e848bd7') DONE
  # record = CohortSymptomsForm.objects.get(id='61ec6af9-a36e-4ab9-903a-ac0dcae1bfc5') DONE
  # record = CohortSymptomsForm.objects.get(id='62d2fde4-9a2c-45dd-bd2e-bc19aaecbf5d') DONE
  # record = CohortSymptomsForm.objects.get(id='70d94a76-8d6d-4a96-9adc-b933a1531d34') DONE
  # record = CohortSymptomsForm.objects.get(id='7420d8db-c1db-44b1-b655-39d58f85641c') DONE
  # record = CohortSymptomsForm.objects.get(id='74ce3617-7857-41a9-8b05-839672f3c0de') DONE
  # record = CohortSymptomsForm.objects.get(id='761739b5-8695-4e9e-9958-c81a5706b5a4') DONE
  

# Batch -------------------------------------------------------------------

  # record = CohortSymptomsForm.objects.get(id='7fe6f907-1a38-4445-b6b9-9db7d81bc362') DONE
  # record = CohortSymptomsForm.objects.get(id='83c50d4d-b47f-4530-b520-cf298aac001c') DONE
  # record = CohortSymptomsForm.objects.get(id='84d1e407-0dee-44d1-b5fb-d8269fdb40ff') DONE
  # record = CohortSymptomsForm.objects.get(id='85c8beed-5175-4097-a159-9f6d132afac1') DONE
  # record = CohortSymptomsForm.objects.get(id='8768930e-7c61-4168-b72f-06e122eaa2f2') d902e0ca-1aac-4d5a-ae0c-eb2e1b73c878 DELETE 2017-01-07
  # record = CohortSymptomsForm.objects.get(id='88aaf0b8-c970-4638-9623-849180046a01') DONE
  # record = CohortSymptomsForm.objects.get(id='8dc4a7c1-cffb-4d29-96ba-84d3e61f505f') DONE
  # record = CohortSymptomsForm.objects.get(id='9640f67d-ab79-4e6c-84be-72362b219a52') DONE
  # record = CohortSymptomsForm.objects.get(id='9744dee2-47aa-47ea-9af6-a7d4a5b2de99') DONE
  # record = CohortSymptomsForm.objects.get(id='9faa729f-84c7-4aae-abd1-7bd0f0e057ee') DONE
  # record = CohortSymptomsForm.objects.get(id='a09a6b82-6c87-407b-a332-b2591486cab2') DONE
  # record = CohortSymptomsForm.objects.get(id='a31fe7ae-61ca-4bbb-a14e-72c8eb739f17') DONE
  # record = CohortSymptomsForm.objects.get(id='a51f9814-c338-4162-8cfc-1f76fbbee196') DONE
  # record = CohortSymptomsForm.objects.get(id='aa1ddc4e-f750-4ac9-a0b6-bd780620520c') DONE
  # record = CohortSymptomsForm.objects.get(id='aa5dc400-d5e7-4c47-a1cf-dbb05c422d04') DONE
  # record = CohortSymptomsForm.objects.get(id='ab578c78-6640-4cec-b0d4-ab2cece68c1c') d5ec0336-fa0f-4b72-a19c-3d324b31421e DELETE 2008-05-15
  # record = CohortSymptomsForm.objects.get(id='ab664d8a-e283-46f8-ac2d-0ee117a3a696') DONE
  # record = CohortSymptomsForm.objects.get(id='b6405a6b-f8ee-4ff5-9c64-703c06a6de77') DONE
  # record = CohortSymptomsForm.objects.get(id='bacc772c-7474-4c3d-b38c-c36ac754360f') DONE
  # record = CohortSymptomsForm.objects.get(id='bc3a6a73-cd3b-45e5-ba3b-0a834940f906') DONE
  # record = CohortSymptomsForm.objects.get(id='bc6ac46f-b45f-4805-b86c-bc334a2a1e5a') DONE
  # record = CohortSymptomsForm.objects.get(id='c299994f-2495-4089-904a-784c5944a6ba') a0ddcb5e-13f1-44d0-bed5-450e8fb9ab2d change date from 2016-10-26 to 2014-10-26
  # record = CohortSymptomsForm.objects.get(id='c2c7044b-874e-4969-9925-0bead6ba0d63') a87c3d1f-cbf3-4210-bb9f-4aa34a7aad3d delete date 2016-04-10
  # record = CohortSymptomsForm.objects.get(id='c62b330c-355a-4dcf-bd33-1aded1f3eb86') 897921e3-2a8f-4562-90c5-c997a6a18b49 delete date 2011-02-22
  # record = CohortSymptomsForm.objects.get(id='cbb1087b-80c8-4f7e-b6f5-0f0e6cd7910b') DONE
  # record = CohortSymptomsForm.objects.get(id='cc163dbb-5210-4e1b-a9f0-eb921d132975') DONE
  # record = CohortSymptomsForm.objects.get(id='cce9a0a4-8013-416b-aca5-5dc4235367c2') DONE
  # record = CohortSymptomsForm.objects.get(id='cdf4ebc8-2a21-4951-9d01-5c0cc5511c3f') DONE
  # record = CohortSymptomsForm.objects.get(id='d0119475-83fe-4869-8e20-5aba4b787eed') DONE
  # record = CohortSymptomsForm.objects.get(id='d1a0e6b8-4896-406f-95c1-55162b4356f4') 0997987e-4397-4d09-9e6c-dfbff31dd8f1 delete date 17-02-11
  # record = CohortSymptomsForm.objects.get(id='d499a679-0a26-4deb-825b-d14fe5c83c14') DONE
  # record = CohortSymptomsForm.objects.get(id='da5d73e2-3719-4969-a99a-556af337c5c5') DONE
  # record = CohortSymptomsForm.objects.get(id='e0faf8f2-2ede-426a-b8cc-b428030a53d6') DONE
  # record = CohortSymptomsForm.objects.get(id='e30e14b4-47f1-49e1-9439-c05678e22b4f') CHECK
  # record = CohortSymptomsForm.objects.get(id='e70a6499-744d-4719-a84d-cb8cc76f4282') aba4c06f-96e5-4cbb-ab95-109128742933 delete 2006-10-26
  # record = CohortSymptomsForm.objects.get(id='e9363c7c-a2ef-4703-b230-03d6a9c2e281') DONE
  # record = CohortSymptomsForm.objects.get(id='e98561c5-6c61-44e1-891e-d9d4f6d5837d') CHECK
  # record = CohortSymptomsForm.objects.get(id='f69bed03-ba58-4dd9-bae2-bc13d9bc1f33') DONE


# Fix forms > 45 doi ------------------------------------------------------
  
  # form_id = '313278f9-4694-4dcb-930b-f682de9b347a' DONE
  # form_id = '6902693c-485a-490c-bf0b-e9f11cae1f27' DONE
  # form_id = '6e7513a3-f84d-40ae-be48-dbe59438b704' DONE
  # form_id = '05a250b3-02e8-4bc3-9947-a5ccd697bc86' DONE
  # form_id = 'd68c220e-b758-46ce-af6e-ce81b31507d8' CHECK NEG
  form_id = '8ab730bf-05c6-4a0e-97fd-a895bf2fe341' 
  form_id = '15398518-dc72-4020-8757-52a7758e1ad7' 
  form_id = '4ba335a5-0962-4314-ad94-4f610bceac68' 
  # form_id = 'f42d1d71-7e6f-4579-ae33-8543dbba7e8f' CHECK NEG 3 with duplicate dates. Do these represent all pre- first visit forms NEG
  form_id = '94034485-35fe-42eb-b5fc-8786c152a419' 
  form_id = '8bf58ccc-356b-4438-bf77-bfca741a953f' 
  form_id = '1c37e87f-bbf6-4d82-8afe-b4922bc79e4c' 
  form_id = '32fe1abc-701a-4aa0-bda3-99d0a0ce3fe9' 
  # form_id = '17582dcb-ad8b-43e3-9e3b-ec292f2bc1f6' CHECK possibly correct
  form_id = '4768de5e-2e09-4b7d-9697-94d5da619446' 
  # form_id = '2c4629f9-5c0c-491c-9582-0d3e0dfb0464' CHECK messed up
  form_id = 'bbccf2fe-1f56-4e76-bdc5-06bf7d34dae9' 
  form_id = 'dbd2a14d-c199-4508-9b8d-ee22d0f9d749' 
  form_id = '0a3d19e0-3b21-4fd5-a7f3-2e072ee5cf0c' 
  form_id = '58f7f1bd-c5a5-41b1-a0b7-d9ccf9261c7e' 
  form_id = '33784451-bcc8-4a11-bbb5-1ab61c2923fd' 
  # form_id = '1f4914f7-5a1a-4118-9d0e-01ae17179b97' Check NEG
  # form_id = '3892a20b-d325-4447-983d-423a26a1f7cb' Check NEG
  # form_id = '2d2a034c-f95b-4cd8-a570-185ab7211f3c' Check NEG
  # form_id = '684557f7-b951-47a3-87d2-c65030584250' Check NEG
  form_id = '74ecf82a-d5ca-4075-8262-af5183eed69c' 
  form_id = '1e560f0f-d179-4a2d-ba7d-80450bb9f956' 
  # form_id = 'e0af4f75-a8ee-4f22-8dce-65402f42f42d' Check NEG
  form_id = '230fecd7-d0e1-4790-acc8-227247d35af5' 
  form_id = '77d75a14-d160-4dd0-8b60-375c993032b2' 
  form_id = '2e5c6dfb-585e-46b4-bc0a-bfd56029ac93' 
  # form_id = '52425bdb-59fd-45bf-a7ca-d09c876d8204' Check NEG
  form_id = '55f0b5c5-ae76-4050-a13e-fd1ffc02df4d' 
  form_id = 'b29895d7-b4e0-4a1a-9ab2-08b456817124' 
  # form_id = '2699d33b-4402-4442-aa38-2ad7666a7eb7' Check NEG
  }

(id <- pos_id[48])
  positivos %>% 
    filter(fs_id == "2699d33b-4402-4442-aa38-2ad7666a7eb7")

 
  

# Doi 0 no symptoms -------------------------------------------------------

  
  
doi0_id <- c("0a82e5f3-3cff-49e7-9690-b6ee393d2641", "0c97a6e1-54d1-438c-99f7-f06887b41501", 
      "145cf112-92ec-4ef0-a17c-9bc55b0c9fa4", "156812cd-8667-4c7d-b9a1-4197d5379d5d", 
      "1be05ced-b73b-4860-a555-faefef95e72c", "211b9f1e-6a31-4843-b40e-395109240301", 
      "27b67eb8-9741-4daa-8455-be8a554334cb", "4ef78254-801e-4bca-8e56-0a40b0ec9dfd", 
      "561c8be3-1ead-41b0-9cca-ebddaad6efe7", "60b3d6f6-5f64-429f-9785-cbfe7a2cf1b0", 
      "66f2c13c-cf55-4333-aab5-fce85d7837e0", "69cdeebb-f8cc-4546-b712-950ce4c658f0", 
      "88aaf0b8-c970-4638-9623-849180046a01", "8edd41d1-0640-4c69-8267-5f809e5d21ce", 
      "95840956-e358-44c3-81ac-43d3fd73a623", "b38ca4c1-2a03-43bf-af43-99f9245b4bb9", 
      "c28f9a54-da32-4e62-a2e7-8ef50c4699cb", "e0e7dc41-0d22-463c-b3c7-14c0f0997b1e", 
      "e9e8afee-c347-4f6a-8929-6882caa8f74d", "eaa3403e-2da9-48f9-bdb2-50106f8dd21a", 
      "f9650065-5bc2-40a5-a42f-0f9f9c20055d", "fbc7f2d9-f356-49e7-a7ee-7686fb4ab78c")    

  


  doi0 <- function(x){
    (test <- pd_symptoms_dummy %>% 
        group_by(fs_id) %>% 
        filter(fs_id == x) %>% 
       mutate(doi = as.integer(doi)) %>% 
        select(c("result", "fs_id", "fsfu_id", "case_date",  "doi",
                 "date", "first_symptom_date", "first_specimen_date", "second_specimen_date", 
                  "symptoms_daily", "temperature", "pulse", 
                  "torniquet_test", "num_symp")))
    print(test)
    test %>% ggplot(aes(as.integer(doi), num_symp)) +
      geom_point() + geom_line()
  }  

(id <- doi0_id[1])
doi0(id)  

map(doi0_id, doi0)  
  
  
  negatives_doi <- fs_raw %>% 
    filter(fs_id %in% fs_pcr_elisa$fs_id) %>% 
    mutate(doi = as.numeric(date - first_symptom_date)) %>% 
    group_by(fs_id) %>% 
    mutate(n = row_number(), max_n = max(n)) %>% 
    filter(max_n > 2, doi < 0) %>%
    arrange(desc(n)) %>% 
    select(c("fs_id", "fsfu_id", "case_date",  "doi",
             "date", "first_symptom_date", "first_specimen_date", "second_specimen_date", 
             "pregnant", "symptom_all", "symptoms_daily", "temperature", "pulse", 
             "respiration", "diastole", "systole", "torniquet_test", 
             "n", "max_n"))