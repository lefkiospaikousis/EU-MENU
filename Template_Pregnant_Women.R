# A template for Pregnat Women ####

#we need the consumption_mapped data!!!

consumption_mapped = readxl::read_xlsx("Consumption_EUMENU_mapped_fdx2_fdx1 ALL SUBJECTS.xlsx")

preg_consumption_wksh = consumption_mapped %>% 
  filter(AgeGroup == "Pregnant Women") %>% 
  #select coumns we need and rename some
  select("SUBJECTID" = ORSUBCODE, DAY, "AMOUNTOFFOOD" = AMOUNTFRAW, 
         "FOODatL4" = fdx1_name) %>% 
  arrange(SUBJECTID) %>% 
  rowid_to_column(var = "SERIAL")


# the pregnat women ids
EU_MENU_SubjectDemo = readxl::read_xlsx("EU_MENU_SubjectDemo.xlsx")

preg_women_ids = EU_MENU_SubjectDemo %>% 
  filter(AgeGroup == "Pregnant Women") %>% 
  distinct(`EFSA CODE`) %>% pull()

# Creation
# Get the subject info
subjects = read_xlsx(here("FINAL DATA JUNE 12", "SUBJECTS.xlsx"))

# filter the pregannt and create the subjects worksheet
preg_subject_wksh = 
  subjects %>% 
  filter(ORSUBCODE %in% preg_women_ids) %>% 
  left_join(select(EU_MENU_SubjectDemo,
                   `EFSA CODE`, AREA = district_en),
            by = c("ORSUBCODE" = "EFSA CODE")) %>% 
  arrange(ORSUBCODE) %>% 
  add_column(WCOEFF = 1) %>% 
  add_column(POP_CLASS = "Pregnata Women") %>% 
  mutate(GENDER = "FEMALE") %>% 
  select("SUBJECTID" = ORSUBCODE, GENDER, AGE, WEIGHT,POP_CLASS, AREA, WCOEFF) 

# write the dataset
writexl::write_xlsx( x = list("Consumption" = preg_consumption_wksh,
                              "Subjects" = preg_subject_wksh), 
                     #file name. include the number of subjects
                     paste0("Subjects_Consumption_EUMENU Pregnant Women (N=",nrow(preg_subject_wksh),").xlsx"))