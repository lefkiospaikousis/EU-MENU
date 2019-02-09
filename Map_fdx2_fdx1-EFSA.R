
# ##################################################################################################-
# This script maps the FoodEx2 code to FoodEx1 for the EU MENU data 
# You will need the following files:
#
## a) CONSUMPTION.xlsx - the consumption data
# 
## b) MTX_9.8.xlsx - You get this from the (updated .ecf file (catalog) and the new Catalogue Browser)
#
#    To get the MTX_9.8.xlsx file follow these steps
#
#    1. Downloaded the latest version of EFSA Catalogues Browser 1.2.5 
#       from https://github.com/openefsa/catalogue-browser/wiki/External-EFSA's-users
# 
#    2. Downloaded the latest .ecf file [ MTX 9.8 [27/06/2018] ] 
#       from https://github.com/openefsa/catalogue-browser/wiki/Use-the-tool-without-DCF-account , 
# 
#    Then within the catalogue browser
#    a)FILE->IMPORT the .ecf file, and
#    b)FILE->OPEN
# 
#    3. Selected  "Exposure Hierarchy" and then Tools->Export Excel
#
## c) Foodex1.xlsx - The FoodEx1 categorisation file. You also get this from EFSA
#
# You can also get it from https://www.dropbox.com/s/xx99phl2rk4z28q/Foodex1.xlsx?dl=0
#
## d) SUBJECTS.xlsx - The subjects information
#     This is the SUBJECTS file of the EU MENU
# ##################################################################################################-


# If needed, set to your own system locale
# Sys.setlocale(category = "LC_ALL", locale = "Your_Language")


# Load the PACKAGES #### 
# [install the packages if not on your machine
# via install.pagkages("package_name")]

library(tidyverse)
library(readxl)
library(writexl)

# Read the files ####

  #1. EU MENU consumption file
  consumption <- read_xlsx("CONSUMPTION.xlsx") 
  
  # Get the "base" part of the FoodEx2 code
  # It is the first 5 letters of the full foodex2 code before the `#`
  # A split is performed at the '#' into the 'base' and the 'facets' parts of the full foodex2 code
  # ignore the warnings about "Expected pieces" - it means there were no FACETS to the base code
  consumption = consumption %>%
    separate(FOODEXCODE, c("base", "facets"), 
             sep = "#", 
             # keep the original FOODEX CODE
             remove = FALSE)
  
  #2. Mapping file extracted from CatalogueBrowser64
  mapping_df <- read_xlsx("MTX_9.8.xlsx", 
                          sheet = "term") %>%
    #select and rename only relevant columns
    select(foodexOldCode,                   # This stores the FoodEx1 code
           "base" = termCode,               # the `base` part of FoodEx2 code
           "fdx2_name" = termExtendedName,  # the FoodEx2 name
           prodTreat                        # this stores the treatment of the product [if any]
           )                  
  
  #3. The FOODEX1 categorisation
  foodex1 <- read_xlsx("Foodex1.xlsx")
  
  #4. The Subject dara
  subjects = read_xlsx("SUBJECTS.xlsx")


# MAPPING ####

  #1. Map the FoodEx2 to foodexOldCode [FoodEx1]
  
  # the mapping is done on the 'base' of mapping data
  # and on the 'base' code of the consumption dataset
  consumption_mapped = consumption %>%
    left_join(mapping_df, by = "base")

  # Check if we have a foodOldCode (FoodEx1) for all instances
  # Hopefully you will see only TRUE! If not check your files
  consumption_mapped %>% count("Are we OK" = !is.na(foodexOldCode))


  #2. FoodEx1 name
  # Now we map the FoodEx1 code from the foodex1 dataset
  #I also bring the 'Name' and call it "fdx1_name"
  consumption_mapped <- consumption_mapped %>%
    left_join(select(foodex1,
                     Code	, 
                     "fdx1_name" = Name), 
              by = c("foodexOldCode"= "Code"))

  #Did we get all the foodex1 names?
  consumption_mapped %>% count( "Are we OK" = !is.na(fdx1_name))


# Save as an excel file
write_xlsx(consumption_mapped, "Consumption_EUMENU_mapped_fdx2_fdx1.xlsx")


# Add the Subject Demographics ####

# Should you wish to add the subject demographics on the consumption dataset

consumption_mapped_demo = consumption_mapped %>% 
  # Perhaps keep only relevant variables so the the dataset does not get huge
  # for example:
  select(SURVEY ,RECORDIDENTIFIER, ORSUBCODE, FOODEXCODE, DAY, 
         foodexOldCode, fdx2_name, fdx1_name, prodTreat,
         ENRECIPEDESC,	AMOUNTRECIPE, AMOUNTFRAW, AMOUNTFCOOKED) %>%
  # join the subect demographics - keep only the variables you need
  left_join(
    select(subjects, 
           # get what?
           ORSUBCODE,         # Unique subject code - we need this to do the matching
           GENDER,AGE, WEIGHT # what other data we need
           ),
    by = "ORSUBCODE"
  ) 
  
# Filer by Lot ####

  lot1_data = consumption_mapped_demo %>% 
    filter(str_detect(SURVEY, "Lot1"))
  
  lot2_data = consumption_mapped_demo %>% 
    filter(str_detect(SURVEY, "Lot2"))
  
  #write on disk
  write_xlsx(consumption_mapped_demo, "Consumption_EUMENU_mapped_fdx2_fdx1 ALL SUBJECTS.xlsx")
  write_xlsx(lot1_data, "Consumption_EUMENU_mapped_fdx2_fdx1 Lot1.xlsx")
  write_xlsx(lot2_data, "Consumption_EUMENU_mapped_fdx2_fdx1 Lot2.xlsx")


# The food items with treatment (PRODTR) ####
  processed_foods = consumption_mapped %>% 
    filter(!is.na(prodTreat)) %>% 
    count(fdx2_name, fdx1_name, 
          foodexOldCode, prodTreat, 
          sort = T) 

  # write to an excel file on disk
  write_xlsx(processed_foods, "processed_foods.xlsx")

