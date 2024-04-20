#############################################################################################
#                                           *
#                                    ELENI DOMZARIDOU
#                                   Otorrhoea PROJECT
#                                UNIVERSITY OF MANCHESTER
#                              June 2023 - Stopford Building
#                                       CPRD-Aurum  
#                                           *
#
# [*] The following lines use the CPRD's Aurum drug issue files to identify
#     patient records with antibiotics exposure
#
#############################################################################################

library(dplyr)
library(lubridate)
library(haven)

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Lookups")

# --------------------------------------------
# read product code dictionary from CPRD
# --------------------------------------------

proddict.a <- read_dta(file = "202106_emisproductdictionary.dta")

head(proddict.a)

proddict.a <- proddict.a %>% filter(drugsubstancename != "")
#test <- tidyr::separate(proddict.a, col= drugsubstancename, into = "new_drugsubstancename", sep = "[^[:alnum:]]+", remove = F)


# ----------------------------------------------------------------
# Scan the dictionary to find ABs/Antifungal product codes
# ----------------------------------------------------------------
# The following ABs substances are those that exist in TvS list and
# were further updated from the most frequent products prescribed
# for PO

head(proddict.a)

# ---------------
# Antibiotics
# ---------------

search_AB <- "Amikacin|Amoxicillin|Co-amoxiclav|Clobetasone|Fluconazole|Clarithromycin|Ampicillin|Azithromycin|Azlocillin|Aztreonam|Bacitracin|Bacampicillin|Benzylpenicillin|Carbenicillin|Carfecillin|Cefaclor|Cefadroxil|Cefalexin|Cefaloridine|Cefalotin|Cefamandole|Cefazolinsodium|Cefixime|Cefodizime|Cefotaxime|Cefoxitinsodium|Cefpirome|Cefpodoxime|Cefprozil|Cefradine|Cefsulodin|Ceftaroline|Ceftazidime|Ceftazidime|Ceftibuten|Ceftizoxime|Ceftriaxone|Cefuroxime|Chloramphenicol|Chlorexidine|Chlortetracycline|Ciclacillin|Cilastatin|Cinoxacin|Ciprofloxacin|Clarithromycin|Clindamycin|Doxycycline|Erythromycin|Flucloxacilline|Framycetin|Fusidic|Gentamicin|Gramicidin|Levofloxacin|Lincomycin|Linezolid|Mupirocin|Neomycin|Nitrofurantoin|Vancomycin|Oxfloxacin|Oxytetracycline|Phenoxymethylpenicillin|Polymyxin|Rifampicin|Trimethoprim"

my_ABs_list <- proddict.a %>% filter(grepl(search_AB, drugsubstancename, fixed = F))
my_ABs_list$prodcodeid <- as.character(my_ABs_list$prodcodeid)
my_ABs_list$dmdid <- as.character(my_ABs_list$dmdid)
my_ABs_list$myAB_flag <- 1
my_ABs_list <- my_ABs_list %>% mutate(aa = row_number())

# merge this list with Elliot's reviewd list so we get the classification into topical; oral; other
Elliots_ab_list <- read.delim(file = "ABs_prodcodes_Aurum_ED_EH_v1_classes.txt", sep = "\t", header = T,
                              fill = T, quote = "")

Elliots_ab_list <- Elliots_ab_list %>% select(c(aa, routeofadministration_EH))

# merge with the initial extraction so we can have the classes available
my_ABs_list <- left_join(my_ABs_list, Elliots_ab_list, by = "aa")
my_ABs_list <- my_ABs_list %>% rename("class_EH" = "routeofadministration_EH")

write.table(my_ABs_list, "ABs_prodcodes_Aurum_ED_EH_classes_final.txt")

# ---------------
# Antifungals
# ---------------

search_AF <- "Clotrimazole|Econazole|Miconazole|Terbinafin|Fluconazole|Ketoconazole|Nystatin|Amphotericin"
my_AFs_list <- proddict.a %>% filter(grepl(search_AF, drugsubstancename, fixed = F))
my_AFs_list$prodcodeid <- as.character(my_AFs_list$prodcodeid)
my_AFs_list$myAF_flag <- 1
my_AFs_list <- my_AFs_list %>% mutate(aa = row_number())

# merge this list with Elliot's reviewd list so we get the classification into topical; oral; other
Elliots_af_list <- read.delim(file = "AFs_prodcodes_Aurum_ED_EH_v1_classes.txt", sep = "\t", header = T,
                              fill = T, quote = "")

Elliots_af_list <- Elliots_af_list %>% select(c(aa, EH_class))

# merge with the initial extraction so we can have the classes available
my_AFs_list <- left_join(my_AFs_list, Elliots_af_list, by = "aa")
my_AFs_list$dmdid <- as.character(my_AFs_list$dmdid)

write.table(my_AFs_list, "AFs_prodcodes_Aurum_ED_EH_classes_final.txt")

# clean env
# rm(Elliots_ab_list, Elliots_af_list, search_AB, search_AF)