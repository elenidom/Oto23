
# -----------------------------------------------------------------
# read the Drug Issue files
# -----------------------------------------------------------------
# The following lines read all the prescription records for those patids
# that exist in my initial cohort extraction (namely, the cohort before
# filtering due to PO episodes, dates, age etc).

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/DrugIssue/")

drugIssue_fileslist = list.files(pattern = ".dta")

# read the patids with patients who have a PO code
setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted/")

mypatids <- read.csv("final_PO_IR_master_coh.csv")

# filter to those patients with an otorrhoea code
mypatids <- mypatids %>% filter(PO_event == 1)
mypatids$patid <- as.character(mypatids$patid)
mypatids$myeventdate <- as_date(mypatids$myeventdate)

# --------------------------------------------------
# search those product codes in the drug issue files
# --------------------------------------------------

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/DrugIssue/")

library(haven)

for(i in 1:length(drugIssue_fileslist)){
  read_dta <- read_dta(file = drugIssue_fileslist[i])
  read_dta$patid <- as.character(read_dta$patid)
  read_dta$prodcodeid <- as.character(read_dta$prodcodeid)
  read_dta$issuedate <- as_date(read_dta$issuedate)
  merged_drugIssue <- inner_join(mypatids, read_dta, by = "patid")
  # keep only those records with prescription date same as the PO diagnosis date:
  merged_drugIssue <- merged_drugIssue %>% filter(myeventdate == issuedate)
  assign(paste0("DrugIssue_PO_coh", i), merged_drugIssue)
  print(i) # print iteration number
}

# unite all the merged files (I've done this manually)
u <- dplyr::union(DrugIssue_PO_coh1, DrugIssue_PO_coh2) # files 1+2

# append them manually (e.g. for i 3 to 771)
for(i in 3:length(drugIssue_fileslist)){ # rest of files
  u <- dplyr::union(u, eval(parse(text = paste0("DrugIssue_PO_coh", i))))
}

# -----------------------------------------------------------------
# The following files include prescriptions, only for those patids 
# who had a PO event recorded at the same day as the PO diagnosis
# -----------------------------------------------------------------

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted/")

write.csv(u, "DrugIssue_PO_coh_new.csv")



