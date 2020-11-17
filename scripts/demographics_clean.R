##############################################
#' most of the code below is adjusted from 
#' the abcd official github:
#' https://github.com/ABCD-STUDY/analysis-nda 
##############################################

library(readr)
library(data.table)

source("config.R")

demographics_set = read.csv(file = paste0(demographics_files_path,"pdem02.txt"), sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))

#remove details line
demographics_set = demographics_set[-1,]
demographics_set = droplevels(demographics_set)

#remove columns introduced by NDA
demographics_set = demographics_set[,!(names(demographics_set) %in% c("pdem02_id", "collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name", "dataset_id"))]

#remove empty columns 
names(demographics_set)[sapply(demographics_set, function(x) all((x=="NA")|(is.na(x))))]
demographics_set = demographics_set[!sapply(demographics_set, function(x) all((x=="NA")|(is.na(x))))]

#rearrange data
###1. convert variables names to be more readable  
###2. change outliers (777,999) to be NA
demographics_set = data.table(demographics_set)

## sex
#convert the NIH sex at birth (equal to demo_sex_v2)
demographics_set[, sex_br := (sex == "F")*1]
demographics_set[,demo_sex_v2 := NULL]

## age
#interview age will be used instead of age
demographics_set[,demo_brthdat_v2:=NULL] 
demographics_set[, age := as.numeric(as.character(interview_age))]

## gender
demographics_set[,gender := as.numeric(as.character(demo_gender_id_v2))]
demographics_set[(gender %in%  c(777,999)) ,gender := NA] 
demographics_set[, gender:= gender-1]
demographics_set[, demo_gender_id_v2:= NULL]

##ethnicity
demographics_set[demo_ethn_v2 == "1", ethnicity_hisp := 1]
demographics_set[demo_ethn_v2 == "2", ethnicity_hisp := 0]
demographics_set[(demo_ethn_v2 %in% c(777,999)), ethnicity_hisp:= NA]
demographics_set = demographics_set[, demo_ethn_v2 := NULL]


##child race
#refuse to answer and dont know will be 0

# White
demographics_set[, race_white:= (demo_race_a_p___10 == "1")*1 ]

# Black
demographics_set[, race_black:= (demo_race_a_p___11 == "1")*1 ]

# Asian
demographics_set[, race_asian:= 0]
demographics_set[ (demo_race_a_p___18 == "1" | demo_race_a_p___19 == "1" | demo_race_a_p___20 == "1" |
                     demo_race_a_p___21 == "1" | demo_race_a_p___22 == "1" | demo_race_a_p___23 == "1" |
                     demo_race_a_p___24 =="1"), race_asian:= 1 ]

# AIAN: American Indian and Alaska Native
demographics_set[, race_aian:= 0]
demographics_set[ (demo_race_a_p___12 == "1" | demo_race_a_p___13 == "1"), race_aian:=1 ]


#NHPI: Native Hawaiian and Other Pacific
demographics_set[, race_nhpi:= 0]
demographics_set[ demo_race_a_p___14 == "1" | demo_race_a_p___15 == "1" | demo_race_a_p___16 == "1" |
                    demo_race_a_p___17 == "1", race_nhpi:= 1 ]

# Other
demographics_set[, race_other:= 0 ]
demographics_set[ demo_race_a_p___25 == "1", race_other:= 1 ]

# Mixed
demographics_set[, race_mixed:= (race_white + race_black + race_asian + race_aian + race_nhpi + race_other)]
demographics_set[, table(race_mixed, useNA = "if")]
demographics_set[ race_mixed <= 1, race_mixed:= 0]
demographics_set[ race_mixed > 1, race_mixed:= 1]
demographics_set[, table(race_mixed, useNA = "if")]

demographics_set[,colnames(demographics_set)[(grepl("^demo_race_a_p___",colnames(demographics_set)))] := NULL]


##parents education
demographics_set[(demo_prnt_ed_v2 %in%  c(777,999)), demo_prnt_ed_v2:= NA]
demographics_set[(demo_prtnr_ed_v2 %in%  c(777,999)), demo_prtnr_ed_v2:= NA]
demographics_set[, parents_avg_edu:= (as.numeric(as.character(demo_prnt_ed_v2)) + as.numeric(as.character(demo_prtnr_ed_v2)))/2]
#in case of edu is missing in one of the parents, it will be the edu the of other parent 
demographics_set[is.na(parents_avg_edu), parents_avg_edu:= as.numeric(as.character(demo_prnt_ed_v2))]
demographics_set[is.na(parents_avg_edu), parents_avg_edu:= as.numeric(as.character(demo_prtnr_ed_v2))]

##income 
demographics_set[,household_income:= demo_comb_income_v2]
demographics_set[( household_income %in%  c(777,999)) ,household_income:= NA]
demographics_set[,demo_comb_income_v2 := NULL]

#married status 
demographics_set[demo_prnt_marital_v2 == 777 , demo_prnt_marital_v2:= NA]
demographics_set[,separated_or_divorced := 0]
demographics_set[(demo_prnt_marital_v2 %in%  c(3,4)), separated_or_divorced := 1]

demographics_set = droplevels(demographics_set)

#remove irrelevant columns
# demographics_set [, c("demoi_p_select_language___1", "demo_adopt_agex_v2_bl_dk", "demo_years_us_v2_dk" ) := NULL]

#remove outliers of "number of household members"
demographics_set[demo_roster_v2 %in% c(60,77), demo_roster_v2:= NA]

write.csv(file = "outputs/adha_demographics.csv",x = demographics_set[,c("src_subject_id", "interview_date", "interview_age", "demo_prim" , "eventname", "sex",
                                                                                      "demo_ed_v2", "race_white", "race_black", "race_aian", "race_nhpi", "race_asian", "race_other","race_mixed" ,"ethnicity_hisp", 
                                                                                      "demo_prnt_marital_v2", "household_income", "demo_roster_v2", "age", "sex_br", "gender", "parents_avg_edu", "separated_or_divorced")], row.names=F, na = "")






