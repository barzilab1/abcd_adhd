library(readr)

source("config.R")


###cbcls
cbcls01 = read.csv(file = paste0(psychopathology_files_path,"abcd_cbcls01.txt"), sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))

#remove details line
cbcls01 = cbcls01[-1,]
cbcls01 = droplevels(cbcls01)

#remove columns introduced by NDA
cbcls01 = cbcls01[,!(names(cbcls01) %in% c("abcd_cbcls01_id", "collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name", "dataset_id"))]


#get all the t scorse
cbcls_t_score = cbcls01[cbcls01$eventname == "baseline_year_1_arm_1", grepl("^(src|interview|event|sex)|_t$", colnames(cbcls01))]
  
  
write.csv(file = "outputs/cbcls_t_score.csv",x = cbcls_t_score, row.names = F, na = "")
                                                                           