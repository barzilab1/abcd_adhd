
source("config.R")

ksad_y = read.csv(file = paste0(suicide_files_path,"abcd_ksad501.txt"), sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))

ksad_y = ksad_y[-1,]
ksad_y = ksad_y[,!(names(ksad_y) %in% c("abcd_ksad501_id", "collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name", "dataset_id"))]


#555 and 888 will be treated as NA
ksad_y[ksad_y == "888" | ksad_y == "555"] = NA


#all kids diagnosis
ksad_y_diagnosis = ksad_y[ksad_y$eventname == "baseline_year_1_arm_1",grepl("src|inter|event|sex|_((8[3-4][0-9])|863|864|869|870|(91[1-4])|969|970)_t",colnames(ksad_y))]

ksad_y_diagnosis = droplevels(ksad_y_diagnosis)

#remove empty col
ksad_y_diagnosis = ksad_y_diagnosis[,!colSums(is.na(ksad_y_diagnosis)) == nrow(ksad_y_diagnosis)]

#create diagnosis variables
#if 0 or NA then 0
ksad_y_diagnosis$diagnosis_bipolar_y = apply(ksad_y_diagnosis[,grepl("ksads_2_.*_t", colnames(ksad_y_diagnosis))],1 ,function(x) {any(x == 1)*1})
ksad_y_diagnosis$diagnosis_depression_y = apply(ksad_y_diagnosis[,grepl("ksads_1_.*_t", colnames(ksad_y_diagnosis))],1 ,function(x) {any(x == 1)*1})
ksad_y_diagnosis$diagnosis_DMDD_y = ksad_y_diagnosis$ksads_3_848_t
ksad_y_diagnosis$diagnosis_anxiety_y = apply(ksad_y_diagnosis[,grepl("ksads_(8|10)_.*_t", colnames(ksad_y_diagnosis))],1 ,function(x) {any(x == 1)*1})
ksad_y_diagnosis$diagnosis_sleep_y = apply(ksad_y_diagnosis[,grepl("ksads_22_.*_t", colnames(ksad_y_diagnosis))],1 ,function(x) {any(x == 1)*1})

summary(ksad_y_diagnosis) 

write.csv(file = "outputs/ksad_y_diagnosis.csv",x = ksad_y_diagnosis, row.names=F, na = "")


