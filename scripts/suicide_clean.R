library(readr)
library(data.table)
#abcd_ksad01.txt
#abcd_ksad501.txt

ksad_p = read.csv(file = "abcd data/abcd_ksad01.txt", sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))
ksad_y = read.csv(file = "abcd data/abcd_ksad501.txt", sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))


#remove details line
ksad_p = ksad_p[-1,]
ksad_p = droplevels(ksad_p)

ksad_y = ksad_y[-1,]
ksad_y = droplevels(ksad_y)

#remove columns introduced by NDA
ksad_p = ksad_p[,!(names(ksad_p) %in% c("abcd_ksad01_id", "collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name", "dataset_id"))]
ksad_y = ksad_y[,!(names(ksad_y) %in% c("abcd_ksad501_id", "collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name", "dataset_id"))]


##calculate suicidality
#if one of the items is 1, the result will be 1
#if one of the items is NA and the rest is 0, the result will be NA

#ideation
ksad_p$SI_current_p = apply(ksad_p[,which(grepl("ksads_23_(946|947|948|949|950|951)", colnames(ksad_p)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SI_current_y = apply(ksad_y[,which(grepl("ksads_23_(946|947|948|949|950|951)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_p$SI_past_p = apply(ksad_p[,which(grepl("ksads_23_(957|958|959|960|961|962)", colnames(ksad_p)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SI_past_y = apply(ksad_y[,which(grepl("ksads_23_(957|958|959|960|961|962)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_p$SI_p = (ksad_p$SI_current_p == 1 | ksad_p$SI_past_p == 1)*1
ksad_y$SI_y = (ksad_y$SI_current_y == 1 | ksad_y$SI_past_y == 1)*1


#attempt
ksad_p$SA_current_p = apply(ksad_p[,which(grepl("ksads_23_(952|953|954)", colnames(ksad_p)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SA_current_y = apply(ksad_y[,which(grepl("ksads_23_(952|953|954)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_p$SA_past_p = apply(ksad_p[,which(grepl("ksads_23_(963|964|965)", colnames(ksad_p)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SA_past_y = apply(ksad_y[,which(grepl("ksads_23_(963|964|965)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_p$SA_p = (ksad_p$SA_current_p == 1 | ksad_p$SA_past_p == 1)*1
ksad_y$SA_y = (ksad_y$SA_current_y == 1 | ksad_y$SA_past_y == 1)*1


#suicidality: combination of ideation and attempt
ksad_p$suicidality_p = (ksad_p$SI_p == 1 | ksad_p$SA_p == 1)*1
ksad_y$suicidality_y = (ksad_y$SI_y == 1 | ksad_y$SA_y == 1)*1
  


#create separate variables for baseline and 1 year follow up

suicide_set = merge(ksad_y,ksad_p)

suicide_set_baseline = suicide_set[which(grepl("baseline", suicide_set$eventname)),which(grepl("ksads_23_|src|inter|event|gender|SI|SA|sui", colnames(suicide_set)))]

suicide_set_1_year_follow = suicide_set[which(grepl("1_year_follow_up", suicide_set$eventname)),which(grepl("src|interview_a|gender|SI_|SA_|sui", colnames(suicide_set)))]

suicide_set_1_year_follow$suicidality_current_1_year_p = (suicide_set_1_year_follow$SI_current_p == 1 | suicide_set_1_year_follow$SA_current_p == 1)*1
suicide_set_1_year_follow$suicidality_current_1_year_y = (suicide_set_1_year_follow$SI_current_y == 1 | suicide_set_1_year_follow$SA_current_y == 1)*1

#remove parent report, they all empty 
suicide_set_1_year_follow = suicide_set_1_year_follow[,which(colSums(is.na(suicide_set_1_year_follow)) != 4948)]

#update names
colnames(suicide_set_1_year_follow)[2] = "interview_age_1_year"
colnames(suicide_set_1_year_follow)[4] = "SI_current_1_year_y"
colnames(suicide_set_1_year_follow)[5] = "SI_past_1_year_y"
colnames(suicide_set_1_year_follow)[6] = "SI_1_year_y"
colnames(suicide_set_1_year_follow)[7] = "SA_current_1_year_y"
colnames(suicide_set_1_year_follow)[8] = "SA_past_1_year_y"
colnames(suicide_set_1_year_follow)[9] = "SA_1_year_y"
colnames(suicide_set_1_year_follow)[10] = "suicidality_1_year_y"


suicide_firstyear_ontopof_baseline = merge(suicide_set_baseline, suicide_set_1_year_follow , all.x = T)
write.csv(file = "data/suicide_firstyear_ontopof_baseline.csv",x = suicide_firstyear_ontopof_baseline,row.names=F, na = "")
