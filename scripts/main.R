library(readr)
# library(Amelia)
############## adhd project

adhd_demographics <- read_csv("data/adha_demographics.csv")
# cbcls01 <- read_csv("data/cbcls01.csv")
externalize_ksad <- read_csv("data/externalize_ksad_adhd.csv")
meds_tagged <- read_csv("data/meds_tagged.csv")
suicide_firstyear_ontopof_baseline <- read_csv("data/suicide_firstyear_ontopof_baseline.csv")

#demographics
adhd_demographics$demo_roster_v2[adhd_demographics$demo_roster_v2 > 50] = NA


#medication 
meds_tagged$any_adhd_med = apply(meds_tagged[,c("MPH", "Amphetamine", "alpha_agonist", "atomoxetine")],1 ,function(x) {any(x == 1)*1})
meds_tagged$antidepression_meds = apply(meds_tagged[,c("SSRI", "Other_antidepressants_anxiety")],1 ,function(x) {any(x == 1)*1})
###either depression or pscy
meds_tagged$any_depression_psychotic_meds = apply(meds_tagged[,c("antidepression_meds", "Antipsychotic")],1 ,function(x) {any(x == 1)*1})

meds_tagged = meds_tagged[which(grepl("baseline", meds_tagged$eventname)),!(grepl("interview_date", colnames(meds_tagged)))]
#remove empty cols
meds_tagged = meds_tagged[,which(colSums(is.na(meds_tagged)) != 11875)]


#ksad externalize
externalize_ksad = externalize_ksad[which(grepl("baseline", externalize_ksad$eventname)),]


adhd_dataset = merge(adhd_demographics, meds_tagged)
# adhd_dataset = merge(adhd_dataset, cbcls01)
adhd_dataset = merge(adhd_dataset, externalize_ksad)

adhd_dataset_sui = merge(adhd_dataset,suicide_firstyear_ontopof_baseline)
#remove NIH gender, eventname
adhd_dataset = adhd_dataset[,!(colnames(adhd_dataset) %in% c("gender", "eventname"))]
adhd_dataset_sui = adhd_dataset_sui[,!(colnames(adhd_dataset_sui) %in% c("gender", "eventname"))]

write.csv(file = "outputs/adhd_dataset_sui_all_features.csv",x = adhd_dataset_sui,row.names=F, na = "")

#select ADHD first model features
adhd_sub_set = adhd_dataset_sui[,(grepl("^(src|inter|brought|mph|amp|alpha|ato|ssri|other|an|SI|SA|sui|race|demo|ethnicity|household|age|sex|gender|parents|cbcl)|(_symptoms_sum)|Diagnosis", colnames(adhd_dataset_sui), ignore.case = T))]

write.csv(file = "outputs/adhd_dataset_sui.csv",x = adhd_sub_set,row.names=F, na = "")






################### not relevant ################3
# #check suicide
# length(which(rowSums(is.na(adhd_sub_set[,grepl("^(SI|SA|sui)", colnames(adhd_sub_set), ignore.case = T)])) == 14)) #39
# length(which(rowSums(is.na(adhd_sub_set[,grepl("^(SI|SA|sui).*_y$", colnames(adhd_sub_set), ignore.case = T)])) == 7)) #81-39 = 42
# length(which(rowSums(is.na(adhd_sub_set[,grepl("^(SI|SA|sui).*_p$", colnames(adhd_sub_set), ignore.case = T)])) == 7)) #151-39 = 112
# 
# View(adhd_sub_set[which(#rowSums(is.na(adhd_sub_set[,grepl("^(SI|SA|sui).*_p$", colnames(adhd_sub_set), ignore.case = T)])) == 7 &
#                           rowSums( is.na(adhd_sub_set[,grepl("symptoms_sum$", colnames(adhd_sub_set), ignore.case = T)])) == 4  ),])
# 
# #remove kids without sui
# adhd_sub_set = adhd_sub_set[-which( 
#                           (rowSums(is.na(adhd_sub_set[,grepl("^(SI|SA|sui).*_p$", colnames(adhd_sub_set), ignore.case = T)])) == 7) |
#                           (rowSums(is.na(adhd_sub_set[,grepl("^(SI|SA|sui).*_y$", colnames(adhd_sub_set), ignore.case = T)])) == 7) 
#                           ),]
# 
# summary(adhd_sub_set)
# 
# #check for kids with no data
# to_imput_set = adhd_sub_set[,c(5:17,19:50)]
# which(rowSums(is.na(to_imput_set)) > 30) #2
# adhd_sub_set = adhd_sub_set[-which(rowSums(is.na(to_imput_set)) > 30),]
# 
# #imputation
# set.seed(135)
# amelia = amelia(adhd_sub_set, m = 1, idvars = c(1:4,18,25:26,50:64), ords = c(5,12:14,27:49), noms = c(6:11,15:16,19:24))
# adhd_sub_set_imputed = amelia$imputations$imp1
# 
# #updates according to the imputation
# adhd_sub_set_imputed$ksads_externalizing_symptoms_sum = adhd_sub_set_imputed$ksads_ADHD_symptoms_sum + adhd_sub_set_imputed$ksads_ODD_symptoms_sum + adhd_sub_set_imputed$ksads_CONDUCT_symptoms_sum
# adhd_sub_set_imputed$parents_avg_edu = round(adhd_sub_set_imputed$parents_avg_edu/.5)*.5
# adhd_sub_set_imputed$any_adhd_med = apply(adhd_sub_set_imputed[,c("MPH", "Amphetamine", "alpha_agonist", "atomoxetine")],1 ,function(x) {any(x == 1)*1})
# adhd_sub_set_imputed$antidepression_meds = apply(adhd_sub_set_imputed[,c("SSRI", "Other_antidepressants_anxiety")],1 ,function(x) {any(x == 1)*1})
# 
# write.csv(file = "outputs/adhd_dataset_sui_imputed.csv",x = adhd_sub_set_imputed,row.names=F, na = "")
# 
# 
# 
