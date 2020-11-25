library(readr)
# library(Amelia)

############## merge all datasets for the  adhd project ############## 

adhd_demographics <- read_csv("outputs/adha_demographics.csv")
cbcls_t_score.csv <- read_csv("outputs/cbcls_t_score.csv")
exposome_set <- read_csv("outputs/exposome_set.csv")
externalize_ksad <- read_csv("outputs/externalize_ksad_adhd.csv")
meds_tagged <- read_csv("outputs/meds_tagged.csv", col_types = cols(.default = "n", 
                                                                    "src_subject_id" = "c",
                                                                    "med1_rxnorm_p" = "c",
                                                                    "med3_rxnorm_p" = "c",
                                                                    "med4_rxnorm_p" = "c",
                                                                    "med7_rxnorm_p" = "c",
                                                                    "med9_rxnorm_p" = "c",
                                                                    "med12_rxnorm_p" = "c",
                                                                    "med_otc_1_rxnorm_p" = "c",
                                                                    "med_otc_2_rxnorm_p" = "c", 
                                                                    "med_otc_3_rxnorm_p" = "c",
                                                                    "med_otc_4_rxnorm_p" = "c",
                                                                    "med_otc_5_rxnorm_p" = "c",
                                                                    "med_otc_6_rxnorm_p" = "c",
                                                                    "eventname" = "c") )
suicide_firstyear_ontopof_baseline <- read_csv("outputs/suicide_firstyear_ontopof_baseline.csv")


#medication 
meds_tagged$any_adhd_med = apply(meds_tagged[,c("MPH", "Amphetamine", "alpha_agonist", "atomoxetine")],1 ,function(x) {any(x == 1)*1})
meds_tagged$antidepression_meds = apply(meds_tagged[,c("SSRI", "Other_antidepressants_anxiety")],1 ,function(x) {any(x == 1)*1})
#either depression or pscy
meds_tagged$any_depression_psychotic_meds = apply(meds_tagged[,c("antidepression_meds", "Antipsychotic")],1 ,function(x) {any(x == 1)*1})


#ksad externalize
externalize_ksad = externalize_ksad[which(grepl("baseline", externalize_ksad$eventname)),]


adhd_dataset = merge(adhd_demographics, meds_tagged)
adhd_dataset = merge(adhd_dataset, cbcls_t_score.csv)
adhd_dataset = merge(adhd_dataset, externalize_ksad)
adhd_dataset = merge(adhd_dataset, exposome_set)

adhd_dataset_sui = merge(adhd_dataset,suicide_firstyear_ontopof_baseline)
#remove NIH sex, eventname
adhd_dataset = adhd_dataset[,!(colnames(adhd_dataset) %in% c("sex", "eventname"))]
adhd_dataset_sui = adhd_dataset_sui[,!(colnames(adhd_dataset_sui) %in% c("sex", "eventname"))]

write.csv(file = "outputs/adhd_dataset_sui_all_features.csv",x = adhd_dataset_sui,row.names=F, na = "")

#select ADHD first model features
adhd_sub_set = adhd_dataset_sui[,(grepl("^(src|inter|brought|mph|amp|alpha|ato|ssri|other|an|SI|SA|sui|race|demo|ethnicity|household|age|sex|gender|parents|rel_fami)|(_symptoms_sum)|Diagnosis|positive_school_involvement|fes_y_ss_fc|parent_monitor_mea|stq_y_ss_weekend", colnames(adhd_dataset_sui), ignore.case = T))]

write.csv(file = "outputs/adhd_dataset_sui.csv",x = adhd_sub_set,row.names=F, na = "")



#creates bios!
#remove rows with missing data only in the colunms that are part of the main analysis
# adhd_sub_set_clean = adhd_sub_set[complete.cases(adhd_sub_set[,c("sex_br","age","race_white","race_black","ethnicity_hisp", "parents_avg_edu",
#                                                     "any_adhd_med", "any_depression_psychotic_meds",
#                                                     "ksads_externalizing_exclude_attentation_symptoms_sum",
#                                                     "suicidality_y")]),]


#select one participant from each family 
set.seed(131)
adhd_one_family_member = do.call(rbind, 
                              lapply(split(adhd_sub_set,adhd_sub_set$rel_family_id),
                                    function(x){x[sample(nrow(x),1),]}))

write.csv(file = "outputs/adhd_one_family_member.csv",x = adhd_one_family_member, row.names=F, na = "")


#remove all families 
adhd_no_family = adhd_sub_set[!(duplicated(adhd_sub_set$rel_family_id) | duplicated(adhd_sub_set$rel_family_id, fromLast=TRUE)), ]
write.csv(file = "outputs/adhd_no_family.csv",x = adhd_no_family, row.names=F, na = "")


                                                                 

