library(readr)
library(data.table)

source("config.R")

ksad_p = read.csv(file = paste0(suicide_files_path,"abcd_ksad01.txt"), sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))
ksad_y = read.csv(file = paste0(suicide_files_path,"abcd_ksad501.txt"), sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))


#remove details line
ksad_p = ksad_p[-1,]
ksad_y = ksad_y[-1,]

#remove columns introduced by NDA
ksad_p = ksad_p[,!(names(ksad_p) %in% c("abcd_ksad01_id", "collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name", "dataset_id"))]
ksad_y = ksad_y[,!(names(ksad_y) %in% c("abcd_ksad501_id", "collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name", "dataset_id"))]

#555 and 888 will be treated as NA
ksad_p[ksad_p == "888" | ksad_p == "555"] = NA
ksad_y[ksad_y == "888" | ksad_y == "555"] = NA

ksad_p = droplevels(ksad_p)
ksad_y = droplevels(ksad_y)


#ksad externalizing symptoms
#unlike suicide, here if (0|NA) then 0
# externalize items
externalize_ksad_p = ksad_p[,which(grepl("^(src|inter|event|sex|ksads_(1[4-6|9]|2(0|4))_)", colnames(ksad_p)))]
externalize_ksad_y = ksad_y[,which(grepl("^(src|inter|event|sex|ksads_([1-3]|8|10|22)_(8|9)[0-9][0-9])", colnames(ksad_y)))]


#ADHD
#for each pair, create one variable to represent the adhd symptoms 

pairs_items = c(394 , 410)
pairs_items = rbind(pairs_items, c(395 , 411))
pairs_items = rbind(pairs_items, c(396 , 412))
pairs_items = rbind(pairs_items, c(397 , 413))
pairs_items = rbind(pairs_items, c(398 , 414))
pairs_items = rbind(pairs_items, c(399 , 415))
pairs_items = rbind(pairs_items, c(400 , 416))
pairs_items = rbind(pairs_items, c(401 , 417))
pairs_items = rbind(pairs_items, c(402 , 418))
pairs_items = rbind(pairs_items, c(403 , 419))
pairs_items = rbind(pairs_items, c(404 , 420))
pairs_items = rbind(pairs_items, c(405 , 421))
pairs_items = rbind(pairs_items, c(406 , 422))
pairs_items = rbind(pairs_items, c(407 , 423))
pairs_items = rbind(pairs_items, c(408 , 424))
pairs_items = rbind(pairs_items, c(409 , 425))


for(i in 1:dim(pairs_items)[1]){
  new_col_name = paste0("temp_adhd_",i)
  e_1 = paste0("ksads_14_",pairs_items[i,1],"_p")
  e_2 = paste0("ksads_14_",pairs_items[i,2],"_p") 
  
  externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
  externalize_ksad_p[,new_col_name] = ifelse( ( is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0)) ),
                                              0, externalize_ksad_p[,new_col_name])
  print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}
#add "sustaining attention" symptom
externalize_ksad_p$temp_adhd_17 = apply(externalize_ksad_p[,grepl("ksads_14_(7[6-9])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 1)*1})
summary(externalize_ksad_p[,grepl("temp_adhd_17|ksads_14_(7[6-9])_p", colnames(externalize_ksad_p))])

#add "Easily distracted" symptom
externalize_ksad_p$temp_adhd_18 = apply(externalize_ksad_p[,grepl("ksads_14_(8[0-3])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 1)*1})
summary(externalize_ksad_p[,grepl("temp_adhd_18|ksads_14_(8[0-3])_p", colnames(externalize_ksad_p))])

#add "Difficulty remaining seated" symptom
externalize_ksad_p$temp_adhd_19 = apply(externalize_ksad_p[,grepl("ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 1)*1})
externalize_ksad_p$temp_adhd_19 = ifelse( ( is.na(externalize_ksad_p$temp_adhd_19) & (apply(externalize_ksad_p[,grepl("ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 0)})) ),
                                          0, externalize_ksad_p$temp_adhd_19)
summary(externalize_ksad_p[,grepl("temp_adhd_19|ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))])

#add "Impulsivity" symptom
externalize_ksad_p$temp_adhd_20 = (externalize_ksad_p$ksads_14_88_p == 1 | externalize_ksad_p$ksads_14_89_p == 1 |externalize_ksad_p$ksads_14_90_p == 1) *1
summary(externalize_ksad_p[,c("temp_adhd_20", "ksads_14_88_p", "ksads_14_89_p", "ksads_14_90_p")])


#create summary for adhd symptoms 
externalize_ksad_p$ksads_ADHD_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_adhd_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA (rowSums gave a number to all rows)
externalize_ksad_p$ksads_ADHD_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_adhd_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == 20] = NA


#ADHD exclude attention
#for each pair, create one variable to represent the adhd symptoms excluding attention
pairs_items = c(401 , 417)
pairs_items = rbind(pairs_items, c(402 , 418))
pairs_items = rbind(pairs_items, c(403 , 419))
pairs_items = rbind(pairs_items, c(404 , 420))
pairs_items = rbind(pairs_items, c(405 , 421))
pairs_items = rbind(pairs_items, c(406 , 422))
pairs_items = rbind(pairs_items, c(407 , 423))
pairs_items = rbind(pairs_items, c(408 , 424))


for(i in 1:dim(pairs_items)[1]){
  new_col_name = paste0("temp_adhd_ex_",i)
  e_1 = paste0("ksads_14_",pairs_items[i,1],"_p")
  e_2 = paste0("ksads_14_",pairs_items[i,2],"_p") 
  
  externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
  externalize_ksad_p[,new_col_name] = ifelse( (is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0))), 
                                              0, externalize_ksad_p[,new_col_name])
  print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}

#add "Difficulty remaining seated" symptom
externalize_ksad_p$temp_adhd_ex_9 = apply(externalize_ksad_p[,grepl("ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 1)*1})
externalize_ksad_p$temp_adhd_ex_9 = ifelse( ( is.na(externalize_ksad_p$temp_adhd_ex_9) & (apply(externalize_ksad_p[,grepl("ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))],1 ,function(x) {any(x == 0)})) ),
                                          0, externalize_ksad_p$temp_adhd_ex_9)
summary(externalize_ksad_p[,grepl("temp_adhd_ex_9|ksads_14_(8[4-7])_p", colnames(externalize_ksad_p))])

#add "Impulsivity" symptom
externalize_ksad_p$temp_adhd_ex_10 = (externalize_ksad_p$ksads_14_88_p == 1 | externalize_ksad_p$ksads_14_89_p == 1 |externalize_ksad_p$ksads_14_90_p == 1) *1
summary(externalize_ksad_p[,c("temp_adhd_ex_10", "ksads_14_88_p", "ksads_14_89_p", "ksads_14_90_p")])

#create summary for adhd symptoms excluding attention
externalize_ksad_p$ksads_ADHD_exclude_attention_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_adhd_ex_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA (rowSums gave a number to all rows)
externalize_ksad_p$ksads_ADHD_exclude_attention_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_adhd_ex_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == 10] = NA
# View(externalize_ksad_p[,which(grepl("adhd_ex", colnames(externalize_ksad_p), ignore.case=TRUE))])


#ODD
#for each pair, create one variable to represent the ODD symptoms 
pairs_items = c(91, 92)
pairs_items = rbind(pairs_items, c(93 , 94))
pairs_items = rbind(pairs_items, c(95 , 96))
pairs_items = rbind(pairs_items, c(432 , 439))
pairs_items = rbind(pairs_items, c(433 , 440))
pairs_items = rbind(pairs_items, c(434 , 441))
pairs_items = rbind(pairs_items, c(435 , 442))
pairs_items = rbind(pairs_items, c(436 , 443))
pairs_items = rbind(pairs_items, c(437 , 444))

for(i in 1:9){
  new_col_name = paste0("temp_odd_",i)
  e_1 = paste0("ksads_15_",pairs_items[i,1],"_p")
  e_2 = paste0("ksads_15_",pairs_items[i,2],"_p") 
  
  externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
  externalize_ksad_p[,new_col_name] = ifelse( (is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0))), 
                                              0, externalize_ksad_p[,new_col_name])
  print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}

#create summary for ODD
externalize_ksad_p$ksads_ODD_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_odd_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA (rowSums gave a number to all rows)
externalize_ksad_p$ksads_ODD_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_odd_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == 9] = NA
# View(adhd_ksad_p[is.na(adhd_ksad_p$ksads_15_95_p),which(grepl("odd", colnames(adhd_ksad_p), ignore.case=TRUE))])


#Conduct
pairs_items = c(98 , 99)
pairs_items = rbind(pairs_items, c(100, 101))
pairs_items = rbind(pairs_items, c(102 , 103))
pairs_items = rbind(pairs_items, c(104 , 105))
pairs_items = rbind(pairs_items, c(106 , 107))
pairs_items = rbind(pairs_items, c(447 , 448))
pairs_items = rbind(pairs_items, c(449 , 450))
pairs_items = rbind(pairs_items, c(451 , 452))
pairs_items = rbind(pairs_items, c(453 , 454))
pairs_items = rbind(pairs_items, c(455 , 456))
pairs_items = rbind(pairs_items, c(457 , 458))
pairs_items = rbind(pairs_items, c(459 , 460))
pairs_items = rbind(pairs_items, c(461 , 462))
pairs_items = rbind(pairs_items, c(463 , 464))
pairs_items = rbind(pairs_items, c(465 , 466))

for(i in 1:15){
  new_col_name = paste0("temp_conduct_",i)
  e_1 = paste0("ksads_16_",pairs_items[i,1],"_p")
  e_2 = paste0("ksads_16_",pairs_items[i,2],"_p") 
  
  externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
  externalize_ksad_p[,new_col_name] = ifelse( (is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0))), 
                                              0, externalize_ksad_p[,new_col_name])
  print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}

#create summary for Conduct
externalize_ksad_p$ksads_CONDUCT_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_conduct_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA (rowSums gave a number to all rows)
externalize_ksad_p$ksads_CONDUCT_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_conduct_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == 15] = NA
# View(adhd_ksad_p[is.na(adhd_ksad_p$ksads_16_102_p),which(grepl("conduct", colnames(adhd_ksad_p), ignore.case=TRUE))])


externalize_ksad_p$ksads_externalizing_symptoms_sum = externalize_ksad_p$ksads_ADHD_symptoms_sum + externalize_ksad_p$ksads_ODD_symptoms_sum + externalize_ksad_p$ksads_CONDUCT_symptoms_sum 
externalize_ksad_p$ksads_externalizing_exclude_attentation_symptoms_sum = externalize_ksad_p$ksads_ADHD_exclude_attention_symptoms_sum + externalize_ksad_p$ksads_ODD_symptoms_sum + externalize_ksad_p$ksads_CONDUCT_symptoms_sum 

#remove temp 
externalize_ksad_p = externalize_ksad_p[,!( grepl("temp_", colnames(externalize_ksad_p)))]



#ksad externalizing Diagnosis
#unlike suicide, here if (0|NA) then 0

externalize_ksad_p$ksads_ADHD_Diagnosis = apply(externalize_ksad_p[,which(grepl("ksads_14_(85[3-6])", colnames(externalize_ksad_p)))],1 ,function(x) {any(x == 1)*1})
print(summary(externalize_ksad_p[,which(grepl("ksads_ADHD_Diagnosis|ksads_14_(85[3-6])", colnames(externalize_ksad_p)))]))

externalize_ksad_p$ksads_ODD_Diagnosis = apply(externalize_ksad_p[,which(grepl("ksads_15_(901|902)", colnames(externalize_ksad_p)))],1 , function(x) {any(x == 1)*1})
print(summary(externalize_ksad_p[,which(grepl("ksads_ODD_Diagnosis|ksads_15_(901|902)", colnames(externalize_ksad_p)))]))

externalize_ksad_p$ksads_CONDUCT_Diagnosis = apply(externalize_ksad_p[,which(grepl("ksads_16_(897|898|899|900)", colnames(externalize_ksad_p)))],1 ,function(x) {any(x == 1)*1})
print(summary(externalize_ksad_p[,which(grepl("ksads_CONDUCT_Diagnosis|ksads_16_(897|898|899|900)", colnames(externalize_ksad_p)))]))

externalize_ksad_p$ksads_any_externalizing_diagnosis = (externalize_ksad_p$ksads_ADHD_Diagnosis | externalize_ksad_p$ksads_ODD_Diagnosis | externalize_ksad_p$ksads_CONDUCT_Diagnosis)*1

externalize_ksad = merge(externalize_ksad_y, externalize_ksad_p)

#remove not relevant ODD items for ADHD project only 
#ksads_15_446_p ksads_15_445_p ksads_15_438_p ksads_15_97_p
externalize_ksad_adhd = externalize_ksad[,!( grepl("ksads_15_(97|44[5-6]|438)_p", colnames(externalize_ksad)))]
write.csv(file = "outputs/externalize_ksad_adhd.csv",x = externalize_ksad_adhd, row.names=F, na = "")


