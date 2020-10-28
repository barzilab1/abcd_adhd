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


#ksad externalizing symptoms
#unlike suicide, here if (0|NA) then 0

# externalize items
externalize_ksad_y = ksad_y[,which(grepl("^(src|inter|event|gender|ksads_([1-3]|8|10|22)_(8|9)[0-9][0-9])", colnames(ksad_y)))]
externalize_ksad_p = ksad_p[,which(grepl("^(src|inter|event|gender|ksads_(1[4-6|9]|2(0|4))_)", colnames(ksad_p)))]


#ADHD
pairs_items = c(76 , 78)
pairs_items = rbind(pairs_items, c(77 , 79))
pairs_items = rbind(pairs_items, c(80 , 82))
pairs_items = rbind(pairs_items, c(81 , 83))
pairs_items = rbind(pairs_items, c(84 , 86))
pairs_items = rbind(pairs_items, c(85 , 87))
pairs_items = rbind(pairs_items, c(394 , 410))
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


for(i in 1:nrow(pairs_items)){
  new_col_name = paste0("temp_adhd_",i)
  e_1 = paste0("ksads_14_",pairs_items[i,1],"_p")
  e_2 = paste0("ksads_14_",pairs_items[i,2],"_p") 
  
  externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
  externalize_ksad_p[,new_col_name] = ifelse( (is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0))), 0, externalize_ksad_p[,new_col_name])
  print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}
#add the tri
externalize_ksad_p$temp_adhd_23 = (externalize_ksad_p$ksads_14_88_p == 1 | externalize_ksad_p$ksads_14_89_p == 1 |externalize_ksad_p$ksads_14_90_p == 1) *1
summary(externalize_ksad_p[,c("temp_adhd_23", "ksads_14_88_p", "ksads_14_89_p", "ksads_14_90_p")])

externalize_ksad_p$ksads_ADHD_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_adhd_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA
externalize_ksad_p$ksads_ADHD_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_adhd_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == 23] = NA


#ADHD exclude attention
pairs_items = c(84 , 86)
pairs_items = rbind(pairs_items, c(85 , 87))
pairs_items = rbind(pairs_items, c(401 , 417))
pairs_items = rbind(pairs_items, c(402 , 418))
pairs_items = rbind(pairs_items, c(403 , 419))
pairs_items = rbind(pairs_items, c(404 , 420))
pairs_items = rbind(pairs_items, c(405 , 421))
pairs_items = rbind(pairs_items, c(406 , 422))
pairs_items = rbind(pairs_items, c(407 , 423))
pairs_items = rbind(pairs_items, c(408 , 424))


for(i in 1:nrow(pairs_items)){
  new_col_name = paste0("temp_adhd_ex_",i)
  e_1 = paste0("ksads_14_",pairs_items[i,1],"_p")
  e_2 = paste0("ksads_14_",pairs_items[i,2],"_p") 
  
  externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
  externalize_ksad_p[,new_col_name] = ifelse( (is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0))), 0, externalize_ksad_p[,new_col_name])
  print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}
#add the tri
externalize_ksad_p$temp_adhd_ex_11 = (externalize_ksad_p$ksads_14_88_p == 1 | externalize_ksad_p$ksads_14_89_p == 1 |externalize_ksad_p$ksads_14_90_p == 1) *1
summary(externalize_ksad_p[,c("temp_adhd_ex_11", "ksads_14_88_p", "ksads_14_89_p", "ksads_14_90_p")])

externalize_ksad_p$ksads_ADHD_exclude_attention_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_adhd_ex_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA
externalize_ksad_p$ksads_ADHD_exclude_attention_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_adhd_ex_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == 11] = NA


#ODD
pairs_items = c(91, 92)
pairs_items = rbind(pairs_items, c(93 , 94))
pairs_items = rbind(pairs_items, c(95 , 96))
pairs_items = rbind(pairs_items, c(432 , 439))
pairs_items = rbind(pairs_items, c(433 , 440))
pairs_items = rbind(pairs_items, c(434 , 441))
pairs_items = rbind(pairs_items, c(435 , 442))
pairs_items = rbind(pairs_items, c(436 , 443))
pairs_items = rbind(pairs_items, c(437 , 444))

for(i in 1:nrow(pairs_items)){
  new_col_name = paste0("temp_odd_",i)
  e_1 = paste0("ksads_15_",pairs_items[i,1],"_p")
  e_2 = paste0("ksads_15_",pairs_items[i,2],"_p") 
  
  externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
  externalize_ksad_p[,new_col_name] = ifelse( (is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0))), 0, externalize_ksad_p[,new_col_name])
  print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}

externalize_ksad_p$ksads_ODD_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_odd_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA
externalize_ksad_p$ksads_ODD_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_odd_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == 9] = NA

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

for(i in 1:nrow(pairs_items)){
  new_col_name = paste0("temp_conduct_",i)
  e_1 = paste0("ksads_16_",pairs_items[i,1],"_p")
  e_2 = paste0("ksads_16_",pairs_items[i,2],"_p") 
  
  externalize_ksad_p[,new_col_name] = (externalize_ksad_p[,e_1] == 1 | externalize_ksad_p[,e_2] == 1) *1
  externalize_ksad_p[,new_col_name] = ifelse( (is.na(externalize_ksad_p[,new_col_name]) & ((externalize_ksad_p[,e_1] == 0 | externalize_ksad_p[,e_2] == 0))), 0, externalize_ksad_p[,new_col_name])
  print(summary(externalize_ksad_p[,c(new_col_name, e_1 , e_2)]))
}

externalize_ksad_p$ksads_CONDUCT_symptoms_sum = rowSums(externalize_ksad_p[,which(grepl("temp_conduct_", colnames(externalize_ksad_p)))], na.rm = T)
#fix rows with all NA
externalize_ksad_p$ksads_CONDUCT_symptoms_sum[rowSums(is.na(externalize_ksad_p[,which(grepl("temp_conduct_", colnames(externalize_ksad_p), ignore.case=TRUE))])) == 15] = NA


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
write.csv(file = "suicide_2.01/outputs/externalize_ksad.csv",x = externalize_ksad, row.names=F, na = "")

#remove not relevant ODD items for ADHD only 
#ksads_15_446_p ksads_15_445_p ksads_15_438_p ksads_15_97_p
externalize_ksad_adhd = externalize_ksad[,!( grepl("ksads_15_(97|44[5-6]|438)_p", colnames(externalize_ksad)))]
write.csv(file = "suicide_2.01/outputs/externalize_ksad_adhd.csv",x = externalize_ksad_adhd, row.names=F, na = "")


