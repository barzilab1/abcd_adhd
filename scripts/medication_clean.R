library(readr)

source("config.R")

medsy01 = read.csv(file = paste0(medication_files_path, "medsy01.txt"), sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))


#remove details line
medsy01 = medsy01[-1,]

#remove odd names
ind = which(grepl("<span style=", medsy01$med1_rxnorm_p))
medsy01$med1_rxnorm_p[ind] = NA
ind = which(grepl("<span style=", medsy01$med_otc_1_rxnorm_p))
medsy01$med_otc_1_rxnorm_p[ind] = NA
medsy01 = droplevels(medsy01)

#remove columns introduced by NDA
medsy01 = medsy01[,!(names(medsy01) %in% c("medsy01_id", "collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name", "dataset_id"))]

#split the data according to baseline and follow_up
medsy01_base = medsy01[which(grepl("baseline", medsy01$eventname)),]
medsy01_follow_up = medsy01[which(grepl("follow_up", medsy01$eventname)),]

medsy01_base = droplevels(medsy01_base)
medsy01_follow_up = droplevels(medsy01_follow_up)

############# get a list of all meds in the dataset, according to the time point for tagging ###########
meds_base = meds_follow_up = otc_base = otc_follow_up = list()

for(i in 1:15){
  colname = paste0("med",i,"_rxnorm_p")
  colname_otc = paste0("med_otc_",i,"_rxnorm_p")
  
  #in 2.01 -> the name of the med was included
  #in 3.01 -> only the med number includded
  meds_base[[i]] =  levels(medsy01_base[,colname])
  meds_follow_up[[i]] =  levels(medsy01_follow_up[,colname])
  otc_base[[i]] = levels(medsy01_base[,colname_otc])
  otc_follow_up[[i]] =  levels(medsy01_follow_up[,colname_otc])
  
}

meds_base = unique(unlist(meds_base))
meds_follow_up = unique(unlist(meds_follow_up))
otc_base = unique(unlist(otc_base))
otc_follow_up = unique(unlist(otc_follow_up))

#bug while reading the txt file: fix numbers that include ".0" at the end of the number
meds_base = unique(ifelse(is.na(as.numeric(meds_base)), meds_base, as.numeric(meds_base)))
meds_follow_up = unique(ifelse(is.na(as.numeric(meds_follow_up)), meds_follow_up, as.numeric(meds_follow_up)))
otc_base = unique(ifelse(is.na(as.numeric(otc_base)), otc_base, as.numeric(otc_base)))
otc_follow_up = unique(ifelse(is.na(as.numeric(otc_follow_up)), otc_follow_up, as.numeric(otc_follow_up)))

##meds
med_only_base = setdiff(meds_base, meds_follow_up)
med_only_follow_up = setdiff(meds_follow_up ,meds_base)
med_both = intersect(meds_base, meds_follow_up)

#otc
otc_only_base = setdiff(otc_base, otc_follow_up)
otc_only_follow_up = setdiff(otc_follow_up ,otc_base)
otc_both = intersect(otc_base, otc_follow_up)


#create the meds list for tagging 
all_meds = cbind(med_both, rep(1,length(med_both)), rep(1,length(med_both)), rep("med",length(med_both)))
all_meds = rbind(all_meds, cbind(med_only_base, rep(1,length(med_only_base)), rep(0,length(med_only_base)), rep("med",length(med_only_base))))
all_meds = rbind(all_meds, cbind(med_only_follow_up, rep(0,length(med_only_follow_up)), rep(1,length(med_only_follow_up)), rep("med",length(med_only_follow_up))))

all_meds = rbind(all_meds, cbind(otc_both, rep(1,length(otc_both)), rep(1,length(otc_both)), rep("otc",length(otc_both))))
all_meds = rbind(all_meds, cbind(otc_only_base, rep(1,length(otc_only_base)), rep(0,length(otc_only_base)), rep("otc",length(otc_only_base))))
all_meds = rbind(all_meds, cbind(otc_only_follow_up, rep(0,length(otc_only_follow_up)), rep(1,length(otc_only_follow_up)), rep("otc",length(otc_only_follow_up))))

colnames(all_meds) = c("med", "baseline", "follow_up","type")


write.csv(file="outputs/medication_summary.csv",na.omit(all_meds),row.names=F)


#################### create the medication table according to Ran's tagging  ####################


med_dataset = medsy01[,grepl("(src|interview_a|gender|event|brou|_rxnorm_p)", colnames(medsy01))]
tagged_med = read_csv("input data/medication_tagged.csv")

#split the med name to get the numbers 
tagged_med$med_number = vapply(strsplit(tagged_med$med," "), `[`, 1, FUN.VALUE=character(1))

#check that all meds exist in Ran's coding
all_meds = as.data.frame(all_meds)
all_meds$med = as.character(all_meds$med)
setdiff(all_meds$med[all_meds$baseline == 1], tagged_med$med_number) #3 otc meds missing tag - no need in ADHD project


#add medication category to each child according to tagging
for(i in 2:13){
  
  #get med category
  colname = colnames(tagged_med)[i]
  
  #get the medications in the category
  meds = unique(tagged_med$med_number[which(tagged_med[,i] == 1)])
  
  #bug while reading the txt file: add ".0" to all meds numbers as it may appear in any of the two versions in the med_dataset
  meds = union(meds, paste0(meds,".0"))
  
  #mark the kids
  med_dataset[,colname] = 0
  med_dataset[apply(med_dataset, 1, function(r) any(r %in% meds)), colname] = 1
  
}


#fix the tagging for parents that refused to answer (brought_medications == 2) or NA
med_dataset[(med_dataset$brought_medications %in% c(2,NA)),colnames(tagged_med)[2:13]] = NA

med_dataset = med_dataset[med_dataset$eventname == "baseline_year_1_arm_1",]

#remove empty col
med_dataset = med_dataset[!sapply(med_dataset, function(x) all((x=="NA")|(is.na(x))))]

write.csv(file = paste0("outputs/meds_tagged.csv"),x = med_dataset ,row.names=F, na = "")





