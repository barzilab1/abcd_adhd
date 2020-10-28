library(readr)
#medsy01

medsy01 <- read.csv("abcd data/medsy01.txt", sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))

#remove details line
medsy01 = medsy01[-1,]

#remove odd names
ind = which(grepl("<span style=", medsy01$med1_rxnorm_p))
medsy01$med1_rxnorm_p[ind] = NA
medsy01 = droplevels(medsy01)

#remove columns introduced by NDA
medsy01 = medsy01[,!(names(medsy01) %in% c("medsy01_id", "collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name", "dataset_id"))]

#split the data according to baseline and first year time points
medsy01_base = medsy01[which(grepl("baseline", medsy01$eventname)),]
medsy01_one_year = medsy01[which(grepl("follow_up", medsy01$eventname)),]


############# get a list of all meds in the dataset, according to the time point
meds_base = list()
meds_follow_up = list()
otc_base = list()
otc_follow_up = list()

for(i in 1:15){
  colname = paste0("med",i,"_rxnorm_p")
  colname_otc = paste0("med_otc_",i,"_rxnorm_p")
  
  meds_base[[i]] =  as.character(unique(na.exclude(medsy01_base[,colname])))
  meds_follow_up[[i]] =  as.character(unique(na.exclude(medsy01_one_year[,colname])))
  otc_base[[i]] =  as.character(unique(na.exclude(medsy01_base[,colname_otc])))
  otc_follow_up[[i]] =  as.character(unique(na.exclude(medsy01_one_year[,colname_otc])))

}

meds_base = unique(unlist(meds_base))
meds_follow_up = unique(unlist(meds_follow_up))
otc_base = unique(unlist(otc_base))
otc_follow_up = unique(unlist(otc_follow_up))


##meds
med_only_base = setdiff(meds_base, meds_follow_up)
med_only_follow_up = setdiff(meds_follow_up ,meds_base)
med_both = intersect(meds_base, meds_follow_up)

#otc
otc_only_base = setdiff(otc_base, otc_follow_up)
otc_only_follow_up = setdiff(otc_follow_up ,otc_base)
otc_both = intersect(otc_base, otc_follow_up)

all_meds = matrix(ncol = 4)
colnames(all_meds) = c("med", "baseline", "1_year_follow_up","type")

all_meds = cbind(med_both, rep(1,length(med_both)),rep(1,length(med_both)), rep("med",length(med_both)))
all_meds = rbind(all_meds, cbind(med_only_base, rep(1,length(med_only_base)),rep(0,length(med_only_base)), rep("med",length(med_only_base))))
all_meds = rbind(all_meds, cbind(med_only_follow_up, rep(0,length(med_only_follow_up)),rep(1,length(med_only_follow_up)), rep("med",length(med_only_follow_up))))

all_meds = rbind(all_meds, cbind(otc_both, rep(1,length(otc_both)),rep(1,length(otc_both)), rep("otc",length(otc_both))))
all_meds = rbind(all_meds, cbind(otc_only_base, rep(1,length(otc_only_base)),rep(0,length(otc_only_base)), rep("otc",length(otc_only_base))))
all_meds = rbind(all_meds, cbind(otc_only_follow_up, rep(0,length(otc_only_follow_up)),rep(1,length(otc_only_follow_up)), rep("otc",length(otc_only_follow_up))))


write.csv(file="outputs/medication_summary.csv",na.omit(all_meds),row.names=F)




#################### create the medication table according to Ran's tagging  

med_dataset = medsy01[,grepl("(src|interview_a|gender|event|brou|_rxnorm_p)", colnames(medsy01))]
Ran_coding <- read_csv("data/medication_summary_Ran_coding.csv")

#add medication category to each child according to Ran
for(i in 2:13){
  
  #get med category
  colname = colnames(Ran_coding)[i]
  
  #get the medications in the category
  meds = Ran_coding$med[which(Ran_coding[,i] == 1)]
  
  #mark the kids
  med_dataset[,colname] = 0
  med_dataset[apply(med_dataset, 1, function(r) any(r %in% meds)),colname] = 1
  
}

#remove empty col
med_dataset = med_dataset[!sapply(med_dataset, function(x) all((x=="NA")|(is.na(x))))]

#fix for parents that refused to answer (brought_medications == 2 or NA)
med_dataset[(med_dataset$brought_medications %in% c(2,NA)), colnames(Ran_coding)[2:13]] = NA

write.csv("data/meds_tagged.csv",x = med_dataset,row.names=F, na = "")



